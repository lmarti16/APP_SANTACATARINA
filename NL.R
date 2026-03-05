# app_santa_catarina.R ---------------------------------------------------------
# SANTA CATARINA, NL · Explorador electoral por sección
# ── v2.0 ──────────────────────────────────────────────
# - Login con shinymanager (admin/123, user/User2025!#)
# - Años reales desde columnas ANIO_*
# - Overlays filtrados por atributo (no geometría)
# - Mapa Tiempo sin vibración
# - UI glassmorphism + animaciones
# - Geometrías simplificadas para rendimiento

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(sf)
  library(leaflet)
  library(bslib)
  library(plotly)
  library(DT)
  library(stringr)
  library(htmltools)
  library(dplyr)
  library(ggmap)
  library(shinymanager)
})

sf::sf_use_s2(FALSE)

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

withSpinner <- function(x, ...) x
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  withSpinner <- shinycssloaders::withSpinner
}
has_fullscreen <- requireNamespace("leaflet.extras", quietly = TRUE)
has_ggmap      <- requireNamespace("ggmap", quietly = TRUE)

# ==========================================================
# CREDENCIALES LOGIN
# ==========================================================
credentials <- data.frame(
  user     = c("admin", "user", "ush", "lezama"),
  password = c("123", "User2025!#", "ush2025", "lezama2025"),
  admin    = c(TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# ── Credenciales desde .Renviron ─────────────────────────────
GOOGLE_API_KEY      <- Sys.getenv("GOOGLE_API_KEY", unset = "")
OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY", unset = "")
AWS_ACCESS_KEY_ID   <- Sys.getenv("AWS_ACCESS_KEY_ID", unset = "")
AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
AWS_DEFAULT_REGION  <- Sys.getenv("AWS_DEFAULT_REGION", unset = "")

# ==========================================================
# 0) PATHS
# ==========================================================
PATH_CSV  <- "DATA_SANTACATARINA/full_secciones_24__UNIVERSO_SC_NL.csv"
PATH_SECC <- "DATA_SANTACATARINA/Secciones_19__Secciones_19__UNIVERSO_SC_NL.gpkg"
PATH_MUN  <- "DATA_SANTACATARINA/MUN_MAP__MAP_MUN__UNIVERSO_SC_NL.gpkg"
PATH_DF   <- "DATA_SANTACATARINA/DF_MAP__DF_MEX_CLEAN__UNIVERSO_SC_NL.gpkg"
PATH_DL   <- "DATA_SANTACATARINA/DL_MAP__DL_MEX_CLEAN__UNIVERSO_SC_NL.gpkg"
PATH_TRAD <- "DATA_SANTACATARINA/TRADUCTOR.csv"

# ==========================================================
# TOLERANCIA PARA SIMPLIFICACIÓN DE GEOMETRÍAS
# ==========================================================
SIMPLIFY_TOL <- 0.0003

# ==========================================================
# 1) ESTILO Y COLORES — TEMA MODERNO GLASSMORPHISM
# ==========================================================
APP_TITLE  <- "Santa Catarina \u00b7 NL"
APP_SUB    <- "Explorador electoral por secci\u00f3n"
ACCENT     <- "#1A73E8"
ACCENT2    <- "#E8F0FE"
ACCENT_RGB <- "26,115,232"
BG         <- "#F0F2F5"
CARD_BG    <- "rgba(255,255,255,0.72)"
CARD_SOLID <- "#FFFFFF"
BORDER     <- "rgba(218,220,224,0.55)"
TXT        <- "#1A1D21"
TXT_SEC    <- "#5F6368"
MUTED      <- "#80868B"
SUCCESS    <- "#1E8E3E"
DANGER     <- "#D93025"
DL_GROUP   <- "Distritos locales"
MUN_GROUP  <- "Municipio"
DF_GROUP   <- "Distritos federales"

BASEMAPS <- c(
  "Claro"        = "CartoDB.Positron",
  "Oscuro"       = "CartoDB.DarkMatter",
  "Calles"       = "OpenStreetMap.Mapnik",
  "Sat\u00e9lite" = "Esri.WorldImagery"
)
BASEMAP_DEFAULT <- "Claro"

party_colors <- c(
  PAN    = "#005BAC",
  PRI    = "#D50000",
  PRD    = "#FFD200",
  PVEM   = "#2EAD4A",
  PT     = "#E2001A",
  MC     = "#FF6A00",
  MORENA = "#7A013A",
  PANAL  = "#00AEEF",
  NAEM   = "#00B5E2",
  PES    = "#5B2C83",
  ES     = "#EC008C",
  FXM    = "#9B1B72",
  RSP    = "#C71585",
  SI     = "#00C1B2",
  VIDA   = "#6D28D9"
)

PARTY_SET <- unique(c(
  names(party_colors), "PVEM", "PT", "MC", "MORENA",
  "PES", "RSP", "FXM", "VIDA"
))

# Partidos individuales (sin coaliciones) para vista PURO
PARTY_SET_PURO <- unique(c(
  "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA",
  "PANAL", "NAEM", "PES", "ES", "FXM", "RSP", "SI", "VIDA"
))

# ── Logos de partidos desde www/ ──────────────────
# Se autodetectan al arrancar la app
party_logo_map <- c(
  PAN    = "PAN.jpg",
  PRI    = "PRI.jpg",
  PRD    = "PRD.jpg",
  PVEM   = "PVEM.jpg",
  PT     = "PT.jpg",
  MC     = "MC.jpg",
  MORENA = "MORENA.jpg",
  PANAL  = "PANAL.png",
  NAEM   = "NAEM.png",
  PES    = "PES.png",
  FXM    = "FXM.png",
  RSP    = "RSP.png",
  VIDA   = "VIDA.jpg",
  ES     = "ESO.jpg",
  SI     = "PSI.png"
)

# Verificar qué logos existen realmente en www/
www_files <- character(0)
if (dir.exists("www")) {
  www_files <- list.files("www", pattern = "\\.(jpg|jpeg|png|svg)$", ignore.case = TRUE)
  party_logo_map <- party_logo_map[party_logo_map %in% www_files]
  # Auto-detectar logos adicionales por nombre de partido
  for (p in PARTY_SET) {
    if (!(p %in% names(party_logo_map))) {
      candidates <- www_files[toupper(tools::file_path_sans_ext(www_files)) == toupper(p)]
      if (length(candidates) > 0) party_logo_map[[p]] <- candidates[1]
    }
  }
  # Auto-detectar logos de coaliciones desde www/ (ej: PAN_PRI_PRD.png, PVEM_PT_MORENA.jpg)
  all_www_names <- toupper(tools::file_path_sans_ext(www_files))
  for (i in seq_along(www_files)) {
    nm <- all_www_names[i]
    if (grepl("_", nm) && !(nm %in% names(party_logo_map))) {
      party_logo_map[[nm]] <- www_files[i]
    }
  }
}

party_logo_src <- function(party) {
  party <- toupper(party %||% "")
  if (party %in% names(party_logo_map)) return(party_logo_map[[party]])
  # Intentar variantes: sin espacios, con/sin underscores
  party_under <- gsub("[[:space:]]", "_", party)
  if (party_under %in% names(party_logo_map)) return(party_logo_map[[party_under]])
  party_nospace <- gsub("[[:space:]]", "", party)
  if (party_nospace %in% names(party_logo_map)) return(party_logo_map[[party_nospace]])
  NULL
}

# Descomponer coalición en partidos individuales
decompose_coalition <- function(party) {
  party <- toupper(party %||% "")
  # Separar por _, -, espacio
  parts <- unlist(strsplit(party, "[_\\-\\s]+"))
  parts <- parts[nzchar(parts)]
  if (length(parts) <= 1) return(NULL)
  parts
}

# Obtener logos individuales de una coalición
coalition_logo_parts <- function(party) {
  parts <- decompose_coalition(party)
  if (is.null(parts)) return(NULL)
  srcs <- character(0)
  for (p in parts) {
    s <- party_logo_src(p)
    if (!is.null(s)) srcs <- c(srcs, s)
  }
  if (length(srcs) == 0) return(NULL)
  srcs
}

party_logo_tag <- function(party, height = "22px", style = "") {
  src <- party_logo_src(party)
  if (!is.null(src)) {
    return(tags$img(
      src = src, height = height,
      style = paste0("border-radius:4px; object-fit:contain; vertical-align:middle; ",
                     "box-shadow:0 1px 3px rgba(0,0,0,.1); ", style),
      alt = party
    ))
  }
  # Intentar coalición compuesta
  parts <- coalition_logo_parts(party)
  if (!is.null(parts)) {
    h_num <- as.integer(gsub("\\D", "", height))
    h_each <- paste0(max(12, h_num - 4), "px")
    logos <- lapply(parts, function(s) {
      tags$img(src = s, height = h_each,
               style = "border-radius:3px; object-fit:contain; vertical-align:middle; box-shadow:0 1px 2px rgba(0,0,0,.08); margin-right:1px;",
               alt = party)
    })
    return(div(style = "display:inline-flex; align-items:center; gap:1px;", logos))
  }
  NULL
}

party_logo_inline <- function(party, height = "18px") {
  src <- party_logo_src(party)
  if (!is.null(src)) {
    return(paste0('<img src="', src, '" height="', height,
                  '" style="border-radius:3px;object-fit:contain;vertical-align:middle;',
                  'box-shadow:0 1px 2px rgba(0,0,0,.08);margin-right:4px;" alt="', party, '">'))
  }
  # Intentar coalición compuesta
  parts <- coalition_logo_parts(party)
  if (!is.null(parts)) {
    h_num <- as.integer(gsub("\\D", "", height))
    h_each <- paste0(max(10, h_num - 4), "px")
    imgs <- paste0(sapply(parts, function(s) {
      paste0('<img src="', s, '" height="', h_each,
             '" style="border-radius:2px;object-fit:contain;vertical-align:middle;',
             'box-shadow:0 1px 2px rgba(0,0,0,.06);margin-right:1px;" alt="">')
    }), collapse = "")
    return(imgs)
  }
  ""
}

party_logos_strip <- function(parties, height = "20px") {
  parties <- parties[!is.na(parties) & nzchar(parties)]
  if (length(parties) == 0) return(NULL)
  items <- lapply(parties, function(p) {
    logo <- party_logo_tag(p, height = height)
    col  <- if (p %in% names(party_colors)) unname(party_colors[[p]]) else "#9AA0A6"
    div(
      style = paste0(
        "display:inline-flex; align-items:center; gap:4px; padding:3px 8px; ",
        "border-radius:8px; background:rgba(0,0,0,.03); font-size:11px; font-weight:700; ",
        "color:", col, ";"
      ),
      logo, if (is.null(logo)) span(p) else NULL
    )
  })
  div(style = "display:flex; flex-wrap:wrap; gap:5px; margin:6px 0;", items)
}

office_labels <- c(
  ALC  = "Ayuntamiento",
  DL   = "Dip. local",
  DF   = "Dip. federal",
  GOB  = "Gubernatura",
  SEN  = "Senado",
  PRES = "Presidencia"
)

label_office <- function(x) office_labels[[x]] %||% x

guess_party_for_color <- function(label) {
  up  <- toupper(label %||% "")
  hit <- PARTY_SET[vapply(PARTY_SET, function(p) grepl(paste0("\\b", p, "\\b"), up), logical(1))]
  hit[1] %||% NA_character_
}

# Intensificar un color hex (más saturado/oscuro)
intensify_hex <- function(hex, factor = 0.25) {
  r <- strtoi(substr(hex, 2, 3), 16L)
  g <- strtoi(substr(hex, 4, 5), 16L)
  b <- strtoi(substr(hex, 6, 7), 16L)
  # Mezclar hacia negro para intensificar
  r <- max(0, round(r * (1 - factor)))
  g <- max(0, round(g * (1 - factor)))
  b <- max(0, round(b * (1 - factor)))
  sprintf("#%02X%02X%02X", r, g, b)
}

fill_color_winner <- function(w) {
  up <- toupper(w %||% "")
  # Primero: ¿es una coalición (tiene _ o múltiples partidos)?
  parts <- decompose_coalition(up)
  if (!is.null(parts) && length(parts) >= 2) {
    for (pt in parts) {
      if (pt %in% names(party_colors)) {
        return(intensify_hex(unname(party_colors[[pt]]), 0.2))
      }
    }
  }
  # Partido individual
  if (up %in% names(party_colors)) return(unname(party_colors[[up]]))
  p <- guess_party_for_color(up)
  if (!is.na(p) && p %in% names(party_colors)) return(unname(party_colors[[p]]))
  "#9AA0A6"
}

# ==========================================================
# 2) HELPERS BASE
# ==========================================================
pick_col <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) NA_character_ else hit[1L]
}

safe_make_valid <- function(x) tryCatch(st_make_valid(x), error = function(e) x)

safe_simplify <- function(x, tol = SIMPLIFY_TOL) {
  tryCatch({
    x_simple <- st_simplify(x, preserveTopology = TRUE, dTolerance = tol)
    safe_make_valid(x_simple)
  }, error = function(e) x)
}

read_csv_flex <- function(path) {
  for (enc in c("UTF-8", "Latin-1")) {
    out <- tryCatch(fread(path, encoding = enc), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  fread(path)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = 0, big.mark = ","))
}

fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", paste0(formatC(100 * x, format = "f", digits = 2), "%"))
}

fmt_signed_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x), "\u2014",
    paste0(ifelse(x >= 0, "+", "\u2212"),
           formatC(abs(x), format = "f", digits = 0, big.mark = ","))
  )
}

fmt_signed_pp <- function(x_pp) {
  x_pp <- suppressWarnings(as.numeric(x_pp))
  ifelse(
    is.na(x_pp), "\u2014",
    paste0(ifelse(x_pp >= 0, "+", "\u2212"),
           formatC(abs(x_pp), format = "f", digits = 2), " pp")
  )
}

safe_num_matrix <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (!length(cols)) return(NULL)
  dat0 <- as.data.frame(st_drop_geometry(df))
  M    <- as.matrix(dat0[, cols, drop = FALSE])
  suppressWarnings(storage.mode(M) <- "numeric")
  M[is.na(M)] <- 0
  M
}

hex_mix <- function(a, b, t = 0.5) {
  a <- grDevices::col2rgb(a) / 255
  b <- grDevices::col2rgb(b) / 255
  x <- (1 - t) * a + t * b
  grDevices::rgb(x[1, 1], x[2, 1], x[3, 1], maxColorValue = 1)
}

safe_centroids_wgs84 <- function(x) {
  if (is.null(x) || NROW(x) == 0L) return(x)
  x2  <- tryCatch(st_transform(x, 4326), error = function(e) x)
  ctr <- suppressWarnings(st_centroid(x2))
  if (is.na(st_crs(ctr))) st_crs(ctr) <- 4326
  ctr
}

coords_from_sf <- function(x) {
  if (is.null(x) || NROW(x) == 0L)
    return(data.frame(LNG = numeric(0), LAT = numeric(0)))
  ctr <- safe_centroids_wgs84(x)
  xy  <- st_coordinates(ctr)
  data.frame(
    LNG = suppressWarnings(as.numeric(xy[, "X"])),
    LAT = suppressWarnings(as.numeric(xy[, "Y"]))
  )
}

has_httr <- requireNamespace("httr", quietly = TRUE)

# ── Geocodificación robusta con Google API directa ──────
google_reverse_geocode <- function(lat, lng,
                                   api_key  = GOOGLE_API_KEY,
                                   language = GOOGLE_GEOCODE_LANGUAGE) {
  if (!is.finite(lat) || !is.finite(lng)) return(NA_character_)
  if (!nzchar(api_key) || identical(api_key, "AQUI_TU_API_KEY_GOOGLE")) return(NA_character_)
  
  # Intentar con httr primero (más confiable)
  if (has_httr) {
    addr <- tryCatch({
      url <- paste0(
        "https://maps.googleapis.com/maps/api/geocode/json",
        "?latlng=", format(lat, scientific = FALSE, digits = 8),
        ",", format(lng, scientific = FALSE, digits = 8),
        "&key=", api_key,
        "&language=", language
      )
      resp <- httr::GET(url, httr::timeout(12))
      if (httr::status_code(resp) == 200) {
        body <- httr::content(resp, as = "parsed")
        if (!is.null(body$status) && body$status == "OK" && length(body$results) > 0) {
          # Buscar el resultado más específico (street_address > route > neighborhood)
          best <- NULL
          for (res in body$results) {
            types <- unlist(res$types)
            if ("street_address" %in% types) { best <- res$formatted_address; break }
            if (is.null(best) && any(c("route", "premise", "subpremise") %in% types))
              best <- res$formatted_address
            if (is.null(best) && any(c("neighborhood", "sublocality") %in% types))
              best <- res$formatted_address
          }
          if (is.null(best)) best <- body$results[[1]]$formatted_address
          best
        } else NA_character_
      } else NA_character_
    }, error = function(e) NA_character_)
    
    if (!is.na(addr) && nzchar(addr)) return(addr)
  }
  
  # Fallback a ggmap
  if (has_ggmap) {
    tryCatch({
      if (!exists(".ggmap_registered", envir = .GlobalEnv)) {
        ggmap::register_google(key = api_key, write = FALSE)
        assign(".ggmap_registered", TRUE, envir = .GlobalEnv)
      }
      out <- ggmap::revgeocode(
        location = c(lng, lat), output = "address",
        force = TRUE, language = language
      )
      as.character(out[1] %||% NA_character_)
    }, error = function(e) NA_character_)
  } else {
    NA_character_
  }
}

# Wrapper con delay para múltiples puntos (evitar rate limit)
batch_reverse_geocode <- function(lats, lngs, api_key = GOOGLE_API_KEY,
                                  delay = 0.12) {
  n <- length(lats)
  addresses <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    addresses[i] <- google_reverse_geocode(lats[i], lngs[i], api_key)
    if (i < n) Sys.sleep(delay)
  }
  addresses
}

# ==========================================================
# 3) CARGA DE DATOS
# ==========================================================
load_data <- function() {
  base_dt <- read_csv_flex(PATH_CSV)
  
  if (!("SECCION" %in% names(base_dt)))
    stop("El CSV no trae columna SECCION.", call. = FALSE)
  
  base_dt[, SECCION := as.integer(SECCION)]
  base_dt <- base_dt[!is.na(SECCION)]
  base_dt <- unique(base_dt, by = "SECCION")
  secs_keep <- sort(unique(base_dt$SECCION))
  
  sec_sf <- st_read(PATH_SECC, quiet = TRUE) |> safe_make_valid()
  nms    <- names(sec_sf)
  col_sec <- pick_col(nms, c("SECCION", "SECC", "SECCION_ID"))
  if (is.na(col_sec))
    stop("El GPKG no trae una columna SECCION/SECC/SECCION_ID.", call. = FALSE)
  
  sec_sf[[col_sec]] <- as.integer(sec_sf[[col_sec]])
  sec_sf <- sec_sf[!is.na(sec_sf[[col_sec]]) & sec_sf[[col_sec]] %in% secs_keep, , drop = FALSE]
  if (nrow(sec_sf) == 0L)
    stop("El GPKG no comparte secciones con el CSV.", call. = FALSE)
  
  sec_sf <- sec_sf[!duplicated(sec_sf[[col_sec]]), ]
  sec_sf <- tryCatch(st_transform(sec_sf, 4326), error = function(e) sec_sf)
  if (is.na(st_crs(sec_sf))) st_crs(sec_sf) <- 4326
  sec_sf <- safe_simplify(sec_sf)
  
  sec_dt <- as.data.table(st_drop_geometry(sec_sf))
  setnames(sec_dt, col_sec, "SECCION")
  sec_dt[, SECCION := as.integer(SECCION)]
  sec_dt[, geom := st_geometry(sec_sf)]
  setkey(sec_dt, SECCION)
  setkey(base_dt, SECCION)
  
  dt     <- merge(sec_dt, base_dt, by = "SECCION", all = FALSE)
  sf_all <- st_as_sf(as.data.frame(dt), sf_column_name = "geom", crs = st_crs(sec_sf))
  class(sf_all) <- setdiff(class(sf_all), "data.table")
  
  dl_col <- pick_col(names(sf_all), c("DISTRITO_LOCAL", "DISTRITO_L", "ID_DISTRITO_LOCAL", "DIST_L"))
  if (!is.na(dl_col)) {
    suppressWarnings(sf_all[[dl_col]] <- as.integer(sf_all[[dl_col]]))
    sf_all$DISTRITO_L <- sf_all[[dl_col]]
  }
  
  # Cargar capa DL desde el GPKG dedicado (no construir desde secciones)
  dl_sf <- NULL
  if (!is.null(PATH_DL) && file.exists(PATH_DL)) {
    dl_sf <- tryCatch({
      lyr <- st_read(PATH_DL, quiet = TRUE) |> safe_make_valid()
      lyr <- tryCatch(st_transform(lyr, 4326), error = function(e) lyr)
      if (is.na(st_crs(lyr))) st_crs(lyr) <- 4326
      # Detectar columna de número de distrito
      dl_id_col <- pick_col(names(lyr), c("DISTRITO_L", "DISTRITO_LOCAL", "DISTRITO", "ID_DL"))
      if (!is.na(dl_id_col)) {
        suppressWarnings(lyr[["DISTRITO_L"]] <- as.integer(lyr[[dl_id_col]]))
      }
      # Detectar columna de nombre
      dl_nm_col <- pick_col(names(lyr), c("NOMBRE_DISTRITO_LOCAL", "NOM_DISTRITO_LOCAL",
                                          "NOMGEO", "NOMBRE", "NOM_DL"))
      if (!is.na(dl_nm_col) && dl_nm_col != "NOMBRE_DISTRITO_LOCAL") {
        lyr[["NOMBRE_DISTRITO_LOCAL"]] <- lyr[[dl_nm_col]]
      }
      safe_simplify(lyr)
    }, error = function(e) {
      message("Aviso: no se pudo cargar DL GPKG: ", conditionMessage(e))
      NULL
    })
  }
  # Fallback: construir desde secciones si el GPKG no se cargó
  if (is.null(dl_sf) && "DISTRITO_L" %in% names(sf_all)) {
    tmp <- sf_all |> filter(!is.na(DISTRITO_L))
    if (nrow(tmp) > 0) {
      dl_name_col <- pick_col(names(tmp), c("NOMBRE_DISTRITO_LOCAL", "NOM_DISTRITO_LOCAL"))
      if (!is.na(dl_name_col)) {
        dl_sf <- suppressWarnings(
          tmp |>
            group_by(DISTRITO_L) |>
            summarise(NOMBRE_DISTRITO_LOCAL = dplyr::first(.data[[dl_name_col]]),
                      .groups = "drop") |> safe_make_valid()
        )
      } else {
        dl_sf <- suppressWarnings(
          tmp |> group_by(DISTRITO_L) |> summarise(.groups = "drop") |> safe_make_valid()
        )
      }
      dl_sf <- safe_simplify(dl_sf)
    }
  }
  
  list(sf = sf_all, dl = dl_sf)
}

dat    <- load_data()
sf_all <- dat$sf
dl_sf  <- dat$dl

# ── TRADUCTOR: catálogo de variables INEGI ────────
TRADUCTOR <- tryCatch({
  trad <- read_csv_flex(PATH_TRAD)
  if (!all(c("Eje", "Indicador", "VARIABLE") %in% names(trad)))
    stop("Falta columna Eje/Indicador/VARIABLE")
  as.data.frame(trad)
}, error = function(e) {
  message("Aviso: no se pudo cargar TRADUCTOR: ", conditionMessage(e))
  data.frame(Eje = character(0), Indicador = character(0), VARIABLE = character(0),
             stringsAsFactors = FALSE)
})

# Detectar columnas _INEGI en sf_all y matchear con TRADUCTOR
INEGI_COLS <- grep("_INEGI$", names(sf_all), value = TRUE)
INEGI_VARS <- sub("_INEGI$", "", INEGI_COLS)
# Mapeo: VARIABLE -> columna en sf_all
INEGI_COL_MAP <- setNames(INEGI_COLS, INEGI_VARS)

# Filtrar TRADUCTOR solo a variables que existen en los datos
if (NROW(TRADUCTOR) > 0) {
  TRADUCTOR <- TRADUCTOR[TRADUCTOR$VARIABLE %in% INEGI_VARS, ]
  TRADUCTOR$COL_NAME <- INEGI_COL_MAP[TRADUCTOR$VARIABLE]
}

# Ejes disponibles (ordenados)
EJES_DISPONIBLES <- if (NROW(TRADUCTOR) > 0) unique(TRADUCTOR$Eje) else character(0)
message(">> INEGI: ", length(INEGI_COLS), " columnas, ", NROW(TRADUCTOR),
        " en traductor, ", length(EJES_DISPONIBLES), " ejes")

INIT_BBOX <- sf::st_bbox(sf_all)
INIT_LNG  <- as.numeric((INIT_BBOX["xmin"] + INIT_BBOX["xmax"]) / 2)
INIT_LAT  <- as.numeric((INIT_BBOX["ymin"] + INIT_BBOX["ymax"]) / 2)
INIT_ZOOM <- 12L

SECC_DL_COL      <- if ("DISTRITO_L" %in% names(sf_all)) "DISTRITO_L" else NA_character_
SECC_DL_NAME_COL <- pick_col(names(sf_all), c("NOMBRE_DISTRITO_LOCAL", "NOM_DISTRITO_LOCAL"))
SECC_MUN_COL     <- pick_col(names(sf_all), c("NOMBRE_MUNICIPIO", "NOM_MUN", "MUNICIPIO"))
SECC_DF_COL      <- pick_col(names(sf_all), c("DISTRITO_FEDERAL", "DISTRITO_F", "DIST_F", "DF_NUM"))
SECC_DF_NAME_COL <- pick_col(names(sf_all), c("NOMBRE_DISTRITO_FEDERAL", "NOM_DF"))

# Capas auxiliares
load_aux_layer <- function(path, label) {
  if (is.null(path)) return(NULL)
  tryCatch({
    lyr <- st_read(path, quiet = TRUE) |> safe_make_valid()
    lyr <- tryCatch(st_transform(lyr, 4326), error = function(e) lyr)
    if (is.na(st_crs(lyr))) st_crs(lyr) <- 4326
    lyr <- safe_simplify(lyr)
    lyr
  }, error = function(e) {
    message("Aviso: no se pudo cargar ", label, ": ", conditionMessage(e))
    NULL
  })
}

mun_sf <- load_aux_layer(PATH_MUN, "municipios")
dfe_sf <- load_aux_layer(PATH_DF, "distritos federales")

# ==========================================================
# 4) DETECCIÓN DE ELECCIONES + AÑOS REALES DESDE ANIO_*
# ==========================================================
detect_elections <- function(nms) {
  m <- stringr::str_match(nms, "_(ALC|DL|DF|GOB|SEN|PRES)_(\\d{2})$")
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0L) return(data.table(office = character(0), yr2 = character(0)))
  unique(data.table(office = m[, 2], yr2 = m[, 3]))
}

elex <- detect_elections(names(sf_all))
if (nrow(elex) == 0L)
  stop("No detect\u00e9 columnas de elecci\u00f3n con patr\u00f3n *_ALC_24, etc.", call. = FALSE)

# ── Años reales desde columnas ANIO_<OFFICE>_<YR2> ──────
resolve_real_year <- function(office, yr2, df) {
  anio_col <- paste0("ANIO_", office, "_", yr2)
  fallback <- as.integer(paste0("20", yr2))
  if (anio_col %in% names(df)) {
    vals <- suppressWarnings(as.integer(df[[anio_col]]))
    vals <- vals[!is.na(vals) & vals > 1900 & vals < 2100]
    if (length(vals) > 0) return(as.integer(names(sort(table(vals), decreasing = TRUE))[1]))
  }
  fallback
}

elex[, year := mapply(resolve_real_year, office, yr2,
                      MoreArgs = list(df = as.data.frame(st_drop_geometry(sf_all))))]
elex[, year := as.integer(year)]
elex[, office_lbl := vapply(office, label_office, character(1))]
elex[, key        := paste0(office, "_", yr2)]
elex[, label      := paste0(year, " \u00b7 ", office_lbl)]
elex[, office_ord := match(office, c("PRES", "GOB", "SEN", "DF", "DL", "ALC"))]
setorder(elex, year, office_ord)

ELECTION_CHOICES <- setNames(elex$key, elex$label)

DEFAULT_ELECTION <- {
  if ("ALC_24" %in% elex$key) "ALC_24"
  else if ("DL_24" %in% elex$key) "DL_24"
  else tail(elex$key, 1L)
}

parse_key <- function(key) {
  sp <- strsplit(key, "_", fixed = TRUE)[[1]]
  list(
    office = sp[1] %||% "ALC",
    yr2    = sp[2] %||% "24",
    year   = elex$year[match(key, elex$key)] %||% as.integer(paste0("20", sp[2] %||% "24"))
  )
}

key_label <- function(key) elex$label[match(key, elex$key)] %||% key

# ==========================================================
# 5) HELPERS DE MÉTRICAS Y VOTOS
# ==========================================================
metric_col <- function(df, key, bases) {
  k     <- parse_key(key)
  cands <- unlist(lapply(bases, function(b) c(paste0(b, "_", k$office, "_", k$yr2), b)))
  cands <- unique(cands)
  hit   <- cands[cands %in% names(df)]
  hit[1] %||% NA_character_
}

total_col <- function(df, key) metric_col(df, key, c("VTE", "TOTAL_VOTOS", "NUM_VOTOS_VALIDOS"))
ln_col    <- function(df, key) metric_col(df, key, c("LISTA_NOMINAL"))
valid_col <- function(df, key) metric_col(df, key, c("NUM_VOTOS_VALIDOS", "VOTOS_VALIDOS", "VTE", "TOTAL_VOTOS"))
cas_col   <- function(df, key) metric_col(df, key, c("CASILLAS"))
nulos_col <- function(df, key) metric_col(df, key, c("NULOS", "NUM_VOTOS_NULOS"))

party_from_col <- function(col, key, vote_type) {
  k <- parse_key(key)
  if (vote_type == "CAND") {
    x <- sub(paste0("_", k$office, "_", k$yr2, "$"), "", col)
    x <- sub("^CAND_", "", x)
    return(x)  # Mantener underscores (PAN_PRD_PRI) para que matcheen con logos
  }
  if (vote_type == "DISTRIBUIDO") {
    return(sub(paste0("_DISTRIBUIDO_", k$office, "_", k$yr2, "$"), "", col))
  }
  sub(paste0("_", k$office, "_", k$yr2, "$"), "", col)
}

vote_cols_raw <- function(df, key, vote_type) {
  k   <- parse_key(key)
  nms <- names(df)
  if (vote_type == "CAND") {
    return(grep(paste0("^CAND_.*_", k$office, "_", k$yr2, "$"), nms, value = TRUE))
  }
  if (vote_type == "DISTRIBUIDO") {
    cols <- grep(paste0("^[A-Z0-9]+_DISTRIBUIDO_", k$office, "_", k$yr2, "$"), nms, value = TRUE)
    labs <- vapply(cols, party_from_col, character(1), key = key, vote_type = vote_type)
    return(cols[labs %in% PARTY_SET])
  }
  # PURO: columnas PARTIDO_OFFICE_YR2 que NO contengan DISTRIBUIDO ni CAND
  cols <- grep(paste0("^[A-Z0-9]+_", k$office, "_", k$yr2, "$"), nms, value = TRUE)
  # Excluir las que tienen DISTRIBUIDO o CAND en el nombre
  cols <- cols[!grepl("DISTRIBUIDO", cols, fixed = TRUE)]
  cols <- cols[!grepl("^CAND_", cols)]
  if (!length(cols)) return(character(0))
  labs <- vapply(cols, party_from_col, character(1), key = key, vote_type = vote_type)
  # Solo partidos individuales para PURO
  cols[labs %in% PARTY_SET_PURO]
}

group_votes_matrix <- function(df, key, vote_type) {
  cols <- vote_cols_raw(df, key, vote_type)
  if (!length(cols)) return(list(G = NULL, labels = character(0), cols = character(0)))
  
  M <- safe_num_matrix(df, cols)
  if (is.null(M)) return(list(G = NULL, labels = character(0), cols = character(0)))
  
  labs <- vapply(cols, party_from_col, character(1), key = key, vote_type = vote_type)
  labs[is.na(labs) | !nzchar(labs)] <- "OTROS"
  
  idx_list <- split(seq_along(cols), labs)
  G <- vapply(
    idx_list,
    function(idx) rowSums(M[, idx, drop = FALSE], na.rm = TRUE),
    numeric(nrow(df))
  )
  if (!is.matrix(G)) G <- matrix(G, nrow = nrow(df))
  colnames(G) <- names(idx_list)
  
  list(G = G, labels = colnames(G), cols = cols)
}

winner_by_row <- function(df, key, vote_type) {
  gv <- group_votes_matrix(df, key, vote_type)
  G  <- gv$G
  if (is.null(G) || ncol(G) == 0L) return(rep(NA_character_, nrow(df)))
  idx <- max.col(G, ties.method = "first")
  w   <- colnames(G)[idx]
  w[rowSums(G, na.rm = TRUE) <= 0] <- NA_character_
  w
}

totals_for_view <- function(df, key, vote_type) {
  gv <- group_votes_matrix(df, key, vote_type)
  G  <- gv$G
  if (is.null(G) || ncol(G) == 0L) return(setNames(numeric(0), character(0)))
  tot <- colSums(G, na.rm = TRUE)
  tot <- tot[is.finite(tot) & tot > 0]
  if (!length(tot)) return(setNames(numeric(0), character(0)))
  sort(tot, decreasing = TRUE)
}

top2_from_totals <- function(tot) {
  tot <- tot[is.finite(tot) & tot > 0]
  if (!length(tot)) return(list(w1 = NA_character_, v1 = NA_real_, w2 = NA_character_, v2 = NA_real_))
  tot <- sort(tot, decreasing = TRUE)
  list(
    w1 = names(tot)[1] %||% NA_character_, v1 = as.numeric(tot[[1]] %||% NA_real_),
    w2 = names(tot)[2] %||% NA_character_, v2 = as.numeric(tot[[2]] %||% NA_real_)
  )
}

sum_valid_den <- function(df, key, fallback = NULL) {
  c_val <- valid_col(df, key)
  if (!is.na(c_val) && c_val %in% names(df)) {
    den <- sum(as_num(df[[c_val]]), na.rm = TRUE)
    if (is.finite(den) && den > 0) return(den)
  }
  if (!is.null(fallback)) {
    den2 <- sum(as_num(fallback), na.rm = TRUE)
    if (is.finite(den2) && den2 > 0) return(den2)
  }
  NA_real_
}

get_valid_distribuido_parties <- function(df, keys = NULL) {
  if (is.null(keys)) keys <- elex$key
  all_parties <- character(0)
  for (k in keys) {
    gv <- group_votes_matrix(df, k, "DISTRIBUIDO")
    G  <- gv$G
    if (is.null(G) || ncol(G) == 0L) next
    tot <- colSums(G, na.rm = TRUE)
    ok  <- names(tot)[is.finite(tot) & tot > 0]
    ok  <- setdiff(ok, "OTROS")
    all_parties <- unique(c(all_parties, ok))
  }
  sort(all_parties)
}

get_valid_cand_parties <- function(df, keys = NULL) {
  if (is.null(keys)) keys <- elex$key
  all_parties <- character(0)
  for (k in keys) {
    gv <- group_votes_matrix(df, k, "CAND")
    G  <- gv$G
    if (is.null(G) || ncol(G) == 0L) next
    tot <- colSums(G, na.rm = TRUE)
    ok  <- names(tot)[is.finite(tot) & tot > 0]
    ok  <- setdiff(ok, "OTROS")
    all_parties <- unique(c(all_parties, ok))
  }
  sort(all_parties)
}

# ==========================================================
# 6) PALETAS — con clamp para evitar warnings
# ==========================================================
# Wrapper seguro que clampea valores al dominio antes de aplicar paleta
safe_pal <- function(pal_fn, domain_range) {
  function(x) {
    x <- suppressWarnings(as.numeric(x))
    lo <- domain_range[1]; hi <- domain_range[2]
    x <- ifelse(is.finite(x), pmin(pmax(x, lo), hi), NA_real_)
    suppressWarnings(pal_fn(x))
  }
}

pal_light_accent <- function(domain, accent = ACCENT) {
  domain <- domain[is.finite(domain)]
  if (length(domain) < 2L || diff(range(domain)) == 0) domain <- c(0, 1)
  r <- range(domain, na.rm = TRUE)
  raw_pal <- colorNumeric(
    palette  = grDevices::colorRampPalette(c("#F1F3F4", "#BDC1C6", accent))(256),
    domain   = r,
    na.color = "#00000000"
  )
  pal <- safe_pal(raw_pal, r)
  # Copiar atributos para addLegend
  attr(pal, "colorType") <- "numeric"
  attr(pal, "colorArgs") <- list(na.color = "#00000000")
  pal
}

# Para addLegend necesitamos funciones que tengan $pal y $values
# Usamos una estructura compatible
make_legend_pal <- function(pal_fn, domain_range, na_color = "#00000000") {
  clamped <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    lo <- domain_range[1]; hi <- domain_range[2]
    x <- ifelse(is.finite(x), pmin(pmax(x, lo), hi), NA_real_)
    suppressWarnings(pal_fn(x))
  }
  structure(clamped, colorType = "numeric",
            colorArgs = list(na.color = na_color))
}

make_pal_pos <- function(values, scale = c("linear", "quantile"), accent = ACCENT) {
  scale <- match.arg(scale)
  v   <- suppressWarnings(as.numeric(values))
  v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  
  if (length(dom) < 2L || length(unique(dom)) < 2L) {
    pal <- make_legend_pal(
      colorNumeric(grDevices::colorRampPalette(c("#F1F3F4", "#BDC1C6", accent))(256),
                   domain = c(0, 1), na.color = "#00000000"),
      c(0, 1))
    return(list(pal = pal, values = ifelse(is.na(v), NA_real_, 0)))
  }
  
  r <- range(dom, na.rm = TRUE)
  
  if (scale == "quantile") {
    br <- as.numeric(quantile(dom, probs = seq(0, 1, length.out = 8), na.rm = TRUE, type = 7))
    br <- unique(br[is.finite(br)])
    if (length(br) >= 3L) {
      n_bins <- max(3L, length(br) - 1L)
      cols   <- grDevices::colorRampPalette(c("#F1F3F4", "#BDC1C6", accent))(n_bins)
      # Clamp values to bin range
      v_clamped <- ifelse(is.finite(v), pmin(pmax(v, min(br)), max(br)), NA_real_)
      raw_pal <- colorBin(palette = cols, bins = br, domain = range(br),
                          na.color = "#00000000", pretty = FALSE)
      pal <- make_legend_pal(raw_pal, range(br))
      return(list(pal = pal, values = v_clamped))
    }
  }
  
  raw_pal <- colorNumeric(
    palette = grDevices::colorRampPalette(c("#F1F3F4", "#BDC1C6", accent))(256),
    domain = r, na.color = "#00000000")
  pal <- make_legend_pal(raw_pal, r)
  list(pal = pal, values = v)
}

make_pal_delta <- function(values, scale = c("linear", "quantile"), accent = ACCENT) {
  scale <- match.arg(scale)
  v   <- suppressWarnings(as.numeric(values))
  v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  
  if (length(dom) < 2L || length(unique(dom)) < 2L) {
    raw_pal <- colorNumeric(
      palette = grDevices::colorRampPalette(c(DANGER, "#F1F3F4", ACCENT))(256),
      domain  = c(-1, 1), na.color = "#00000000")
    pal <- make_legend_pal(raw_pal, c(-1, 1))
    return(list(pal = pal, values = ifelse(is.na(v), NA_real_, 0)))
  }
  
  if (scale == "quantile") {
    br <- as.numeric(quantile(dom, probs = seq(0, 1, length.out = 8), na.rm = TRUE, type = 7))
    br <- unique(br[is.finite(br)])
    if (length(br) >= 3L && !any(abs(br) < diff(range(br)) * 0.01))
      br <- sort(unique(c(br, 0)))
    if (length(br) >= 3L) {
      n_bins <- max(3L, length(br) - 1L)
      cols   <- grDevices::colorRampPalette(c(DANGER, "#F1F3F4", ACCENT))(n_bins)
      v_clamped <- ifelse(is.finite(v), pmin(pmax(v, min(br)), max(br)), NA_real_)
      raw_pal <- colorBin(palette = cols, bins = br, domain = range(br),
                          na.color = "#00000000", pretty = FALSE)
      pal <- make_legend_pal(raw_pal, range(br))
      return(list(pal = pal, values = v_clamped))
    }
  }
  
  M <- as.numeric(quantile(abs(dom), probs = 0.98, na.rm = TRUE, type = 7))
  if (!is.finite(M) || M <= 0) M <- max(abs(dom), na.rm = TRUE)
  if (!is.finite(M) || M <= 0) M <- 1
  raw_pal <- colorNumeric(
    palette = grDevices::colorRampPalette(c(DANGER, "#F8F9FA", ACCENT))(256),
    domain  = c(-M, M), na.color = "#00000000")
  pal <- make_legend_pal(raw_pal, c(-M, M))
  # Clamp extreme values
  v <- ifelse(is.finite(v), pmin(pmax(v, -M), M), NA_real_)
  list(pal = pal, values = v)
}

# ── Base64 encoding para logos en Plotly ──────────
party_logo_b64 <- local({
  .cache <- list()
  function(party) {
    party <- toupper(party %||% "")
    if (party %in% names(.cache)) return(.cache[[party]])
    src <- party_logo_src(party)
    if (is.null(src)) {
      # No hay logo directo — intentar logo de coalición del www/
      .cache[[party]] <<- NULL
      return(NULL)
    }
    path <- file.path("www", src)
    if (!file.exists(path)) { .cache[[party]] <<- NULL; return(NULL) }
    tryCatch({
      raw  <- readBin(path, "raw", file.info(path)$size)
      ext  <- tolower(tools::file_ext(src))
      mime <- switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg",
                     png = "image/png", svg = "image/svg+xml", "image/png")
      b64  <- base64enc::base64encode(raw)
      uri  <- paste0("data:", mime, ";base64,", b64)
      .cache[[party]] <<- uri
      uri
    }, error = function(e) { .cache[[party]] <<- NULL; NULL })
  }
})

# Obtener todas las URIs b64 de los componentes de una coalición
coalition_logos_b64 <- function(party) {
  parts <- decompose_coalition(party)
  if (is.null(parts)) return(NULL)
  uris <- list()
  for (p in parts) {
    u <- party_logo_b64(p)
    if (!is.null(u)) uris[[length(uris) + 1L]] <- u
  }
  if (length(uris) == 0) return(NULL)
  uris
}

has_b64 <- requireNamespace("base64enc", quietly = TRUE)

plotly_bar_with_logos <- function(dd, cols_bar, hover, margin_l = 140,
                                  logo_only = FALSE) {
  levels_y <- levels(dd$lbl)
  n <- length(levels_y)
  
  # Determinar qué partidos tienen logo y cuáles no
  has_logo  <- logical(n)
  has_parts <- vector("list", n)  # Para coaliciones: lista de b64 URIs
  if (has_b64) {
    for (i in seq_len(n)) {
      b <- party_logo_b64(as.character(levels_y[i]))
      if (!is.null(b)) {
        has_logo[i] <- TRUE
      } else {
        # Intentar coalición
        parts_b64 <- coalition_logos_b64(as.character(levels_y[i]))
        if (!is.null(parts_b64) && length(parts_b64) > 0) {
          has_parts[[i]] <- parts_b64
          has_logo[i] <- TRUE
        }
      }
    }
  }
  
  # Y axis: mostrar texto solo para los que NO tienen logo
  tick_labels <- vapply(seq_len(n), function(i) {
    if (logo_only && has_logo[i]) "" else as.character(levels_y[i])
  }, character(1))
  tick_vals <- levels_y
  
  p <- plot_ly(dd, x = ~votos, y = ~lbl, type = "bar", orientation = "h",
               marker = list(color = cols_bar,
                             line = list(color = "rgba(0,0,0,.04)", width = 0.3)),
               text = hover, hoverinfo = "text",
               texttemplate = "%{x:,.0f}", textposition = "outside",
               textfont = list(size = 10.5, color = TXT_SEC, family = "DM Sans"))
  
  # Agregar logos como imágenes
  imgs <- list()
  if (has_b64) {
    for (i in seq_len(n)) {
      lbl_i <- as.character(levels_y[i])
      b64_direct <- party_logo_b64(lbl_i)
      if (!is.null(b64_direct)) {
        # Logo único
        imgs[[length(imgs) + 1L]] <- list(
          source = b64_direct,
          x = -0.008, xref = "paper",
          y = lbl_i, yref = "y",
          sizex = 0.06, sizey = 0.82,
          xanchor = "right", yanchor = "middle",
          layer = "above"
        )
      } else if (length(has_parts[[i]]) > 0) {
        # Coalición: colocar logos individuales lado a lado
        parts_uris <- has_parts[[i]]
        np <- length(parts_uris)
        base_x <- -0.008
        step   <- 0.035
        total_w <- step * np
        start_x <- base_x - total_w + step
        for (j in seq_along(parts_uris)) {
          imgs[[length(imgs) + 1L]] <- list(
            source = parts_uris[[j]],
            x = start_x + (j - 1) * step, xref = "paper",
            y = lbl_i, yref = "y",
            sizex = 0.035, sizey = 0.68,
            xanchor = "center", yanchor = "middle",
            layer = "above"
          )
        }
      }
    }
  }
  
  # Calcular margen izquierdo adaptativo
  any_text <- any(!has_logo)
  max_text_len <- if (any_text) max(nchar(tick_labels[tick_labels != ""])) else 0
  if (logo_only) {
    any_coalition <- any(vapply(has_parts, function(x) length(x) > 1, logical(1)))
    ml <- if (any_text) max(65, max_text_len * 8)
    else if (any_coalition) 80
    else 52
  } else {
    ml <- margin_l
  }
  
  yaxis_cfg <- list(
    title = "", gridcolor = "transparent",
    tickvals = tick_vals, ticktext = tick_labels,
    tickfont = list(color = TXT, size = 11, family = "DM Sans", weight = 700)
  )
  
  p |> layout(
    xaxis = list(title = "", gridcolor = "#E8EAED",
                 tickfont = list(color = TXT_SEC, size = 10),
                 zeroline = FALSE),
    yaxis = yaxis_cfg,
    font  = list(color = TXT, family = "DM Sans"),
    margin = list(l = ml, r = 50, b = 15, t = 5),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = FALSE,
    images = if (length(imgs) > 0) imgs else NULL
  ) |> config(displayModeBar = FALSE, responsive = TRUE)
}

treemap_colorscale_for_party <- function(party) {
  party <- toupper(party %||% "")
  base <- NULL
  if (party %in% names(party_colors)) {
    base <- unname(party_colors[[party]])
  } else {
    # Intentar coalición
    parts <- decompose_coalition(party)
    if (!is.null(parts)) {
      for (pt in parts) {
        if (pt %in% names(party_colors)) { base <- unname(party_colors[[pt]]); break }
      }
    }
  }
  if (is.null(base)) base <- ACCENT
  base_intense <- intensify_hex(base, 0.15)
  list(
    list(0.00, "#F8F9FA"),
    list(0.40, hex_mix("#FFFFFF", base, 0.30)),
    list(0.75, hex_mix("#FFFFFF", base, 0.65)),
    list(1.00, base_intense)
  )
}

# ==========================================================
# 7) LEAFLET HELPERS
# ==========================================================
add_layers_control <- function(m, overlay_groups = NULL) {
  og <- overlay_groups %||% character(0)
  if (length(og) > 0) {
    m |> addLayersControl(
      baseGroups    = names(BASEMAPS),
      overlayGroups = og,
      options       = layersControlOptions(collapsed = TRUE, position = "topright")
    )
  } else {
    m |> addLayersControl(
      baseGroups = names(BASEMAPS),
      options    = layersControlOptions(collapsed = TRUE, position = "topright")
    )
  }
}

add_dl_layer <- function(m, dl_data) {
  if (is.null(dl_data) || NROW(dl_data) == 0) return(m)
  nms <- names(dl_data)
  id_col   <- pick_col(nms, c("DISTRITO_L", "DISTRITO_LOCAL", "DISTRITO", "ID_DL"))
  name_col <- pick_col(nms, c("NOMBRE_DISTRITO_LOCAL", "NOM_DISTRITO_LOCAL", "NOMGEO", "NOMBRE", "NOM_DL"))
  lbl <- if (!is.na(id_col) && !is.na(name_col)) {
    paste0("DL ", dl_data[[id_col]], " \u00b7 ", dl_data[[name_col]])
  } else if (!is.na(id_col)) {
    paste0("DL ", dl_data[[id_col]])
  } else {
    rep("DL", nrow(dl_data))
  }
  m |> addPolygons(
    data = dl_data, group = DL_GROUP,
    fillColor = "#1A73E8", fillOpacity = 0.04,
    color = "#1A73E8", weight = 1.8, opacity = 0.45,
    label = lapply(lbl, HTML)
  )
}

add_mun_layer <- function(m, mun_data) {
  if (is.null(mun_data) || NROW(mun_data) == 0) return(m)
  nms_m    <- names(mun_data)
  name_col <- pick_col(nms_m, c("NOMGEO", "NOM_MUN", "NOMBRE", "MUNICIPIO", "NAME"))
  lbl <- if (!is.na(name_col)) {
    paste0("<b>Municipio:</b> ", mun_data[[name_col]])
  } else {
    rep("<b>Municipio</b>", nrow(mun_data))
  }
  m |> addPolygons(
    data = mun_data, group = MUN_GROUP,
    fillColor = "#E8453C", fillOpacity = 0.03,
    color = "#E8453C", weight = 2.2, opacity = 0.55,
    dashArray = "6,4",
    label = lapply(lbl, HTML)
  )
}

add_df_layer <- function(m, df_data) {
  if (is.null(df_data) || NROW(df_data) == 0) return(m)
  nms_d    <- names(df_data)
  id_col   <- pick_col(nms_d, c("DISTRITO_FEDERAL", "DISTRITO_F", "DISTRITO", "DF", "ID_DF", "DISTRITO_FED"))
  name_col <- pick_col(nms_d, c("NOMBRE_DISTRITO_FEDERAL", "NOM_DF", "NOMGEO", "NOMBRE"))
  lbl <- if (!is.na(id_col) && !is.na(name_col)) {
    paste0("<b>DF ", df_data[[id_col]], "</b> \u00b7 ", df_data[[name_col]])
  } else if (!is.na(id_col)) {
    paste0("<b>Dist. Federal ", df_data[[id_col]], "</b>")
  } else {
    rep("<b>Dist. Federal</b>", nrow(df_data))
  }
  m |> addPolygons(
    data = df_data, group = DF_GROUP,
    fillColor = "#7B1FA2", fillOpacity = 0.03,
    color = "#7B1FA2", weight = 2.0, opacity = 0.50,
    dashArray = "4,6",
    label = lapply(lbl, HTML)
  )
}

add_all_overlays_clipped <- function(m, dl_data, mun_data, df_data) {
  m <- tryCatch(add_dl_layer(m, dl_data),  error = function(e) m)
  m <- tryCatch(add_mun_layer(m, mun_data), error = function(e) m)
  m <- tryCatch(add_df_layer(m, df_data),   error = function(e) m)
  m
}

hide_all_overlays <- function(m) {
  m <- tryCatch(hideGroup(m, DL_GROUP),  error = function(e) m)
  m <- tryCatch(hideGroup(m, MUN_GROUP), error = function(e) m)
  m <- tryCatch(hideGroup(m, DF_GROUP),  error = function(e) m)
  m
}

restore_map_controls <- function(proxy) {
  proxy <- tryCatch(proxy |> addScaleBar(position = "bottomleft"), error = function(e) proxy)
  if (has_fullscreen) {
    proxy <- tryCatch(
      leaflet.extras::addFullscreenControl(proxy, position = "topleft", pseudoFullscreen = FALSE),
      error = function(e) proxy
    )
  }
  proxy
}

# ── Recortar overlays por ATRIBUTO (no geometría) ──────────
clip_dl_by_attribute <- function(dl_sf, universe_sf) {
  if (is.null(dl_sf) || NROW(dl_sf) == 0) return(NULL)
  if (is.null(universe_sf) || NROW(universe_sf) == 0) return(NULL)
  if (is.na(SECC_DL_COL)) return(NULL)
  dl_nums <- unique(na.omit(as.integer(universe_sf[[SECC_DL_COL]])))
  if (length(dl_nums) == 0) return(NULL)
  # Buscar columna de id en la capa DL
  dl_id_col <- pick_col(names(dl_sf), c("DISTRITO_L", "DISTRITO_LOCAL", "DISTRITO", "ID_DL"))
  if (is.na(dl_id_col)) return(NULL)
  dl_sf[as.integer(dl_sf[[dl_id_col]]) %in% dl_nums, , drop = FALSE]
}

clip_df_by_attribute <- function(dfe_sf, universe_sf) {
  if (is.null(dfe_sf) || NROW(dfe_sf) == 0) return(NULL)
  if (is.null(universe_sf) || NROW(universe_sf) == 0) return(NULL)
  if (is.na(SECC_DF_COL)) return(NULL)
  df_nums <- unique(na.omit(as.integer(universe_sf[[SECC_DF_COL]])))
  if (length(df_nums) == 0) return(NULL)
  nms <- names(dfe_sf)
  id_col <- pick_col(nms, c("DISTRITO_FEDERAL", "DISTRITO_F", "DISTRITO", "DF", "ID_DF", "DISTRITO_FED"))
  if (is.na(id_col)) return(NULL)
  dfe_sf[as.integer(dfe_sf[[id_col]]) %in% df_nums, , drop = FALSE]
}

clip_mun_by_attribute <- function(mun_sf, universe_sf) {
  if (is.null(mun_sf) || NROW(mun_sf) == 0) return(NULL)
  if (is.null(universe_sf) || NROW(universe_sf) == 0) return(NULL)
  if (is.na(SECC_MUN_COL)) {
    # fallback geométrico
    tryCatch({
      union_geom <- st_union(st_geometry(universe_sf))
      hits <- st_intersects(mun_sf, union_geom, sparse = FALSE)
      mun_sf[apply(hits, 1, any), ]
    }, error = function(e) mun_sf)
  } else {
    mun_names <- unique(na.omit(universe_sf[[SECC_MUN_COL]]))
    if (length(mun_names) == 0) return(NULL)
    nms <- names(mun_sf)
    name_col <- pick_col(nms, c("NOMGEO", "NOM_MUN", "NOMBRE", "MUNICIPIO", "NAME"))
    if (is.na(name_col)) {
      tryCatch({
        union_geom <- st_union(st_geometry(universe_sf))
        hits <- st_intersects(mun_sf, union_geom, sparse = FALSE)
        mun_sf[apply(hits, 1, any), ]
      }, error = function(e) mun_sf)
    } else {
      mun_sf[mun_sf[[name_col]] %in% mun_names, , drop = FALSE]
    }
  }
}

# ── Zoom helper: padding reducido ──────────────────
fit_bounds_padded <- function(proxy, df, padding = 0.0008) {
  bb <- tryCatch(st_bbox(df), error = function(e) NULL)
  if (is.null(bb) || !all(is.finite(bb))) return(proxy)
  if (bb["xmin"] == bb["xmax"] || bb["ymin"] == bb["ymax"]) {
    proxy |> flyTo(
      lng  = as.numeric((bb["xmin"] + bb["xmax"]) / 2),
      lat  = as.numeric((bb["ymin"] + bb["ymax"]) / 2),
      zoom = 14
    )
  } else {
    proxy |> flyToBounds(
      lng1 = as.numeric(bb["xmin"] - padding),
      lat1 = as.numeric(bb["ymin"] - padding),
      lng2 = as.numeric(bb["xmax"] + padding),
      lat2 = as.numeric(bb["ymax"] + padding)
    )
  }
}

# ==========================================================
# 8) CSS — GLASSMORPHISM + ANIMACIONES
# ==========================================================
theme <- bs_theme(
  bootswatch = "flatly",
  primary    = ACCENT,
  base_font  = font_google("DM Sans"),
  font_scale = 0.94,
  bg         = BG,
  fg         = TXT
)

css <- paste0('
/* ── Variables ─────────────────────────────────── */
:root {
  --accent:     ', ACCENT, ';
  --accent2:    ', ACCENT2, ';
  --accent-rgb: ', ACCENT_RGB, ';
  --bg:         ', BG, ';
  --card:       ', CARD_BG, ';
  --card-solid: ', CARD_SOLID, ';
  --border:     ', BORDER, ';
  --txt:        ', TXT, ';
  --txt-sec:    ', TXT_SEC, ';
  --muted:      ', MUTED, ';
  --radius:     16px;
  --radius-sm:  10px;
  --shadow:     0 2px 8px rgba(0,0,0,.06), 0 0px 1px rgba(0,0,0,.08);
  --shadow-lg:  0 8px 32px rgba(0,0,0,.08), 0 2px 8px rgba(0,0,0,.06);
  --blur:       saturate(1.6) blur(16px);
}

/* ── Animaciones ───────────────────────────────── */
@keyframes fadeUp {
  from { opacity:0; transform:translateY(10px); }
  to   { opacity:1; transform:translateY(0); }
}
@keyframes shimmer {
  0%   { background-position: -200% 0; }
  100% { background-position: 200% 0; }
}
@keyframes pulse-glow {
  0%, 100% { box-shadow: 0 0 4px rgba(var(--accent-rgb),.15); }
  50%      { box-shadow: 0 0 16px rgba(var(--accent-rgb),.25); }
}
@keyframes gradient-shift {
  0%   { background-position: 0% 50%; }
  50%  { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

/* ── Scrollbar ────────────────────────────────── */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb {
  background: rgba(var(--accent-rgb),.2);
  border-radius: 10px;
}
::-webkit-scrollbar-thumb:hover { background: rgba(var(--accent-rgb),.4); }

/* ── Base ──────────────────────────────────────── */
body {
  background: var(--bg) !important;
  color: var(--txt) !important;
  font-family: "DM Sans", system-ui, -apple-system, sans-serif !important;
}

/* ── Glassmorphism cards ───────────────────────── */
.card-panel {
  background: var(--card);
  backdrop-filter: var(--blur);
  -webkit-backdrop-filter: var(--blur);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  padding: 18px;
  transition: box-shadow 0.25s ease, transform 0.25s ease;
  animation: fadeUp 0.4s ease both;
}
.card-panel:hover {
  box-shadow: var(--shadow-lg);
  transform: translateY(-1px);
}

/* ── KPIs ──────────────────────────────────────── */
.kpiRow {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 10px;
}
.kpi {
  padding: 14px 16px;
  background: var(--card);
  backdrop-filter: var(--blur);
  -webkit-backdrop-filter: var(--blur);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  animation: fadeUp 0.4s ease both;
  transition: box-shadow 0.2s ease, transform 0.2s ease;
  position: relative;
  overflow: hidden;
}
.kpi::before {
  content: "";
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 3px;
  background: linear-gradient(90deg, var(--accent), #9B59B6, rgba(var(--accent-rgb),.15));
  background-size: 200% 100%;
  animation: gradient-shift 4s ease infinite;
  border-radius: var(--radius) var(--radius) 0 0;
}
.kpi:hover { box-shadow: var(--shadow-lg); transform: translateY(-2px); }
.kpi .t { font-size: 10.5px; color: var(--muted); letter-spacing: 0.1em;
           text-transform: uppercase; font-weight: 700; }
.kpi .v { font-size: 22px; color: var(--txt); font-weight: 800;
           margin-top: 5px; line-height: 1.1; }
.kpi .v img { filter: drop-shadow(0 1px 3px rgba(0,0,0,.15)); }
.kpi .s { font-size: 11.5px; color: var(--txt-sec); margin-top: 6px; line-height: 1.4; }
.kpi .s img { filter: drop-shadow(0 1px 2px rgba(0,0,0,.1)); }

/* ── Sidebar ───────────────────────────────────── */
.bslib-sidebar-layout > .sidebar {
  background: var(--card) !important;
  backdrop-filter: var(--blur) !important;
  -webkit-backdrop-filter: var(--blur) !important;
  border-right: 1px solid var(--border) !important;
}
.sidebar-panel-inner { background: transparent !important; }

/* ── Tipografía ────────────────────────────────── */
.blockTitle {
  font-weight: 800;
  color: var(--txt);
  letter-spacing: -0.02em;
  font-size: 15px;
}
.smallHelp { color: var(--muted); font-size: 11.5px; line-height: 1.4; }

/* ── Inputs ────────────────────────────────────── */
.form-control, .form-select, .selectize-input {
  border-radius: var(--radius-sm) !important;
  border-color: var(--border) !important;
  background: rgba(255,255,255,0.6) !important;
  color: var(--txt) !important;
  font-size: 13px !important;
  transition: border-color 0.2s ease, box-shadow 0.2s ease;
}
.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px rgba(var(--accent-rgb), .12) !important;
}
.selectize-input { box-shadow: none !important; }
.selectize-dropdown {
  background: #fff !important;
  border: 1px solid #DADCE0 !important;
  border-radius: 10px !important;
  box-shadow: 0 8px 24px rgba(0,0,0,.12) !important;
  z-index: 9999 !important;
}
.selectize-dropdown .option {
  background: #fff !important;
  color: var(--txt) !important;
  padding: 8px 12px !important;
  font-size: 13px !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .active {
  background: var(--accent-bg) !important;
  color: var(--accent) !important;
}

/* ── Botones ───────────────────────────────────── */
.btn-accent {
  background: linear-gradient(135deg, var(--accent), #4285F4, #9B59B6) !important;
  background-size: 200% 200%;
  animation: gradient-shift 6s ease infinite;
  border: none !important;
  color: #fff !important;
  font-weight: 700 !important;
  border-radius: var(--radius-sm) !important;
  padding: 10px 16px !important;
  font-size: 13.5px !important;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  transition: all 0.25s ease;
  box-shadow: 0 2px 8px rgba(var(--accent-rgb), .3);
}
.btn-accent:hover {
  filter: brightness(1.08);
  box-shadow: 0 4px 24px rgba(var(--accent-rgb), .45), 0 0 40px rgba(var(--accent-rgb), .15);
  transform: translateY(-2px) scale(1.01);
}
.btn-accent:active {
  transform: translateY(0) scale(0.98);
  box-shadow: 0 1px 4px rgba(var(--accent-rgb), .3);
}
.btn-outline-secondary {
  border-radius: var(--radius-sm) !important;
  border-color: var(--border) !important;
  color: var(--txt-sec) !important;
  font-weight: 600 !important;
  font-size: 13px !important;
  transition: all 0.15s ease;
}
.btn-outline-secondary:hover {
  background: rgba(var(--accent-rgb), .06) !important;
  border-color: var(--accent) !important;
  color: var(--accent) !important;
}

/* ── Leaflet ───────────────────────────────────── */
.leaflet-container {
  border-radius: var(--radius);
  border: 1px solid var(--border);
  box-shadow: var(--shadow);
}

/* ── Sliders ───────────────────────────────────── */
.irs-bar, .irs-bar-edge { background: var(--accent) !important; border-color: var(--accent) !important; }
.irs-single, .irs-from, .irs-to { background: var(--accent) !important; border-radius: 6px !important; }
.irs-slider { border-color: var(--accent) !important; }

/* ── Tabs ──────────────────────────────────────── */
.nav-tabs { border-bottom: 2px solid var(--border) !important; }
.nav-tabs .nav-link {
  color: var(--muted) !important;
  font-weight: 700 !important;
  font-size: 13px !important;
  padding: 10px 20px !important;
  border: none !important;
  border-bottom: 2.5px solid transparent !important;
  margin-bottom: -2px;
  transition: all 0.2s ease;
  letter-spacing: 0.01em;
}
.nav-tabs .nav-link:hover {
  color: var(--txt) !important;
  border-bottom-color: rgba(var(--accent-rgb), .3) !important;
}
.nav-tabs .nav-link.active {
  color: var(--accent) !important;
  background: transparent !important;
  border-bottom: 2.5px solid var(--accent) !important;
}

/* ── Tablas ─────────────────────────────────────── */
table.dataTable thead th {
  background: rgba(var(--accent-rgb), .03) !important;
  color: var(--txt-sec) !important;
  font-size: 11.5px !important;
  font-weight: 700 !important;
  border-bottom: 2px solid var(--border) !important;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  white-space: nowrap;
}
table.dataTable thead th img {
  vertical-align: middle;
  margin-right: 3px;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,.1);
}
table.dataTable tbody td {
  font-size: 13px !important;
  border-bottom: 1px solid rgba(0,0,0,.04) !important;
}
table.dataTable tbody tr:hover td {
  background: rgba(var(--accent-rgb), .03) !important;
}
/* Botones Export DT */
.dt-buttons .dt-button {
  border-radius: 8px !important;
  padding: 5px 14px !important;
  font-weight: 700 !important;
  font-size: 11.5px !important;
  letter-spacing: 0.03em;
  border: 1px solid var(--border) !important;
  background: var(--card) !important;
  backdrop-filter: var(--blur);
  color: var(--txt) !important;
  transition: all .2s ease;
}
.dt-buttons .dt-button:hover {
  background: rgba(var(--accent-rgb),.08) !important;
  border-color: var(--accent) !important;
  color: var(--accent) !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(var(--accent-rgb),.15);
}

/* ── Plotly ─────────────────────────────────────── */
.plotly .modebar { display: none !important; }

/* ── Badges ────────────────────────────────────── */
.buffer-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 4px 12px; border-radius: 20px;
  font-size: 11.5px; font-weight: 700;
}
.buffer-badge.inside  { background: #E6F4EA; color: #1E8E3E; }
.buffer-badge.outside { background: #FCE8E6; color: #D93025; }

/* ── Accordion ─────────────────────────────────── */
.accordion-button { font-weight: 700 !important; font-size: 13px !important; }

/* ── Login customization ───────────────────────── */
#shinymanager-auth {
  font-family: "DM Sans", system-ui, -apple-system, sans-serif !important;
}
.panel-auth {
  border-radius: 24px !important;
  background: rgba(255,255,255,0.75) !important;
  backdrop-filter: saturate(1.8) blur(24px);
  -webkit-backdrop-filter: saturate(1.8) blur(24px);
  border: 1px solid rgba(255,255,255,0.4) !important;
  box-shadow:
    0 24px 80px rgba(0,0,0,.08),
    0 8px 32px rgba(0,0,0,.04),
    0 0 120px rgba(var(--accent-rgb),.06),
    inset 0 1px 0 rgba(255,255,255,0.6) !important;
  padding: 36px 32px !important;
  max-width: 380px !important;
  margin: 0 auto !important;
  animation: fadeUp 0.6s ease both !important;
}
.panel-auth .panel-body { padding: 0 !important; }
#shinymanager-auth .form-control {
  border-radius: 12px !important;
  border: 1.5px solid rgba(218,220,224,0.6) !important;
  background: rgba(255,255,255,0.8) !important;
  padding: 12px 16px !important;
  font-size: 14px !important;
  font-weight: 500 !important;
  transition: all 0.25s ease !important;
  box-shadow: 0 1px 3px rgba(0,0,0,.04) !important;
}
#shinymanager-auth .form-control:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 4px rgba(var(--accent-rgb),.12), 0 2px 8px rgba(var(--accent-rgb),.08) !important;
  background: #fff !important;
}
#shinymanager-auth .form-control::placeholder {
  color: #9AA0A6 !important; font-weight: 400 !important;
}
#shinymanager-auth label {
  font-size: 11px !important; font-weight: 700 !important;
  letter-spacing: 0.08em !important; text-transform: uppercase !important;
  color: var(--txt-sec) !important; margin-bottom: 4px !important;
}
#shinymanager-auth .btn-primary {
  background: linear-gradient(135deg, var(--accent), #4285F4, #7C3AED) !important;
  background-size: 200% 200% !important;
  animation: gradient-shift 5s ease infinite !important;
  border: none !important;
  border-radius: 14px !important;
  font-weight: 800 !important; font-size: 14px !important;
  letter-spacing: 0.1em !important; text-transform: uppercase !important;
  padding: 13px 24px !important; width: 100% !important;
  margin-top: 8px !important;
  transition: all 0.25s ease !important;
  box-shadow: 0 4px 16px rgba(var(--accent-rgb),.25) !important;
}
#shinymanager-auth .btn-primary:hover {
  transform: translateY(-2px) !important;
  box-shadow: 0 8px 32px rgba(var(--accent-rgb),.35), 0 0 40px rgba(var(--accent-rgb),.1) !important;
  filter: brightness(1.05) !important;
}
#shinymanager-auth .btn-primary:active {
  transform: translateY(0) scale(0.98) !important;
}

/* ── Radio / Check ─────────────────────────────── */
.form-check-input:checked {
  background-color: var(--accent) !important;
  border-color: var(--accent) !important;
}

/* ── Separator ─────────────────────────────────── */
.sep {
  height: 1px;
  background: linear-gradient(90deg, transparent, var(--border), transparent);
  margin: 10px 0;
  border: none;
}

/* ── Section label ─────────────────────────────── */
.sec-label {
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  background: linear-gradient(90deg, var(--accent), #9B59B6);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  margin-bottom: 4px;
}

/* ── Leaflet container ────────────────────────── */
.leaflet-container {
  border-radius: 12px;
  overflow: hidden;
}
')

# ==========================================================
# 9) UI
# ==========================================================
ui_base <- page_fillable(
  title = APP_TITLE,
  theme = theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700;0,9..40,800;0,9..40,900;1,9..40,400&display=swap",
      rel  = "stylesheet"
    )
  ),
  tags$style(HTML(css)),
  
  layout_sidebar(
    fillable = TRUE,
    
    sidebar = sidebar(
      width = 320,
      style = "padding:14px; height:calc(100vh - 24px); overflow:auto;",
      
      # ── Header compacto con logo de www/ si existe ──
      div(
        style = "text-align:center; margin-bottom:6px;",
        if (file.exists("www/LOGO.PNG.png")) {
          tags$img(src = "LOGO.PNG.png", height = "38px",
                   style = "display:block; margin:0 auto 4px auto;")
        } else {
          tags$span(
            style = paste0(
              "font-size:16px; font-weight:900; letter-spacing:-0.03em; ",
              "background:linear-gradient(135deg,", ACCENT, ",#4285F4,#9B59B6); ",
              "-webkit-background-clip:text; -webkit-text-fill-color:transparent; ",
              "background-clip:text;"
            ),
            "electrend"
          )
        },
        div(style = "font-size:12px; font-weight:800; color:var(--txt); margin-top:1px;", APP_TITLE),
        div(class = "smallHelp", paste0(APP_SUB, " \u00b7 ", nrow(sf_all), " secciones"))
      ),
      div(class = "sep"),
      
      # ── Elección ──
      div(class = "sec-label", "Elecci\u00f3n"),
      selectInput("election", NULL, choices = ELECTION_CHOICES, selected = DEFAULT_ELECTION),
      
      div(class = "sec-label", "Filtros geogr\u00e1ficos"),
      div(class = "smallHelp", "Municipio"),
      selectizeInput("mun_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),
      
      div(class = "smallHelp", "Distrito local"),
      selectizeInput("dl_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),
      
      div(class = "smallHelp", "Distrito federal"),
      selectizeInput("df_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),
      
      div(class = "smallHelp", "Secci\u00f3n"),
      selectizeInput("secciones", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todas\u2026")),
      
      div(class = "sep"),
      
      div(class = "sec-label", "Mapa"),
      bslib::input_switch("map_variable", "Choropleth por partido", value = FALSE),
      
      conditionalPanel(
        condition = "!input.map_variable",
        selectInput("map_view", NULL,
                    choices = c("Ganador" = "winner", "Participaci\u00f3n (%)" = "part",
                                "Total votos" = "tot", "Lista nominal" = "ln"),
                    selected = "winner"),
        conditionalPanel(
          condition = "input.map_view == 'winner'",
          radioButtons("winner_vote_type", NULL,
                       choices  = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO", "CANDIDATURAS" = "CAND"),
                       selected = "DISTRIBUIDO", inline = TRUE)
        )
      ),
      
      conditionalPanel(
        condition = "input.map_variable",
        radioButtons("choro_vote_type", "Votos",
                     choices = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO"),
                     selected = "DISTRIBUIDO", inline = TRUE),
        uiOutput("ui_choro_party"),
        radioButtons("choro_metric", NULL,
                     choices = c("Votos" = "votes", "%" = "pct"),
                     selected = "pct", inline = TRUE),
        radioButtons("choro_scale", NULL,
                     choices = c("Lineal" = "linear", "Cuantiles" = "quantile"),
                     selected = "linear", inline = TRUE),
        sliderInput("choro_opacity", "Opacidad", min = 0.20, max = 0.90, value = 0.65, step = 0.05)
      ),
      
      div(class = "sep"),
      
      actionButton("generar", "GENERAR", class = "btn btn-accent w-100", icon = icon("bolt")),
      div(style = "height:6px;"),
      actionButton("reset", "Limpiar", class = "btn btn-outline-secondary w-100", icon = icon("rotate-left")),
      div(style = "height:6px;"),
      downloadButton("download_csv", "CSV", class = "btn btn-outline-secondary w-100"),
      div(style = "height:8px;"),
      uiOutput("ui_status_run")
    ),
    
    navset_tab(
      id = "main_tabs",
      
      # ══ TAB: EXPLORAR ═══════════════════════════════
      nav_panel(
        "Explorar",
        div(
          style = "padding:12px;",
          div(class = "kpiRow",
              div(class = "kpi", uiOutput("kpi1")),
              div(class = "kpi", uiOutput("kpi2")),
              div(class = "kpi", uiOutput("kpi3"))),
          div(style = "height:12px;"),
          layout_columns(
            col_widths = c(7, 5),
            div(class = "card-panel",
                h5("Mapa", class = "blockTitle mb-1"),
                div(class = "smallHelp", uiOutput("map_subtitle")),
                leafletOutput("map", height = 540)),
            div(class = "card-panel",
                div(style = "display:flex; justify-content:space-between; align-items:center; gap:10px;",
                    h5("Resultados", class = "blockTitle mb-1"),
                    bslib::input_switch("bar_is_cand", "Candidaturas", value = FALSE)),
                plotlyOutput("bar", height = 520))
          ),
          div(style = "height:12px;"),
          layout_columns(
            col_widths = c(6, 6),
            div(class = "card-panel",
                div(style = "display:flex; justify-content:space-between; align-items:center; gap:10px;",
                    h5("Treemap \u00b7 candidaturas", class = "blockTitle mb-1"),
                    uiOutput("ui_tm_party")),
                withSpinner(plotlyOutput("treemap_votes", height = "310px"), type = 5, size = 1)),
            div(class = "card-panel",
                h5("Treemap \u00b7 % sobre v\u00e1lidos", class = "blockTitle mb-1"),
                uiOutput("ui_tm_party_selected"),
                withSpinner(plotlyOutput("treemap_pct", height = "310px"), type = 5, size = 1))
          ),
          div(style = "height:12px;"),
          div(class = "card-panel",
              div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
                  div(h5("Tabla por secci\u00f3n", class = "blockTitle mb-1"),
                      div(class = "smallHelp", "KPIs + top votos")),
                  radioButtons("table_view", NULL,
                               choices  = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO", "CANDIDATURAS" = "CAND"),
                               selected = "DISTRIBUIDO", inline = TRUE)),
              DTOutput("tbl"))
        )
      ),
      
      # ══ TAB: TIEMPO ═════════════════════════════════
      nav_panel(
        "Tiempo",
        div(
          style = "padding:12px;",
          layout_columns(
            col_widths = c(4, 8),
            div(class = "card-panel",
                h5("Configuraci\u00f3n", class = "blockTitle mb-2"),
                checkboxGroupInput(
                  "ts_offices", "Tipo de elecci\u00f3n",
                  choices  = setNames(sort(unique(elex$office)),
                                      vapply(sort(unique(elex$office)), label_office, character(1))),
                  selected = sort(unique(elex$office)), inline = FALSE),
                radioButtons("ts_vote_type", "Votos para partidos",
                             choices = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO"),
                             selected = "DISTRIBUIDO", inline = TRUE),
                radioButtons("ts_party_metric", "M\u00e9trica",
                             choices = c("Votos" = "votes", "%" = "pct"),
                             selected = "pct", inline = TRUE),
                selectizeInput("ts_parties", "Partidos (vac\u00edo = Top N)",
                               choices = NULL, multiple = TRUE,
                               options = list(plugins = list("remove_button"),
                                              placeholder = "PRI, PAN\u2026")),
                sliderInput("ts_top_n", "Top N", min = 3, max = 15, value = 8, step = 1),
                checkboxInput("ts_include_other", "Incluir OTROS", value = FALSE),
                div(class = "sep"),
                div(class = "sec-label", "Mapa comparativo"),
                div(class = "smallHelp", HTML(
                  "<b>Ref</b> = elecci\u00f3n del sidebar &nbsp;\u00b7&nbsp; <b>Comp</b> = selector abajo")),
                selectInput("ts_map_election", NULL,
                            choices  = ELECTION_CHOICES,
                            selected = head(elex$key, 1) %||% DEFAULT_ELECTION),
                radioButtons("ts_map_view", NULL,
                             choices = c("Participaci\u00f3n (pp)" = "participacion",
                                         "Lista nominal" = "lista",
                                         "Total votos" = "mas_votantes",
                                         "Partido (\u0394)" = "choro_party"),
                             selected = "choro_party", inline = TRUE),
                radioButtons("ts_delta_scale", "Escala",
                             choices = c("Lineal" = "linear", "Cuantiles" = "quantile"),
                             selected = "linear", inline = TRUE),
                sliderInput("ts_map_opacity", "Opacidad",
                            min = 0.20, max = 0.90, value = 0.70, step = 0.05),
                conditionalPanel(
                  condition = "input.ts_map_view == 'choro_party'",
                  uiOutput("ui_ts_choro_party"),
                  radioButtons("ts_choro_metric", NULL,
                               choices = c("Votos (\u0394)" = "votes", "% (pp)" = "pct"),
                               selected = "votes", inline = TRUE)),
                div(class = "sep"),
                downloadButton("download_ts_parties_csv", "Serie partidos",
                               class = "btn btn-outline-secondary btn-sm w-100"),
                div(style = "height:6px;"),
                downloadButton("download_ts_metrics_csv", "Serie m\u00e9tricas",
                               class = "btn btn-outline-secondary btn-sm w-100")
            ),
            div(
              div(class = "card-panel",
                  h5("Partidos en el tiempo", class = "blockTitle mb-1"),
                  div(class = "smallHelp", "Serie por elecci\u00f3n dentro de la selecci\u00f3n"),
                  withSpinner(plotlyOutput("ts_party_plot", height = "360px"), type = 5, size = 1)),
              div(style = "height:12px;"),
              div(class = "card-panel",
                  h5("Mapa (Tiempo)", class = "blockTitle mb-1"),
                  div(class = "smallHelp", uiOutput("ts_map_subtitle")),
                  leafletOutput("map_time", height = 430))
            )
          ),
          div(style = "height:12px;"),
          layout_columns(
            col_widths = c(6, 6),
            div(class = "card-panel",
                h5("M\u00e9tricas en el tiempo", class = "blockTitle mb-1"),
                checkboxGroupInput("ts_metrics", NULL,
                                   choices  = c("Total votos" = "total", "Lista nominal" = "lista",
                                                "Casillas" = "casillas", "V\u00e1lidos" = "validos",
                                                "Nulos" = "nulos"),
                                   selected = c("total", "lista"), inline = TRUE),
                withSpinner(plotlyOutput("ts_metrics_plot", height = "310px"), type = 5, size = 1)),
            div(class = "card-panel",
                h5("Participaci\u00f3n en el tiempo", class = "blockTitle mb-1"),
                withSpinner(plotlyOutput("ts_particip_plot", height = "310px"), type = 5, size = 1))
          )
        )
      ),
      
      # ══ TAB: PAUTA ══════════════════════════════════
      nav_panel(
        "PAUTA",
        div(
          style = "padding:12px;",
          layout_columns(
            col_widths = c(3, 9),
            div(class = "card-panel",
                h5("PAUTA", class = "blockTitle mb-1"),
                div(class = "smallHelp mb-2", HTML(
                  "Usa la elecci\u00f3n y filtros del sidebar.<br>Optimiza por votos de partido o indicador INEGI.")),
                uiOutput("buf_hereda_info"),
                div(class = "sep"),
                radioButtons("buf_mode", NULL,
                             choices = c("Electoral" = "electoral",
                                         "INEGI" = "inegi"),
                             selected = "electoral", inline = TRUE),
                conditionalPanel(
                  condition = "input.buf_mode == 'electoral'",
                  uiOutput("ui_buf_party")
                ),
                conditionalPanel(
                  condition = "input.buf_mode == 'inegi'",
                  if (length(EJES_DISPONIBLES) > 0) tagList(
                    selectInput("buf_ejes", "Eje",
                                choices  = c("Selecciona..." = "", setNames(EJES_DISPONIBLES, EJES_DISPONIBLES)),
                                selected = ""),
                    uiOutput("ui_buf_optim_var")
                  ) else div(class = "smallHelp", "No hay variables INEGI disponibles")
                ),
                div(class = "sep"),
                sliderInput("buf_radius", "Radio (metros)",
                            min = 100, max = 5000, value = 1000, step = 100, post = " m"),
                radioButtons("buf_target_pct", "Cobertura",
                             choices = c("60%" = 60, "80%" = 80, "100%" = 100),
                             selected = 60, inline = TRUE),
                div(class = "sep"),
                radioButtons("buf_map_mode", "Mapa",
                             choices = c("Ganador" = "winner",
                                         "Choropleth" = "choro_party",
                                         "Part." = "part",
                                         "Contraste" = "contrast"),
                             selected = "contrast", inline = TRUE),
                sliderInput("buf_opacity", "Opacidad",
                            min = 0.20, max = 0.90, value = 0.60, step = 0.05),
                div(class = "sep"),
                actionButton("buf_apply", HTML("&#9881; OPTIMIZAR"),
                             class = "btn btn-accent w-100"),
                div(style = "height:6px;"),
                downloadButton("download_buf_csv", "CSV",
                               class = "btn btn-outline-secondary btn-sm w-100"),
                div(style = "height:8px;"),
                uiOutput("buf_status")
            ),
            div(
              div(class = "kpiRow",
                  div(class = "kpi", uiOutput("buf_kpi1")),
                  div(class = "kpi", uiOutput("buf_kpi2")),
                  div(class = "kpi", uiOutput("buf_kpi3")),
                  div(class = "kpi", uiOutput("buf_kpi4"))),
              div(style = "height:12px;"),
              div(class = "card-panel",
                  h5("Mapa (PAUTA)", class = "blockTitle mb-1"),
                  div(class = "smallHelp", "Secciones dentro del buffer"),
                  leafletOutput("map_buf", height = 460)),
              div(style = "height:12px;"),
              layout_columns(
                col_widths = c(6, 6),
                div(class = "card-panel",
                    h5("Dentro vs Fuera", class = "blockTitle mb-1"),
                    withSpinner(plotlyOutput("buf_compare_bar", height = "310px"), type = 5, size = 1)),
                div(class = "card-panel",
                    div(class = "sec-label", "Top partidos (buffer)"),
                    withSpinner(plotlyOutput("buf_party_bar", height = "310px"), type = 5, size = 1))
              ),
              div(style = "height:12px;"),
              div(class = "card-panel",
                  h5("Puntos de activaci\u00f3n", class = "blockTitle mb-1"),
                  div(class = "smallHelp", "Solo puntos semilla \u00b7 votos impactados por cada buffer"),
                  DTOutput("buf_tbl"))
            )
          )
        )
      ),
      
      # ══ TAB: LÉEME ══════════════════════════════════
      nav_panel(
        "L\u00c9EME",
        div(
          style = "padding:12px; max-width:720px;",
          div(class = "card-panel",
              tags$h4("C\u00f3mo usar", class = "blockTitle", style = "margin-top:0;"),
              tags$p("Explorador de resultados electorales por secci\u00f3n en Santa Catarina, NL.",
                     style = "color:var(--txt-sec);"),
              tags$ul(style = "color:var(--txt-sec); line-height:1.8;",
                      tags$li("Secciones del CSV. Distritos locales construidos autom\u00e1ticamente."),
                      tags$li(tags$b("GENERAR"), " fija la selecci\u00f3n y alimenta Explorar + Tiempo."),
                      tags$li(tags$b("PAUTA:"), " Elecci\u00f3n, partido, radio \u2192 ",
                              tags$b("Optimizar"), "."),
                      tags$li("Overlays (DL, MUN, DF) filtrados por atributo. Geometr\u00edas simplificadas."),
                      tags$li("A\u00f1os reales desde columnas ANIO_* del CSV."),
                      tags$li(tags$b("Credenciales:"), " admin/123 \u00f3 user/User2025!#")
              )
          )
        )
      )
    )
  )
)

# ── Wrap UI con login ──────────────────────────────
ui <- secure_app(
  ui_base,
  enable_admin = FALSE,
  theme        = theme,
  tags_top     = tags$div(
    style = "text-align:center; padding:20px 0 8px 0;",
    if (file.exists("www/LOGO.PNG.png")) {
      tags$img(src = "LOGO.PNG.png", height = "56px",
               style = paste0(
                 "display:block; margin:0 auto 10px auto; ",
                 "filter:drop-shadow(0 2px 8px rgba(0,0,0,.12));"))
    } else {
      tags$span(
        style = paste0(
          "font-size:28px; font-weight:900; letter-spacing:-0.03em; ",
          "background:linear-gradient(135deg,", ACCENT, ",#4285F4,#7C3AED); ",
          "background-size:200% 200%; animation:gradient-shift 5s ease infinite; ",
          "-webkit-background-clip:text; -webkit-text-fill-color:transparent; ",
          "background-clip:text; display:block; margin-bottom:6px;"
        ),
        "electrend"
      )
    },
    tags$p(
      style = paste0("color:", TXT, "; font-size:15px; font-weight:800; margin:0; letter-spacing:-0.01em;"),
      APP_TITLE),
    tags$p(
      style = paste0("color:", MUTED, "; font-size:11.5px; margin:4px 0 0 0; letter-spacing:0.04em;"),
      APP_SUB),
    tags$div(
      style = "margin:12px auto 0 auto; width:40px; height:3px; border-radius:2px; background:linear-gradient(90deg, #1A73E8, #7C3AED);")
  ),
  background   = paste0(
    "linear-gradient(135deg, #F8F9FC 0%, ", ACCENT2, " 35%, #F3E8FF 65%, #F8F9FC 100%)"
  ),
  language     = "es"
)

# ==========================================================
# 10) SERVER
# ==========================================================
server <- function(input, output, session) {
  
  # ── Login ───────────────────────────────────────
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # ─── Selectores (Municipio) ─────────────────────
  observe({
    if (is.na(SECC_MUN_COL)) {
      updateSelectizeInput(session, "mun_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    muns <- sort(unique(na.omit(sf_all[[SECC_MUN_COL]])))
    ch   <- as.list(setNames(muns, muns))
    updateSelectizeInput(session, "mun_sel", choices = ch, selected = NULL, server = TRUE)
  })
  
  # Distrito local selector
  observe({
    if (is.na(SECC_DL_COL)) {
      updateSelectizeInput(session, "dl_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    dls <- sort(unique(na.omit(sf_all[[SECC_DL_COL]])))
    if (!is.na(SECC_DL_NAME_COL)) {
      labels <- vapply(dls, function(d) {
        nm <- unique(na.omit(sf_all[[SECC_DL_NAME_COL]][sf_all[[SECC_DL_COL]] == d]))
        if (length(nm) > 0 && nzchar(nm[1])) paste0(d, " - ", nm[1]) else as.character(d)
      }, character(1))
    } else labels <- as.character(dls)
    ch <- as.list(setNames(as.character(dls), labels))
    updateSelectizeInput(session, "dl_sel", choices = ch, selected = NULL, server = TRUE)
  })
  
  # Distrito federal selector
  observe({
    if (is.na(SECC_DF_COL)) {
      updateSelectizeInput(session, "df_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    dfs <- sort(unique(na.omit(as.integer(sf_all[[SECC_DF_COL]]))))
    if (!is.na(SECC_DF_NAME_COL)) {
      labels <- vapply(dfs, function(d) {
        nm <- unique(na.omit(sf_all[[SECC_DF_NAME_COL]][as.integer(sf_all[[SECC_DF_COL]]) == d]))
        if (length(nm) > 0 && nzchar(nm[1])) paste0(d, " - ", nm[1]) else as.character(d)
      }, character(1))
    } else labels <- as.character(dfs)
    ch <- as.list(setNames(as.character(dfs), labels))
    updateSelectizeInput(session, "df_sel", choices = ch, selected = NULL, server = TRUE)
  })
  
  # Sección selector (filtrado dinámico)
  observe({
    df0 <- sf_all
    if (!is.na(SECC_MUN_COL)) {
      muns_in <- input$mun_sel %||% character(0)
      if (length(muns_in) > 0) df0 <- df0[df0[[SECC_MUN_COL]] %in% muns_in, ]
    }
    if (!is.na(SECC_DL_COL)) {
      dls_in <- suppressWarnings(as.integer(input$dl_sel %||% character(0)))
      dls_in <- dls_in[!is.na(dls_in)]
      if (length(dls_in) > 0) df0 <- df0[df0[[SECC_DL_COL]] %in% dls_in, ]
    }
    if (!is.na(SECC_DF_COL)) {
      dfs_in <- suppressWarnings(as.integer(input$df_sel %||% character(0)))
      dfs_in <- dfs_in[!is.na(dfs_in)]
      if (length(dfs_in) > 0) df0 <- df0[as.integer(df0[[SECC_DF_COL]]) %in% dfs_in, ]
    }
    secs <- sort(unique(df0$SECCION))
    ch   <- as.list(setNames(as.character(secs), as.character(secs)))
    cur  <- intersect(isolate(input$secciones) %||% character(0), as.character(secs))
    updateSelectizeInput(session, "secciones", choices = ch, selected = cur, server = TRUE)
  })
  
  winner_vt <- reactive(toupper(input$winner_vote_type %||% "DISTRIBUIDO"))
  
  # ─── Estado aplicado ─────────────────────────────
  applied <- reactiveVal(NULL)
  
  observeEvent(input$generar, {
    ap <- list(
      election         = input$election %||% DEFAULT_ELECTION,
      winner_vote_type = winner_vt(),
      map_variable     = isTRUE(input$map_variable),
      map_view         = input$map_view %||% "winner",
      choro_vote_type  = input$choro_vote_type %||% "DISTRIBUIDO",
      choro_party      = input$choro_party %||% "",
      choro_metric     = input$choro_metric %||% "pct",
      choro_scale      = input$choro_scale %||% "linear",
      choro_opacity    = input$choro_opacity %||% 0.65,
      dl_sel           = input$dl_sel   %||% character(0),
      mun_sel          = input$mun_sel  %||% character(0),
      df_sel           = input$df_sel   %||% character(0),
      secciones        = input$secciones %||% character(0),
      ts               = Sys.time()
    )
    applied(ap)
    showNotification("\u2713 Generado", type = "message", duration = 1.2)
  }, ignoreInit = TRUE)
  
  has_applied <- reactive(!is.null(applied()) && !is.null(applied()$ts))
  
  output$ui_status_run <- renderUI({
    if (!has_applied())
      div(class = "smallHelp", HTML("Configura y presiona <b>GENERAR</b>"))
    else
      div(class = "smallHelp", style = "color:#1E8E3E;",
          paste0("\u2713 ", format(applied()$ts, "%H:%M:%S")))
  })
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "mun_sel",   selected = NULL, server = TRUE)
    updateSelectizeInput(session, "dl_sel",    selected = NULL, server = TRUE)
    updateSelectizeInput(session, "df_sel",    selected = NULL, server = TRUE)
    updateSelectizeInput(session, "secciones", selected = NULL, server = TRUE)
    updateSelectInput(session, "election",     selected = DEFAULT_ELECTION)
    updateSelectInput(session, "map_view",     selected = "winner")
    updateRadioButtons(session, "winner_vote_type", selected = "DISTRIBUIDO")
    bslib::update_switch(session, "map_variable", value = FALSE)
    updateRadioButtons(session, "choro_vote_type", selected = "DISTRIBUIDO")
    updateRadioButtons(session, "choro_metric",    selected = "pct")
    updateRadioButtons(session, "choro_scale",     selected = "linear")
    updateSelectInput(session, "ts_map_election",
                      selected = head(elex$key, 1) %||% DEFAULT_ELECTION)
    updateCheckboxGroupInput(session, "ts_offices", selected = sort(unique(elex$office)))
  }, ignoreInit = TRUE)
  
  # ─── Datos aplicados ─────────────────────────────
  apply_filters <- function(df, ap) {
    if (!is.na(SECC_MUN_COL)) {
      muns <- ap$mun_sel %||% character(0)
      if (length(muns) > 0) df <- df[df[[SECC_MUN_COL]] %in% muns, ]
    }
    if (!is.na(SECC_DL_COL)) {
      dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
      dls <- dls[!is.na(dls)]
      if (length(dls) > 0) df <- df[df[[SECC_DL_COL]] %in% dls, ]
    }
    if (!is.na(SECC_DF_COL)) {
      dfs <- suppressWarnings(as.integer(ap$df_sel %||% character(0)))
      dfs <- dfs[!is.na(dfs)]
      if (length(dfs) > 0) df <- df[as.integer(df[[SECC_DF_COL]]) %in% dfs, ]
    }
    if (length(ap$secciones) > 0) {
      s <- suppressWarnings(as.integer(ap$secciones)); s <- s[!is.na(s)]
      if (length(s) > 0) df <- df[df$SECCION %in% s, ]
    }
    df
  }
  
  has_active_filter <- reactive({
    if (!has_applied()) return(FALSE)
    ap <- applied()
    length(ap$mun_sel) > 0 || length(ap$dl_sel) > 0 ||
      length(ap$df_sel) > 0 || length(ap$secciones) > 0
  })
  
  df_applied <- reactive({
    req(has_applied())
    apply_filters(sf_all, applied())
  })
  
  # ── Overlays filtrados POR ATRIBUTO ──
  clipped_overlays <- reactive({
    df <- df_applied()
    if (is.null(df) || NROW(df) == 0) return(list(dl = NULL, mun = NULL, df = NULL))
    if (!has_active_filter()) return(list(dl = dl_sf, mun = mun_sf, df = dfe_sf))
    
    list(
      dl  = clip_dl_by_attribute(dl_sf, df),
      mun = clip_mun_by_attribute(mun_sf, df),
      df  = clip_df_by_attribute(dfe_sf, df)
    )
  })
  
  dl_applied <- reactive({
    co <- clipped_overlays()
    co$dl
  })
  
  current_overlay_groups <- reactive({
    co <- clipped_overlays()
    og <- character(0)
    if (!is.null(co$dl)  && NROW(co$dl)  > 0) og <- c(og, DL_GROUP)
    if (!is.null(co$mun) && NROW(co$mun) > 0) og <- c(og, MUN_GROUP)
    if (!is.null(co$df)  && NROW(co$df)  > 0) og <- c(og, DF_GROUP)
    og
  })
  
  # ─── UI choropleth partido ──────────────────────
  output$ui_choro_party <- renderUI({
    req(has_applied())
    if (!isTRUE(input$map_variable)) return(NULL)
    ap <- applied(); df <- df_applied(); req(nrow(df) > 0)
    parties <- get_valid_distribuido_parties(df, list(ap$election))
    validate(need(length(parties) > 0L, "Sin partidos para este tipo de voto."))
    sel <- input$choro_party %||% if ("PRI" %in% parties) "PRI" else parties[1]
    if (!(sel %in% parties)) sel <- parties[1]
    selectizeInput("choro_party", "Partido",
                   choices = as.list(setNames(parties, parties)), selected = sel)
  })
  
  # ─── Métricas base ──────────────────────────────
  df_metrics <- reactive({
    req(has_applied())
    ap <- applied(); df <- df_applied(); key <- ap$election
    
    c_tot <- total_col(df, key); c_ln  <- ln_col(df, key)
    c_cas <- cas_col(df, key);   c_val <- valid_col(df, key)
    c_nul <- nulos_col(df, key)
    
    tot  <- if (!is.na(c_tot) && c_tot %in% names(df)) as_num(df[[c_tot]]) else rep(NA_real_, nrow(df))
    ln   <- if (!is.na(c_ln)  && c_ln  %in% names(df)) as_num(df[[c_ln]])  else rep(NA_real_, nrow(df))
    cas  <- if (!is.na(c_cas) && c_cas %in% names(df)) as_num(df[[c_cas]]) else rep(NA_real_, nrow(df))
    val  <- if (!is.na(c_val) && c_val %in% names(df)) as_num(df[[c_val]]) else rep(NA_real_, nrow(df))
    nul  <- if (!is.na(c_nul) && c_nul %in% names(df)) as_num(df[[c_nul]]) else rep(NA_real_, nrow(df))
    part <- ifelse(is.finite(ln) & ln > 0, tot / ln, NA_real_)
    
    list(df = df, key = key,
         c_tot = c_tot, c_ln = c_ln, c_cas = c_cas, c_val = c_val, c_nul = c_nul,
         tot = tot, ln = ln, cas = cas, val = val, nul = nul, part = part)
  })
  
  kpi_placeholder <- function(t, s = "Presiona GENERAR") {
    tagList(div(class = "t", t), div(class = "v", "\u2014"), div(class = "s", s))
  }
  
  # ─── KPIs ───────────────────────────────────────
  output$kpi1 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Resumen"))
    x <- df_metrics()
    tot_sum <- if (!all(is.na(x$tot))) sum(x$tot, na.rm = TRUE) else NA_real_
    ln_sum  <- if (!all(is.na(x$ln)))  sum(x$ln,  na.rm = TRUE) else NA_real_
    cas_sum <- if (!all(is.na(x$cas))) sum(x$cas, na.rm = TRUE) else NA_real_
    part_pp <- if (is.finite(ln_sum) && ln_sum > 0 && is.finite(tot_sum)) 100 * tot_sum / ln_sum else NA_real_
    
    sub <- paste0(
      "Votos: ", if (is.finite(tot_sum)) fmt_int(tot_sum) else "\u2014",
      " \u00b7 LN: ", if (is.finite(ln_sum)) fmt_int(ln_sum) else "\u2014",
      " \u00b7 Part: ", if (is.finite(part_pp)) paste0(formatC(part_pp, format = "f", digits = 2), "%") else "\u2014",
      if (is.finite(cas_sum)) paste0(" \u00b7 Cas: ", fmt_int(cas_sum)) else "")
    tagList(
      div(class = "t", paste0("Secciones \u00b7 ", key_label(x$key))),
      div(class = "v", fmt_int(nrow(x$df))),
      div(class = "s", sub))
  })
  
  output$kpi2 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Ganador global"))
    ap <- applied(); x <- df_metrics()
    tot <- totals_for_view(x$df, x$key, ap$winner_vote_type %||% "DISTRIBUIDO")
    top <- top2_from_totals(tot)
    if (!nzchar(top$w1 %||% "") || !is.finite(top$v1))
      return(kpi_placeholder("Ganador global", "Sin ranking"))
    
    den <- sum_valid_den(x$df, x$key, fallback = tot)
    margin_votes <- if (is.finite(top$v2)) (top$v1 - top$v2) else NA_real_
    margin_pp    <- if (is.finite(den) && den > 0 && is.finite(top$v2)) 100 * margin_votes / den else NA_real_
    
    tagList(
      div(class = "t", "Ganador global"),
      div(class = "v", style = "display:flex; align-items:center; gap:8px;",
          party_logo_tag(top$w1, height = "28px"),
          span(paste0(top$w1, " \u00b7 ", fmt_int(top$v1)))),
      div(class = "s",
          if (is.finite(top$v2))
            HTML(paste0(
              party_logo_inline(top$w2, "14px"),
              "2\u00ba: ", top$w2, " \u00b7 ", fmt_int(top$v2),
              " | margen: ", fmt_int(margin_votes),
              if (is.finite(margin_pp)) paste0(" (", formatC(margin_pp, format = "f", digits = 2), " pp)") else ""))
          else "Sin 2\u00ba lugar"))
  })
  
  output$kpi3 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("PRI \u00b7 puro vs distribuido"))
    x <- df_metrics()
    tot_puro <- totals_for_view(x$df, x$key, "PURO")
    tot_dist <- totals_for_view(x$df, x$key, "DISTRIBUIDO")
    pri_puro <- as.numeric(tot_puro[["PRI"]] %||% NA_real_)
    pri_dist <- as.numeric(tot_dist[["PRI"]] %||% NA_real_)
    if (!is.finite(pri_puro) && !is.finite(pri_dist))
      return(kpi_placeholder("PRI \u00b7 puro vs distribuido", "No hay columnas PRI"))
    pri_puro <- if (is.finite(pri_puro)) pri_puro else 0
    pri_dist <- if (is.finite(pri_dist)) pri_dist else 0
    aporte <- pri_dist - pri_puro
    share  <- if (is.finite(pri_dist) && pri_dist > 0) aporte / pri_dist else NA_real_
    tagList(
      div(class = "t", "PRI \u00b7 puro vs distribuido"),
      div(class = "v", paste0(fmt_int(pri_puro), " | ", fmt_int(pri_dist))),
      div(class = "s", paste0("Aporte coalici\u00f3n: ",
                              if (is.finite(aporte)) fmt_signed_int(aporte) else "\u2014",
                              if (is.finite(share)) paste0(" (", formatC(100*share, format="f", digits=1), "%)") else "")))
  })
  
  output$map_subtitle <- renderUI({
    if (!has_applied()) return(span("Presiona GENERAR", style = "color:var(--muted);"))
    ap <- applied()
    n_secc <- NROW(df_applied())
    txt <- if (isTRUE(ap$map_variable)) {
      paste0("Choropleth \u00b7 ", ap$choro_vote_type, " \u00b7 ", ap$choro_party,
             " \u00b7 ", key_label(ap$election), " \u00b7 ", n_secc, " secc")
    } else {
      mv <- ap$map_view %||% "winner"
      base_txt <- switch(mv,
                         winner = paste0("Ganador (", ap$winner_vote_type, ")"),
                         part   = "Participaci\u00f3n",
                         tot    = "Total votos",
                         ln     = "Lista nominal",
                         "")
      paste0(base_txt, " \u00b7 ", key_label(ap$election), " \u00b7 ", n_secc, " secc")
    }
    span(txt, style = "color:var(--txt-sec);")
  })
  
  # ─── MAPA PRINCIPAL ─────────────────────────────
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(BASEMAPS[["Claro"]], group = "Claro") |>
      addProviderTiles(BASEMAPS[["Oscuro"]], group = "Oscuro") |>
      addProviderTiles(BASEMAPS[["Calles"]], group = "Calles") |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]], group = "Sat\u00e9lite") |>
      addScaleBar(position = "bottomleft") |>
      fitBounds(
        lng1 = as.numeric(INIT_BBOX["xmin"]),
        lat1 = as.numeric(INIT_BBOX["ymin"]),
        lng2 = as.numeric(INIT_BBOX["xmax"]),
        lat2 = as.numeric(INIT_BBOX["ymax"])
      )
    if (has_fullscreen)
      m <- leaflet.extras::addFullscreenControl(m, position = "topleft", pseudoFullscreen = FALSE)
    m
  })
  
  observe({
    req(has_applied())
    ap <- applied(); x <- df_metrics(); df <- x$df
    req(nrow(df) > 0)
    tryCatch({
      co    <- clipped_overlays()
      og    <- current_overlay_groups()
      proxy <- leafletProxy("map", session = session) |>
        clearShapes() |> clearMarkers() |> clearControls()
      proxy <- restore_map_controls(proxy)
      
      lab_base <- paste0(
        "<b>Secci\u00f3n:</b> ", df$SECCION,
        if (!is.na(x$c_tot)) paste0("<br><b>Total votos:</b> ", fmt_int(df[[x$c_tot]])) else "",
        if (!is.na(x$c_ln))  paste0("<br><b>Lista nominal:</b> ", fmt_int(df[[x$c_ln]])) else "",
        "<br><b>Participaci\u00f3n:</b> ", fmt_pct(x$part))
      
      if (isTRUE(ap$map_variable)) {
        gv <- group_votes_matrix(df, ap$election, ap$choro_vote_type %||% "DISTRIBUIDO")
        G  <- gv$G
        if (is.null(G) || ncol(G) == 0L) return()
        pick <- ap$choro_party %||% colnames(G)[1]
        if (!(pick %in% colnames(G))) pick <- colnames(G)[1]
        party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
        v_votes <- as.numeric(G[, pick])
        c_val <- valid_col(df, ap$election)
        den <- if (!is.na(c_val) && c_val %in% names(df)) as_num(df[[c_val]]) else rowSums(G, na.rm = TRUE)
        den[!is.finite(den) | den <= 0] <- NA_real_
        
        if ((ap$choro_metric %||% "pct") == "pct") {
          v <- ifelse(is.na(den), NA_real_, v_votes / den)
          res <- make_pal_pos(v, scale = ap$choro_scale %||% "linear", accent = party_accent)
          lab <- paste0(lab_base, "<br><b>", pick, " (%):</b> ", fmt_pct(v))
          ttl <- HTML(paste0("<span style='color:", party_accent, ";font-weight:900'>\u25cf</span> ", pick, " (%)"))
        } else {
          v <- v_votes
          res <- make_pal_pos(v, scale = ap$choro_scale %||% "linear", accent = party_accent)
          lab <- paste0(lab_base, "<br><b>", pick, " (votos):</b> ", fmt_int(v))
          ttl <- HTML(paste0("<span style='color:", party_accent, ";font-weight:900'>\u25cf</span> ", pick, " (votos)"))
        }
        proxy <- proxy |>
          addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                      fillColor = res$pal(res$values), fillOpacity = ap$choro_opacity %||% 0.65,
                      label = lapply(lab, HTML),
                      highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE)) |>
          suppressWarnings(addLegend(position = "bottomright", pal = res$pal, values = res$values, title = ttl, opacity = 0.9))
      } else {
        mv <- ap$map_view %||% "winner"
        if (mv == "winner") {
          w <- winner_by_row(df, ap$election, ap$winner_vote_type %||% "DISTRIBUIDO")
          df$WINNER <- w; df$FILL <- vapply(df$WINNER, fill_color_winner, character(1))
          lab <- paste0(lab_base, "<br><b>Ganador:</b> ", ifelse(is.na(df$WINNER), "\u2014", df$WINNER))
          proxy <- proxy |>
            addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                        fillColor = ~FILL, fillOpacity = 0.62,
                        label = lapply(lab, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE))
          leg_vals <- sort(unique(df$WINNER[!is.na(df$WINNER)]))
          if (length(leg_vals) > 0L) {
            leg_cols <- vapply(leg_vals, fill_color_winner, character(1))
            proxy <- proxy |>
              suppressWarnings(addLegend(position = "bottomright", colors = as.vector(leg_cols),
                                         labels = as.vector(leg_vals),
                                         title = paste0("Ganador (", ap$winner_vote_type, ")"), opacity = 0.9))
          }
        } else {
          v <- switch(mv, part = x$part, tot = x$tot, ln = x$ln, x$tot)
          pal <- pal_light_accent(v, accent = ACCENT)
          ttl <- switch(mv, part = "Participaci\u00f3n (%)", tot = "Total votos", ln = "Lista nominal", "M\u00e9trica")
          lab <- if (mv == "part") paste0(lab_base, "<br><b>", ttl, ":</b> ", fmt_pct(v))
          else paste0(lab_base, "<br><b>", ttl, ":</b> ", fmt_int(v))
          proxy <- proxy |>
            addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                        fillColor = pal(v), fillOpacity = 0.62,
                        label = lapply(lab, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE)) |>
            suppressWarnings(addLegend(position = "bottomright", pal = pal, values = v, title = ttl, opacity = 0.9))
        }
      }
      
      proxy <- add_all_overlays_clipped(proxy, co$dl, co$mun, co$df)
      proxy <- add_layers_control(proxy, og)
      proxy <- hide_all_overlays(proxy)
      fit_bounds_padded(proxy, df)
      
    }, error = function(e) {
      showNotification(paste("Error mapa:", e$message), type = "error", duration = 8)
    })
  })
  
  # ─── BARRAS ─────────────────────────────────────
  plotly_layout_light <- function(p, xlab = "", ylab = "", margin_l = 55) {
    p |> layout(
      xaxis = list(title = xlab, gridcolor = "#E8EAED", tickfont = list(color = TXT_SEC)),
      yaxis = list(title = ylab, gridcolor = "#E8EAED", tickfont = list(color = TXT_SEC)),
      font  = list(color = TXT, family = "DM Sans, system-ui"),
      margin = list(l = margin_l, r = 10, b = 50, t = 10),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      showlegend = FALSE
    ) |> config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  # ─── Helper para construir bar data ────────────
  build_bar_dd <- function(df, election, vote_type) {
    gv <- group_votes_matrix(df, election, vote_type); G <- gv$G
    if (is.null(G) || ncol(G) == 0L) return(NULL)
    totals <- colSums(G, na.rm = TRUE); totals <- totals[totals > 0]
    if (length(totals) == 0L) return(NULL)
    ord <- order(totals, decreasing = TRUE)
    keep <- names(totals)[ord][seq_len(min(14L, length(ord)))]
    dd <- data.frame(lbl = keep, votos = as.numeric(totals[keep]), stringsAsFactors = FALSE)
    dd <- dd[order(dd$votos, decreasing = TRUE), ]
    dd$lbl <- factor(dd$lbl, levels = rev(dd$lbl))
    dd
  }
  
  output$bar <- renderPlotly({
    req(has_applied())
    ap <- applied(); x <- df_metrics(); df <- x$df
    validate(need(nrow(df) > 0L, "Sin datos"))
    vote_type <- if (isTRUE(input$bar_is_cand)) "CAND" else ap$winner_vote_type %||% "DISTRIBUIDO"
    dd <- build_bar_dd(df, ap$election, vote_type)
    validate(need(!is.null(dd), "Sin votos"))
    cols_bar <- vapply(as.character(dd$lbl), fill_color_winner, character(1))
    hover <- paste0("<b>", dd$lbl, "</b><br>", fmt_int(dd$votos))
    plotly_bar_with_logos(dd, cols_bar, hover, logo_only = TRUE)
  })
  
  # ─── TREEMAPS ───────────────────────────────────
  tm_party_choices <- reactive({
    req(has_applied()); df <- df_applied(); req(nrow(df) > 0)
    get_valid_cand_parties(df, list(applied()$election))
  })
  
  output$ui_tm_party <- renderUI({
    req(has_applied())
    ch <- tm_party_choices(); validate(need(length(ch) > 0L, "Sin candidaturas"))
    def <- ch[1]
    selectInput("tm_party", NULL, choices = ch, selected = def, width = "160px")
  })
  
  output$ui_tm_party_selected <- renderUI({
    if (!has_applied()) return(NULL)
    p <- input$tm_party %||% ""; if (!nzchar(p)) return(NULL)
    logo <- party_logo_inline(p, "16px")
    div(class = "smallHelp", HTML(paste0("Candidatura: <b>", logo, p, "</b>")))
  })
  
  treemap_df <- reactive({
    req(has_applied()); ap <- applied(); df <- df_applied(); req(nrow(df) > 0)
    gv <- group_votes_matrix(df, ap$election, "CAND")
    G  <- gv$G; validate(need(!is.null(G) && ncol(G) > 0L, "Sin matriz CAND"))
    pick <- input$tm_party %||% colnames(G)[1]
    if (!(pick %in% colnames(G))) pick <- colnames(G)[1]
    votos <- as.numeric(G[, pick])
    c_val <- valid_col(df, ap$election)
    den <- if (!is.na(c_val) && c_val %in% names(df)) as_num(df[[c_val]]) else rowSums(G, na.rm = TRUE)
    den[!is.finite(den) | den <= 0] <- NA_real_
    pct <- ifelse(is.na(den), NA_real_, votos / den)
    out <- data.frame(seccion = as.character(df$SECCION), votos = votos, pct = pct, stringsAsFactors = FALSE)
    out[is.finite(out$votos) & out$votos > 0, ]
  })
  
  make_treemap <- function(d, mode = c("votos", "pct"), party = "PRI", top_n = 350L) {
    mode <- match.arg(mode)
    d <- d[is.finite(d$votos) & d$votos > 0, ]
    if (!nrow(d)) return(plotly_empty() |> layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    if (mode == "votos") {
      d <- d[order(d$votos, decreasing = TRUE), ]
      if (nrow(d) > top_n) d <- d[seq_len(top_n), ]
      size_vals <- d$votos; color_vals <- d$votos
    } else {
      d <- d[is.finite(d$pct) & d$pct > 0, ]
      d <- d[order(d$pct, decreasing = TRUE), ]
      if (!nrow(d)) return(plotly_empty() |> layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
      if (nrow(d) > top_n) d <- d[seq_len(top_n), ]
      size_vals <- d$pct; color_vals <- d$pct
    }
    labs <- paste0("Sec ", d$seccion)
    hover_txt <- if (mode == "votos")
      paste0("<b>", labs, "</b><br>Votos: ", fmt_int(d$votos), "<br>%: ", fmt_pct(d$pct))
    else
      paste0("<b>", labs, "</b><br>%: ", fmt_pct(d$pct), "<br>Votos: ", fmt_int(d$votos))
    
    plot_ly(type = "treemap", labels = labs, parents = rep("", length(labs)),
            values = size_vals, hoverinfo = "text", hovertext = hover_txt,
            textinfo = "label", branchvalues = "total", pathbar = list(visible = FALSE),
            marker = list(colors = color_vals, colorscale = treemap_colorscale_for_party(party),
                          showscale = FALSE, line = list(width = 0.5, color = "#E8EAED"))) |>
      layout(margin = list(l=0,r=0,b=0,t=0,pad=0),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  output$treemap_votes <- renderPlotly({
    req(has_applied()); d <- treemap_df(); validate(need(nrow(d) > 0L, "Sin datos"))
    make_treemap(d, mode = "votos", party = input$tm_party %||% "PRI")
  })
  output$treemap_pct <- renderPlotly({
    req(has_applied()); d <- treemap_df(); validate(need(nrow(d) > 0L, "Sin datos"))
    make_treemap(d, mode = "pct", party = input$tm_party %||% "PRI")
  })
  
  # ─── TABLA ──────────────────────────────────────
  tbl_data <- reactive({
    req(has_applied()); ap <- applied(); x <- df_metrics(); df <- x$df; req(nrow(df) > 0)
    vt_tbl <- input$table_view %||% "DISTRIBUIDO"
    out <- data.frame(
      SECCION       = df$SECCION,
      TOTAL_VOTOS   = if (!is.na(x$c_tot)) as_num(df[[x$c_tot]]) else NA_real_,
      LISTA_NOMINAL = if (!is.na(x$c_ln))  as_num(df[[x$c_ln]])  else NA_real_,
      PARTICIPACION = x$part, stringsAsFactors = FALSE)
    if (!is.na(x$c_cas)) out$CASILLAS <- as_num(df[[x$c_cas]])
    if (!is.na(x$c_nul)) out$NULOS    <- as_num(df[[x$c_nul]])
    gv <- group_votes_matrix(df, ap$election, vt_tbl); G <- gv$G
    if (!is.null(G) && ncol(G) > 0L) {
      totals <- colSums(G, na.rm = TRUE)
      keep <- names(sort(totals, decreasing = TRUE))[seq_len(min(8L, length(totals)))]
      out <- cbind(out, as.data.frame(G[, keep, drop = FALSE]))
    }
    out[order(out$SECCION), ]
  })
  
  output$tbl <- renderDT({
    req(has_applied()); d <- tbl_data()
    # Columnas de partido (las que no son base)
    base_cols <- c("SECCION", "TOTAL_VOTOS", "LISTA_NOMINAL", "PARTICIPACION", "CASILLAS", "NULOS")
    party_col_names <- setdiff(names(d), base_cols)
    
    # Crear nombres de columna con logo HTML para partidos
    col_headers <- names(d)
    for (i in seq_along(col_headers)) {
      cn <- col_headers[i]
      if (cn %in% party_col_names) {
        logo_html <- party_logo_inline(cn, "16px")
        if (nzchar(logo_html)) {
          col_headers[i] <- paste0(logo_html, cn)
        }
      }
    }
    
    # Construir tabla con headers HTML
    header_cb <- htmltools::withTags(table(
      class = "display",
      thead(tr(lapply(col_headers, function(h) th(HTML(h)))))
    ))
    
    dt <- datatable(d, rownames = FALSE, filter = "top", container = header_cb,
                    escape = FALSE, extensions = c("Buttons"),
                    options = list(dom = "Bfrtip", scrollY = "520px", scrollX = TRUE,
                                   scrollCollapse = TRUE, pageLength = 1000, autoWidth = TRUE,
                                   buttons = list("copy", "csv", "excel")))
    cols_round <- intersect(c("TOTAL_VOTOS", "LISTA_NOMINAL", "CASILLAS", "NULOS"), names(d))
    cols_round <- c(cols_round, party_col_names)
    cols_round <- intersect(cols_round, names(d))
    if (length(cols_round) > 0L) dt <- dt |> formatRound(cols_round, digits = 0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION", digits = 2)
    dt
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      ap <- applied()
      if (is.null(ap)) return("santa_catarina_sin_generar.csv")
      paste0("santa_catarina_", ap$election, "_", input$table_view %||% "DISTRIBUIDO", ".csv")
    },
    content = function(file) write.csv(tbl_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  # ══════════════════════════════════════════════════
  # TIEMPO
  # ══════════════════════════════════════════════════
  ts_keys <- reactive({
    req(has_applied())
    sel  <- input$ts_offices %||% sort(unique(elex$office))
    keys <- elex$key[elex$office %in% sel]
    if (!length(keys)) keys <- elex$key
    keys
  })
  
  observeEvent(list(has_applied(), input$ts_vote_type, input$ts_include_other, input$ts_offices), {
    req(has_applied()); df <- df_applied()
    vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    out <- character(0)
    for (k in ts_keys()) {
      gv <- group_votes_matrix(df, k, vt)
      G  <- gv$G
      if (is.null(G) || ncol(G) == 0L) next
      tot <- colSums(G, na.rm = TRUE)
      ok  <- names(tot)[is.finite(tot) & tot > 0]
      out <- unique(c(out, ok))
    }
    if (!isTRUE(input$ts_include_other)) out <- setdiff(out, "OTROS")
    out <- sort(unique(out))
    cur_sel <- isolate(input$ts_parties) %||% character(0)
    default_sel <- intersect(c("PRI", "PAN", "MORENA", "MC", "PVEM", "PT", "PRD"), out)
    new_sel <- if (length(cur_sel) > 0 && any(cur_sel %in% out)) intersect(cur_sel, out) else default_sel
    updateSelectizeInput(session, "ts_parties",
                         choices  = as.list(setNames(out, out)),
                         selected = new_sel,
                         server   = TRUE)
  }, ignoreInit = FALSE)
  
  output$ui_ts_choro_party <- renderUI({
    req(has_applied()); df <- df_applied()
    ref_key  <- input$election        %||% DEFAULT_ELECTION
    comp_key <- input$ts_map_election %||% DEFAULT_ELECTION
    out <- get_valid_distribuido_parties(df, c(ref_key, comp_key))
    validate(need(length(out) > 0L, "Sin partidos"))
    def <- if ("PRI" %in% out) "PRI" else out[1]
    selectInput("ts_choro_party", "Partido", choices = out, selected = def)
  })
  
  output$ts_map_subtitle <- renderUI({
    if (!has_applied()) return(span("Presiona GENERAR", style = "color:var(--muted);"))
    ref_key  <- input$election        %||% DEFAULT_ELECTION
    comp_key <- input$ts_map_election %||% DEFAULT_ELECTION
    n_secc   <- NROW(df_applied())
    span(HTML(paste0(
      "\u0394 = <b>", key_label(ref_key), "</b> \u2212 <b>", key_label(comp_key), "</b>",
      " \u00b7 ", n_secc, " secc"
    )), style = "color:var(--txt-sec);")
  })
  
  ts_party_series <- reactive({
    req(has_applied())
    df <- df_applied(); keys <- ts_keys(); vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    metric <- input$ts_party_metric %||% "pct"
    sel <- toupper(input$ts_parties %||% character(0))
    if (!isTRUE(input$ts_include_other)) sel <- setdiff(sel, "OTROS")
    
    if (length(sel) == 0L) {
      acc <- list()
      for (k in keys) {
        gv <- group_votes_matrix(df, k, vt); if (is.null(gv$G)) next
        tot <- colSums(gv$G, na.rm = TRUE)
        if (!isTRUE(input$ts_include_other)) tot <- tot[names(tot) != "OTROS"]
        tot <- tot[is.finite(tot) & tot > 0]
        for (nm in names(tot)) acc[[nm]] <- (acc[[nm]] %||% 0) + as.numeric(tot[[nm]])
      }
      ord <- order(unlist(acc), decreasing = TRUE)
      sel <- names(acc)[ord][seq_len(min(as.integer(input$ts_top_n %||% 8L), length(ord)))]
    }
    
    rows <- list()
    for (k in keys) {
      gv <- group_votes_matrix(df, k, vt); G <- gv$G
      if (is.null(G) || ncol(G) == 0L) next
      c_val <- valid_col(df, k)
      den <- if (!is.na(c_val) && c_val %in% names(df)) sum(as_num(df[[c_val]]), na.rm = TRUE) else sum(G, na.rm = TRUE)
      if (!is.finite(den) || den <= 0) den <- NA_real_
      tot <- colSums(G, na.rm = TRUE)
      for (p in sel) {
        v <- if (p %in% names(tot)) as.numeric(tot[[p]]) else 0
        y <- if (metric == "pct") { if (is.finite(den) && den > 0) v / den else NA_real_ } else v
        rows[[length(rows) + 1L]] <- data.frame(
          key = k, label = key_label(k), party = p, value = y,
          raw_votes = v, den = den, stringsAsFactors = FALSE)
      }
    }
    out <- do.call(rbind, rows)
    if (is.null(out) || nrow(out) == 0) return(data.frame())
    out$label <- factor(out$label, levels = vapply(keys, key_label, character(1)))
    out
  })
  
  output$ts_party_plot <- renderPlotly({
    req(has_applied()); d <- ts_party_series()
    validate(need(nrow(d) > 0L, "Sin serie"))
    metric <- input$ts_party_metric %||% "pct"; p <- plot_ly()
    for (pp in unique(d$party)) {
      dd <- d[d$party == pp, ]
      y  <- if (metric == "pct") 100 * dd$value else dd$value
      col_line <- if (pp %in% names(party_colors)) unname(party_colors[[pp]]) else ACCENT
      hover <- if (metric == "pct")
        paste0("<b>", dd$label, "</b><br>", pp, "<br>%: ",
               ifelse(is.na(dd$value), "\u2014", paste0(formatC(100*dd$value, format="f", digits=2), "%")),
               "<br>Votos: ", fmt_int(dd$raw_votes))
      else paste0("<b>", dd$label, "</b><br>", pp, "<br>Votos: ", fmt_int(y))
      p <- p |> add_trace(data = dd, x = ~label, y = y,
                          type = "scatter", mode = "lines+markers", name = pp,
                          line = list(color = col_line, width = 2),
                          marker = list(size = 7, color = col_line),
                          text = hover, hoverinfo = "text")
    }
    p |> layout(
      xaxis  = list(title = "", tickangle = -25, gridcolor = "#E8EAED"),
      yaxis  = list(title = if (metric == "pct") "% (v\u00e1lidos)" else "Votos", gridcolor = "#E8EAED"),
      font   = list(color = TXT, family = "DM Sans"),
      margin = list(l = 55, r = 10, b = 85, t = 10),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      legend = list(orientation = "h", x = 0, y = 1.10)
    ) |> config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  ts_metrics_series <- reactive({
    req(has_applied()); df <- df_applied(); keys <- ts_keys()
    rows <- list()
    for (k in keys) {
      c_tot <- total_col(df, k); c_ln <- ln_col(df, k); c_cas <- cas_col(df, k)
      c_val <- valid_col(df, k); c_nul <- nulos_col(df, k)
      tot <- if (!is.na(c_tot) && c_tot %in% names(df)) sum(as_num(df[[c_tot]]), na.rm = TRUE) else NA_real_
      ln  <- if (!is.na(c_ln)  && c_ln  %in% names(df)) sum(as_num(df[[c_ln]]),  na.rm = TRUE) else NA_real_
      cas <- if (!is.na(c_cas) && c_cas %in% names(df)) sum(as_num(df[[c_cas]]), na.rm = TRUE) else NA_real_
      val <- if (!is.na(c_val) && c_val %in% names(df)) sum(as_num(df[[c_val]]), na.rm = TRUE) else NA_real_
      nul <- if (!is.na(c_nul) && c_nul %in% names(df)) sum(as_num(df[[c_nul]]), na.rm = TRUE) else NA_real_
      part <- if (is.finite(tot) && is.finite(ln) && ln > 0) tot / ln else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        key = k, label = key_label(k),
        total = tot, lista = ln, casillas = cas, validos = val, nulos = nul,
        participacion = part, stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, rows)
    if (is.null(out) || nrow(out) == 0) return(data.frame())
    out$label <- factor(out$label, levels = vapply(keys, key_label, character(1)))
    out
  })
  
  output$ts_metrics_plot <- renderPlotly({
    req(has_applied()); d <- ts_metrics_series()
    validate(need(nrow(d) > 0L, "Sin serie"))
    sel <- intersect(input$ts_metrics %||% c("total","lista"),
                     c("total","lista","casillas","validos","nulos"))
    validate(need(length(sel) > 0L, "Selecciona al menos una m\u00e9trica"))
    name_map <- c(total = "Total votos", lista = "Lista nominal",
                  casillas = "Casillas", validos = "V\u00e1lidos", nulos = "Nulos")
    p <- plot_ly()
    for (m in sel) {
      y <- d[[m]]; hover <- paste0("<b>", d$label, "</b><br>", name_map[[m]], ": ", fmt_int(y))
      p <- p |> add_trace(x = d$label, y = y, type = "scatter", mode = "lines+markers",
                          name = name_map[[m]], text = hover, hoverinfo = "text")
    }
    p |> layout(xaxis = list(title="", tickangle=-25, gridcolor="#E8EAED"),
                yaxis = list(title="Conteos", gridcolor="#E8EAED"),
                font = list(color=TXT, family="DM Sans"),
                margin = list(l=55,r=10,b=85,t=10),
                paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
                legend = list(orientation="h",x=0,y=1.12)) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$ts_particip_plot <- renderPlotly({
    req(has_applied()); d <- ts_metrics_series()
    validate(need(nrow(d) > 0L, "Sin serie"))
    hover <- paste0("<b>", d$label, "</b><br>Part: ", fmt_pct(d$participacion),
                    "<br>Votos: ", fmt_int(d$total), "<br>LN: ", fmt_int(d$lista))
    plot_ly(data = d, x = ~label, y = ~I(100*participacion),
            type = "scatter", mode = "lines+markers", name = "Participaci\u00f3n",
            text = hover, hoverinfo = "text",
            line = list(color = ACCENT, width = 2.5),
            marker = list(size = 8, color = ACCENT)) |>
      layout(xaxis = list(title="", tickangle=-25, gridcolor="#E8EAED"),
             yaxis = list(title="Participaci\u00f3n (%)", gridcolor="#E8EAED"),
             font = list(color=TXT, family="DM Sans"),
             margin = list(l=55,r=10,b=85,t=10),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)", showlegend=FALSE) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$download_ts_parties_csv <- downloadHandler(
    filename = function() paste0("serie_partidos_SC_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) write.csv(ts_party_series(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_ts_metrics_csv <- downloadHandler(
    filename = function() paste0("serie_metricas_SC_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) write.csv(ts_metrics_series(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  
  # ─── MAPA TIEMPO (SIN VIBRACIÓN) ───────────────
  output$map_time <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(BASEMAPS[["Claro"]], group = "Claro") |>
      addProviderTiles(BASEMAPS[["Oscuro"]], group = "Oscuro") |>
      addProviderTiles(BASEMAPS[["Calles"]], group = "Calles") |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]], group = "Sat\u00e9lite") |>
      addScaleBar(position = "bottomleft") |>
      fitBounds(lng1 = as.numeric(INIT_BBOX["xmin"]), lat1 = as.numeric(INIT_BBOX["ymin"]), lng2 = as.numeric(INIT_BBOX["xmax"]), lat2 = as.numeric(INIT_BBOX["ymax"]))
    if (has_fullscreen)
      m <- leaflet.extras::addFullscreenControl(m, position = "topleft", pseudoFullscreen = FALSE)
    m
  })
  
  # ══ FIX: NO observar map_time_bounds → elimina vibración ══
  observeEvent(
    list(
      has_applied(), input$main_tabs,
      input$election, input$ts_map_election,
      input$ts_map_view, input$ts_delta_scale, input$ts_map_opacity,
      input$ts_vote_type, input$ts_choro_party, input$ts_choro_metric
    ),
    {
      req(has_applied())
      if (!identical(input$main_tabs, "Tiempo")) return()
      
      df <- df_applied()
      req(NROW(df) > 0)
      
      tryCatch({
        ref_key  <- input$election        %||% DEFAULT_ELECTION
        comp_key <- input$ts_map_election  %||% DEFAULT_ELECTION
        view     <- input$ts_map_view     %||% "choro_party"
        scale    <- input$ts_delta_scale  %||% "linear"
        opac     <- input$ts_map_opacity  %||% 0.70
        
        co    <- clipped_overlays()
        og    <- current_overlay_groups()
        proxy <- leafletProxy("map_time", session = session) |>
          clearShapes() |> clearMarkers() |> clearControls()
        proxy <- restore_map_controls(proxy)
        
        c_tot_r <- total_col(df, ref_key);  c_ln_r <- ln_col(df, ref_key)
        tot_r   <- if (!is.na(c_tot_r) && c_tot_r %in% names(df)) as_num(df[[c_tot_r]]) else rep(NA_real_, nrow(df))
        ln_r    <- if (!is.na(c_ln_r)  && c_ln_r  %in% names(df)) as_num(df[[c_ln_r]])  else rep(NA_real_, nrow(df))
        part_r  <- ifelse(is.finite(ln_r) & ln_r > 0, tot_r / ln_r, NA_real_)
        
        c_tot_c <- total_col(df, comp_key); c_ln_c <- ln_col(df, comp_key)
        tot_c   <- if (!is.na(c_tot_c) && c_tot_c %in% names(df)) as_num(df[[c_tot_c]]) else rep(NA_real_, nrow(df))
        ln_c    <- if (!is.na(c_ln_c)  && c_ln_c  %in% names(df)) as_num(df[[c_ln_c]])  else rep(NA_real_, nrow(df))
        part_c  <- ifelse(is.finite(ln_c) & ln_c > 0, tot_c / ln_c, NA_real_)
        
        hdr <- paste0(
          "<b>Sec:</b> ", df$SECCION,
          "<br><span style='opacity:.7'>Ref:</span> <b>", key_label(ref_key), "</b>",
          "<br><span style='opacity:.7'>Comp:</span> <b>", key_label(comp_key), "</b>")
        
        add_delta <- function(proxy, df, delta, ref_val, comp_val, ttl, fmt_fn, scale, opac) {
          pal <- make_pal_delta(delta, scale = scale)
          lab <- paste0(
            hdr, "<br><br><b>", ttl, "</b>",
            "<br>Ref: ",  fmt_fn(ref_val),
            "<br>Comp: ", fmt_fn(comp_val),
            "<br><b>\u0394:</b> ",
            if (identical(fmt_fn, fmt_pct)) fmt_signed_pp(delta) else fmt_signed_int(delta))
          proxy |>
            addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                        fillColor = pal$pal(pal$values), fillOpacity = opac,
                        label = lapply(lab, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE)) |>
            suppressWarnings(addLegend(position = "bottomright", pal = pal$pal, values = pal$values,
                                       title = paste0("\u0394 ", ttl), opacity = 0.9))
        }
        
        if (view == "participacion") {
          delta <- 100 * (part_r - part_c)
          proxy <- add_delta(proxy, df, delta, part_r, part_c,
                             "Participaci\u00f3n (%)", fmt_pct, scale, opac)
          
        } else if (view == "lista") {
          delta <- ln_r - ln_c
          proxy <- add_delta(proxy, df, delta, ln_r, ln_c,
                             "Lista nominal", fmt_int, scale, opac)
          
        } else if (view == "mas_votantes") {
          delta <- tot_r - tot_c
          proxy <- add_delta(proxy, df, delta, tot_r, tot_c,
                             "Total votos", fmt_int, scale, opac)
          
        } else {
          vt   <- input$ts_vote_type %||% "DISTRIBUIDO"
          pick <- input$ts_choro_party
          met  <- input$ts_choro_metric %||% "votes"
          
          gv_r <- group_votes_matrix(df, ref_key,  vt); G_r <- gv_r$G
          gv_c <- group_votes_matrix(df, comp_key, vt); G_c <- gv_c$G
          if (is.null(G_r) || ncol(G_r) == 0L) return()
          
          if (is.null(pick) || !nzchar(pick)) pick <- if ("PRI" %in% colnames(G_r)) "PRI" else colnames(G_r)[1]
          if (!(pick %in% colnames(G_r))) pick <- colnames(G_r)[1]
          
          votes_r <- as.numeric(G_r[, pick])
          votes_c <- if (!is.null(G_c) && pick %in% colnames(G_c)) as.numeric(G_c[, pick]) else rep(0, nrow(df))
          
          c_val_r <- valid_col(df, ref_key);  c_val_c <- valid_col(df, comp_key)
          den_r   <- if (!is.na(c_val_r) && c_val_r %in% names(df)) as_num(df[[c_val_r]]) else rowSums(G_r, na.rm = TRUE)
          den_c   <- if (!is.null(G_c) && !is.na(c_val_c) && c_val_c %in% names(df)) as_num(df[[c_val_c]]) else {
            if (!is.null(G_c)) rowSums(G_c, na.rm = TRUE) else rep(NA_real_, nrow(df))
          }
          den_r[!is.finite(den_r) | den_r <= 0] <- NA_real_
          den_c[!is.finite(den_c) | den_c <= 0] <- NA_real_
          
          if (met == "pct") {
            pct_r    <- ifelse(is.na(den_r), NA_real_, votes_r / den_r)
            pct_c    <- ifelse(is.na(den_c), NA_real_, votes_c / den_c)
            delta_pp <- 100 * (pct_r - pct_c)
            
            pal <- make_pal_delta(delta_pp, scale = scale)
            lab <- paste0(hdr, "<br><br><b>", pick, "</b>",
                          "<br>Ref %: ",  fmt_pct(pct_r),
                          "<br>Comp %: ", fmt_pct(pct_c),
                          "<br><b>\u0394:</b> ", fmt_signed_pp(delta_pp))
            proxy <- proxy |>
              addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                          fillColor = pal$pal(pal$values), fillOpacity = opac,
                          label = lapply(lab, HTML),
                          highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE)) |>
              suppressWarnings(addLegend(position = "bottomright", pal = pal$pal, values = pal$values,
                                         title = paste0("\u0394 ", pick, " (pp)"), opacity = 0.9))
          } else {
            delta <- votes_r - votes_c
            pal   <- make_pal_delta(delta, scale = scale)
            lab   <- paste0(hdr, "<br><br><b>", pick, "</b>",
                            "<br>Ref: ",  fmt_int(votes_r),
                            "<br>Comp: ", fmt_int(votes_c),
                            "<br><b>\u0394:</b> ", fmt_signed_int(delta))
            proxy <- proxy |>
              addPolygons(data = df, color = "#BDC1C6", weight = 0.8,
                          fillColor = pal$pal(pal$values), fillOpacity = opac,
                          label = lapply(lab, HTML),
                          highlightOptions = highlightOptions(color = ACCENT, weight = 2, bringToFront = TRUE)) |>
              suppressWarnings(addLegend(position = "bottomright", pal = pal$pal, values = pal$values,
                                         title = paste0("\u0394 ", pick, " (votos)"), opacity = 0.9))
          }
        }
        
        proxy <- add_all_overlays_clipped(proxy, co$dl, co$mun, co$df)
        proxy <- add_layers_control(proxy, og)
        proxy <- hide_all_overlays(proxy)
        fit_bounds_padded(proxy, df)
        
      }, error = function(e) {
        showNotification(paste("Error mapa tiempo:", e$message), type = "error", duration = 8)
      })
    },
    ignoreInit = TRUE
  )
  
  # ══════════════════════════════════════════════════
  # PAUTA (BUFFERS)
  # ══════════════════════════════════════════════════
  buf_applied <- reactiveVal(NULL)
  
  # ── Info heredada de Explorar ──────────────────
  output$buf_hereda_info <- renderUI({
    if (!has_applied()) {
      return(div(class = "smallHelp", style = "color:#D93025;",
                 HTML("<b>Primero presiona GENERAR</b> en el sidebar.")))
    }
    ap <- applied()
    n  <- NROW(df_applied())
    div(class = "smallHelp", style = "color:#1E8E3E; font-weight:600;",
        HTML(paste0("\u2713 ", key_label(ap$election),
                    " \u00b7 ", ap$winner_vote_type,
                    " \u00b7 ", n, " secc")))
  })
  
  # Selector de variable INEGI para optimizar (única)
  output$ui_buf_optim_var <- renderUI({
    eje <- input$buf_ejes %||% ""
    if (!nzchar(eje) || NROW(TRADUCTOR) == 0) return(NULL)
    sub <- TRADUCTOR[TRADUCTOR$Eje == eje, ]
    if (NROW(sub) == 0) return(NULL)
    ch <- setNames(sub$VARIABLE, sub$Indicador)
    sel <- sub$VARIABLE[1]
    selectInput("buf_optim_var", "Variable a maximizar",
                choices = ch, selected = sel)
  })
  
  # Variable INEGI seleccionada (reactivo) — siempre una sola
  buf_inegi_vars <- reactive({
    v <- input$buf_optim_var %||% ""
    if (nzchar(v) && v %in% names(INEGI_COL_MAP)) v else character(0)
  })
  
  # Universo PAUTA = hereda filtros del sidebar (mismos que Explorar)
  df_pauta_universe <- reactive({
    if (has_applied()) {
      df_applied()
    } else {
      sf_all
    }
  })
  
  output$ui_buf_party <- renderUI({
    if (!has_applied()) return(div(class = "smallHelp", "Genera primero"))
    ap  <- applied()
    key <- ap$election
    univ <- df_pauta_universe()
    validate(need(NROW(univ) > 0L, "0 secciones."))
    parties <- get_valid_distribuido_parties(univ, list(key))
    if (length(parties) == 0L) return(NULL)
    sel <- input$buf_party %||% if ("PRI" %in% parties) "PRI" else parties[1]
    if (!(sel %in% parties)) sel <- parties[1]
    selectInput("buf_party", "Partido a maximizar",
                choices = as.list(setNames(parties, parties)), selected = sel)
  })
  
  
  observeEvent(input$buf_apply, {
    tryCatch({
      req(has_applied())
      ap         <- applied()
      radius     <- input$buf_radius      %||% 1000
      key        <- ap$election
      vt         <- ap$winner_vote_type   %||% "DISTRIBUIDO"
      mode       <- input$buf_mode        %||% "electoral"
      target_pct <- as.numeric(input$buf_target_pct %||% 60) / 100
      univ       <- df_pauta_universe()
      
      if (NROW(univ) == 0L) {
        showNotification("Sin secciones. Genera primero.", type = "warning"); return()
      }
      
      # Vector de valores a maximizar según modo
      if (mode == "inegi") {
        optim_var <- input$buf_optim_var %||% ""
        col_nm    <- INEGI_COL_MAP[[optim_var]]
        if (is.null(col_nm) || !(col_nm %in% names(univ))) {
          showNotification("Selecciona variable INEGI para maximizar.", type = "warning"); return()
        }
        target_vals <- as_num(univ[[col_nm]]); target_vals[!is.finite(target_vals)] <- 0
        pick <- optim_var
        # También cargar matriz electoral para la tabla
        gv <- group_votes_matrix(univ, key, vt); G <- gv$G
      } else {
        pick <- input$buf_party %||% "PRI"
        gv   <- group_votes_matrix(univ, key, vt); G <- gv$G
        if (is.null(G) || ncol(G) == 0L) {
          showNotification("Sin datos de votos.", type = "warning"); return()
        }
        if (!(pick %in% colnames(G))) pick <- colnames(G)[1]
        target_vals <- as.numeric(G[, pick]); target_vals[!is.finite(target_vals)] <- 0
      }
      
      total_votes <- sum(target_vals, na.rm = TRUE)
      if (!is.finite(total_votes) || total_votes <= 0) {
        showNotification("Sin valores para optimizar.", type = "warning"); return()
      }
      target_limit <- total_votes * target_pct
      
      showNotification("Calculando\u2026", type = "message", duration = 2, id = "buf_calc")
      
      n <- nrow(univ)
      univ_proj      <- tryCatch(st_transform(univ, 32614), error = function(e) univ)
      centroids_proj <- suppressWarnings(st_centroid(univ_proj))
      coords_proj    <- st_coordinates(centroids_proj)
      dist_mat       <- as.matrix(stats::dist(coords_proj))
      
      covered          <- rep(FALSE, n)
      selected_idx     <- integer(0)
      cumulative       <- 0
      
      repeat {
        if (cumulative >= target_limit) break
        best_i <- NA_integer_; best_new <- -Inf
        for (i in seq_len(n)) {
          if (i %in% selected_idx) next
          inside_i  <- which(dist_mat[i, ] <= radius)
          new_secs  <- inside_i[!covered[inside_i]]
          new_val   <- sum(target_vals[new_secs], na.rm = TRUE)
          if (new_val > best_new) { best_new <- new_val; best_i <- i }
        }
        if (is.na(best_i) || best_new <= 0) break
        selected_idx  <- c(selected_idx, best_i)
        newly_covered <- which(dist_mat[best_i, ] <= radius)
        covered[newly_covered] <- TRUE
        cumulative    <- sum(target_vals[covered], na.rm = TRUE)
      }
      
      achieved_pct <- if (total_votes > 0) cumulative / total_votes else 0
      inside_secs  <- univ$SECCION[which(covered)]
      outside_secs <- setdiff(univ$SECCION, inside_secs)
      optimal_secs <- univ$SECCION[selected_idx]
      df_in  <- univ[univ$SECCION %in% inside_secs, ]
      df_out <- univ[univ$SECCION %in% outside_secs, ]
      
      buf_geoms_list <- lapply(selected_idx, function(i) {
        ctr <- centroids_proj[i, ]
        bg  <- st_buffer(st_geometry(ctr), dist = radius)
        tryCatch(st_transform(bg, 4326), error = function(e) bg)
      })
      if (length(buf_geoms_list) > 0) {
        buf_sfc <- do.call(c, lapply(buf_geoms_list, function(g) {
          if (inherits(g, "sfg")) st_sfc(g, crs = 4326) else g
        }))
        if (is.na(st_crs(buf_sfc))) st_crs(buf_sfc) <- 4326
      } else buf_sfc <- st_sfc(crs = 4326)
      
      seed_points <- data.frame(
        ORDEN = integer(0), SECCION = integer(0),
        LNG = numeric(0), LAT = numeric(0),
        DIRECCION_GOOGLE = character(0), stringsAsFactors = FALSE)
      
      if (length(selected_idx) > 0) {
        seed_sf <- univ[selected_idx, ]; seed_xy <- coords_from_sf(seed_sf)
        seed_points <- data.frame(
          ORDEN = seq_len(nrow(seed_sf)), SECCION = seed_sf$SECCION,
          LNG = seed_xy$LNG, LAT = seed_xy$LAT,
          DIRECCION_GOOGLE = NA_character_, stringsAsFactors = FALSE)
        if (nzchar(GOOGLE_API_KEY) && !identical(GOOGLE_API_KEY, "AQUI_TU_API_KEY_GOOGLE")) {
          seed_points$DIRECCION_GOOGLE <- batch_reverse_geocode(
            lats = seed_points$LAT, lngs = seed_points$LNG,
            api_key = GOOGLE_API_KEY, delay = 0.15)
        }
      }
      
      buf_applied(list(
        seeds = optimal_secs, selected_idx = selected_idx,
        radius = radius, key = key, vt = vt, buf_sfc = buf_sfc,
        inside_secs = inside_secs, outside_secs = outside_secs,
        df_in = df_in, df_out = df_out,
        best_votes = cumulative, total_votes = total_votes,
        achieved_pct = achieved_pct, target_pct = target_pct,
        party = pick, mode = mode, seed_points = seed_points,
        universe_n = nrow(univ), ts = Sys.time()))
      
      removeNotification("buf_calc")
      lbl_mode <- if (mode == "inegi") {
        idx <- match(pick, TRADUCTOR$VARIABLE)
        if (!is.na(idx)) TRADUCTOR$Indicador[idx] else pick
      } else paste0("votos ", pick)
      showNotification(
        paste0("\u2713 ", length(selected_idx), " pt(s) \u00b7 ",
               length(inside_secs), " secc \u00b7 ",
               formatC(100 * achieved_pct, format = "f", digits = 1), "% ",
               lbl_mode),
        type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error buffer:", e$message), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)
  
  has_buf <- reactive(!is.null(buf_applied()))
  
  output$buf_status <- renderUI({
    if (!has_buf()) return(div(class = "smallHelp", "Presiona OPTIMIZAR"))
    ba <- buf_applied()
    is_inegi <- identical(ba$mode, "inegi")
    lbl <- if (is_inegi) {
      idx <- match(ba$party, TRADUCTOR$VARIABLE)
      if (!is.na(idx)) TRADUCTOR$Indicador[idx] else ba$party
    } else paste0("votos ", ba$party %||% "")
    div(class = "smallHelp", style = "color:#1E8E3E;",
        HTML(paste0(
          "\u2713 <b>", length(ba$seeds), " pto(s)</b> \u00b7 ",
          length(ba$inside_secs), " secc \u00b7 ", ba$radius, "m",
          "<br>", fmt_int(ba$best_votes), " ", lbl,
          " (<b>", formatC(100 * ba$achieved_pct, format = "f", digits = 1), "%</b>)",
          if (is_inegi) " [INEGI]" else "")))
  })
  
  buf_kpi_ph <- function(t, s = "Aplica un buffer") {
    tagList(div(class = "t", t), div(class = "v", "\u2014"), div(class = "s", s))
  }
  
  output$buf_kpi1 <- renderUI({
    if (!has_buf()) return(buf_kpi_ph("Puntos / Secciones"))
    ba <- buf_applied()
    seed_txt <- if (length(ba$seeds) > 0) paste(ba$seeds, collapse = ", ") else "\u2014"
    tagList(
      div(class = "t", paste0(length(ba$seeds), " punto(s) \u00b7 ",
                              formatC(100 * ba$achieved_pct, format = "f", digits = 1), "%")),
      div(class = "v", paste0(fmt_int(length(ba$inside_secs)), " secciones")),
      div(class = "s", paste0("Radio: ", ba$radius, "m \u00b7 Centros: ", seed_txt)))
  })
  
  output$buf_kpi2 <- renderUI({
    if (!has_buf()) return(buf_kpi_ph("Lista nominal"))
    ba <- buf_applied(); key <- ba$key
    c_ln  <- ln_col(ba$df_in, key)
    ln_in <- if (!is.na(c_ln) && c_ln %in% names(ba$df_in)) sum(as_num(ba$df_in[[c_ln]]), na.rm = TRUE) else NA_real_
    c_ln2  <- ln_col(ba$df_out, key)
    ln_out <- if (!is.na(c_ln2) && c_ln2 %in% names(ba$df_out) && NROW(ba$df_out) > 0)
      sum(as_num(ba$df_out[[c_ln2]]), na.rm = TRUE) else NA_real_
    tagList(
      div(class = "t", "Lista nominal (dentro)"),
      div(class = "v", fmt_int(ln_in)),
      div(class = "s", paste0("Fuera: ", fmt_int(ln_out))))
  })
  
  output$buf_kpi3 <- renderUI({
    if (!has_buf()) return(buf_kpi_ph("Participaci\u00f3n"))
    ba <- buf_applied(); key <- ba$key
    c_tot <- total_col(ba$df_in, key); c_ln <- ln_col(ba$df_in, key)
    tot_in <- if (!is.na(c_tot) && c_tot %in% names(ba$df_in)) sum(as_num(ba$df_in[[c_tot]]), na.rm = TRUE) else NA_real_
    ln_in  <- if (!is.na(c_ln)  && c_ln  %in% names(ba$df_in)) sum(as_num(ba$df_in[[c_ln]]),  na.rm = TRUE) else NA_real_
    part <- if (is.finite(tot_in) && is.finite(ln_in) && ln_in > 0) tot_in / ln_in else NA_real_
    tagList(
      div(class = "t", "Participaci\u00f3n (dentro)"),
      div(class = "v", fmt_pct(part)),
      div(class = "s", paste0("Votos: ", fmt_int(tot_in))))
  })
  
  output$buf_kpi4 <- renderUI({
    if (!has_buf()) return(buf_kpi_ph("Ganador (dentro)"))
    ba <- buf_applied()
    tot <- totals_for_view(ba$df_in, ba$key, ba$vt)
    top <- top2_from_totals(tot)
    if (!nzchar(top$w1 %||% "")) return(buf_kpi_ph("Ganador (dentro)", "Sin votos"))
    tagList(
      div(class = "t", "Ganador (dentro)"),
      div(class = "v", style = "display:flex; align-items:center; gap:8px;",
          party_logo_tag(top$w1, height = "26px"),
          span(paste0(top$w1, " \u00b7 ", fmt_int(top$v1)))),
      div(class = "s",
          if (is.finite(top$v2))
            HTML(paste0(party_logo_inline(top$w2, "14px"),
                        "2\u00ba: ", top$w2, " \u00b7 ", fmt_int(top$v2)))
          else ""))
  })
  
  # Buffer MAP
  output$map_buf <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(BASEMAPS[["Claro"]], group = "Claro") |>
      addProviderTiles(BASEMAPS[["Oscuro"]], group = "Oscuro") |>
      addProviderTiles(BASEMAPS[["Calles"]], group = "Calles") |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]], group = "Sat\u00e9lite") |>
      addScaleBar(position = "bottomleft") |>
      fitBounds(lng1 = as.numeric(INIT_BBOX["xmin"]), lat1 = as.numeric(INIT_BBOX["ymin"]), lng2 = as.numeric(INIT_BBOX["xmax"]), lat2 = as.numeric(INIT_BBOX["ymax"]))
    if (has_fullscreen)
      m <- leaflet.extras::addFullscreenControl(m, position = "topleft", pseudoFullscreen = FALSE)
    m
  })
  
  observe({
    req(has_buf())
    req(isTRUE(input$main_tabs == "PAUTA"))
    ba <- buf_applied(); req(!is.null(ba))
    
    tryCatch({
      df_in  <- ba$df_in
      key    <- ba$key;   vt     <- ba$vt
      mode   <- input$buf_map_mode %||% "winner"
      opac   <- input$buf_opacity  %||% 0.60
      
      pauta_univ <- df_pauta_universe()
      dl_clip  <- clip_dl_by_attribute(dl_sf, pauta_univ)
      mun_clip <- clip_mun_by_attribute(mun_sf, pauta_univ)
      df_clip  <- clip_df_by_attribute(dfe_sf, pauta_univ)
      
      proxy <- leafletProxy("map_buf", session = session) |>
        clearShapes() |> clearMarkers() |> clearControls()
      proxy <- restore_map_controls(proxy)
      
      if (!is.null(ba$buf_sfc) && length(ba$buf_sfc) > 0) {
        buf_sf_draw <- st_sf(id = seq_along(ba$buf_sfc), geometry = ba$buf_sfc)
        proxy <- proxy |>
          addPolygons(data = buf_sf_draw, color = ACCENT, weight = 2.5, dashArray = "8,6",
                      fillColor = ACCENT, fillOpacity = 0.06,
                      label = lapply(paste0("<b>Buffer #", buf_sf_draw$id, ":</b> ", ba$radius, "m"), HTML))
      }
      
      if (!is.null(df_in) && NROW(df_in) > 0) {
        c_tot <- total_col(df_in, key); c_ln <- ln_col(df_in, key)
        tot <- if (!is.na(c_tot) && c_tot %in% names(df_in)) as_num(df_in[[c_tot]]) else rep(NA_real_, nrow(df_in))
        ln  <- if (!is.na(c_ln)  && c_ln  %in% names(df_in)) as_num(df_in[[c_ln]])  else rep(NA_real_, nrow(df_in))
        part <- ifelse(is.finite(ln) & ln > 0, tot / ln, NA_real_)
        is_seed <- df_in$SECCION %in% ba$seeds
        lab_base <- paste0(
          "<b>Sec:</b> ", df_in$SECCION,
          ifelse(is_seed, " <span style='color:#1A73E8;font-weight:700'>(\u2605)</span>", ""),
          "<br><b>Votos:</b> ", fmt_int(tot),
          "<br><b>LN:</b> ", fmt_int(ln),
          "<br><b>Part:</b> ", fmt_pct(part))
        
        if (mode == "winner") {
          w <- winner_by_row(df_in, key, vt)
          df_in$WINNER <- w; df_in$FILL <- vapply(df_in$WINNER, fill_color_winner, character(1))
          lab <- paste0(lab_base, "<br><b>Ganador:</b> ", ifelse(is.na(df_in$WINNER), "\u2014", df_in$WINNER))
          proxy <- proxy |>
            addPolygons(data = df_in, color = "#5F6368", weight = 1,
                        fillColor = ~FILL, fillOpacity = opac,
                        label = lapply(lab, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2.5, bringToFront = TRUE))
          leg_vals <- sort(unique(df_in$WINNER[!is.na(df_in$WINNER)]))
          if (length(leg_vals) > 0L) {
            leg_cols <- vapply(leg_vals, fill_color_winner, character(1))
            proxy <- proxy |>
              suppressWarnings(addLegend(position = "bottomright", colors = as.vector(leg_cols),
                                         labels = as.vector(leg_vals), title = "Ganador", opacity = 0.9))
          }
        } else if (mode == "choro_party") {
          pick <- input$buf_party
          gv <- group_votes_matrix(df_in, key, vt); G <- gv$G
          if (!is.null(G) && ncol(G) > 0L) {
            if (is.null(pick) || !nzchar(pick) || !(pick %in% colnames(G)))
              pick <- if ("PRI" %in% colnames(G)) "PRI" else colnames(G)[1]
            party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
            v_votes <- as.numeric(G[, pick])
            c_val_buf <- valid_col(df_in, key)
            den_buf <- if (!is.na(c_val_buf) && c_val_buf %in% names(df_in)) as_num(df_in[[c_val_buf]]) else rowSums(G, na.rm = TRUE)
            den_buf[!is.finite(den_buf) | den_buf <= 0] <- NA_real_
            v_pct <- ifelse(is.na(den_buf), NA_real_, v_votes / den_buf)
            res <- make_pal_pos(v_pct, scale = "linear", accent = party_accent)
            lab <- paste0(lab_base, "<br><b>", pick, " (%):</b> ", fmt_pct(v_pct),
                          "<br><b>", pick, ":</b> ", fmt_int(v_votes))
            ttl <- HTML(paste0("<span style='color:", party_accent, ";font-weight:900'>\u25cf</span> ", pick, " (%)"))
            proxy <- proxy |>
              addPolygons(data = df_in, color = "#5F6368", weight = 1,
                          fillColor = res$pal(res$values), fillOpacity = opac,
                          label = lapply(lab, HTML),
                          highlightOptions = highlightOptions(color = ACCENT, weight = 2.5, bringToFront = TRUE)) |>
              suppressWarnings(addLegend(position = "bottomright", pal = res$pal, values = res$values, title = ttl, opacity = 0.9))
          }
        } else if (mode == "part") {
          pal <- pal_light_accent(part)
          lab <- paste0(lab_base)
          proxy <- proxy |>
            addPolygons(data = df_in, color = "#5F6368", weight = 1,
                        fillColor = pal(part), fillOpacity = opac,
                        label = lapply(lab, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2.5, bringToFront = TRUE)) |>
            suppressWarnings(addLegend(position = "bottomright", pal = pal, values = part, title = "Part. (%)", opacity = 0.9))
        } else {
          proxy <- proxy |>
            addPolygons(data = df_in, color = ACCENT, weight = 1.2,
                        fillColor = ACCENT, fillOpacity = 0.25,
                        label = lapply(lab_base, HTML),
                        highlightOptions = highlightOptions(color = ACCENT, weight = 2.5, bringToFront = TRUE)) |>
            suppressWarnings(addLegend(position = "bottomright", colors = c(ACCENT, "#E8EAED"),
                                       labels = c("Dentro", "Fuera"), title = "Buffer", opacity = 0.9))
        }
        
        seed_sf <- df_in[is_seed, ]
        if (NROW(seed_sf) > 0) {
          seed_centroids <- safe_centroids_wgs84(seed_sf)
          seed_rank <- match(seed_sf$SECCION, ba$seeds)
          seed_xy   <- coords_from_sf(seed_sf)
          seed_addr <- rep(NA_character_, NROW(seed_sf))
          if (!is.null(ba$seed_points) && NROW(ba$seed_points) > 0) {
            idx <- match(seed_sf$SECCION, ba$seed_points$SECCION); ok <- which(!is.na(idx))
            if (length(ok) > 0) seed_addr[ok] <- ba$seed_points$DIRECCION_GOOGLE[idx[ok]]
          }
          seed_lab <- paste0(
            "<b>#", seed_rank, "</b> Sec ", seed_sf$SECCION,
            "<br>", formatC(seed_xy$LNG, format="f", digits=5), ", ",
            formatC(seed_xy$LAT, format="f", digits=5),
            ifelse(is.na(seed_addr) | !nzchar(seed_addr), "", paste0("<br>", seed_addr)))
          proxy <- proxy |>
            addCircleMarkers(data = seed_centroids, radius = 7,
                             color = "#FFFFFF", fillColor = ACCENT, fillOpacity = 1, weight = 2, opacity = 1,
                             label = lapply(seed_lab, HTML))
        }
      }
      
      og <- character(0)
      if (!is.null(dl_clip)  && NROW(dl_clip)  > 0) og <- c(og, DL_GROUP)
      if (!is.null(mun_clip) && NROW(mun_clip) > 0) og <- c(og, MUN_GROUP)
      if (!is.null(df_clip)  && NROW(df_clip)  > 0) og <- c(og, DF_GROUP)
      proxy <- add_all_overlays_clipped(proxy, dl_clip, mun_clip, df_clip)
      proxy <- add_layers_control(proxy, og)
      proxy <- hide_all_overlays(proxy)
      
      if (!is.null(df_in) && NROW(df_in) > 0) {
        fit_bounds_padded(proxy, df_in)
      }
      
    }, error = function(e) {
      showNotification(paste("Error mapa PAUTA:", e$message), type = "error", duration = 8)
    })
  })
  
  # Buffer comparison bar
  output$buf_compare_bar <- renderPlotly({
    req(has_buf()); ba <- buf_applied(); key <- ba$key
    c_tot_i <- total_col(ba$df_in, key);  c_ln_i <- ln_col(ba$df_in, key)
    c_tot_o <- total_col(ba$df_out, key); c_ln_o <- ln_col(ba$df_out, key)
    tot_in  <- if (!is.na(c_tot_i) && c_tot_i %in% names(ba$df_in)) sum(as_num(ba$df_in[[c_tot_i]]), na.rm = TRUE) else 0
    ln_in   <- if (!is.na(c_ln_i)  && c_ln_i  %in% names(ba$df_in)) sum(as_num(ba$df_in[[c_ln_i]]),  na.rm = TRUE) else 0
    tot_out <- if (NROW(ba$df_out) > 0 && !is.na(c_tot_o) && c_tot_o %in% names(ba$df_out)) sum(as_num(ba$df_out[[c_tot_o]]), na.rm = TRUE) else 0
    ln_out  <- if (NROW(ba$df_out) > 0 && !is.na(c_ln_o)  && c_ln_o  %in% names(ba$df_out)) sum(as_num(ba$df_out[[c_ln_o]]),  na.rm = TRUE) else 0
    part_in  <- if (ln_in  > 0) 100 * tot_in  / ln_in  else NA_real_
    part_out <- if (ln_out > 0) 100 * tot_out / ln_out else NA_real_
    dd <- data.frame(
      metric = rep(c("Secciones", "Lista Nominal", "Total Votos", "Part. (%)"), each = 2),
      zona   = rep(c("Dentro", "Fuera"), 4),
      valor  = c(length(ba$inside_secs), length(ba$outside_secs),
                 ln_in, ln_out, tot_in, tot_out,
                 if (is.finite(part_in)) part_in else 0,
                 if (is.finite(part_out)) part_out else 0),
      stringsAsFactors = FALSE)
    dd$metric <- factor(dd$metric, levels = c("Secciones", "Lista Nominal", "Total Votos", "Part. (%)"))
    dd$zona   <- factor(dd$zona, levels = c("Dentro", "Fuera"))
    hover_txt <- ifelse(
      dd$metric == "Part. (%)",
      paste0("<b>", dd$metric, "</b><br>", dd$zona, ": ", formatC(dd$valor, format = "f", digits = 2), "%"),
      paste0("<b>", dd$metric, "</b><br>", dd$zona, ": ", fmt_int(dd$valor)))
    plot_ly(dd, x = ~metric, y = ~valor, color = ~zona, type = "bar",
            colors = unname(c(ACCENT, "#BDC1C6")), text = hover_txt, hoverinfo = "text") |>
      layout(barmode = "group",
             xaxis = list(title = "", gridcolor = "#E8EAED"),
             yaxis = list(title = "", gridcolor = "#E8EAED"),
             font = list(color = TXT, family = "DM Sans"),
             margin = list(l = 55, r = 10, b = 50, t = 10),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             legend = list(orientation = "h", x = 0, y = 1.10)) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$buf_party_bar <- renderPlotly({
    req(has_buf()); ba <- buf_applied()
    validate(need(NROW(ba$df_in) > 0L, "Sin secciones dentro"))
    tot <- totals_for_view(ba$df_in, ba$key, ba$vt)
    validate(need(length(tot) > 0L, "Sin votos"))
    keep <- names(tot)[seq_len(min(12L, length(tot)))]
    dd <- data.frame(lbl = keep, votos = as.numeric(tot[keep]), stringsAsFactors = FALSE)
    dd <- dd[order(dd$votos, decreasing = TRUE), ]
    dd$lbl <- factor(dd$lbl, levels = rev(dd$lbl))
    cols <- vapply(as.character(dd$lbl), fill_color_winner, character(1))
    hover <- paste0("<b>", dd$lbl, "</b><br>Votos: ", fmt_int(dd$votos))
    plotly_bar_with_logos(dd, cols, hover, logo_only = TRUE)
  })
  
  # Buffer table — SOLO puntos semilla con votos impactados por buffer
  buf_tbl_data <- reactive({
    req(has_buf()); ba <- buf_applied()
    key <- ba$key; vt <- ba$vt
    validate(need(length(ba$seeds) > 0L, "Sin puntos"))
    
    # Datos del universo para calcular impacto por buffer
    univ <- df_pauta_universe()
    req(NROW(univ) > 0)
    
    # Proyectar para distancias
    univ_proj <- tryCatch(st_transform(univ, 32614), error = function(e) univ)
    centroids_proj <- suppressWarnings(st_centroid(univ_proj))
    coords_proj <- st_coordinates(centroids_proj)
    
    # Matriz de votos por partido
    gv <- group_votes_matrix(univ, key, vt); G <- gv$G
    party_cols <- character(0)
    if (!is.null(G) && ncol(G) > 0L) {
      totals <- colSums(G, na.rm = TRUE)
      party_cols <- names(sort(totals[totals > 0], decreasing = TRUE))
      party_cols <- party_cols[seq_len(min(12L, length(party_cols)))]
    }
    
    # Construir una fila por cada punto semilla
    rows <- list()
    for (k in seq_along(ba$selected_idx)) {
      i <- ba$selected_idx[k]
      # Secciones dentro del radio de este punto
      dists <- sqrt(rowSums((coords_proj - matrix(coords_proj[i, ], nrow = NROW(coords_proj),
                                                  ncol = 2, byrow = TRUE))^2))
      inside <- which(dists <= ba$radius)
      n_secs <- length(inside)
      
      # Votos impactados
      c_tot <- total_col(univ, key); c_ln <- ln_col(univ, key)
      tot_buf <- if (!is.na(c_tot) && c_tot %in% names(univ))
        sum(as_num(univ[[c_tot]][inside]), na.rm = TRUE) else NA_real_
      ln_buf <- if (!is.na(c_ln) && c_ln %in% names(univ))
        sum(as_num(univ[[c_ln]][inside]), na.rm = TRUE) else NA_real_
      part_buf <- if (is.finite(ln_buf) && ln_buf > 0 && is.finite(tot_buf)) tot_buf / ln_buf else NA_real_
      
      row <- data.frame(
        PUNTO = k,
        SECCION_CENTRO = univ$SECCION[i],
        SECCIONES_IMPACTADAS = n_secs,
        stringsAsFactors = FALSE
      )
      
      # Coordenadas
      if (!is.null(ba$seed_points) && NROW(ba$seed_points) >= k) {
        row$LNG <- ba$seed_points$LNG[k]
        row$LAT <- ba$seed_points$LAT[k]
        row$DIRECCION <- ba$seed_points$DIRECCION_GOOGLE[k]
      } else {
        xy <- coords_from_sf(univ[i, ])
        row$LNG <- xy$LNG; row$LAT <- xy$LAT
        row$DIRECCION <- NA_character_
      }
      
      row$TOTAL_VOTOS <- tot_buf
      row$LISTA_NOMINAL <- ln_buf
      row$PARTICIPACION <- part_buf
      
      # Votos de cada partido impactados por este buffer
      if (!is.null(G) && length(party_cols) > 0) {
        for (p in party_cols) {
          row[[p]] <- sum(G[inside, p], na.rm = TRUE)
        }
      }
      
      # Variables INEGI seleccionadas (sumar por buffer)
      inegi_sel <- buf_inegi_vars()
      if (length(inegi_sel) > 0) {
        for (v in inegi_sel) {
          col_nm <- INEGI_COL_MAP[[v]]
          if (!is.null(col_nm) && col_nm %in% names(univ)) {
            row[[v]] <- sum(as_num(univ[[col_nm]][inside]), na.rm = TRUE)
          }
        }
      }
      
      rows[[k]] <- row
    }
    
    out <- do.call(rbind, rows)
    
    # ── Fila TOTAL: unión de todos los buffers (sin doble conteo) ──
    all_inside <- unique(unlist(lapply(seq_along(ba$selected_idx), function(k) {
      i <- ba$selected_idx[k]
      dists <- sqrt(rowSums((coords_proj - matrix(coords_proj[i, ], nrow = NROW(coords_proj),
                                                  ncol = 2, byrow = TRUE))^2))
      which(dists <= ba$radius)
    })))
    n_total <- length(all_inside)
    c_tot <- total_col(univ, key); c_ln <- ln_col(univ, key)
    tot_all <- if (!is.na(c_tot) && c_tot %in% names(univ))
      sum(as_num(univ[[c_tot]][all_inside]), na.rm = TRUE) else NA_real_
    ln_all <- if (!is.na(c_ln) && c_ln %in% names(univ))
      sum(as_num(univ[[c_ln]][all_inside]), na.rm = TRUE) else NA_real_
    part_all <- if (is.finite(ln_all) && ln_all > 0 && is.finite(tot_all)) tot_all / ln_all else NA_real_
    total_row <- data.frame(
      PUNTO = NA_integer_, SECCION_CENTRO = NA_integer_,
      SECCIONES_IMPACTADAS = n_total,
      LNG = NA_real_, LAT = NA_real_,
      DIRECCION = "\u2211 TOTAL (uni\u00f3n)",
      TOTAL_VOTOS = tot_all, LISTA_NOMINAL = ln_all, PARTICIPACION = part_all,
      stringsAsFactors = FALSE)
    if (!is.null(G) && length(party_cols) > 0) {
      for (p in party_cols) total_row[[p]] <- sum(G[all_inside, p], na.rm = TRUE)
    }
    # INEGI totales
    inegi_sel <- buf_inegi_vars()
    if (length(inegi_sel) > 0) {
      for (v in inegi_sel) {
        col_nm <- INEGI_COL_MAP[[v]]
        if (!is.null(col_nm) && col_nm %in% names(univ)) {
          total_row[[v]] <- sum(as_num(univ[[col_nm]][all_inside]), na.rm = TRUE)
        }
      }
    }
    out <- rbind(out, total_row)
    out
  })
  
  output$buf_tbl <- renderDT({
    req(has_buf()); d <- buf_tbl_data()
    validate(need(NROW(d) > 0, "Sin puntos"))
    
    # Columnas base vs partido vs INEGI
    base_cols <- c("PUNTO", "SECCION_CENTRO", "SECCIONES_IMPACTADAS",
                   "LNG", "LAT", "DIRECCION",
                   "TOTAL_VOTOS", "LISTA_NOMINAL", "PARTICIPACION")
    inegi_sel <- buf_inegi_vars()
    party_num_cols <- setdiff(names(d), c(base_cols, inegi_sel))
    
    # Headers: logos para partidos, labels legibles para INEGI
    col_headers <- names(d)
    for (i in seq_along(col_headers)) {
      cn <- col_headers[i]
      if (cn %in% party_num_cols) {
        logo_html <- party_logo_inline(cn, "16px")
        if (nzchar(logo_html)) col_headers[i] <- paste0(logo_html, cn)
      } else if (cn %in% inegi_sel && NROW(TRADUCTOR) > 0) {
        # Buscar label legible en TRADUCTOR
        idx <- match(cn, TRADUCTOR$VARIABLE)
        if (!is.na(idx)) {
          col_headers[i] <- paste0(
            "<span style='font-size:9px;color:#1A73E8;font-weight:700;'>",
            TRADUCTOR$Eje[idx], "</span><br>",
            "<span style='font-size:10px;'>", TRADUCTOR$Indicador[idx], "</span>")
        }
      }
    }
    
    header_cb <- htmltools::withTags(table(
      class = "display",
      thead(tr(lapply(col_headers, function(h) th(HTML(h)))))
    ))
    
    # Índice de la fila TOTAL (última)
    total_row_idx <- nrow(d)
    
    dt <- datatable(d, rownames = FALSE, filter = "top", container = header_cb,
                    escape = FALSE, extensions = c("Buttons"),
                    options = list(
                      dom = "Bfrtip", buttons = list("copy", "csv", "excel"),
                      pageLength = 30, scrollX = TRUE, scrollY = 380, autoWidth = TRUE,
                      rowCallback = DT::JS(paste0(
                        "function(row, data, index) {",
                        "  if (index === ", total_row_idx - 1, ") {",
                        "    $(row).css({'background':'linear-gradient(90deg,rgba(26,115,232,.08),rgba(26,115,232,.03))',",
                        "                'font-weight':'800','border-top':'2px solid #1A73E8'});",
                        "  }",
                        "}"
                      ))
                    ))
    all_num_cols <- c(
      intersect(c("TOTAL_VOTOS", "LISTA_NOMINAL", "SECCIONES_IMPACTADAS"), names(d)),
      party_num_cols,
      intersect(inegi_sel, names(d))
    )
    all_num_cols <- intersect(all_num_cols, names(d))
    if (length(all_num_cols) > 0L) dt <- dt |> formatRound(all_num_cols, digits = 0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION", digits = 2)
    if ("LNG" %in% names(d)) dt <- dt |> formatRound("LNG", digits = 6)
    if ("LAT" %in% names(d)) dt <- dt |> formatRound("LAT", digits = 6)
    dt
  })
  
  output$download_buf_csv <- downloadHandler(
    filename = function() paste0("pauta_buffer_SC_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) write.csv(buf_tbl_data(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  
  # ─── Forzar render de mapas ocultos ─────────────
  outputOptions(output, "map_time", suspendWhenHidden = FALSE)
  outputOptions(output, "map_buf",  suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
