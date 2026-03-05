# R/data.R ────────────────────────────────────────────────────
# Carga de datos, detección de elecciones, métricas de votos
# ─────────────────────────────────────────────────────────────

# ==========================================================
# CARGA DE DATOS
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

  dl_sf <- NULL
  if (!is.null(PATH_DL) && file.exists(PATH_DL)) {
    dl_sf <- tryCatch({
      lyr <- st_read(PATH_DL, quiet = TRUE) |> safe_make_valid()
      lyr <- tryCatch(st_transform(lyr, 4326), error = function(e) lyr)
      if (is.na(st_crs(lyr))) st_crs(lyr) <- 4326
      dl_id_col <- pick_col(names(lyr), c("DISTRITO_L", "DISTRITO_LOCAL", "DISTRITO", "ID_DL"))
      if (!is.na(dl_id_col)) {
        suppressWarnings(lyr[["DISTRITO_L"]] <- as.integer(lyr[[dl_id_col]]))
      }
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

# ==========================================================
# DETECCIÓN DE ELECCIONES + AÑOS REALES
# ==========================================================
detect_elections <- function(nms) {
  m <- stringr::str_match(nms, "_(ALC|DL|DF|GOB|SEN|PRES)_(\\d{2})$")
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0L) return(data.table(office = character(0), yr2 = character(0)))
  unique(data.table(office = m[, 2], yr2 = m[, 3]))
}

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
# HELPERS DE MÉTRICAS Y VOTOS
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
    return(x)
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
  cols <- grep(paste0("^[A-Z0-9]+_", k$office, "_", k$yr2, "$"), nms, value = TRUE)
  cols <- cols[!grepl("DISTRIBUIDO", cols, fixed = TRUE)]
  cols <- cols[!grepl("^CAND_", cols)]
  if (!length(cols)) return(character(0))
  labs <- vapply(cols, party_from_col, character(1), key = key, vote_type = vote_type)
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
