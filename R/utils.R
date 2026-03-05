# R/utils.R ───────────────────────────────────────────────────
# Funciones utilitarias: formateo, colores, logos, helpers
# ─────────────────────────────────────────────────────────────

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

withSpinner <- function(x, ...) x
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  withSpinner <- shinycssloaders::withSpinner
}
has_fullscreen <- requireNamespace("leaflet.extras", quietly = TRUE)
has_ggmap      <- requireNamespace("ggmap", quietly = TRUE)
has_httr       <- requireNamespace("httr", quietly = TRUE)
has_b64        <- requireNamespace("base64enc", quietly = TRUE)

# ==========================================================
# LOGO HELPERS
# ==========================================================
party_logo_src <- function(party) {
  party <- toupper(party %||% "")
  if (party %in% names(party_logo_map)) return(party_logo_map[[party]])
  party_under <- gsub("[[:space:]]", "_", party)
  if (party_under %in% names(party_logo_map)) return(party_logo_map[[party_under]])
  party_nospace <- gsub("[[:space:]]", "", party)
  if (party_nospace %in% names(party_logo_map)) return(party_logo_map[[party_nospace]])
  NULL
}

decompose_coalition <- function(party) {
  party <- toupper(party %||% "")
  parts <- unlist(strsplit(party, "[_\\-\\s]+"))
  parts <- parts[nzchar(parts)]
  if (length(parts) <= 1) return(NULL)
  parts
}

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

# ==========================================================
# COLOR HELPERS
# ==========================================================
guess_party_for_color <- function(label) {
  up  <- toupper(label %||% "")
  hit <- PARTY_SET[vapply(PARTY_SET, function(p) grepl(paste0("\\b", p, "\\b"), up), logical(1))]
  hit[1] %||% NA_character_
}

intensify_hex <- function(hex, factor = 0.25) {
  r <- strtoi(substr(hex, 2, 3), 16L)
  g <- strtoi(substr(hex, 4, 5), 16L)
  b <- strtoi(substr(hex, 6, 7), 16L)
  r <- max(0, round(r * (1 - factor)))
  g <- max(0, round(g * (1 - factor)))
  b <- max(0, round(b * (1 - factor)))
  sprintf("#%02X%02X%02X", r, g, b)
}

fill_color_winner <- function(w) {
  up <- toupper(w %||% "")
  parts <- decompose_coalition(up)
  if (!is.null(parts) && length(parts) >= 2) {
    for (pt in parts) {
      if (pt %in% names(party_colors)) {
        return(intensify_hex(unname(party_colors[[pt]]), 0.2))
      }
    }
  }
  if (up %in% names(party_colors)) return(unname(party_colors[[up]]))
  p <- guess_party_for_color(up)
  if (!is.na(p) && p %in% names(party_colors)) return(unname(party_colors[[p]]))
  "#9AA0A6"
}

hex_mix <- function(a, b, t = 0.5) {
  a <- grDevices::col2rgb(a) / 255
  b <- grDevices::col2rgb(b) / 255
  x <- (1 - t) * a + t * b
  grDevices::rgb(x[1, 1], x[2, 1], x[3, 1], maxColorValue = 1)
}

# ==========================================================
# FORMAT HELPERS
# ==========================================================
pick_col <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) NA_character_ else hit[1L]
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

# ==========================================================
# SF / GEO HELPERS
# ==========================================================
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

# ==========================================================
# INEGI HELPERS
# ==========================================================
get_inegi_col_map <- function(sf_obj = NULL) {
  inegi_map <- get0("INEGI_COL_MAP", ifnotfound = NULL, inherits = TRUE)
  if (is.character(inegi_map) && length(inegi_map) > 0 && !is.null(names(inegi_map))) {
    return(inegi_map)
  }

  src <- sf_obj
  if (is.null(src)) src <- get0("sf_all", ifnotfound = NULL, inherits = TRUE)
  if (is.null(src)) return(setNames(character(0), character(0)))

  inegi_cols <- grep("_INEGI$", names(src), value = TRUE)
  inegi_vars <- sub("_INEGI$", "", inegi_cols)
  setNames(inegi_cols, inegi_vars)
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

# ==========================================================
# GEOCODE HELPERS
# ==========================================================
GOOGLE_GEOCODE_LANGUAGE <- "es"

google_reverse_geocode <- function(lat, lng,
                                   api_key  = GOOGLE_API_KEY,
                                   language = GOOGLE_GEOCODE_LANGUAGE) {
  if (!is.finite(lat) || !is.finite(lng)) return(NA_character_)
  if (!nzchar(api_key) || identical(api_key, "AQUI_TU_API_KEY_GOOGLE")) return(NA_character_)

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
