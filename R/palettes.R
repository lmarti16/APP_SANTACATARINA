# R/palettes.R ────────────────────────────────────────────────
# Paletas de colores, Plotly helpers, base64 logos
# ─────────────────────────────────────────────────────────────

# ==========================================================
# PALETAS — con clamp para evitar warnings
# ==========================================================
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
  attr(pal, "colorType") <- "numeric"
  attr(pal, "colorArgs") <- list(na.color = "#00000000")
  pal
}

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
  v <- ifelse(is.finite(v), pmin(pmax(v, -M), M), NA_real_)
  list(pal = pal, values = v)
}

# ==========================================================
# BASE64 LOGOS PARA PLOTLY
# ==========================================================
party_logo_b64 <- local({
  .cache <- list()
  function(party) {
    party <- toupper(party %||% "")
    if (party %in% names(.cache)) return(.cache[[party]])
    src <- party_logo_src(party)
    if (is.null(src)) {
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

treemap_colorscale_for_party <- function(party) {
  party <- toupper(party %||% "")
  base <- NULL
  if (party %in% names(party_colors)) {
    base <- unname(party_colors[[party]])
  } else {
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
# PLOTLY BAR WITH LOGOS
# ==========================================================
plotly_bar_with_logos <- function(dd, cols_bar, hover, margin_l = 140,
                                  logo_only = FALSE) {
  levels_y <- levels(dd$lbl)
  n <- length(levels_y)

  has_logo  <- logical(n)
  has_parts <- vector("list", n)
  if (has_b64) {
    for (i in seq_len(n)) {
      b <- party_logo_b64(as.character(levels_y[i]))
      if (!is.null(b)) {
        has_logo[i] <- TRUE
      } else {
        parts_b64 <- coalition_logos_b64(as.character(levels_y[i]))
        if (!is.null(parts_b64) && length(parts_b64) > 0) {
          has_parts[[i]] <- parts_b64
          has_logo[i] <- TRUE
        }
      }
    }
  }

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

  imgs <- list()
  if (has_b64) {
    for (i in seq_len(n)) {
      lbl_i <- as.character(levels_y[i])
      b64_direct <- party_logo_b64(lbl_i)
      if (!is.null(b64_direct)) {
        imgs[[length(imgs) + 1L]] <- list(
          source = b64_direct,
          x = -0.008, xref = "paper",
          y = lbl_i, yref = "y",
          sizex = 0.06, sizey = 0.82,
          xanchor = "right", yanchor = "middle",
          layer = "above"
        )
      } else if (length(has_parts[[i]]) > 0) {
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
