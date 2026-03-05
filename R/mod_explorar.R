# R/mod_explorar.R ────────────────────────────────────────────
# Módulo Shiny: pestaña "Explorar"
# UI + Server para KPIs, mapa principal, barras, treemaps, tabla
# ─────────────────────────────────────────────────────────────

mod_explorar_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "padding:12px;",
    div(class = "kpiRow",
        div(class = "kpi", uiOutput(ns("kpi1"))),
        div(class = "kpi", uiOutput(ns("kpi2"))),
        div(class = "kpi", uiOutput(ns("kpi3")))),
    div(style = "height:12px;"),
    layout_columns(
      col_widths = c(7, 5),
      div(class = "card-panel",
          h5("Mapa", class = "blockTitle mb-1"),
          div(class = "smallHelp", uiOutput(ns("map_subtitle"))),
          leafletOutput(ns("map"), height = 540)),
      div(class = "card-panel",
          div(style = "display:flex; justify-content:space-between; align-items:center; gap:10px;",
              h5("Resultados", class = "blockTitle mb-1"),
              bslib::input_switch(ns("bar_is_cand"), "Candidaturas", value = FALSE)),
          plotlyOutput(ns("bar"), height = 520))
    ),
    div(style = "height:12px;"),
    layout_columns(
      col_widths = c(6, 6),
      div(class = "card-panel",
          div(style = "display:flex; justify-content:space-between; align-items:center; gap:10px;",
              h5("Treemap \u00b7 candidaturas", class = "blockTitle mb-1"),
              uiOutput(ns("ui_tm_party"))),
          withSpinner(plotlyOutput(ns("treemap_votes"), height = "310px"), type = 5, size = 1)),
      div(class = "card-panel",
          h5("Treemap \u00b7 % sobre v\u00e1lidos", class = "blockTitle mb-1"),
          uiOutput(ns("ui_tm_party_selected")),
          withSpinner(plotlyOutput(ns("treemap_pct"), height = "310px"), type = 5, size = 1))
    ),
    div(style = "height:12px;"),
    div(class = "card-panel",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
            div(h5("Tabla por secci\u00f3n", class = "blockTitle mb-1"),
                div(class = "smallHelp", "KPIs + top votos")),
            radioButtons(ns("table_view"), NULL,
                         choices  = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO", "CANDIDATURAS" = "CAND"),
                         selected = "DISTRIBUIDO", inline = TRUE)),
        DTOutput(ns("tbl")))
  )
}

mod_explorar_server <- function(id, has_applied, applied, df_applied,
                                clipped_overlays, current_overlay_groups,
                                sf_all, dl_sf, mun_sf, dfe_sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      create_base_leaflet(INIT_BBOX)
    })

    observe({
      req(has_applied())
      ap <- applied(); x <- df_metrics(); df <- x$df
      req(nrow(df) > 0)
      tryCatch({
        co    <- clipped_overlays()
        og    <- current_overlay_groups()
        proxy <- leafletProxy(ns("map"), session = session) |>
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
        if (!is.null(co$mun) && NROW(co$mun) > 0) {
          proxy <- tryCatch(showGroup(proxy, MUN_GROUP), error = function(e) proxy)
        }
        fit_bounds_padded(proxy, df)

      }, error = function(e) {
        showNotification(paste("Error mapa:", e$message), type = "error", duration = 8)
      })
    })

    # ─── BARRAS ─────────────────────────────────────
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
      selectInput(ns("tm_party"), NULL, choices = ch, selected = def, width = "160px")
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
      base_cols <- c("SECCION", "TOTAL_VOTOS", "LISTA_NOMINAL", "PARTICIPACION", "CASILLAS", "NULOS")
      party_col_names <- setdiff(names(d), base_cols)

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

    # Exportar tbl_data para el downloadHandler del sidebar
    return(list(tbl_data = tbl_data))
  })
}
