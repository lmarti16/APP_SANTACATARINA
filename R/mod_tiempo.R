# R/mod_tiempo.R ──────────────────────────────────────────────
# Módulo Shiny: pestaña "Tiempo"
# Series temporales de partidos, métricas, participación, mapa comparativo
# ─────────────────────────────────────────────────────────────

mod_tiempo_ui <- function(id, elex = NULL, election_choices = NULL, default_election = NULL) {
  ns <- NS(id)

  offices <- sort(unique((elex %||% data.frame(office = character(0)))$office))
  office_labels_ui <- if (length(offices) > 0) vapply(offices, label_office, character(1)) else character(0)
  election_choices_ui <- election_choices %||% setNames(character(0), character(0))
  default_election_ui <- default_election %||% (if (!is.null(elex) && NROW(elex) > 0) head(elex$key, 1) else NULL)

  div(
    style = "padding:12px;",
    layout_columns(
      col_widths = c(4, 8),
      div(class = "card-panel",
          h5("Configuraci\u00f3n", class = "blockTitle mb-2"),
          checkboxGroupInput(
            ns("ts_offices"), "Tipo de elecci\u00f3n",
            choices  = setNames(offices, office_labels_ui),
            selected = offices, inline = FALSE),
          radioButtons(ns("ts_vote_type"), "Votos para partidos",
                       choices = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO"),
                       selected = "DISTRIBUIDO", inline = TRUE),
          radioButtons(ns("ts_party_metric"), "M\u00e9trica",
                       choices = c("Votos" = "votes", "%" = "pct"),
                       selected = "pct", inline = TRUE),
          selectizeInput(ns("ts_parties"), "Partidos (vac\u00edo = Top N)",
                         choices = NULL, multiple = TRUE,
                         options = list(plugins = list("remove_button"),
                                        placeholder = "PRI, PAN\u2026")),
          sliderInput(ns("ts_top_n"), "Top N", min = 3, max = 15, value = 8, step = 1),
          checkboxInput(ns("ts_include_other"), "Incluir OTROS", value = FALSE),
          div(class = "sep"),
          div(class = "sec-label", "Mapa comparativo"),
          div(class = "smallHelp", HTML(
            "<b>Ref</b> = elecci\u00f3n del sidebar &nbsp;\u00b7&nbsp; <b>Comp</b> = selector abajo")),
          selectInput(ns("ts_map_election"), NULL,
                      choices  = election_choices_ui,
                      selected = default_election_ui),
          radioButtons(ns("ts_map_view"), NULL,
                       choices = c("Participaci\u00f3n (pp)" = "participacion",
                                   "Lista nominal" = "lista",
                                   "Total votos" = "mas_votantes",
                                   "Partido (\u0394)" = "choro_party"),
                       selected = "choro_party", inline = TRUE),
          radioButtons(ns("ts_delta_scale"), "Escala",
                       choices = c("Lineal" = "linear", "Cuantiles" = "quantile"),
                       selected = "linear", inline = TRUE),
          sliderInput(ns("ts_map_opacity"), "Opacidad",
                      min = 0.20, max = 0.90, value = 0.70, step = 0.05),
          conditionalPanel(
            condition = paste0("input['", ns("ts_map_view"), "'] == 'choro_party'"),
            uiOutput(ns("ui_ts_choro_party")),
            radioButtons(ns("ts_choro_metric"), NULL,
                         choices = c("Votos (\u0394)" = "votes", "% (pp)" = "pct"),
                         selected = "votes", inline = TRUE)),
          div(class = "sep"),
          downloadButton(ns("download_ts_parties_csv"), "Serie partidos",
                         class = "btn btn-outline-secondary btn-sm w-100"),
          div(style = "height:6px;"),
          downloadButton(ns("download_ts_metrics_csv"), "Serie m\u00e9tricas",
                         class = "btn btn-outline-secondary btn-sm w-100")
      ),
      div(
        div(class = "card-panel",
            h5("Partidos en el tiempo", class = "blockTitle mb-1"),
            div(class = "smallHelp", "Serie por elecci\u00f3n dentro de la selecci\u00f3n"),
            withSpinner(plotlyOutput(ns("ts_party_plot"), height = "360px"), type = 5, size = 1)),
        div(style = "height:12px;"),
        div(class = "card-panel",
            h5("Mapa (Tiempo)", class = "blockTitle mb-1"),
            div(class = "smallHelp", uiOutput(ns("ts_map_subtitle"))),
            leafletOutput(ns("map_time"), height = 430))
      )
    ),
    div(style = "height:12px;"),
    layout_columns(
      col_widths = c(6, 6),
      div(class = "card-panel",
          h5("M\u00e9tricas en el tiempo", class = "blockTitle mb-1"),
          checkboxGroupInput(ns("ts_metrics"), NULL,
                             choices  = c("Total votos" = "total", "Lista nominal" = "lista",
                                          "Casillas" = "casillas", "V\u00e1lidos" = "validos",
                                          "Nulos" = "nulos"),
                             selected = c("total", "lista"), inline = TRUE),
          withSpinner(plotlyOutput(ns("ts_metrics_plot"), height = "310px"), type = 5, size = 1)),
      div(class = "card-panel",
          h5("Participaci\u00f3n en el tiempo", class = "blockTitle mb-1"),
          withSpinner(plotlyOutput(ns("ts_particip_plot"), height = "310px"), type = 5, size = 1))
    )
  )
}

mod_tiempo_server <- function(id, has_applied, applied, df_applied,
                               clipped_overlays, current_overlay_groups,
                               main_tabs, election_input,
                               elex, default_election) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    elex_local <- elex %||% data.table(office = character(0), key = character(0))
    default_election_local <- default_election %||% (if (NROW(elex_local) > 0) elex_local$key[1] else NULL)

    ts_keys <- reactive({
      req(has_applied())
      sel  <- input$ts_offices %||% sort(unique(elex_local$office))
      keys <- elex_local$key[elex_local$office %in% sel]
      if (!length(keys)) keys <- elex_local$key
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
      ref_key  <- election_input()        %||% default_election_local
      comp_key <- input$ts_map_election %||% default_election_local
      out <- get_valid_distribuido_parties(df, c(ref_key, comp_key))
      validate(need(length(out) > 0L, "Sin partidos"))
      def <- if ("PRI" %in% out) "PRI" else out[1]
      selectInput(ns("ts_choro_party"), "Partido", choices = out, selected = def)
    })

    output$ts_map_subtitle <- renderUI({
      if (!has_applied()) return(span("Presiona GENERAR", style = "color:var(--muted);"))
      ref_key  <- election_input()        %||% default_election_local
      comp_key <- input$ts_map_election %||% default_election_local
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

    # ─── MAPA TIEMPO ───────────────────────────────
    output$map_time <- renderLeaflet({
      create_base_leaflet(INIT_BBOX)
    })

    observeEvent(
      list(
        has_applied(), main_tabs(),
        election_input(), input$ts_map_election,
        input$ts_map_view, input$ts_delta_scale, input$ts_map_opacity,
        input$ts_vote_type, input$ts_choro_party, input$ts_choro_metric
      ),
      {
        req(has_applied())
        if (!identical(main_tabs(), "Tiempo")) return()

        df <- df_applied()
        req(NROW(df) > 0)

        tryCatch({
          ref_key  <- election_input()        %||% default_election_local
          comp_key <- input$ts_map_election  %||% default_election_local
          view     <- input$ts_map_view     %||% "choro_party"
          scale    <- input$ts_delta_scale  %||% "linear"
          opac     <- input$ts_map_opacity  %||% 0.70

          co    <- clipped_overlays()
          og    <- current_overlay_groups()
          proxy <- leafletProxy(ns("map_time"), session = session) |>
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
          # En mapa de Tiempo mantener visible el contorno municipal cuando exista.
          if (!is.null(co$mun) && NROW(co$mun) > 0) {
            proxy <- tryCatch(showGroup(proxy, MUN_GROUP), error = function(e) proxy)
          }
          fit_bounds_padded(proxy, df)

        }, error = function(e) {
          showNotification(paste("Error mapa tiempo:", e$message), type = "error", duration = 8)
        })
      },
      ignoreInit = TRUE
    )

    outputOptions(output, "map_time", suspendWhenHidden = FALSE)

    # Exportar ts_offices para reset
    return(list(
      ts_offices_id = ns("ts_offices")
    ))
  })
}
