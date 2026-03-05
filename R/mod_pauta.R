# R/mod_pauta.R ───────────────────────────────────────────────
# Módulo Shiny: pestaña "PAUTA" (optimización de buffers)
# ─────────────────────────────────────────────────────────────

mod_pauta_ui <- function(id) {
  ns <- NS(id)
  ejes_disponibles <- get0("EJES_DISPONIBLES", ifnotfound = character(0), inherits = TRUE)
  div(
    style = "padding:12px;",
    layout_columns(
      col_widths = c(3, 9),
      div(class = "card-panel",
          h5("PAUTA", class = "blockTitle mb-1"),
          div(class = "smallHelp mb-2", HTML(
            "Usa la elecci\u00f3n y filtros del sidebar.<br>Optimiza por votos de partido o indicador INEGI.")),
          uiOutput(ns("buf_hereda_info")),
          div(class = "sep"),
          radioButtons(ns("buf_mode"), NULL,
                       choices = c("Electoral" = "electoral",
                                   "INEGI" = "inegi"),
                       selected = "electoral", inline = TRUE),
          conditionalPanel(
            condition = paste0("input['", ns("buf_mode"), "'] == 'electoral'"),
            uiOutput(ns("ui_buf_party"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("buf_mode"), "'] == 'inegi'"),
            tagList(
              if (length(ejes_disponibles) > 0) {
                selectInput(ns("buf_ejes"), "Eje",
                            choices  = c("Selecciona..." = "", setNames(ejes_disponibles, ejes_disponibles)),
                            selected = "")
              },
              uiOutput(ns("ui_buf_optim_var"))
            )
          ),
          div(class = "sep"),
          sliderInput(ns("buf_radius"), "Radio (metros)",
                      min = 100, max = 5000, value = 1000, step = 100, post = " m"),
          radioButtons(ns("buf_target_pct"), "Cobertura",
                       choices = c("60%" = 60, "80%" = 80, "100%" = 100),
                       selected = 60, inline = TRUE),
          div(class = "sep"),
          radioButtons(ns("buf_map_mode"), "Mapa",
                       choices = c("Ganador" = "winner",
                                   "Choropleth" = "choro_party",
                                   "Part." = "part",
                                   "Contraste" = "contrast"),
                       selected = "contrast", inline = TRUE),
          sliderInput(ns("buf_opacity"), "Opacidad",
                      min = 0.20, max = 0.90, value = 0.60, step = 0.05),
          div(class = "sep"),
          actionButton(ns("buf_apply"), HTML("&#9881; OPTIMIZAR"),
                       class = "btn btn-accent w-100"),
          div(style = "height:6px;"),
          downloadButton(ns("download_buf_csv"), "CSV",
                         class = "btn btn-outline-secondary btn-sm w-100"),
          div(style = "height:8px;"),
          uiOutput(ns("buf_status"))
      ),
      div(
        div(class = "kpiRow",
            div(class = "kpi", uiOutput(ns("buf_kpi1"))),
            div(class = "kpi", uiOutput(ns("buf_kpi2"))),
            div(class = "kpi", uiOutput(ns("buf_kpi3"))),
            div(class = "kpi", uiOutput(ns("buf_kpi4")))),
        div(style = "height:12px;"),
        div(class = "card-panel",
            h5("Mapa (PAUTA)", class = "blockTitle mb-1"),
            div(class = "smallHelp", "Secciones dentro del buffer"),
            leafletOutput(ns("map_buf"), height = 460)),
        div(style = "height:12px;"),
        layout_columns(
          col_widths = c(6, 6),
          div(class = "card-panel",
              h5("Dentro vs Fuera", class = "blockTitle mb-1"),
              withSpinner(plotlyOutput(ns("buf_compare_bar"), height = "310px"), type = 5, size = 1)),
          div(class = "card-panel",
              div(class = "sec-label", "Top partidos (buffer)"),
              withSpinner(plotlyOutput(ns("buf_party_bar"), height = "310px"), type = 5, size = 1))
        ),
        div(style = "height:12px;"),
        div(class = "card-panel",
            h5("Puntos de activaci\u00f3n", class = "blockTitle mb-1"),
            div(class = "smallHelp", "Solo puntos semilla \u00b7 votos impactados por cada buffer"),
            DTOutput(ns("buf_tbl")))
      )
    )
  )
}

mod_pauta_server <- function(id, has_applied, applied, df_applied,
                              sf_all, dl_sf, mun_sf, dfe_sf,
                              main_tabs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    TRADUCTOR <- get0("TRADUCTOR", ifnotfound = data.frame(), inherits = TRUE)
    if (!is.data.frame(TRADUCTOR)) TRADUCTOR <- data.frame()
    INEGI_COL_MAP <- get0("INEGI_COL_MAP", ifnotfound = NULL, inherits = TRUE)
    if (!(is.character(INEGI_COL_MAP) && length(INEGI_COL_MAP) > 0 && !is.null(names(INEGI_COL_MAP)))) {
      INEGI_COLS <- grep("_INEGI$", names(sf_all), value = TRUE)
      INEGI_VARS <- sub("_INEGI$", "", INEGI_COLS)
      INEGI_COL_MAP <- setNames(INEGI_COLS, INEGI_VARS)
    }

    traductor_label <- function(variable) {
      if (!NROW(TRADUCTOR) || !all(c("VARIABLE", "Indicador") %in% names(TRADUCTOR))) return(variable)
      idx <- match(variable, TRADUCTOR$VARIABLE)
      if (length(idx) == 1L && !is.na(idx)) TRADUCTOR$Indicador[idx] else variable
    }

    buf_applied <- reactiveVal(NULL)

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

    output$ui_buf_optim_var <- renderUI({
      if (!length(INEGI_COL_MAP)) return(div(class = "smallHelp", "No hay variables INEGI disponibles"))
      has_trad <- all(c("Eje", "VARIABLE", "Indicador") %in% names(TRADUCTOR))
      if (!has_trad) {
        vars <- names(INEGI_COL_MAP)
        sel <- input$buf_optim_var %||% vars[1]
        if (!(sel %in% vars)) sel <- vars[1]
        return(selectInput(ns("buf_optim_var"), "Variable a maximizar",
                           choices = as.list(setNames(vars, vars)), selected = sel))
      }
      eje <- input$buf_ejes %||% ""
      if (!nzchar(eje) || NROW(TRADUCTOR) == 0) return(NULL)
      sub <- TRADUCTOR[TRADUCTOR$Eje == eje, ]
      sub <- sub[sub$VARIABLE %in% names(INEGI_COL_MAP), ]
      if (NROW(sub) == 0) return(NULL)
      ch <- setNames(sub$VARIABLE, sub$Indicador)
      sel <- sub$VARIABLE[1]
      selectInput(ns("buf_optim_var"), "Variable a maximizar",
                  choices = ch, selected = sel)
    })

    buf_inegi_vars <- reactive({
      v <- input$buf_optim_var %||% ""
      if (nzchar(v) && v %in% names(INEGI_COL_MAP)) v else character(0)
    })

    df_pauta_universe <- reactive({
      if (has_applied()) df_applied() else sf_all
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
      selectInput(ns("buf_party"), "Partido a maximizar",
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

        if (mode == "inegi") {
          optim_var <- input$buf_optim_var %||% ""
          col_nm    <- INEGI_COL_MAP[[optim_var]]
          if (is.null(col_nm) || !(col_nm %in% names(univ))) {
            showNotification("Selecciona variable INEGI para maximizar.", type = "warning"); return()
          }
          target_vals <- as_num(univ[[col_nm]]); target_vals[!is.finite(target_vals)] <- 0
          pick <- optim_var
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
          traductor_label(pick)
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
        traductor_label(ba$party)
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
      create_base_leaflet(INIT_BBOX)
    })

    observe({
      req(has_buf())
      req(isTRUE(main_tabs() == "PAUTA"))
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

        proxy <- leafletProxy(ns("map_buf"), session = session) |>
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

    # Buffer table
    buf_tbl_data <- reactive({
      req(has_buf()); ba <- buf_applied()
      key <- ba$key; vt <- ba$vt
      validate(need(length(ba$seeds) > 0L, "Sin puntos"))

      univ <- df_pauta_universe()
      req(NROW(univ) > 0)

      univ_proj <- tryCatch(st_transform(univ, 32614), error = function(e) univ)
      centroids_proj <- suppressWarnings(st_centroid(univ_proj))
      coords_proj <- st_coordinates(centroids_proj)

      gv <- group_votes_matrix(univ, key, vt); G <- gv$G
      party_cols <- character(0)
      if (!is.null(G) && ncol(G) > 0L) {
        totals <- colSums(G, na.rm = TRUE)
        party_cols <- names(sort(totals[totals > 0], decreasing = TRUE))
        party_cols <- party_cols[seq_len(min(12L, length(party_cols)))]
      }

      rows <- list()
      for (k in seq_along(ba$selected_idx)) {
        i <- ba$selected_idx[k]
        dists <- sqrt(rowSums((coords_proj - matrix(coords_proj[i, ], nrow = NROW(coords_proj),
                                                    ncol = 2, byrow = TRUE))^2))
        inside <- which(dists <= ba$radius)
        n_secs <- length(inside)

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

        if (!is.null(G) && length(party_cols) > 0) {
          for (p in party_cols) {
            row[[p]] <- sum(G[inside, p], na.rm = TRUE)
          }
        }

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

      # Fila TOTAL
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

      base_cols <- c("PUNTO", "SECCION_CENTRO", "SECCIONES_IMPACTADAS",
                     "LNG", "LAT", "DIRECCION",
                     "TOTAL_VOTOS", "LISTA_NOMINAL", "PARTICIPACION")
      inegi_sel <- buf_inegi_vars()
      party_num_cols <- setdiff(names(d), c(base_cols, inegi_sel))

      col_headers <- names(d)
      for (i in seq_along(col_headers)) {
        cn <- col_headers[i]
        if (cn %in% party_num_cols) {
          logo_html <- party_logo_inline(cn, "16px")
          if (nzchar(logo_html)) col_headers[i] <- paste0(logo_html, cn)
        } else if (cn %in% inegi_sel && NROW(TRADUCTOR) > 0 && all(c("VARIABLE", "Eje", "Indicador") %in% names(TRADUCTOR))) {
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

    outputOptions(output, "map_buf", suspendWhenHidden = FALSE)
  })
}
