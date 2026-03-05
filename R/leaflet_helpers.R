# R/leaflet_helpers.R ─────────────────────────────────────────
# Funciones auxiliares para mapas Leaflet
# ─────────────────────────────────────────────────────────────

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

# ── Recortar overlays por ATRIBUTO ──────────────────────────
clip_dl_by_attribute <- function(dl_sf, universe_sf) {
  if (is.null(dl_sf) || NROW(dl_sf) == 0) return(NULL)
  if (is.null(universe_sf) || NROW(universe_sf) == 0) return(NULL)
  if (is.na(SECC_DL_COL)) return(NULL)
  dl_nums <- unique(na.omit(as.integer(universe_sf[[SECC_DL_COL]])))
  if (length(dl_nums) == 0) return(NULL)
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

# Helper para crear mapa base leaflet reutilizable
create_base_leaflet <- function(init_bbox) {
  m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
    addProviderTiles(BASEMAPS[["Claro"]], group = "Claro") |>
    addProviderTiles(BASEMAPS[["Oscuro"]], group = "Oscuro") |>
    addProviderTiles(BASEMAPS[["Calles"]], group = "Calles") |>
    addProviderTiles(BASEMAPS[["Sat\u00e9lite"]], group = "Sat\u00e9lite") |>
    addScaleBar(position = "bottomleft") |>
    fitBounds(
      lng1 = as.numeric(init_bbox["xmin"]),
      lat1 = as.numeric(init_bbox["ymin"]),
      lng2 = as.numeric(init_bbox["xmax"]),
      lat2 = as.numeric(init_bbox["ymax"])
    )
  if (has_fullscreen)
    m <- leaflet.extras::addFullscreenControl(m, position = "topleft", pseudoFullscreen = FALSE)
  m
}
