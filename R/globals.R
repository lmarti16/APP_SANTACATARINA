# R/globals.R ─────────────────────────────────────────────────
# Constantes globales, colores, configuración de la app
# ─────────────────────────────────────────────────────────────

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
GOOGLE_API_KEY        <- Sys.getenv("GOOGLE_API_KEY", unset = "")
OPENAI_API_KEY        <- Sys.getenv("OPENAI_API_KEY", unset = "")
AWS_ACCESS_KEY_ID     <- Sys.getenv("AWS_ACCESS_KEY_ID", unset = "")
AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
AWS_DEFAULT_REGION    <- Sys.getenv("AWS_DEFAULT_REGION", unset = "")

# ==========================================================
# PATHS
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
# ESTILO Y COLORES — TEMA MODERNO GLASSMORPHISM
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

PARTY_SET_PURO <- unique(c(
  "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA",
  "PANAL", "NAEM", "PES", "ES", "FXM", "RSP", "SI", "VIDA"
))

office_labels <- c(
  ALC  = "Ayuntamiento",
  DL   = "Dip. local",
  DF   = "Dip. federal",
  GOB  = "Gubernatura",
  SEN  = "Senado",
  PRES = "Presidencia"
)

label_office <- function(x) office_labels[[x]] %||% x

# ── Logos de partidos desde www/ ──────────────────
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
  for (p in PARTY_SET) {
    if (!(p %in% names(party_logo_map))) {
      candidates <- www_files[toupper(tools::file_path_sans_ext(www_files)) == toupper(p)]
      if (length(candidates) > 0) party_logo_map[[p]] <- candidates[1]
    }
  }
  all_www_names <- toupper(tools::file_path_sans_ext(www_files))
  for (i in seq_along(www_files)) {
    nm <- all_www_names[i]
    if (grepl("_", nm) && !(nm %in% names(party_logo_map))) {
      party_logo_map[[nm]] <- www_files[i]
    }
  }
}
