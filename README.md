# APP_SANTACATARINA

Aplicación Shiny para exploración electoral por sección en Santa Catarina, NL, con módulos de:

- **Explorar** (mapa, filtros y tabla principal).
- **Tiempo** (comparativos entre elecciones y métricas históricas).
- **PAUTA** (optimización de puntos/buffers para cobertura electoral o por indicador INEGI).

---

## Qué aprendimos y cómo quedó documentado

Esta sección resume los problemas reales que aparecieron en producción y las decisiones que se tomaron para que la app sea más estable y entendible.

### 1) Mapeo INEGI robusto (`*_INEGI`)

**Problema observado:**
- `Error buffer: objeto 'INEGI_COL_MAP' no encontrado`.
- En algunos flujos `runGitHub()` y recargas, módulos podían ejecutarse sin encontrar objetos globales esperados.

**Aprendizaje / solución:**
- El mapeo INEGI no debe depender de un solo objeto global.
- Se mantiene el mapeo explícito cuando existe, y si no, se deriva desde columnas con sufijo `"_INEGI"` del universo de secciones.
- Se dejó helper global `get_inegi_col_map()` para compatibilidad con rutas reactivas antiguas.

### 2) Compatibilidad de entorno para módulos (`source(..., local = FALSE)`)

**Problema observado:**
- Objetos que sí existen en `app.R` podían no estar visibles para módulos en ciertos contextos.

**Aprendizaje / solución:**
- Exponer en `.GlobalEnv` los objetos críticos de INEGI (`TRADUCTOR`, `EJES_DISPONIBLES`, `INEGI_COL_MAP`) evita errores por alcance.
- Esto reduce diferencias entre ejecución local y `shiny::runGitHub(...)`.

### 3) Etiquetas amigables para variables INEGI en PAUTA

**Problema observado:**
- En tabla de PAUTA aparecían claves técnicas (ej. `PSINDER`) en vez de nombres amigables.

**Aprendizaje / solución:**
- Para encabezados INEGI se usa `TRADUCTOR` (`Eje` + `Indicador`) cuando existe.
- La detección de columnas INEGI se normalizó (mayúsculas/espacios) para evitar falsos negativos.
- Resultado esperado: mostrar etiquetas como **"Población sin afiliación a servicios de salud"** en lugar de `PSINDER`.

### 4) Simplificación en módulo Tiempo

**Cambio aplicado:**
- Se retiraron controles `Top N` e `Incluir OTROS`.
- Comportamiento actual:
  - Si no se seleccionan partidos, se toma fallback automático de top 8.
  - `OTROS` se excluye consistentemente.

**Aprendizaje:**
- Menos controles visibles = menos ambigüedad y menor superficie de error para usuarios finales.

---

## Guía rápida de troubleshooting

Si vuelves a ver algo de este estilo:

- `no se pudo encontrar la función "get_inegi_col_map"`
- `objeto 'INEGI_COL_MAP' no encontrado`

revisa en este orden:

1. Que `TRADUCTOR.csv` tenga columnas: `Eje`, `Indicador`, `VARIABLE`.
2. Que el dataset de secciones incluya columnas `*_INEGI`.
3. Que al iniciar app aparezca un log similar a:
   - `>> INEGI: <n> columnas, <m> en traductor, <k> ejes`
4. Que `app.R` exponga objetos críticos a `.GlobalEnv`.
5. Que `R/utils.R` tenga `get_inegi_col_map()` disponible.

---

## Nota de mantenimiento

Al hacer cambios en PAUTA/INEGI:

- Evitar dependencias frágiles de alcance (scope) en funciones reactivas.
- Preferir fallbacks explícitos sobre hard-fails por objetos faltantes.
- Priorizar etiquetas amigables de `TRADUCTOR` para UI y tablas.
