# Preparación de datos de crédito

Esta carpeta construye los modelos y resultados precalculados que usan las apps
de explicabilidad y evaluación. Las apps deben leer estos artefactos, filtrar sus
tablas y graficarlas; no deben volver a entrenar modelos ni recalcular SHAP, ICE,
ALE o importancias durante su ejecución.

## Qué debo ejecutar

Desde la raíz del repositorio, instala primero las dependencias que te falten:

```r
pak::pkg_install(c(
  "butcher", "cli", "dplyr", "modeldata", "purrr", "randomForest",
  "rpart", "rsample", "tibble", "tidyr", "xgboost"
))
pak::pkg_install("jbkunst/celavi")
```

Para generar **todos los inputs de todas las apps**, ejecuta solamente:

```r
source("R/credit-data/06-prepare-combined.R")
```

El script ejecuta en orden las etapas 01 a 05, valida que sus resultados
pertenezcan al mismo split y a los mismos modelos, y finalmente crea el artefacto
combinado.

`06-prepare-combined.R` siempre reconstruye todas las etapas desde cero y
reemplaza los artefactos anteriores.

> Los scripts usan rutas relativas. Deben ejecutarse desde la raíz de
> `visual-data-lab`, no desde `R/credit-data`.

## Resumen ejecutivo de los scripts

### `00-helpers.R`

**Objetivo:** definir las funciones comunes para entrenar, predecir, guardar
artefactos y calcular log-loss y AUC.

**Entrada:** ninguna; contiene definiciones de funciones.

**Salida:** ninguna en disco. Los otros scripts lo cargan con `source()`.

El cálculo SHAP vive en `shap-explorer/local_shap.R`. Ese archivo conserva
`local_shap_trace()` como versión educativa y lenta, y
`local_shap_trace_optimized()` como versión equivalente para el procesamiento
rápido. Tanto `02-prepare-shap.R` como la app interactiva usan la optimizada;
la educativa se conserva como referencia legible.

### `01-prepare-data.R`

**Objetivo:** preparar `modeldata::credit_data`, eliminar casos incompletos,
crear un split estratificado 75/25, entrenar regresión logística, árbol de
clasificación, Random Forest y XGBoost, y calcular sus probabilidades en test.
También reduce los modelos con `butcher` cuando eso no altera sus predicciones.

**Entrada:** `modeldata::credit_data`.

**Salida:**

```text
R/credit-data/credit-models.rds
```

Contiene train, test, predictores, modelos, predicciones, baseline y metadatos.
Es el artefacto intermedio del cual dependen las etapas siguientes.

### `02-prepare-shap.R`

**Objetivo:** calcular explicaciones locales mediante una aproximación SHAP
marginal Monte Carlo. Usa el test completo como background y reutiliza las
mismas permutaciones aleatorias entre perfiles para hacerlos comparables.

**Entrada:** `R/credit-data/credit-models.rds`.

**Salida:**

```text
shap-explorer/shap-credit.rds
```

Contiene test, modelos, predicciones, baseline y una contribución por modelo,
observación y predictor. Verifica que baseline más contribuciones reconstruya
cada predicción. Conserva el booster nativo de XGBoost para calcular nuevas
predicciones y explicaciones en el servidor.

### `03-prepare-effects.R`

**Objetivo:** precalcular curvas ICE y efectos ALE para todos los modelos y
predictores. La curva PDP se puede obtener posteriormente promediando ICE.

**Entrada:** `R/credit-data/credit-models.rds`.

**Salida:**

```text
variable-effects/credit-effects.rds
```

Contiene la muestra test, los predictores, `ice_values`, `ale_values` y los
parámetros usados para construir las grillas.

### `04-prepare-importance.R`

**Objetivo:** descomponer la calidad del modelo mediante permutation y una
aproximación marginal SAGE, y preparar curvas compactas para explicar AUC,
Gini, log-loss, KS y cumulative gains en train y test.

**Entradas:**

```text
R/credit-data/credit-models.rds
```

**Salida:**

```text
model-quality/credit-quality.rds
```

Contiene los resultados de permutation y SAGE en `importance_values`, además de
curvas diagnósticas, resúmenes de calidad y pérdidas individuales.

Permutation y SAGE se calculan para log-loss, `1 − AUC ROC` y
`1 − KS`. La app presenta AUC, Gini y KS como métricas donde valores mayores
indican mejor calidad; solo log-loss conserva la dirección de pérdida.

Las magnitudes de métodos con métricas distintas no deben compararse como si
estuvieran en la misma escala.

### `05-prepare-evaluation.R`

**Objetivo:** construir curvas ROC/KS y gains/lift, además del resumen de
log-loss, AUC, Gini y KS. Los scores empatados se agrupan en gains/lift para que
cada punto represente un umbral realmente aplicable.

**Entrada:** `R/credit-data/credit-models.rds`.

**Salida:**

```text
model-evaluation/credit-evaluation.rds
```

Contiene predicciones, `threshold_curve`, `gains_curve` y
`evaluation_summary`.

### `06-prepare-combined.R`

**Objetivo:** ejecutar todo el pipeline, comprobar que los artefactos son
compatibles y consolidar la información compartida sin duplicaciones
innecesarias.

**Entradas:** los scripts 01 a 05 y sus artefactos.

**Salida principal:**

```text
R/credit-data/credit-analysis.rds
```

Contiene modelos, train, test, predicciones, explicaciones, importancias,
evaluación y metadatos en un solo objeto autocontenido.

## Outputs para las apps

Al ejecutar `06-prepare-combined.R` quedan disponibles:

```text
shap-explorer/shap-credit.rds
variable-effects/credit-effects.rds
model-quality/credit-quality.rds
model-evaluation/credit-evaluation.rds
R/credit-data/credit-analysis.rds
```

Las apps actuales pueden usar sus artefactos específicos. Para apps nuevas o
análisis integrados, se recomienda `credit-analysis.rds`.

## Contenido del bundle completo

El objeto `credit-analysis.rds` tiene la siguiente estructura:

| Componente | Contenido | Principal consumidor |
|---|---|---|
| `train` | Muestra usada para entrenar los modelos | Auditoría y reentrenamiento |
| `test` | Casos de evaluación, `row_id`, resultado real y predictores | Todas las apps |
| `predictors` | Nombres de las variables explicativas | Todas las apps |
| `models` | Los cuatro modelos entrenados y reducidos, incluido XGBoost nativo | Auditoría y predicción fuera de Shinylive |
| `predictions` | Probabilidad por `model` y `row_id` | SHAP y evaluación |
| `baseline` | Predicción promedio por modelo | SHAP |
| `explanations$shap_values` | Contribución local por modelo, caso y variable | SHAP Explorer |
| `explanations$ice_values` | Predicciones ICE por modelo, caso, variable y grilla | Variable Effects |
| `explanations$ale_values` | Efectos ALE locales y acumulados | Variable Effects |
| `importance_values` | Permutation y SAGE por métrica | Model Quality Explorer |
| `importance_diagnostics` | ROC, KS, CAP/gains y pérdidas individuales | Model Quality Explorer |
| `evaluation$threshold_curve` | Tasas ROC y brecha KS por umbral | Model Evaluation |
| `evaluation$gains_curve` | Ganancia y lift acumulados, con empates agrupados | Model Evaluation |
| `evaluation$summary` | Log-loss, AUC, Gini, KS y umbral KS | Model Evaluation |
| `metadata` | Semillas, muestra, convenciones y parámetros metodológicos | Todas las apps y auditoría |

## Intersección con los RDS específicos

| Contenido | SHAP | Effects | Importance | Evaluation | Bundle completo |
|---|:---:|:---:|:---:|:---:|:---:|
| `train` | — | — | — | — | Sí |
| `test` | Sí | Sí | — | Resultado dentro de `predictions` | Sí, una vez |
| `predictors` | Sí | Sí | Sí | — | Sí, una vez |
| `models` | Sí, con XGBoost nativo | — | — | — | Sí, con XGBoost nativo |
| `predictions` | Sí | — | — | Sí | Sí, sin repetir `status_bad` |
| `baseline` | Sí | — | — | — | Sí |
| SHAP local | Sí | — | — | — | `explanations$shap_values` |
| ICE | — | Sí | — | — | `explanations$ice_values` |
| ALE | — | Sí | — | — | `explanations$ale_values` |
| Importancias globales | — | — | Sí | — | `importance_values` |
| ROC y KS | — | — | — | Sí | `evaluation$threshold_curve` |
| Gains y lift | — | — | — | Sí | `evaluation$gains_curve` |
| Resumen de métricas | — | — | — | Sí | `evaluation$summary` |
| Metadatos comunes | Sí | Sí | Sí | Sí | Una sola copia más metadatos por método |

Los RDS específicos son subconjuntos orientados a cada app. El bundle no los
guarda como cuatro objetos anidados: extrae sus tablas útiles, conserva una sola
copia de los objetos compartidos y organiza los resultados por tema.

## Consideraciones metodológicas

- Todos los modelos reciben los mismos predictores numéricos y el mismo train.
- El test también se usa como distribución de referencia para explicaciones e
  importancias. Esto es práctico para una app educativa, pero una evaluación de
  producción debería reservar un test final independiente.
- Los hiperparámetros son especificaciones fijas; el pipeline compara esas
  especificaciones y no pretende optimizar cada algoritmo.
- SHAP y SAGE son aproximaciones Monte Carlo marginales, no cálculos exactos.
- Las semillas, el tamaño de la muestra y la cantidad de casos incompletos
  eliminados quedan registrados en los metadatos.
