#' Title Obtiene los insumos para calcular las IF, cuando se trabaja con datos de la muestra
#'
#' @param base1 base de datos a utilizar
#' @param id identificador de cada unidad
#' @param var1 variable a analizar, sin expandir
#' @param var2 variable a analizar, multiplicada por el factor de expansion
#'
#' @return un data frame, con los descriptivos (n, suma, media, varianza, minimo,
#'   maximo, suma expandida, media expandida) de la variable a analizar,
#'   ademas del folio a eliminar y su respectivo valor de la variable analizada.
#'   Cada fila es un calculo distinto: el primero incluye todas las unidades,
#'   el segundo excluye la unidad con mayor valor, el tercero las dos unidades
#'   con mayor valor, asi sucesivamente hasta llegar a n=1
#' @import dplyr purrr
#' @export
#'
#' @examples analisis_muestra(base1 = LifeCycleSavings, id = sr, var1 = pop15, var2 = dpi)
analisis_muestra <- function(base1,
                             id,
                             var1,
                             var2) {

  total_act <- analisis_total(base=base1,variable={{var1}})

  df_acumulada_act <- purrr::map_dfr((purrr::accumulate(1:(nrow(base1)-1),
                                                        analisis_mayor,
                                                        variable = {{var1}},
                                                        folio = {{id}},
                                                        .init = base1)),
                                     'df_stats')

  total_act_e <- analisis_total(base = base1,
                                variable = {{var2}})

  df_acumulada_act_e <- purrr::map_dfr((purrr::accumulate(1:(nrow(base1)-1),
                                                          analisis_mayor,
                                                          variable = {{var2}},
                                                          folio = {{id}},
                                                          .init = base1)),
                                       'df_stats')

  dplyr::bind_cols(dplyr::bind_rows(total_act,
                                    df_acumulada_act),
                   dplyr::bind_rows(total_act_e,
                                    df_acumulada_act_e)) %>%
    dplyr::select(1:8, 10:11) %>%
    dplyr::rename(n=1,
                  suma=2,
                  media=3,
                  varianza=4,
                  minimo=5,
                  maximo=6,
                  rut_eliminado=7,
                  venta_eliminada=8,
                  suma_exp=9,
                  media_exp=10)
}
