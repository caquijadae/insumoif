#' Title Obtiene los insumos para calcular las IF, cuando se trabaja con datos del marco
#'
#' @param base1 base de datos a utilizar
#' @param var1 variable a analizar
#' @param id identificador de cada unidad
#'
#' @return un data frame, con los descriptivos (n, suma, media, varianza, minimo, maximo) de la variable a analizar, ademas del folio a eliminar y su respectivo valor de la variable analizada. Cada fila es un calculo distinto: el primero incluye todas las unidades, el segundo excluye la unidad con mayor valor, el tercero las dos unidades con mayor valor, asi sucesivamente hasta llegar a n=1
#' @export
#'
#' @examples analisis_marco(base1 = LifeCycleSavings, id = sr, var1 = pop15)
analisis_marco <- function(base1,
                           id,
                           var1) {

  total_act <- analisis_total(base = base1,
                              variable = {{ var1 }})

  df_acumulada_act <-purrr::map_dfr((purrr::accumulate(1:(nrow(base1)-1),
                                                       analisis_mayor,
                                                       variable = {{ var1 }},
                                                       folio = {{ id }},
                                                       .init = base1)),
                                    'df_stats')

  dplyr::bind_rows(total_act,
                   df_acumulada_act)
}
