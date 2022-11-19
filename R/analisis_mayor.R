#' Calcula las estimaciones eliminando la venta mayor de forma iterativa.
#'
#' @param df_in base de datos a utilizar
#' @param variable variable para la que deseo realizar los cálculos
#' @param folio identificador de cada unidad
#' @param n es una constante, igual a 1 (no modificar)
#'
#' @return una lista, el primer elemento es la base de datos completa y los
#'   siguientes son los descriptivos de la variable (el primer cálculo elimina
#'   la venta más grande, el segundo elimina las dos más grandes, así sucesivamente)
#' @importFrom rlang `:=`
#' @export
#'
#' @examples analisis_mayor(df_in = LifeCycleSavings, variable = pop15, folio = sr)
analisis_mayor <- function(df_in,
                           variable,
                           folio,
                           n = 1){

  # Distinguir entre base inicial y listas posteriores.
  if(!is.data.frame(df_in)){
    df_in <- df_in$df
  }

  # Número de filas a mantener
  n_filas <- nrow(df_in) - 1

  var_str <- rlang::as_label(dplyr::enquo(variable))
  folio1 <- rlang::as_label(dplyr::enquo(folio))

  df_out <- df_in %>%
    dplyr::slice_min(order_by = {{ variable }},
                     n = n_filas,
                     with_ties = FALSE)

  folio_diff = setdiff(df_in[[folio1]],
                       df_out[[folio1]])

  venta_diff = setdiff(df_in[[var_str]],
                       df_out[[var_str]])

  v_out <- df_out[[var_str]]

  list(df = df_out,
       df_stats = dplyr::tibble(!!paste0(folio1, '_eliminado') := folio_diff,
                                !!paste0(var_str, '_eliminado') := venta_diff,
                                n_filas = n_filas,
                                sum = sum(v_out),
                                mean = mean(v_out),
                                var = stats::var(v_out),
                                min = min(v_out),
                                max = max(v_out))
  )
}
