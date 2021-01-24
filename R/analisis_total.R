
#' Title Calcula las estimaciones con todos los casos.
#'
#' @param base base de datos a utilizar
#' @param variable variable para la que deseo realizar los cálculos
#'
#' @return un objeto, con el número de casos, suma, media, varianza, mínimo y máximo de la variable escogida
#' @export
#'
#' @examples analisis_total(base = LifeCycleSavings, variable = pop15)
analisis_total <- function(base,
                           variable) {
  base %>% dplyr::summarise(n_filas=dplyr::n(),
                            sum=sum({{variable}}),
                            mean=mean({{variable}}),
                            var=stats::var({{variable}}),
                            min=min({{variable}}),
                            max=max({{variable}}))
}
