#' Validation and Analysis of Census Sample Variables
#' @title Validation and Analysis of Census Sample Variables
#' @name validar_muestra_censal
#' @param data Data frame containing census data
#' @param variable Name of the variable to analyze
#' @param filters Vector of filter values for the variable
#' @param labels Vector of labels corresponding to the filter values
#' @return A data frame summarizing the analysis results.
#' @export
#'


validar_muestra_censal <- function(data, variable, filters, labels) {
  if (length(filters) != length(labels)) {
    stop("La longitud de los filtros y las etiquetas debe ser la misma.")
  }
  referencia <- sym(variable)
  data <- data %>% mutate(!!referencia := as.numeric(!!referencia))
  total_filas <- nrow(data)
  total_factor <- sum(data$FACTOR)
  data <- data %>% filter(!!referencia %in% filters)
  recodificar <- setNames(labels, filters)
  data <- data %>% mutate(!!referencia := recode(!!referencia, !!!recodificar))
  data %>%
    rename(indicador = !!referencia) %>%
    group_by(indicador) %>%
    summarise(
      unweighted = n() / total_filas * 100,
      weighted = sum(FACTOR) / total_factor * 100,
      rw = unweighted / weighted,
      sdeff = deffK(FACTOR) %>% round(3),
      .groups = "drop"
    )
}
