#' Control Group Match Plotting
#'
#' @param output
#' @param match.var
#'
#' @return A plot of each chosen matching variable grouped by test stores to check for match quality.
#' @export
#'
#' @examples
#'
plot.control <- function(output,
                         match.var = NULL,
                         all.var = FALSE,
                         xlab = "Time",
                         ylab = "Metric",
                         title = ""
                         ...) {
  if (is.null(match.var)) {
    stop(
      "Matching variables are required for this function to work. Please add at least one matching variable"
    )
  }

  if (all.var == FALSE) {
    output <- output %>%
      mutate(id = as.factor(id)) %>%
      filter(match.var == match.var)
    ggplot(., x = timepoint, y = match.var, color = id,...) +
      geom_point() +
      geom_line() +
      labs (title = title, ylab = ylab, xlab = xlab)
  }

  if (all.var == TRUE) {
    output <- output %>%
      mutate(id = as.factor(id)) %>%
      ggplot(., x = timepoint, y = match.var, color = id,...) +
      geom_point() +
      geom_line() +
      facet_grid(. ~ match.var) +
      labs = title = title
  }





}
