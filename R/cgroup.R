#' Control Group Matching
#'
#' @param data
#' @param test.ids
#' @param n
#' @param match.var
#' @param group.var
#' @param keep.match.vars
#'
#' @return
#' @export
#'
#' @examples
#'
match <-  function(data,
           test.ids = NULL,
           n = 1,
           match.var = NULL,
           group.var = NULL,
           impute = NULL,
           keep.match.vars = FALSE) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package \"dplyr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop("Package \"tidyr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (class(data)!= "ctrlClass") {
      stop("Data is not ctrlClass, pre-process dataframe through the tidyUp function.")
      }

    if (is.null(group.var)) {
      data %>%
        mutate(group.var = 1)
    }

    if (is.null(match.var)) {
      stop(
        "Matching variables are required for this function to work. Please add at least one matching variable"
      )
    }

    if (is.null(test.ids)) {
      stop("At least one test id is required for this function to work.")
    }

    out <- tapply(data, list(as.factor(test.ids)), score())
    output <- out %>%
      filter(rank <= n)

      return(print(output))
  }
