#' Control Group Selection
#'
#' @param test.group A dataframe
#' @param n A number
#' @param matching A dataframe
#' @return You have a long way to go
#' @export

cgroup <- function(data, test.ids = NULL, n = 1, match.var = NULL, group.var = NULL, impute = FALSE){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package \"tidyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  if (is.null(group.var)) {
    data %>%
      mutate(group.var = 1)
  }

  if (is.null(match.var)) {
    stop("Matching variables are required for this function to work. Please add at least one matching variable")
  }

  if (is.null(test.ids)) {
    stop("At least one test id is required for this function to work.")
  }



data <- as.tibble(data)

if (impute == FALSE) {
  data %>%
    group_by()


}




  return(print(output))
}
