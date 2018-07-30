#' tidyUp Data Frame for matching (object)
#'
#' @param data
#'
#' @return Create an object for input into cgroup function.
#' @export
#'
#' @examples
#' data <- data.frame(id = c(1,1,2,2,3,3),
#'                    timepoint = c(1,2,1,2,1,2),
#'                    metric1 = c(1,2,3,4,5,6),
#'                    metric2 = c(1,1,2,2,3,3))
#'
#' test <- tidyUp(data)
#' head(test)
#'
tidyUp <- function(data, impute = NULL) {

if (sum(colnames(data) %in% c("id", "timepoint"))>0) {
  stop("You must have a variable named 'id' and 'timepoint' in your dataframe.")
  }

  dq <- data %>%
    group_by(id) %>%
    summarise(count = n())
  imp <- case_when(dq, sum(count < max(count)) > 0 ~ 1,
                   TRUE ~ 0) %>%

  if (is.null(impute) & imp == 1) {
    stop("You have varying data points per id and have chosen not to impute missing values")
  }

  if (!is.null(impute) & imp == 1) {
    data <- merge(expand.grid(group = unique(data$id),
                              time = unique(data$timepoint)),
                  data,
                  all = TRUE)

    for (i in names(match.var)) {
      data <- data %>%
        replace_na(impute(i, na.rm = T))
    }
  }

  data <- as.tibble(data) %>%
    melt(., id.vars = c("id", "timepoint"),
         variable.name = "metric",
         value.name = "value")

  setClass("ctrlClass", representation(id = "integer",
                                       timepoint = "date",
                                       metric = "character",
                                       value = "numeric"))
  class(data) <- "ctrlClass"

  return(data)
}

