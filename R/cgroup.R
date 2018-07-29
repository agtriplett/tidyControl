#' Control Group Matching
#'
#' @param data
#' @param test.ids
#' @param n
#' @param match.var
#' @param group.var
#' @param impute
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


    dq <- data %>%
      group_by(id) %>%
      summarise(count = n())
    imp <- case_when(dq, sum(count < max(count)) > 0 ~ 1,
                     TRUE ~ 0)

    if (is.null(impute) & imp == 1) {
      stop("You have varying data points per id and have chosen not to impute missing values")
    }

    if (!is.null(impute) & imp == 1) {
      data <- merge(expand.grid(group = unique(data$id),
                                time = unique(data$date)),
                    data,
                    all = TRUE)

      for (i in names(match.var)) {
        data <- data %>%
          replace_na(impute(i, na.rm = T))
      }
    }

    outut <- 1




      return(print(output))
  }
