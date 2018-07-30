#' Match Scoring
#'
#' @param data
#' @param test.id
#' @param match.var
#' @param group.var
#'
#' @return A match score for each compatible id for a given test.id
#'
#' @examples
score <- function(data,test.id, match.var, group.var) {


  match.df <- data %>%
    filter(group.var == test.df$group.var) %>%
    mutate(index = row_number())

  test.df <- data %>%
    filter(id == test.id) %>%
    mutate(index = row_number())

  df <- merge(test.df, match.df, by = "index", all = TRUE, sort = FALSE)

  cor.df <- cor(df$a.value, df$b.value)

  resid.df <- abs(df$a.value - df$b.value)

  score <- cor.df %>%
    left_join(resid.df, by = c("id" = "id")) %>%
    mutate(score = cor.df*resid.df,
           rank = dense_rank())

  return(score)

}
