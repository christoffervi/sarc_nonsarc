#' Calculate the risk ratio
#'
#' Calculates the risk ratio and confidence intervals for two events.
#'
#' @param data A data frame containing the variables for the two events.
#' @param event1 The name of the first event variable in `data`.
#' @param event2 The name of the second event variable in `data`.
#' @param n_cor The number of multiple comparisons correction (default is 1).
#'
#' @return A tibble with the risk ratio, confidence interval, and p-value.
#'
#' @examples
#' calculate_rr(mydata, var1, var2, 2)
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo as_label
#' @importFrom epitools riskratio
#'
#' @export
calculate_rr <- function(data, event1, event2, n_cor = 1) {
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  data <- {{data}} %>%  select(event1 ={{event1}}, event2= {{event2}})
  epitools::riskratio(data$event1,data$event2, conf.level = 1-(.05/n_cor))$measure %>% 
    matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), paste(name1,"(+)")), c(".est", ".lower", ".upper"))) %>% 
    as_tibble(.,rownames = "rowname") %>% mutate(term=name2) %>% 
    bind_cols(epitools::riskratio(data$event1,data$event2)$p.value %>% 
                matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), 
                                                   paste(name1,"(+)")), 
                                                 c("midp", "fisher", "chi"))) %>% 
                as_tibble(.,rownames = "rowname") %>% 
                select(fisher)) %>% mutate(p = fisher*n_cor) %>% 
    select(rowname,.est,.lower,.upper,p,term)
}
