#' Calculate the risk ratio of an event occurring after another exposure
#'
#' Calculates the risk ratio and confidence intervals for two events by age group.
#'
#' @param data A data frame containing the variables for the two events.
#' @param event1 The name of the first event variable in `data`.
#' @param event2 The name of the second event variable in `data`.
#' @param n_cor The number of multiple comparisons correction (default is 1).
#' @param identical Is the timing of the events allowed to be identical (default is TRUE). Setting to FALSE will delete events registered simultaneously.
#'
#' @return A tibble with the risk ratio, confidence interval, and p-value.
#'
#' @examples
#' calculate_rr_age(mydata, var1, var2, 2)
#'
#' @importFrom dplyr case_when if_else mutate select
#' @importFrom rlang enquo as_label
#' @importFrom epitools riskratio
#'
#' @export
calculate_rr_age <-
  function(data, event1, event2, n_cor = 1, identical =T) {
    event1 <- enquo(event1)
    event2 <- enquo(event2)
    name1 <- as_label(event1)
    name2 <- as_label(event2)
    
    if(identical ==T)
    {
      data <- {{data}} %>%
        dplyr::select(event1 ={{event1}}, event2= {{event2}}) %>%
        dplyr::mutate(event1= dplyr::case_when(event1<=event2~1,
                                               is.na(event2)&!is.na(event1)~1,
                                               T~0),
                      event2= dplyr::if_else(is.na(event2),0,1))}
    else{data <- {{data}} %>%
      dplyr::filter(event1==event2) %>%
      dplyr::select(event1 ={{event1}}, event2= {{event2}}) %>%
      dplyr::mutate(event1= dplyr::case_when(event1<event2~1,
                                             is.na(event2)&!is.na(event1)~1,
                                             T~0),
                    event2= dplyr::if_else(is.na(event2),0,1))
    }
    
    epitools::riskratio(data$event1,data$event2, conf.level = 1-(.05/n_cor))$measure %>%
      matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), paste(name1,"(+)")), c(".est", ".lower", ".upper"))) %>%
      tibble::as_tibble(.,rownames = "exposure") %>% dplyr::mutate(outcome=name2) %>%
      dplyr::bind_cols(epitools::riskratio(data$event1,data$event2)$p.value %>%
                         matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"),
                                                            paste(name1,"(+)")),
                                                          c("midp", "fisher", "chi"))) %>%
                         tibble::as_tibble(.,rownames = "exposure") %>%
                         dplyr::select(fisher)) %>% dplyr::mutate(p = fisher*n_cor) %>%
      dplyr::select(exposure,.est,.lower,.upper,p,outcome)
  }

