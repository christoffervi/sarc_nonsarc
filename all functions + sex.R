
chris_hr_time_interaction <- function(data, id, start, 
                                      time1, event1, 
                                      time2, event2, 
                                      interaction, 
                                      type = "fu",
                                      interaction_term = T,
                                      other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        time1 = {{time1}},
        time2 = {{time2}},
        interaction = {{interaction}},
        stop = {{time2}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time2 > start)
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  if(interaction_term == T){
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var*interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var*interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  } else {
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var+ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var+ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  }}


######################

chris_hr_time_strata <- function(data, id, start, 
                                 time1, event1, 
                                 time2, event2, 
                                 group, 
                                 type = "fu", other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        time1 = {{time1}},
        time2 = {{time2}},
        group = {{group}},
        stop = {{time2}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time2 > start)
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  if (type == "fu") {
    # Perform Cox proportional hazards regression analysis for follow-up time
    df_new %>% 
      coxph(Surv(fu_begin, fu_end, event) ~ var+ strata(group)+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = str_replace(term, "var", name1),
        term = str_replace(term, "event_", ""),
        term = str_replace(term, "strata", ""),
        outcome = name2,
        outcome = str_replace(outcome, "event_", "")
      )
  } else {
    # Perform Cox proportional hazards regression analysis for full time
    df_new %>% 
      coxph(Surv(tstart, tstop, event) ~ var+strata(group)+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = str_replace(term, 'var', name1),
        term = str_replace(term, "event_", ""),
        term = str_replace(term, "strata", ""),
        outcome = name2,
        outcome = str_replace(outcome, "event_", "")
      )
  }
  
}



#' Calculate Hazard Ratios for Time-to-Event Analysis with Time-varying Variables
#'
#' This function performs survival analysis using Cox proportional hazards regression to calculate hazard ratios and confidence intervals for time-to-event data with time-varying effects. It supports two types of analysis: follow-up time analysis and full time analysis.
#'
#' @param data A data frame containing the necessary columns for the analysis.
#' @param id The column name representing the subject ID.
#' @param start The column name representing the start time.
#' @param time1 The column name representing the time variable for event 1.
#' @param event1 The column name representing the event indicator for event 1.
#' @param time2 The column name representing the time variable for event 2.
#' @param event2 The column name representing the event indicator for event 2.
#' @param type A string indicating the type of analysis to be performed. Default is "fu" for follow-up time analysis. Alternative option is "full" for full time analysis.
#'
#' @return A data frame containing the hazard ratios, confidence intervals, and additional information for the specified analysis type.
#'
#' @examples
#' # Example usage for follow-up time analysis
#' result_fu <- chris_hr_time(data = my_data,
#'                                   id = subject_id,
#'                                   start = start_time,
#'                                   time1 = event1_time,
#'                                   event1 = event1_indicator,
#'                                   time2 = event2_time,
#'                                   event2 = event2_indicator,
#'                                   type = "fu")
#'
#' # Example usage for full time analysis
#' result_full <- chris_hr_time(data = my_data,
#'                                      id = subject_id,
#'                                      start = start_time,
#'                                      time1 = event1_time,
#'                                      event1 = event1_indicator,
#'                                      time2 = event2_time,
#'                                      event2 = event2_indicator,
#'                                      type = "full")
#'
#' @references
#' Harrell Jr, F. E., Lee, K. L., & Mark, D. B. (1996). Multivariable prognostic models: issues in developing models, evaluating assumptions and adequacy, and measuring and reducing errors. Statistics in medicine, 15(4), 361-387.
#' Therneau, T. M., & Grambsch, P. M. (2000). Modeling survival data: extending the Cox model. Springer.
#'
#' @export
chris_hr_time <- function(data, id, start, time1, event1, time2, event2, type = "fu", other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        time1 = {{time1}},
        time2 = {{time2}},
        stop = {{time2}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time2 > start)
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  if (type == "fu") {
    # Perform Cox proportional hazards regression analysis for follow-up time
    df_new %>% 
      coxph(Surv(fu_begin, fu_end, event) ~ var+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = if_else(row_number()==1,name1,"sex"),
        term = str_replace(term, "event_", ""),
        outcome = name2,
        outcome = str_replace(outcome, "event_", "")
      )
  } else {
    # Perform Cox proportional hazards regression analysis for full time
    df_new %>% 
      coxph(Surv(tstart, tstop, event) ~ var+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = if_else(row_number()==1,name1,"sex"),
        term = str_replace(term, "event_", ""),
        outcome = name2,
        outcome = str_replace(outcome, "event_", "")
      )
  }
}




#######################




chris_hr_time_interaction2 <- function(data, id, start, 
                                       time1, event1, 
                                       time2, event2, 
                                       interaction, 
                                       type = "fu",
                                       interaction_term = T,
                                       other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        time1 = {{time1}},
        time2 = {{time2}},
        interaction = {{interaction}},
        stop = {{time2}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time2 > start)
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  if(interaction_term == T){
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  } else {
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  }}



########################




chris_hr_time_interaction3 <- function(data, id, start, 
                                       time1, event1, 
                                       time2, event2, 
                                       interaction, 
                                       type = "fu",
                                       interaction_term = T,
                                       other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        time1 = {{time1}},
        time2 = {{time2}},
        interaction = {{interaction}},
        stop = {{time2}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time2 > start)
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  if(interaction_term == T){
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  } else {
    if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, "var", name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var:interaction+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', name1),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "interaction", ""),
          outcome = name2,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  }}

#################
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
      dplyr::select(event1 ={{event1}}, event2= {{event2}}) %>%
      #  dplyr::filter(event1!=event2) %>%
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




##################
chris_hr_time_double_event <- function(data, id, start, 
                                      time1, event1, 
                                      time2, event2, 
                                      time3, event3,type = 'fu',
                                      other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  event3 <- enquo(event3)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  time3 <- enquo(time3)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name3 <- as_label(event3)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        event3 = {{event3}},
        time1 = {{time1}},
        time2 = {{time2}},
        time3 = {{time3}},
        stop = {{time3}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time3 > start) %>% 
    mutate(double_event = case_when(event1+event2==2 & time2>=time1~1,
                                    T~0),
           double_time = case_when(event1+event2==2 & time2>=time1~time2,
                                   T~time3))
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(double_time, double_event, 0),
                             event = event(time3, event3),
                             tstop = time3,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  if (type == "fu") {
      # Perform Cox proportional hazards regression analysis for follow-up time
      df_new %>% 
        coxph(Surv(fu_begin, fu_end, event) ~ var+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', paste(name1,'to', name2)),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "event_", ""),
          outcome = name3,
          outcome = str_replace(outcome, "event_", "")
        )
    } else {
      # Perform Cox proportional hazards regression analysis for full time
      df_new %>% 
        coxph(Surv(tstart, tstop, event) ~ var+other, data = .) %>% 
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
        mutate(
          term = str_replace(term, 'var', paste(name1, 'to', name2)),
          term = str_replace(term, "event_", ""),
          term = str_replace(term, "event_", ""),
          outcome = name3,
          outcome = str_replace(outcome, "event_", "")
        )
    }
  }


################


##################
chris_hr_time_double_event_bidirection <- function(data, id, start, 
                                       time1, event1, 
                                       time2, event2, 
                                       time3, event3,type = 'fu',
                                       other = sex) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  event3 <- enquo(event3)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  time3 <- enquo(time3)
  
  # Convert quosure to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name3 <- as_label(event3)
  name_start <- as_label(start)
  
  {
    # Select relevant columns from the input data
    data <- {{data}} %>%
      dplyr::select(
        id = {{id}},
        start = {{start}},
        event1 = {{event1}},
        event2 = {{event2}},
        event3 = {{event3}},
        time1 = {{time1}},
        time2 = {{time2}},
        time3 = {{time3}},
        stop = {{time3}},
        other = {{other}}
      )
  }
  
  # Filter data based on the condition
  df <- data %>% filter(time3 > start) %>% 
    mutate(double_event = case_when(event1+event2==2~1,
                                    T~0),
           double_time = case_when(event1+event2==2 & time2>=time1~time2,
                                   event1+event2==2 & time2<=time1~time1,
                                   T~time3))
  
  # Perform survival analysis using tmerge and coxph functions
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(double_time, double_event, 0),
                             event = event(time3, event3),
                             tstop = time3,
                             tstart = start
  ) %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  if (type == "fu") {
    # Perform Cox proportional hazards regression analysis for follow-up time
    df_new %>% 
      coxph(Surv(fu_begin, fu_end, event) ~ var+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = str_replace(term, 'var', paste(name1,'to', name2)),
        term = str_replace(term, "event_", ""),
        term = str_replace(term, "event_", ""),
        outcome = name3,
        outcome = str_replace(outcome, "event_", "")
      )
  } else {
    # Perform Cox proportional hazards regression analysis for full time
    df_new %>% 
      coxph(Surv(tstart, tstop, event) ~ var+other, data = .) %>% 
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(
        term = str_replace(term, 'var', paste(name1, 'to', name2)),
        term = str_replace(term, "event_", ""),
        term = str_replace(term, "event_", ""),
        outcome = name3,
        outcome = str_replace(outcome, "event_", "")
      )
  }
}




#################################
# The functions with an additional time-varying covariate



chris_hr_time <- function(data, id, start, time1, event1, time2, event2, 
                          
                          type = "fu", other = sex,
                          t2_srt = NULL, event_srt = NULL) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  other <- enquo(other)
  
  # Optional time-varying covariate
  t2_srt_q <- enquo(t2_srt)
  event_srt_q <- enquo(event_srt)
  
  # Convert labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_srt <- as_label(event_srt_q)
  
  # Build base data with optional columns
  data <- {{data}} %>%
    dplyr::select(
      id = !!id,
      start = !!start,
      event1 = !!event1,
      time1 = !!time1,
      event2 = !!event2,
      time2 = !!time2,
      stop = !!time2,
      other = !!other,
      t2_srt = !!t2_srt_q,
      event_srt = !!event_srt_q
    )
  
  df <- data %>% filter(time2 > start)
  
  # First tmerge with first time-varying covariate
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start)
  
  # Add second time-varying covariate if supplied
  if (!missing(t2_srt) && !missing(event_srt)) {
    df_new <- survival::tmerge(df_new, df, id,
                               var2 = tdc(t2_srt, event_srt, 0))
  }
  
  # Add follow-up intervals
  df_new <- df_new %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  # Build model formula depending on presence of second covariate
  if (!missing(t2_srt) && !missing(event_srt)) {
    formula <- as.formula(Surv(fu_begin, fu_end, event) ~ var + var2 + other)
    formula_full <- as.formula(Surv(tstart, tstop, event) ~ var + var2 + other)
  } else {
    formula <- as.formula(Surv(fu_begin, fu_end, event) ~ var + other)
    formula_full <- as.formula(Surv(tstart, tstop, event) ~ var + other)
  }
  
  # Run Cox model
  model <- if (type == "fu") {
    coxph(formula, data = df_new)
  } else {
    coxph(formula_full, data = df_new)
  }
  
  # Return tidy summary
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      term = case_when(
        term == "var" ~ str_replace(name1, "event_", ""),
        term == "var2" ~ str_replace(name_srt, "event_", ""),
        TRUE ~ as.character(term)
      ),
      outcome = str_replace(name2, "event_", "")
    )
}




###############


chris_hr_time_interaction <- function(data, id, start, 
                                      time1, event1, 
                                      time2, event2, 
                                      interaction, 
                                      type = "fu",
                                      interaction_term = TRUE,
                                      other = sex,
                                      t2_srt = NULL, event_srt = NULL) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  interaction <- enquo(interaction)
  other <- enquo(other)
  
  t2_srt_q <- enquo(t2_srt)
  event_srt_q <- enquo(event_srt)
  
  # Labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_srt <- as_label(event_srt_q)
  
  # Select relevant variables
  data <- {{data}} %>%
    dplyr::select(
      id = !!id,
      start = !!start,
      event1 = !!event1,
      time1 = !!time1,
      event2 = !!event2,
      time2 = !!time2,
      stop = !!time2,
      interaction = !!interaction,
      other = !!other,
      t2_srt = !!t2_srt_q,
      event_srt = !!event_srt_q
    )
  
  df <- data %>% filter(time2 > start)
  
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start)
  
  # Add second time-varying covariate if present
  if (!missing(t2_srt) && !missing(event_srt)) {
    df_new <- survival::tmerge(df_new, df, id,
                               var2 = tdc(t2_srt, event_srt, 0))
  }
  
  df_new <- df_new %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  # Build formula
  if (!missing(t2_srt) && !missing(event_srt)) {
    base_formula <- if (interaction_term) {
      ~ var * interaction + var2 + other
    } else {
      ~ var + var2 + var:interaction + other
    }
  } else {
    base_formula <- if (interaction_term) {
      ~ var * interaction + other
    } else {
      ~ var + var:interaction + other
    }
  }
  
  # Fit model
  model <- if (type == "fu") {
    coxph(update(base_formula, Surv(fu_begin, fu_end, event) ~ .), data = df_new)
  } else {
    coxph(update(base_formula, Surv(tstart, tstop, event) ~ .), data = df_new)
  }
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      term = str_replace_all(term, c("var" = name1, "event_" = "", "interaction" = "")),
      outcome = str_replace(name2, "event_", "")
    )
}





#########
chris_hr_time_strata <- function(data, id, start, 
                                 time1, event1, 
                                 time2, event2, 
                                 group, 
                                 type = "fu", other = sex,
                                 t2_srt = NULL, event_srt = NULL) {
  
  # Convert input parameters to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  group <- enquo(group)
  other <- enquo(other)
  
  t2_srt_q <- enquo(t2_srt)
  event_srt_q <- enquo(event_srt)
  
  # Convert to labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_srt <- as_label(event_srt_q)
  
  # Select relevant variables
  data <- {{data}} %>%
    dplyr::select(
      id = !!id,
      start = !!start,
      event1 = !!event1,
      time1 = !!time1,
      event2 = !!event2,
      time2 = !!time2,
      stop = !!time2,
      group = !!group,
      other = !!other,
      t2_srt = !!t2_srt_q,
      event_srt = !!event_srt_q
    )
  
  df <- data %>% filter(time2 > start)
  
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start)
  
  # Add second time-varying covariate if supplied
  if (!missing(t2_srt) && !missing(event_srt)) {
    df_new <- survival::tmerge(df_new, df, id,
                               var2 = tdc(t2_srt, event_srt, 0))
  }
  
  df_new <- df_new %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  # Build formula
  if (!missing(t2_srt) && !missing(event_srt)) {
    base_formula <- ~ var + var2 + strata(group) + other
  } else {
    base_formula <- ~ var + strata(group) + other
  }
  
  # Run model
  model <- if (type == "fu") {
    coxph(update(base_formula, Surv(fu_begin, fu_end, event) ~ .), data = df_new)
  } else {
    coxph(update(base_formula, Surv(tstart, tstop, event) ~ .), data = df_new)
  }
  
  # Format output
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      term = str_replace_all(term, c("var" = name1, "event_" = "", "strata" = "")),
      outcome = str_replace(name2, "event_", "")
    )
}

##################

chris_hr_time_interaction2srt <- function(data, id, start, 
                                       time1, event1, 
                                       time2, event2, 
                                       interaction, 
                                       type = "fu",
                                       interaction_term = TRUE,
                                       other = sex,
                                       t2_srt = t2_srt, event_srt = event_srt) {
  
  # Convert inputs to quosures
  id <- enquo(id)
  start <- enquo(start)
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  time1 <- enquo(time1)
  time2 <- enquo(time2)
  interaction <- enquo(interaction)
  other <- enquo(other)
  
  t2_srt_q <- enquo(t2_srt)
  event_srt_q <- enquo(event_srt)
  
  # Labels
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  name_srt <- as_label(event_srt_q)
  
  # Select relevant columns
  data <- {{data}} %>%
    dplyr::select(
      id = !!id,
      start = !!start,
      event1 = !!event1,
      time1 = !!time1,
      event2 = !!event2,
      time2 = !!time2,
      stop = !!time2,
      interaction = !!interaction,
      other = !!other,
      t2_srt = !!t2_srt_q,
      event_srt = !!event_srt_q
    )
  
  df <- data %>% filter(time2 > start)
  
  # First tmerge
  df_new <- survival::tmerge(df, df, id,
                             var = tdc(time1, event1, 0),
                             event = event(time2, event2),
                             tstop = time2,
                             tstart = start)
  
  # Add second time-varying covariate if provided
  if (!missing(t2_srt) && !missing(event_srt)) {
    df_new <- survival::tmerge(df_new, df, id,
                               var2 = tdc(t2_srt, event_srt, 0))
  }
  
  df_new <- df_new %>%
    mutate(
      fu = tstop - tstart,
      fu_begin = tstart - start,
      fu_end = tstop - start
    )
  
  # Build formula
  if (!missing(t2_srt) && !missing(event_srt)) {
    base_formula <- if (interaction_term) {
      ~ var:interaction + var2 + other
    } else {
      ~ var:interaction + var2 + other
    }
  } else {
    base_formula <- ~ var:interaction + other
  }
  
  # Fit model
  model <- if (type == "fu") {
    coxph(update(base_formula, Surv(fu_begin, fu_end, event) ~ .), data = df_new)
  } else {
    coxph(update(base_formula, Surv(tstart, tstop, event) ~ .), data = df_new)
  }
  
  # Tidy output
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      term = str_replace_all(term, c("var" = name1, "event_" = "", "interaction" = "")),
      outcome = str_replace(name2, "event_", "")
    )
}
