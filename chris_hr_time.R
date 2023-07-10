#' Calculate Hazard Ratios with Time-Interaction Term for Time-to-Event Analysis
#'
#' This function performs survival analysis using Cox proportional hazards regression to calculate hazard ratios and confidence intervals for time-to-event data. It supports two types of analysis: follow-up time analysis and full time analysis. Additionally, it includes an option to include an interaction term between the time variable and a specified interaction variable.
#'
#' @param data A data frame containing the necessary columns for the analysis.
#' @param id The column name representing the subject ID.
#' @param start The column name representing the start time.
#' @param time1 The column name representing the time variable for event 1.
#' @param event1 The column name representing the event indicator for event 1.
#' @param time2 The column name representing the time variable for event 2.
#' @param event2 The column name representing the event indicator for event 2.
#' @param interaction The column name representing the interaction variable to include in the analysis.
#' @param type A string indicating the type of analysis to be performed. Default is "fu" for follow-up time analysis. Alternative option is "full" for full time analysis.
#' @param interaction_term A logical value indicating whether to include the interaction term in the Cox proportional hazards regression model. Default is TRUE.
#'
#' @return A data frame containing the hazard ratios, confidence intervals, and additional information for the specified analysis type.
#'
#' @examples
#' # Example usage for follow-up time analysis with interaction term
#' result_fu_interact <- chris_hr_time_interaction(data = my_data,
#'                                                 id = subject_id,
#'                                                 start = start_time,
#'                                                 time1 = event1_time,
#'                                                 event1 = event1_indicator,
#'                                                 time2 = event2_time,
#'                                                 event2 = event2_indicator,
#'                                                 interaction = interaction_variable,
#'                                                 type = "fu",
#'                                                 interaction_term = TRUE)
#'
#' # Example usage for full time analysis without interaction term
#' result_full <- chris_hr_time_interaction(data = my_data,
#'                                          id = subject_id,
#'                                          start = start_time,
#'                                          time1 = event1_time,
#'                                          event1 = event1_indicator,
#'                                          time2 = event2_time,
#'                                          event2 = event2_indicator,
#'                                          interaction = interaction_variable,
#'                                          type = "full",
#'                                          interaction_term = FALSE)
#'
#' @references
#' Harrell Jr, F. E., Lee, K. L., & Mark, D. B. (1996). Multivariable prognostic models: issues in developing models, evaluating assumptions and adequacy, and measuring and reducing errors. Statistics in medicine, 15(4), 361-387.
#' Therneau, T. M., & Grambsch, P. M. (2000). Modeling survival data: extending the Cox model. Springer.
#'
#' @export
chris_hr_time_interaction <- function(data, id, start, 
                                      time1, event1, 
                                      time2, event2, 
                                      interaction, 
                                      type = "fu",
                                      interaction_term = T) {
  
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
        stop = {{time2}}
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
      coxph(Surv(fu_begin, fu_end, event) ~ var*interaction, data = .) %>% 
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
      coxph(Surv(tstart, tstop, event) ~ var*interaction, data = .) %>% 
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
        coxph(Surv(fu_begin, fu_end, event) ~ var+ var:interaction, data = .) %>% 
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
        coxph(Surv(tstart, tstop, event) ~ var+ var:interaction, data = .) %>% 
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
