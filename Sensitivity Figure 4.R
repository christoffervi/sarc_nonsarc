

library(dplyr)
library(survival)
library(purrr)

df_rmst <- dfposnegproband %>%
  mutate(
    age_entry = primary_diagnosis_age,
    age_exit = t2_death,
    event = event_death
  ) %>%
  filter(
    !is.na(age_entry), !is.na(age_exit), !is.na(event),
    age_exit > age_entry,  # necessary for left-truncation
    age_entry <= 85.1        # only include those eligible to reach tau
  )


age_from <- 44

get_rmst <- function(data, tau = 85.1, from_age = age_from) {
  fit <- survfit(Surv(age_entry, age_exit, event) ~ 1, data = data)
  
  surv_data <- data.frame(time = fit$time, surv = fit$surv)
  surv_data <- surv_data %>% filter(time >= from_age & time <= tau)
  
  if (nrow(surv_data) == 0) return(NA)
  
  # Add boundary points at from_age and tau if not present
  if (!from_age %in% surv_data$time) {
    s1 <- approx(fit$time, fit$surv, xout = from_age)$y
    surv_data <- rbind(data.frame(time = from_age, surv = s1), surv_data)
  }
  if (!tau %in% surv_data$time) {
    s2 <- approx(fit$time, fit$surv, xout = tau)$y
    surv_data <- rbind(surv_data, data.frame(time = tau, surv = s2))
  }
  
  # Trapezoidal AUC
  rmst <- sum(diff(surv_data$time) * head(surv_data$surv, -1))
  return(rmst)
}

set.seed(2024)

bootstrap_rmst <- function(data, group_var, B = 1000, tau = 85.1, from_age = age_from) {
  groups <- unique(data[[group_var]])
  
  boot_results <- map_dfr(1:B, function(i) {
    sample_data <- data %>% group_by(!!sym(group_var)) %>% sample_frac(replace = TRUE) %>% ungroup()
    
    group_rmst <- map_dbl(groups, function(g) {
      d <- sample_data %>% filter(!!sym(group_var) == g)
      get_rmst(d, tau = tau, from_age = from_age)
    })
    
    tibble(iter = i, group = groups, rmst = group_rmst)
  })
  
  summary_df <- boot_results %>%
    group_by(group) %>%
    summarise(
      rmst_mean = mean(rmst, na.rm = TRUE),
      rmst_lower = quantile(rmst, 0.025, na.rm = TRUE),
      rmst_upper = quantile(rmst, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Difference + CI
  diffs <- boot_results %>%
    pivot_wider(names_from = group, values_from = rmst) %>%
    mutate(diff = .[[2]] - .[[3]])  # assumes group 1 vs 2
  
  diff_summary <- tibble(
    group_diff = unique(data[[group_var]])[1],
    rmst_diff = mean(diffs$diff, na.rm = TRUE),
    rmst_diff_lower = quantile(diffs$diff, 0.025, na.rm = TRUE),
    rmst_diff_upper = quantile(diffs$diff, 0.975, na.rm = TRUE)
  )
  
  list(by_group = summary_df, diff = diff_summary)
}

# Run it
results <- bootstrap_rmst(df_rmst, group_var = "sarc_status", B = 100, tau = 85.1, from_age = age_from)

results$by_group  # RMST by group with CI
results$diff      # RMST difference with CI

rmstnumber<-format(round(pull(results$diff[2]),2),nsmall = 1)
########

# Assume df has columns: age_entry, age_exit, event_death, sarc_status
# Prepare survival fit
fit <- survfit(Surv(age_entry, age_exit, event) ~ sarc_status, data = df_rmst)

# Convert to tidy format
library(broom)
df_surv <- broom::tidy(fit) %>%
  filter(time >= 44 & time <= 85.1)



df_surv %>% arrange(time) %>% 
  mutate(sarc_neg = if_else(strata=='sarc_status=SARC(-)', estimate,NA),
         sarc_pos = if_else(strata=='sarc_status=SARC(+)', estimate,NA),
  ) %>% 
  fill(c(sarc_neg, sarc_pos), .direction = 'downup') %>% 
  ggplot() +
  geom_ribbon(aes(x = time, ymin = sarc_neg, ymax = sarc_pos),
              show.legend = F,
              #fill = scico(1, palette = 'cork', begin = .5), 
              fill = 'slategray', 
              alpha = .7) +
  geom_step(aes(x = time, y = estimate, color = strata), size = 1.5,
            show.legend = F) +
  scale_color_scico_d(palette = 'batlow') +
  scale_color_manual(values = c(scico(1, palette = 'batlow'),'#CC79A7'))+
  scale_x_continuous(breaks = seq(44,85,4))+
  scale_y_continuous(breaks = seq(0,1,.1))+
  annotate('text', x = c(72,72,78),
           y= c(0.84,0.62,0.585),
           angle = c(-18,-40,-40),
           family = 'Roboto',
           label = c('Non-sarcomeric', 'Sarcomeric', glue::glue('RMST Δ{rmstnumber} years')))+
  # 
  #  scale_color_manual(values = c("blue", "darkorange")) +
  # scale_fill_manual(values = 'blue')+
  labs(
    title = "Age-Specific Survival (from 44-85 years)",
    x = "Age (years)",
    y = "Survival Probability",
    color = "Group"
  ) +
  coord_cartesian(xlim = c(44, 85), ylim = c(0, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.title =  element_markdown(family = "Roboto", color = "black"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))
ggsave(filename = 'Figure 4.pdf', device = cairo_pdf , height = 12, width = 14, units = "cm", dpi =2000)
ggsave(filename = 'Figure 4.tiff', compression = 'lzw' , height = 12, width = 16, units = "cm", dpi =2000)
ggplotly()

# Figure X. Survival curves and restricted mean survival time (RMST) difference between sarcomeric and non-sarcomeric HCM.
# Kaplan–Meier curves showing age-specific survival from age 50 to 85 for 
# individuals with sarcomeric (pink) and non-sarcomeric (black) hypertrophic cardiomyopathy (HCM). 
# The shaded area between the curves represents the difference in restricted mean survival time (RMST), quantifying the average number of life-years lost 
# between the two groups over this age range. Labels indicate the survival curves, and the gray shaded region illustrates the cumulative difference in survival. 
# RMST difference from age 50 to 80 was estimated to be 3.5 years (95% CI: 0.9 to 4.0) shorter in patients with sarcomeric HCM
# 

theme_minimal(base_size = 14)

