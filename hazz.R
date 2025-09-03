

#########
# Assuming cumulative_baseline_haz is obtained as shown previously
library(survival)

# Fit the Cox model (using hypothetical data)
cox_model <- coxph(Surv(t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x1<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 20))+
  
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2))
x1
# # Plotting the approximate instantaneous hazard
# plot(time_points, instantaneous_hazard, type = "s", xlab = "Time", ylab = "Instantaneous Hazard",
#      main = "Approximate Instantaneous Baseline Hazard from Cox Model")


cox_model <- coxph(Surv(t2_vt-first_encounter_age, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x2<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<15) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 20))+
  
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Follow-up as timescale')+
  coord_cartesian(xlim = c(0,15), ylim = c(0,2))
x2

cox_model <- coxph(Surv(first_encounter_age, t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x3<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 20))+
  scale_x_continuous(breaks = seq(0,70,10))+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale with left-truncation')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2))
x3


x1+x2+x3
ggsave(filename = 'hazard.pdf', device = cairo_pdf , height = 12, width = 30, units = "cm", dpi =3000)








































################
# Fit the Cox model (using hypothetical data)
cox_model <- coxph(Surv(t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x1<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 5), se = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,0.5))
x1
# # Plotting the approximate instantaneous hazard
# plot(time_points, instantaneous_hazard, type = "s", xlab = "Time", ylab = "Instantaneous Hazard",
#      main = "Approximate Instantaneous Baseline Hazard from Cox Model")


cox_model <- coxph(Surv(t2_vt-first_encounter_age, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x2<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<15) %>% 
  ggplot(aes(x=x, y=y))+
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 5), se = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Follow-up as timescale')+
  coord_cartesian(xlim = c(0,15), ylim = c(0,0.5))
x2

cox_model <- coxph(Surv(first_encounter_age, t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x3<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 5), se = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale with left-truncation')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,0.5))
x3


x1+x2+x3
ggsave(filename = 'hazard2a.pdf', device = cairo_pdf , height = 12, width = 30, units = "cm", dpi =3000)



















#########
# Assuming cumulative_baseline_haz is obtained as shown previously
library(survival)

# Fit the Cox model (using hypothetical data)
cox_model <- coxph(Surv(t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x1<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  #  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2))
x1
# # Plotting the approximate instantaneous hazard
# plot(time_points, instantaneous_hazard, type = "s", xlab = "Time", ylab = "Instantaneous Hazard",
#      main = "Approximate Instantaneous Baseline Hazard from Cox Model")


cox_model <- coxph(Surv(t2_vt-first_encounter_age, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x2<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  #  filter(instantaneous_hazard>0) %>% 
  filter(x<15) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Follow-up as timescale')+
  coord_cartesian(xlim = c(0,15), ylim = c(0,2))
x2

cox_model <- coxph(Surv(first_encounter_age, t2_vt, event_vt) ~ 1, data = dfposneg %>% filter(t2_vt>first_encounter_age))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x3<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  # filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale with left-truncation')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2))
x3



tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 5), se = FALSE)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale with left-truncation')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2))


x1+x2+x3
ggsave(filename = 'hazard3.pdf', device = cairo_pdf , height = 12, width = 30, units = "cm", dpi =3000)


























































#########
# Assuming cumulative_baseline_haz is obtained as shown previously
library(survival)

# Fit the Cox model (using hypothetical data)
cox_model <- coxph(Surv(t2_obstruction, event_obstruction) ~ 1, data = dfposneg %>% filter(t2_obstruction>echo_age0))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x1<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2.5))
x1
# # Plotting the approximate instantaneous hazard
# plot(time_points, instantaneous_hazard, type = "s", xlab = "Time", ylab = "Instantaneous Hazard",
#      main = "Approximate Instantaneous Baseline Hazard from Cox Model")


cox_model <- coxph(Surv(t2_obstruction-first_encounter_age, event_obstruction) ~ 1, data = dfposneg %>% filter(t2_obstruction>echo_age0))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x2<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<15) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Follow-up as timescale')+
  coord_cartesian(xlim = c(0,15), ylim = c(0,2.5))
x2

cox_model <- coxph(Surv(first_encounter_age, t2_obstruction, event_obstruction) ~ 1, data = dfposneg %>% filter(t2_obstruction>echo_age0))
cumulative_baseline_haz <- basehaz(cox_model, centered = FALSE)

# Calculate differences in cumulative hazard and time
diff_cum_haz <- diff(cumulative_baseline_haz$hazard)
diff_time <- diff(cumulative_baseline_haz$time)

# Approximate instantaneous hazard as the rate of change of the cumulative hazard
instantaneous_hazard <- diff_cum_haz / diff_time

# Prepare time points for plotting (mid-points of time intervals)
time_points <- head(cumulative_baseline_haz$time, -1) + diff_time / 2
x3<-
  tibble(x=time_points,y=instantaneous_hazard ) %>% 
  filter(instantaneous_hazard>0) %>% 
  filter(x<75) %>% 
  ggplot(aes(x=x, y=y))+geom_line()+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y= element_line(color = "gray79", linetype=3))+
  
  labs(x= 'Time', y = 'hazard',
       title = 'Age as timescale with left-truncation')+
  coord_cartesian(xlim = c(0,75), ylim = c(0,2.5))
x3


x1+x2+x3
ggsave(filename = 'hazardLVOT.pdf', device = cairo_pdf , height = 12, width = 30, units = "cm", dpi =3000)
