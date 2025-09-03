hr_df %>% left_join(pairs %>% select('term' = source , 'outcome' = destination, pair_frequency, value, end, sarc_status) %>% filter(!is.na(end))) %>% 
  filter(p.value < 0.05/53) %>% 
  filter(estimate >1) %>% 
  filter(!(outcome %in% c('htn'))) %>% 
  filter(!(term %in% c('nyha_hf', 'otherMale', 'sex', 'vt'))) %>% 
  ggplot()+
  #geom_segment(aes(x=value, xend= end, y= term, yend=term, group = outcome), position = position_dodge(width = 1))+
  geom_linerange(aes(xmin=value, xmax= end, y = term,  group = outcome), position = position_dodge(width = .751))+
  
  geom_point(aes(x= value, y = term,group =outcome), color = 'black', position = position_dodge(width = .751))+
  geom_point(aes(x=end, y = term, color = outcome, group =outcome, size = log(estimate)), position = position_dodge(width = .751))+
  facet_wrap(facets = ~sarc_status)




pairs_df %>% 
  filter(sarc_status!='pos_neg') %>% 
  filter(.est >1) %>%
  filter(p<0.05) %>% 
  separate(pair, c('term', 'outcome'), ' ') %>% 
  filter(!(outcome %in% c('htn', 'icd', 'ablation'))) %>% 
  filter(!(term %in% c('nyha_hf', 'otherMale', 'sex', 'vt', 'arrhythmia_nsvt', 'stroke', 'htxvad'))) %>% 
  mutate(term = factor(term, levels = c('htn', 'obstruction', 'af', 'lvsd'), 
                       labels = c('Hypertension', 'LV obstruction', 'Atrial fibrillation', 'LV systolic dysfunction'))) %>% 
  ggplot()+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(0,100,5))+
  
  geom_rect(data = tibble(value = 35, end = 65, 
                          term = seq(.5,3.5,1), termend = seq(1.5,4.5,1),
                          terms = c('0','1', '0', '1')),  
            aes(xmin=value, xmax= end, ymin = term, ymax = termend),
            fill = c('black', 'white', 'black', 'white','black', 'white', 'black', 'white'),
            alpha = .1, show.legend = F)+  
  geom_linerange(aes(xmin=value, xmax= end, y = term,  group = outcome), position = position_dodge(width = .751))+
  
  # geom_point(aes(x= value, y = term,group =outcome), color = 'black', position = position_dodge(width = .751))+
  geom_point(aes(x=end, y = term, color = outcome, group =outcome, size = log(.est)), position = position_dodge(width = .751))+
  geom_text(aes(x=end+1, y = term,  group =outcome, label = outcome),hjust = 0, position = position_dodge(width = .751), size = 3)+
  
  #  scico::scale_color_scico_d()+
  scale_color_manual(values = c(chris_colors_1))+
  facet_wrap(facets = ~sarc_status)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        panel.grid.minor.y= element_line(color = "gray79", linetype=3))

