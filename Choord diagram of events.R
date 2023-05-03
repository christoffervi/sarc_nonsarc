m<-
  pairs %>% group_by(pair) %>% 
  filter(row_number()==1) %>% ungroup() %>% 
  select(source, destination, pair_frequency) %>% 
  mutate(source = fct_relevel(source, "htn", "af", "obstruction", "stroke", "icd", "srt", "vt", "htxvad" ),
         # source = factor(source, label = c("Hypertension","Atrial fibrillation","LV obstruction", 
         #                                   "Stroke","ICD","SRT", "Composite VT","Cardiac transplantation"
         #                                       ))
  ) %>%
  arrange(source) %>% rename(n = pair_frequency) %>% 
  pivot_wider(names_from = destination, values_from = n) %>% 
  select(c(.$source)) %>% as.matrix()

m
library(chorddiag)
chord_col <- scico(12, palette = "batlow")
chord_diag<-chorddiag(m, groupnamePadding = 7,
                      width = 650,
                      height = 550,
                      groupThickness = .15,
                      groupPadding = 5,
                      groupColors = chord_col,
                      groupnameFontsize = 17,
                      groupNames = c("Hypertension", "Atrial fibrillation", "LV obstruction", "Stroke", "ICD", "SRT", "Composite VT", "Cardiac transplantation", "Ablation", "Composite HF", "Syncope"))
chord_diag
#htmltools::save_html(chord_diag, "my_chord_diag__.html")
#webshot::webshot("my_chord_diag__.html", "my_chord_diag__.png")

