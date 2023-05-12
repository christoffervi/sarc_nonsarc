(p4+q4+r4)/(p5+q5+r5)/(p1+q1+r1)/(p2+q2+r2)/(p3+q3+r3)+plot_layout(heights = c(3,1,3,.75,.75))+
  plot_annotation(tag_levels = list(c("A","C","E","","","","B","D","F")))
ggsave(filename = "COMBINED.tiff", compression = "lzw", height = 26, width = 40, units = "cm", dpi =900)


inc_edge<-
df %>% group_by(sarc_status) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  mutate(hcm = "HCM",
         from = 0,
         to= row_number())
 inc_node <-
   inc_edge %>% 
   mutate(sarc_status = case_when(str_detect(sarc_status,"0")~"Not tested",
                                  str_detect(sarc_status,"U")~"VUS",
                                  str_detect(sarc_status,"genocop")~"Genocopy",
                                  str_detect(sarc_status,"unc")~"Unclassified",
                                  T~sarc_status),
          id = glue::glue("{sarc_status}, n= {n}")) %>%  select(id, node_nr = to) %>% 
   bind_rows(tibble(id= "HCM, n = 10120", node_nr=0)) %>% 
   mutate(node_group = case_when(str_detect(id, "SAR")~"n1",
                                 id=="HCM, n = 10120"~"n0",
                                 T~"n2")) %>% 
   arrange(node_nr)
inc_node
my_color <-  'd3.scaleOrdinal() .domain(["n0","n1","n2"]) 
.range(["#999999" ,"#377D2B", "#7D2B2B"])'


networkD3::sankeyNetwork(Links = inc_edge, Nodes = inc_node,
                         Source = 'from', Target = 'to', Value = 'n', NodeID = 'id',
                         nodePadding = 50, nodeWidth = 50,
                         NodeGroup = 'node_group',
                         colourScale = my_color,
                         fontSize = 16, fontFamily = "Roboto",
                         width = 400, height = 600) #%>% 
networkD3::saveNetwork("inc.html")
webshot::webshot("inc.html", "inc.png")
