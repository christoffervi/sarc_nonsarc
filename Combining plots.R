(p4+q4+r4)/(p5+q5+r5)/(p1+q1+r1)/(p2+q2+r2)/(p3+q3+r3)+plot_layout(heights = c(3,1,3,.75,.75))+
  plot_annotation(tag_levels = list(c("A","C","E","","","","B","D","F")))
ggsave(filename = "COMBINED.tiff", compression = "lzw", height = 26, width = 60, units = "cm", dpi =900)
