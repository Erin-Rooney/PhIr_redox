#conceptual figure

source("code/0-packages.R")

conceptual_dat = read.csv("processed/conceptual_figure.csv")


conceptual_figure =
  conceptual_dat %>% 
  mutate(meas = factor(meas, levels = c("redox potential", "iron concentration"))) %>%
  mutate(hypothesis = factor(hypothesis, levels = c("predicted", "observed"))) %>%
  ggplot(aes(y = depth, x = x, color = meas))+
  #geom_line(orientation = "y", show.legend = FALSE, size = 2, alpha = 0.3)+
  geom_smooth(method = "auto", orientation = "y", size = 2, formula = 'y ~ x', se = FALSE)+
  scale_color_manual(values = c("#677e8e", "#88a2b9"))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(45, 0)+
  labs(
    x = ' ',
    y = "depth, cm",
    color = "")+
  scale_x_continuous(position="top")+
  facet_grid(hypothesis~meas, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.5, fill = NA),
        strip.placement = "outside", axis.text.x=element_blank(),  #remove y axis labels
        axis.ticks.x=element_blank())
# 


ggsave("formanuscript/conceptual_figure.png", plot = conceptual_figure, height = 5.5, width = 4.5)
