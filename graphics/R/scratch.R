## Put together

main_plot <- df %>% filter(yr_mo != "10_2018" & yr_mo != "11_2018" & yr_mo != "12_2018") %>% 
  ggplot(aes(x=yr_mo, fill=yr)) +
  geom_bar() +
  ggtitle("The title", subtitle = "Volume consitency check by year & month") +
  axis_combmatrix(sep = "_") +
  xlab("Year / Month") +
  theme_combmatrix(combmatrix.label.text = element_text(color = "black", size=10),
                   combmatrix.label.make_space = FALSE,
                   combmatrix.label.extra_spacing =  0 ,
                   plot.margin = unit(c(1.5, 1.5, 1.5, 1), "pt"))

side_plot <- df %>% filter(yr_mo != "10_2018" & yr_mo != "11_2018" & yr_mo != "12_2018") %>%  
  select(mo) %>%
  unnest(cols = mo) %>%
  count(mo) %>%
  mutate(mo = fct_reorder(as.factor(mo), mo, .desc = TRUE)) %>%
  ggplot(aes(y = n, x = mo)) +
  geom_col() +
  coord_flip() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank()) # +
#  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

table(df$yr, df$mo)
test <- chisq.test(table(df$yr, df$mo))
text1 = paste("X-squared = ",round(test$statistic,3),"\ndf = ",test$parameter, "\np-value = ",round(test$p.value,4))

hold<- ggplot() +
  xlim(1,100) + ylim(1,100) +
  # annotate("text", x = 95, y = 80, size=3.5, label = "Test:", hjust=1, vjust=1, fontface =2,colour = "darkgrey") +
  annotate("text", x = 95, y = 70, size=3.5, label = text1, hjust=1, vjust=1, colour = "darkgrey") + 
  theme_void()

p<-cowplot::plot_grid(
  #cowplot::plot_grid(NULL, side_plot + theme(plot.margin = unit(c(-15, -20, 9, 1), "pt")), ncol = 1, rel_heights = c(1.6, 1)),
  cowplot::plot_grid(hold, side_plot + theme(plot.margin = unit(c(-15, -20, 9, 1), "pt")), ncol = 1, rel_heights = c(1.6, 1)),
  main_plot, nrow = 1, rel_widths = c(1, 3.5)
)

p