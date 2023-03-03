exploratory_plots <- function(df_stat, key_preds,key_preds_string,ind,i){
  
p1 <- ggplot(df_stat, aes_string(x = "Model", y = ind[i])) +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle=90)) +
  guides(x =  guide_axis(angle = 90))+
  coord_cartesian(ylim=c(-1, 1))+
  labs(x = "Model", y = paste(ind[i],effect_size))

p2 <- ggplot(df_stat, aes_string(ind[i])) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  theme_bw() +
  labs(y = "Density", x = paste(ind[i],effect_size))

p3 <- ggplot(df_stat, aes_string(x = key_preds[i], y = ind[i], colour="Model")) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  viridis::scale_color_viridis(discrete = TRUE)+
  theme_bw() +
  theme(legend.position = "none")+
  labs(y = paste(ind[i],effect_size),
       x = paste(key_preds_string[i],"(% increase)"))

#p4 <- grid.arrange(grobs = list(p1, p3, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 3)))

#bottom_row <- plot_grid(p2, p3, labels = c('B', 'C'), label_size = 12)

#p4 <- plot_grid(p1, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)

plots <- align_plots(p1, p2, align = 'v', axis = 'l')
# then build the bottom row
bottom_row <- plot_grid(plots[[2]], p3, labels = c('B', 'C'), label_size = 10)

# then combine with the top row for final plot
p4 <- plot_grid(plots[[1]], bottom_row, labels = c('A', ''), label_size = 10, ncol = 1)


ggsave(paste0("Outputs/Meta-regression/Exploratory_plots/Effect_size_distribution_",ind[i],model_specs[j],".tiff"),
       plot=p4,  dpi = 600, width = 200, height = 150, units = "mm",device ="tiff",
       compression="lzw", type="cairo")

return (plot)

}
