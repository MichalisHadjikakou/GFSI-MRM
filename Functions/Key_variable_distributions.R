# Testing base year and 2050 distributions (can change for any year)
Years <- 2010
Figure_type <- "density"

# Histogram/density plots for universal input variables

input_vars <- c("Population","Total_kcal","Animal_kcal","Vegetal_kcal")
                #"Rum_meat_kcal","Mon_meat_kcal","Dairy_eggs_kcal",
#"Yield_cereal","FCR_monogastrics","FCR_ruminants")

df_year <- DF %>% 
  dplyr::filter(ScenYear %in% Years,) %>% # %>% 
  pivot_longer(cols = input_vars,names_to = "Predictor", values_to = "Value") %>% 
  filter(Included=="Yes")

ggplot(df_year, aes(x=Value)) +
  geom_histogram(colour="grey", fill="darkblue",na.rm=T,alpha=0.2,adjust=1) +
  theme_bw()+
  facet_wrap(~factor(Predictor,levels = input_vars),scales = "free")+
  #geom_vline(aes(xintercept=mean(Value, na.rm=T)),   # Ignore NA values for mean
  #           color="red", linetype="dashed", size=1)
  geom_vline(data = df_year %>% 
             group_by(Predictor) %>%
             dplyr::summarise(mean = mean(Value,na.rm=T)),
             aes(xintercept = mean),color="red", linetype="dashed", size=1)

list <-lapply(1:length(input_vars),
              function(x) ggplot2::qplot(df_year[[input_vars[x]]],
                                         geom = Figure_type,fill=I("purple"),xlab = input_vars[x]))

Key_predictors_plot <- cowplot::plot_grid(plotlist = list)


# Histogram/density plots for all indicators

indicator_vars <- ind # All indicators

df_ind <- DF %>% 
  dplyr::filter(ScenYear %in% Years,) %>% # %>% 
  pivot_longer(cols = all_of(ind),names_to = "Indicator", values_to = "Value") %>% 
  filter(Included=="Yes")

ggplot(df_ind, aes(x=Value)) +
  geom_histogram(colour="black", fill="white",na.rm=T) +
  facet_wrap(~Indicator,scales = "free")+
  #geom_vline(aes(xintercept=mean(Value, na.rm=T)),   # Ignore NA values for mean
  #           color="red", linetype="dashed", size=1)
  geom_vline(data = df_ind %>% 
               group_by(Indicator) %>%
               dplyr::summarise(mean = mean(Value,na.rm=T)),
             aes(xintercept = mean),color="red", linetype="dashed", size=1)


list <-lapply(1:length(indicator_vars),
              function(x) ggplot2::qplot(df_year[[indicator_vars[x]]],
                                         geom = Figure_type,fill=I("purple"),xlab = indicator_vars[x]))

PB_indicator_plot <- cowplot::plot_grid(plotlist = list)
