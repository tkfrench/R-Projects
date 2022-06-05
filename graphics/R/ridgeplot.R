library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

# Plot
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, 
                               rel_min_height = 0.01, 
                               quantile_lines = TRUE, 
                               quantiles = 2) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') 

# Plot
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3,
                               rel_min_height = 0.01, 
                               quantile_lines = TRUE, 
                               quantiles = c(0.025, 0.975), 
                               alpha = 0.7) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') 



ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )

# You can use geom_text to add the labels if you presummarize the quantile values. For example:

library(tidyverse)
theme_set(theme_bw())
library(ggridges)


ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, fill="grey80") +
  geom_text(data=iris %>% group_by(Species) %>% 
              summarise(Sepal.Length=median(Sepal.Length)),
            aes(label=sprintf("%1.1f", Sepal.Length)), 
            position=position_nudge(y=-0.1), colour="red", size=3.5)

#########################
library(PSJHR)
library(dplyr)

df <- FakeDataPOC %>% 
  select(INSTITUTE,COHORT, DischargeDt, IP.LOS.Numerator, IP.LOS.Denominator, 
         Overall_Rating, HLMRScore) %>%
  mutate(IP.LOS.Numerator.log = log(IP.LOS.Numerator),
         IP.LOS.OE            = IP.LOS.Numerator / IP.LOS.Denominator,
         mo = substr(DischargeDt,6,7) ,
         yr = substr(DischargeDt,1,4) ) %>%
  filter(!is.na(HLMRScore) & HLMRScore>60 )  
  
ggplot(df, aes(x = HLMRScore,  y = `COHORT`, fill = ..x..)) +
geom_density_ridges_gradient(scale = 3, 
                             rel_min_height = 0.01, 
                             quantile_lines = TRUE, 
                             quantiles = 2) +
  scale_fill_viridis(name = "Score", option = "C") +
  labs(title = 'HLMR Scores by Cohort') 
