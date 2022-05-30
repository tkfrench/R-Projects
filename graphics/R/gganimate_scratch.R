# Charge libraries:
library(ggplot2)
library(gganimate)


#####################
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# https://ugoproto.github.io/ugo_r_doc/pdf/gganimate.pdf
# https://theanlim.rbind.io/post/gganimate-animations-with-ggplot2/
# https://easings.net/en
# https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#56
#########################
df2 <- PSJHR::FakeDataPOC %>%
  filter(COHORT=='CABG-EMR') %>%
  #mutate(ym = as.Date(paste0(substr(DischargeDt,1,4),'-',substr(DischargeDt,6,7),'-01'), "%y%m")) %>%
  mutate(year =substr(DischargeDt,1,4),
         month=substr(DischargeDt,6,7)) %>%
  mutate(ym =as.Date(paste(year,'01', month, sep = "-"),format = "%Y-%d-%m")) %>%
  select(REGION_ABBR, IP.Mortality.Numerator, IP.Mortality.Denominator, IP.LOS.Numerator,ym)%>%
  group_by(REGION_ABBR, ym)%>%
  summarize(Mortality_rate = sum(IP.Mortality.Numerator, na.rm=TRUE)/sum(IP.Mortality.Denominator, na.rm=TRUE),
            n = n(),
            IP.LOS.Numerator= mean(IP.LOS.Numerator, na.rm=TRUE))

a <- df2 %>%  as.data.frame() %>%
ggplot( aes(IP.LOS.Numerator, Mortality_rate,  size = n, color = REGION_ABBR )) +
  geom_point() +
  #scale_x_log10() +
  theme_bw() +
  labs(title = 'Month: {substr(frame_time,1,7)}', x = 'LOS', y = 'Mortality Rate') +
  transition_time(ym) +
  enter_fade() +
  exit_fade() +

  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  #ease_aes('linear') +
  # view_follow(fixed_y = TRUE) + #Let the view follow the data in each frame
  ease_aes('cubic-in-out') # Slow start and end for a smoother look
##  SAVE ## 
# anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")           
animate(a,nframes = 100, fps=10, end_pause=30) #, width = 400, height = 600, res = 35)

####### As FACETS ##########
df2 %>%  as.data.frame() %>%
  ggplot( aes(IP.LOS.Numerator, Mortality_rate,  size = n, color = REGION_ABBR )) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  #scale_x_log10() +
  theme_bw() +
  labs(title = 'Month: {substr(frame_time,1,7)}', x = 'LOS', y = 'Mortality Rate') +
  facet_wrap(~REGION_ABBR) +
  transition_time(ym) +
  enter_fade() +
  exit_fade() +
  shadow_wake(wake_length = 0.2, size = 5, alpha = FALSE, colour = 'grey92') +
  #ease_aes('linear') +
  # view_follow(fixed_y = TRUE) + #Let the view follow the data in each frame
  ease_aes('cubic-in-out') # Slow start and end for a smoother look





anim <- ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  transition_states(Species, transition_length = 2, state_length = 1) +
  shadow_mark(past = TRUE, future = TRUE, colour = 'grey') +
  view_zoom(pause_length = 1, step_length = 2, nsteps = 3)