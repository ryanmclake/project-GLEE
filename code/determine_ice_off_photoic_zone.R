ice <- read_csv("./SGL.5.1.csv") %>%
  select(season, photicdepth) %>%
  filter(season == "iceoff") %>%
  summarize(mean_photic = mean(photicdepth, na.rm = T), 
            sd_photic = sd(photicdepth, na.rm = T))

ggplot(ice, aes(averadiation, photicdepth)) +
  geom_point()
