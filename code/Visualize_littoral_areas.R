

littoral_area <- read_csv("./data/global_littoral_area.csv") %>%
  na.omit(.) %>%
  mutate(lit_frac_gres = (1-(1-(3/max_depth_m))^((max_depth_m/mean_depth_m)-1))) %>%
  na.omit(.)

hist(littoral_area$littoral_fraction, breaks = 200)
hist(littoral_area$lit_frac_gres, breaks = 200)



lit_area_compare <- ggplot(littoral_area, aes(lit_frac_gres, littoral_fraction))+
  geom_point(size = 0.05, aes(color = log(max_depth_m))) +
  geom_abline(intercept = 0, slope = 1, color = "red")+
  theme_classic()+
  ylab("GloBathy Littoral Fraction (3m cutoff)")+
  xlab("G-Res Littoral Fraction (3m cutoff)")



