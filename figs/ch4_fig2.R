#------------------------------------------------------------------------
#Figure 2

ll_binned <- bin_data(data = obs_data %>% distinct(haul_id, .keep_all = T), 
                      x_col = "avg_long", y_col = "avg_lat", 
                      grid_size = c(.0909, .11), group = "set_year")
ll_binned <- ll_binned %>% filter(year >= 2007)


ll_binned %>% group_by(year) %>% summarize(ntows = sum(count))

#Binning works as it should
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year) %>% summarize(ntows = length(unique(haul_id)))





usa <- map_data('state')



ggplot() + geom_tile(data = ll_binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = "black") + labs(fill = "# tows") + facet_wrap(~ year, ncol = 4) +
  geom_map(data = usa, map = usa, 
           aes(x = long, y = lat, map_id = region), fill = 'gray', 
           colour = 'gray70') + scale_x_continuous(limits = c(-126, -120)) + 
  scale_y_continuous(limits = c(34, 48.2)) + coord_fixed(1.3) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text.x = element_text(angle = 45))
