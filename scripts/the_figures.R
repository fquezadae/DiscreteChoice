#-----Code for the figures
load("C:\\Users\\Lewis\\Documents\\GitHub\\ch4\\obs_data_920.Rdata")

setwd("C://Users//Lewis//Documents//ch4")

agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
                                                            ntows = length(unique(haul_id)),
                                                            avg_depth = mean(avg_depth))
agg_effort$set_year <- as.numeric(agg_effort$set_year)

#----------------------------------------------------------------------
state_effort <- obs_data %>% group_by(set_year, d_state) %>% summarize(ntows = length(unique(haul_id))) %>%
  filter(set_year != 2001)

state_effort %>% group_by(set_year) %>% summarize(ntows = sum(ntows))

state_effort %>% ggplot(aes(x = set_year, y = ntows)) + geom_line(aes(group = d_state, colour = d_state)) +
  geom_point(aes(group = d_state, colour = d_state)) + 
  geom_vline(xintercept = 2010.5, lty = 2) + ggsave(width = 10, height = 6.5, 
                                                    file = 'figs/state_tows_effort.png')

obs_data %>% group_by(d_state, set_year, d_port) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = set_year, ntows)) + geom_line(aes(x = set_year, y = ntows, group = d_port,
                                                   colour = d_port)) + 
  geom_vline(xintercept = 2010.5, lty = 2) + geom_point(aes(x = set_year, y = ntows, group = d_port,
                                                            colour = d_port)) +
  facet_wrap(~ d_state) + theme(legend.position = 'none') + ggsave(file = 'figs/state_tows.png', 
                                                                   width = 9.7, height = 6.5)


#----------------------------------------------------------------------
#Overall changes
#Number of vessels decreased
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = nvess)) + geom_line() + 
  geom_point() + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits = c(0, 200)) 

#Number of tows decreased  
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = ntows)) + geom_line() + 
  geom_point() + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits = c(0, 20000)) 

#Was the decrease signficiant
agg_effort1 <- agg_effort %>% filter(set_year >= 2007) %>% as.data.frame

#----------------------------------------------------------------------
source('figs/ch4_fig1.R')
