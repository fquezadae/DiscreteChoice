#----------------------------------------------------------------------
#Make figure of coefficients with CIs
library(ggsidekick)
library(RColorBrewer)
load(file = "output/mod_coefs_wstderror.Rdata")
mod_coefs$low <- mod_coefs$coefs - mod_coefs$ses196
mod_coefs$high <- mod_coefs$coefs + mod_coefs$ses196

mod_coefs$sig <- 'No'
mod_coefs[which(mod_coefs$low < 0 & mod_coefs$high < 0 | 
      mod_coefs$low > 0 & mod_coefs$high > 0), 'sig'] <- 'Yes'

# mod_coefs %>% filter(sig == 'No')

cnames <- data.frame(coef_name = as.character(unique(mod_coefs$coef_name)),
                                    plot_coef_name = c("Individual habit",
                                                       "Individual habit last year",
                                                       "Missing data", "First tow revenue",
                                                       "First tow distance",
                                                       "Later tow revenue", 
                                                       "Later tow distance"))

mod_coefs <- mod_coefs %>% left_join(cnames, by = "coef_name")
mod_coefs$plot_coef_name <- factor(mod_coefs$plot_coef_name, levels = c("First tow distance",
                                            "Later tow distance",
                                            "First tow revenue",
                                            "Later tow revenue",
                                            "Missing data", 
                                            "Individual habit", "Individual habit last year"))


#Jitter the plot years to aid visuals
mod_coefs$plot_year <- 99
ports <- unique(mod_coefs$port)
#Ports order
#ast, new, char, bCC, eur, ftb
ports <- ports[c(1, 6, 2, 3, 4, 5)]
padj <- data.frame(ports = ports, adj = c(-.25, -.15, -.05, .05, .15, .25))
padj$port_name <- c("Astoria", "Newport", "Charleston", "Brookings & Crescent City", "Eureka", "Fort Bragg")
names(padj)[1] <- 'port'

pp <- padj %>% select(port, port_name)
pp$port <- as.character(pp$port)

mod_coefs <- mod_coefs %>% left_join(padj %>% select(port, port_name),
  by = 'port')
mod_coefs$port_name <- factor(mod_coefs$port_name, 
  levels =  c("Astoria", "Newport", "Charleston", 
    "Brookings & Crescent City", "Eureka", "Fort Bragg"))

for(ii in 1:length(ports)){
  inds <- which(mod_coefs$port == ports[ii])  
  the_adj <- padj %>% filter(ports == ports[ii]) %>% pull(adj)
  mod_coefs[inds, 'plot_year'] <- mod_coefs[inds, 'year'] + the_adj
}
mod_coefs$vline <- mod_coefs$year + .5

#Make open and closed based on significance
non_sigs <- mod_coefs %>% filter(sig == 'No')
mod_coefs$port_num <- as.numeric(mod_coefs$port_name)

#----------------------------------------------------------------------
#Make table with the coefficients, formatted 
#as the_coefs_06_27...
mod_coefs$coef_round <- format(round(mod_coefs$coefs, digits = 3),
  nsmall = 3)
mod_coefs$sig_val <- ""
mod_coefs[which(mod_coefs$sig == "Yes"), 'sig_val'] <- "*"
mod_coefs$coef_round <- paste0(mod_coefs$coef_round, " ", 
  mod_coefs$sig_val)
mod_table <- mod_coefs %>% dcast(port_name + plot_coef_name ~ year, 
  value.var = 'coef_round')

#Add in the
write.csv(mod_table, "output/the_coefs_03_20.csv", row.names = F)


#----------------------------------------------------------------------
#The figure png

ggplot(mod_coefs, aes(x = year, colour = port_name, fill = port_name)) + 
  geom_line(aes(x = plot_year, y = coefs, group = port_name), size = .5, alpha = .5) +
  geom_hline(yintercept = 0, lty = 2, col = 'gray') +
  geom_segment(aes(x = plot_year, y = coefs, xend = plot_year, yend = high), alpha = .3) +
  geom_segment(aes(x = plot_year, y = coefs, xend = plot_year, yend = low), alpha = .3) +
  geom_point(aes(x = plot_year, y = coefs, shape = sig)) + 
  geom_vline(xintercept = 2010.5, lty = 2, col = 'gray') +
  geom_point(data = non_sigs, aes(x = plot_year, 
    y = coefs), fill = 'white', pch = 21) + 
  facet_wrap(~ plot_coef_name, scales = 'free_y') + theme_sleek() + 
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3",
    "#e7298a", "#66a61e", "#e6ab02")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3",
    "#e7298a", "#66a61e", "#e6ab02"), guide = FALSE) +
  scale_shape_manual(values = c(1, 19))  + 
  labs(x = "Year", y = "Value", colour = "Port name",
    shape = "Significant (p < 0.05)") +
  theme(legend.position = c(.5, .15), legend.box = 'horizontal') +
  ggsave(file = 'figs/ch4_fig5.png', width = 10, height = 5.4)

#In TIFF
tiff(file = 'figs/ch4_fig5.tiff', width = 10, height = 5.4, res = 300,
  units = 'in')
ggplot(mod_coefs, aes(x = year, colour = port_name, fill = port_name)) + 
  geom_line(aes(x = plot_year, y = coefs, group = port_name), size = .5, alpha = .5) +
  geom_hline(yintercept = 0, lty = 2, col = 'gray') +
  geom_segment(aes(x = plot_year, y = coefs, xend = plot_year, yend = high), alpha = .3) +
  geom_segment(aes(x = plot_year, y = coefs, xend = plot_year, yend = low), alpha = .3) +
  geom_point(aes(x = plot_year, y = coefs, shape = sig)) + 
  geom_vline(xintercept = 2010.5, lty = 2, col = 'gray') +
  geom_point(data = non_sigs, aes(x = plot_year, 
    y = coefs), fill = 'white', pch = 21) + 
  facet_wrap(~ plot_coef_name, scales = 'free_y') + theme_sleek() + 
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3",
    "#e7298a", "#66a61e", "#e6ab02")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3",
    "#e7298a", "#66a61e", "#e6ab02"), guide = FALSE) +
  scale_shape_manual(values = c(1, 19))  + 
  labs(x = "Year", y = "Value", colour = "Port name",
    shape = "Significant (p < 0.05)") +
  theme(legend.position = c(.5, .15), legend.box = 'horizontal')
dev.off()

# ggplot(mod_coefs, aes(colour = port, shape = port),
#        alpha = .5) + geom_point(aes(x = plotyear, y = coefs)) + 
#   geom_segment(aes(x = plotyear, y = coefs, xend = year, yend = high)) +
#   geom_segment(aes(x = year, y = coefs, xend = year, yend = low)) + 
#   geom_vline(xintercept = 2010.5, lty = 2) +
#   # geom_jitter(aes(year, y = coefs)) +
#   facet_wrap(~ coef_name, scales = 'free') + theme_sleek()

# mod_coefs %>% filter(year == 2009, port == "AST", coef_name == 'dummy_prev_days')
