#Read in already run results

# the_ports <- list("EUREKA", "CHARLESTON (COOS BAY)", 
#   "NEWPORT", "ASTORIA / WARRENTON")

# # the_ports <- "EUREKA"
# the_seed <- 1022
# the_days <- 30
# the_hd <- 5.1 #habit distance
# quota_species = c("Canary Rockfish", "Darkblotched Rockfish",
#   "Pacific Ocean Perch", "Yelloweye Rockfish", "Bocaccio Rockfish")
# a_l <- arg_list(ncores = 10, seed = the_seed, r_c = 1, r_s = 100, ports = the_ports,
#                      h_d = the_hd, dyz = the_days, quota_species = quota_species, 
#                      n_c = 'qcos',
#                 nhauls_sampled = 50)

flz <- list.files("/Volumes/udrive/")

flz <- flz[grep('preds', flz)]
pred_files <- paste0("/Volumes/udrive/", flz[grep('preds', flz)])

pp <- lapply(pred_files, FUN = function(xx) {
  load(xx)
  return(preds)
})
pp <- ldply(pp)

#---------------------------
#Add risk coefficient of 0 to the trev cases
pp[which(pp$net_cost == "trev"), 'risk_coefficient'] <- 0
pp$score1 <- round(pp$score1, digits = 5)
pp$score2 <- round(pp$score2, digits = 5)
pp$score3 <- round(pp$score3, digits = 5)
pp$score4 <- round(pp$score4, digits = 5)

pp <- pp %>% filter(is.na(port) == F)
pp$risk_coefficient <- factor(pp$risk_coefficient, 
  levels = c(0, 1, 5, 10, 50, 100))

pp <- pp %>% filter(is.na(port) == F)
pp <- pp %>% filter(is.na(risk_coefficient) == F)

# pp %>% filter(focus_year >= 2011 & port != "CRESCENT CITY and BROOKINGS" &
#   port != "FORT BRAGG") %>% ggplot(aes(x = focus_year, y = score2, 
#     color = risk_coefficient)) + 
#   geom_jitter(height = 0, width = .15) + facet_wrap(~ port, scales = 'free') 
  

#------------------------------------------------------  
#Table of predictions
revs <- pp %>% filter(net_cost == 'trev' & focus_year < 2011)
revs2 <- pp %>% filter(net_cost == 'qcos' & focus_year >= 2011 & 
  risk_coefficient == 1)
revs2 <- revs2 %>% filter(is.na(revs2$port) == FALSE)

revs <- rbind(revs, revs2)

revs$port <- factor(revs$port, levels = c("ASTORIA / WARRENTON",
  'NEWPORT', 'CHARLESTON (COOS BAY)', 'CRESCENT CITY and BROOKINGS', 'EUREKA',
  "FORT BRAGG"))
revs <- revs %>% group_by(focus_year, risk_coefficient, net_cost, port) %>% 
  filter(row_number() == 1) %>% as.data.frame
revs <- revs %>% arrange(port)

revs1 <- revs %>% dcast(port ~ focus_year, value.var = "score1")
revs1$score <- 'score1'

revs2 <- revs %>% dcast(port ~ focus_year, value.var = "score2")
revs2$score <- 'score2'

revs4 <- revs %>% dcast(port ~ focus_year, value.var = "score4")
revs4$score <- 'score4'

pred_table <- rbind(revs1, revs2, revs4)

write.csv(pred_table, file = 'figs/pred_table.csv', row.names = F)

#------------------------------------------------------      
#Supplementary tables by port
#Astoria
ast1 <- pp %>% filter(port == "ASTORIA / WARRENTON" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score1')
ast1$score <- 'score1'
ast2 <- pp %>% filter(port == "ASTORIA / WARRENTON" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score2')
ast2$score <- 'score2'
ast4 <- pp %>% filter(port == "ASTORIA / WARRENTON" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score4')
ast4$score <- 'score4'

asts <- rbind(ast1, ast2, ast4)
write.csv(asts, file = 'figs/ast_preds.csv', row.names = F)

#newport
new1 <- pp %>% filter(port == "NEWPORT" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score1')
new1$score <- 'score1'
new2 <- pp %>% filter(port == "NEWPORT" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score2')
new2$score <- 'score2'
new4 <- pp %>% filter(port == "NEWPORT" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score4')
new4$score <- 'score4'

news <- rbind(new1, new2, new4)
write.csv(news, file = 'figs/new_preds.csv', row.names = F)

#Charleston
cha1 <- pp %>% filter(port == "CHARLESTON (COOS BAY)" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score1')
cha1$score <- 'score1'
cha2 <- pp %>% filter(port == "CHARLESTON (COOS BAY)" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score2')
cha2$score <- 'score2'
cha4 <- pp %>% filter(port == "CHARLESTON (COOS BAY)" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score4')
cha4$score <- 'score4'

chas <- rbind(cha1, cha2, cha4)
write.csv(chas, file = 'figs/cha_preds.csv', row.names = F)

#Eureka
eur1 <- pp %>% filter(port == "EUREKA" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score1')
eur1$score <- 'score1'
eur2 <- pp %>% filter(port == "EUREKA" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score2')
eur2$score <- 'score2'
eur4 <- pp %>% filter(port == "EUREKA" & focus_year >= 2011 & 
  net_cost == "qcos") %>% arrange(risk_coefficient) %>% 
  group_by(focus_year, risk_coefficient) %>% filter(row_number() == 1) %>%
  dcast(port + risk_coefficient ~ focus_year, value.var = 'score4')
eur4$score <- 'score4'

eurs <- rbind(eur1, eur2, eur4)
write.csv(eurs, file = 'figs/eur_preds.csv', row.names = F)









# preds1 <- read_files_predict(the_args = a_l, data_in = tows_clust,
#     return_filenames = FALSE)

# al2 <- a_l
# al2$r_c <- 2
# preds2 <- read_files_predict(the_args = al2, data_in = tows_clust,
#     return_filenames = FALSE)
# grep(paste(preds2$V1, collapse = "|"), flz)

# al5 <- a_l
# al5$r_c <- 5
# preds5 <- read_files_predict(the_args = al5, data_in = tows_clust,
#     return_filenames = FALSE)

# al10 <- a_l
# al10$r_c <- 10
# preds10 <- read_files_predict(the_args = al10, data_in = tows_clust,
#     return_filenames = FALSE)
# # grep(paste(preds10$V1, collapse = "|"), flz)

# al50 <- a_l
# al50$r_c <- 50
# preds50 <- read_files_predict(the_args = al50, data_in = tows_clust,
#     return_filenames = FALSE)
# grep(paste(preds50$V1, collapse = "|"), flz)


# al100 <- a_l
# al100$r_c <- 100
# preds100 <- read_files_predict(the_args = al100, data_in = tows_clust,
#     return_filenames = FALSE)

# all_preds <- rbind(preds1, preds2, preds5, preds10, preds50, preds100)
# save(all_preds, file = "output/all_preds.Rdata")