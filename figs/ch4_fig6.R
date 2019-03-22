#--------------------------------------------------------------
#Load hessian results
load(file = 'output/mod_res_whess_pulled.Rdata')
hess <- lapply(1:length(mod_res), FUN = function(xx){
  out <- mod_res[[xx]]$hess
  # names(out) <- mod_res[[xx]]$filename
  return(out)
})
filenames <- lapply(1:length(mod_res), FUN = function(xx){
  out <- mod_res[[xx]]$filename
  # names(out) <- mod_res[[xx]]$filename
  return(out)
})
filenames <- ldply(filenames)

#--------------------------------------------------------------
# filename <- paste0('model_runs/', list.files('model_runs')[1])
# load(filename)

# #Test to make sure I'm using the hessian and vcov correctly
# vcov(mod) == solve(-mod$hessian)
# coefs <- mod_res[[1]]$coefs
#hessian of the log-likelihood is the negative of the inverse
#of the covariance of the parameter estimates

#--------------------------------------------------------------
nreps <- 1000

set.seed(300)

ratio_cis <- mclapply(1:length(mod_res), FUN = function(ii){
  vc <- solve(-mod_res[[ii]]$hess) #variance-covariance matrix
  cvc <- chol(vc)
  coefs <- mod_res[[ii]]$coefs
  ff <- mod_res[[ii]]$filename
  
  samp_ratios <- lapply(1:nreps, FUN = function(zz){
    #Calculate ratios for one draw
    draws <- rnorm(length(coefs))
    new_coefs <- coefs + cvc %*% draws
    new_ratios <- data.frame(dr1 = new_coefs[5] / new_coefs[4],
      dr = new_coefs[7] / new_coefs[6])
    return(new_ratios)
  })
  samp_ratios <- ldply(samp_ratios)
  emp_values <- data.frame(dr1 = coefs[5] / coefs[4],
    dr = coefs[7] / coefs[6])
  row.names(emp_values) <- NULL
  q1 <- quantile(samp_ratios$dr1, probs = c(.05, .95))
  q2 <- quantile(samp_ratios$dr, probs = c(.05, .95))
  
  ff <- strsplit(mod_res[[ii]]$filename, split = "_")
  port <- ff[[1]][1]
  year <- ff[[1]][5]
  year <- as.integer(gsub("focyr", "", year))
  
  the_out <- data.frame(emp_value = c(emp_values[1, 1], 
    emp_values[1, 2]), ci_05 = c(q1[1], q2[1]),
    ci_95 = c(q1[2], q2[2]), ratio_name = c('dr1', 'dr'),
     year = year, port = port)
  return(the_out)
}, mc.cores = 6)
ratio_cis1 <- ratio_cis

ratio_cis <- ldply(ratio_cis)
ratio_cis$port <- as.character(ratio_cis$port)

#Add in port names
pp <- data.frame(port = unique(ratio_cis$port),
  port_name = c("Astoria", "Charleston", "Brookings & Cres. City",
    "Eureka", "Fort Bragg", "Newport"))
pp <- pp[c(1, 6, 2, 3, 4, 5), ]
pp$caption_label <- paste0(letters[1:6], ") ", pp$port_name)

ratio_cis <- ratio_cis %>% left_join(pp, by = "port")
ratio_cis$port_name <- factor(ratio_cis$caption_label,
  levels = c("a) Astoria", "b) Newport", "c) Charleston", 
    "d) Brookings & Cres. City", "e) Eureka", "f) Fort Bragg"))

#Offset the year slightly
ratio_cis$year_plot <- ratio_cis$year
ratio_cis[which(ratio_cis$ratio_name == 'dr1'), "year_plot"] <- 
 ratio_cis[which(ratio_cis$ratio_name == 'dr1'), "year_plot"] - .15
ratio_cis[which(ratio_cis$ratio_name == 'dr'), "year_plot"] <- 
  ratio_cis[which(ratio_cis$ratio_name == 'dr'), "year_plot"] + .15

#Add coefficient names
ratio_cis$ratio_type <- "poop"
ratio_cis[which(ratio_cis$ratio_name == 'dr1'), "ratio_type"] <- 'First tow'
ratio_cis[which(ratio_cis$ratio_name == 'dr'), "ratio_type"] <- 'Later tow'
ratio_cis$ratio_type <- factor(ratio_cis$ratio_type, 
  levels = c("First tow", "Later tow"))

# ratio_cis %>% filter(port == "AST")
#--------------------------------------------------------------
#Plot the Ratios with the CIs now
library(ggsidekick)
ggplot(ratio_cis, aes(x = year, group = ratio_type)) + 
  geom_point(aes(x = year_plot, y = emp_value,
    shape = ratio_type), size = 1.75) + 
  geom_line(aes(x = year_plot, y = emp_value)) + 
  geom_segment(aes(x = year_plot, xend = year_plot, y = emp_value, 
    yend = ci_95), alpha = .5) +
  geom_segment(aes(x = year_plot, xend = year_plot, y = emp_value, 
    yend = ci_05), alpha = .5) +
  facet_wrap(~ caption_label, scales = 'free_y') + theme_sleek() +
  geom_vline(xintercept = 2010.5, lty = 2, col = 'gray') +
  labs(x = "Year", y = "Distance/Revenue", shape = "") + 
  ggsave(file = "figs/ch4_fig6.png", width = 7.5, height = 5.35)

#--------------------------------------------------------------
