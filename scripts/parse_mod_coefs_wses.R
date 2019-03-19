#----------------------------------------------------------------------
#Load the new data
mod_files <- list.files('model_runs')
mod_files <- mod_files[grep("_seed10_", mod_files)]
mod_files <- mod_files[grep("hdist5.1", mod_files)]
mod_files <- mod_files[grep("runs1_", mod_files)]
mod_files <- mod_files[grep("nday30", mod_files)]
mod_files <- mod_files[-grep("speciesCa", mod_files)]

#----------------------------------------------------------------------
#Extract standard error estimates
# ses <- sqrt(diag(vcov(mod)))
# hess <- mod$hessian
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

# mod_res <- foreach::foreach(ii = 1:length(mod_files)) %dopar% {
# mod_res <- foreach::foreach(ii = 1:10) %dopar% {
mod_res <- lapply(1:length(mod_files), FUN = function(ii){
  load(file = paste0("model_runs//", mod_files[ii]))
  print(ii)
  hess <- mod$hessian
  coefs <- coef(mod)
  ses <- sqrt(diag(vcov(mod)))
  list(ses = ses, coefs = coefs, hess = hess, filename = mod_files[ii])
})
  

# str(summary(mod))
summary(mod)
summary(mod)$CoefTable
e1 <- summary(mod)$CoefTable[, 1]
se1 <- summary(mod)$CoefTable[, 2]
e1 + 1.96 * se1
e1 - 1.96 * se1
summary(mod)$model


#----------------------------------------------------------------------
#Format the coefficient output with the ses

mod_coefs <- lapply(1:length(mod_res), FUN = function(xx){
  temp <- mod_res[[xx]]
  outs <- data.frame(coef_name = names(temp$coefs), coefs = temp$coefs, ses = temp$ses, filename = temp$filename)
  return(outs)
})

mod_coefs <- ldply(mod_coefs)
mod_coefs$ses196 <- 1.96 * mod_coefs$ses
mod_coefs$filename <- as.character(mod_coefs$filename)

#Parse the filenames
ports <- sapply(mod_coefs$filename, FUN = function(yy){
  out <-  strsplit(yy, split = "_")[[1]][1]
  out <- unlist(out)
  return(out)
})
ports <- as.vector(ports)

years <- sapply(mod_coefs$filename, FUN = function(yy){
  out <-  strsplit(yy, split = "_")[[1]][5]
  out <- unlist(out)
  out <- gsub("focyr", "", out)
  return(out)
})
years <- as.vector(years)
years <- as.integer(years)

mod_coefs$port <- ports
mod_coefs$year <- years
save(mod_coefs, file = 'output/mod_coefs_wstderror.Rdata')
save(mod_res, file = 'output/mod_res_whess_pulled.Rdata')

#----------------------------------------------------------------------


mod

unique(mod_coefs$filename)

mod_coefs %>% select(-filename) %>% filter(port == "AST", year == 2012)





#----------------------------------------------------------------------
#Try refitting the model

fit2 <- mlogit(data = mod$model, formula = mod$formula)
cpr <- chol(-hess)
B <- matrix(coefs)

Nsim <- 10
siglevel <- .05
simCS <- matrix(NA, nrow = Nsim, ncol = 1)
for(i in 1:Nsim){
  x <- matrix(rnorm(length(coefs)))
  Z <- B + cpr %*% x
}

summa
k <- length(coefs)
1:6
Cpr <- chol(hess[1:(k - 1), 1:(k - 1)])
matrix()
#----------------------------------------------------------------------

fitted(mod)

#Compare the fitted values to the calculated
fits <- fitted(mod)

dat <- mod$model %>% filter(fished == TRUE)
dat1 <- dat %>% filter(dummy_first == 1)
pred1 <- dat1$miss_rev_adj * coefs["miss_rev_adj:dummy_first"] +
  dat1$distance * coefs["dummy_first:distance"] +
  dat1$dummy_prev_days * coefs["dummy_prev_days"] +
  dat1$dummy_prev_year_days * coefs["dummy_prev_year_days"] +
  dat1$dummy_miss * coefs["dummy_miss"]

dat2 <- dat %>% filter(dummy_first == 0)
pred2 <- dat2$miss_rev_adj * coefs["miss_rev_adj:dummy_not_first"] +
  dat2$distance * coefs["distance:dummy_not_first"] +
  dat2$dummy_prev_days * coefs["dummy_prev_days"] +
  dat2$dummy_prev_year_days * coefs["dummy_prev_year_days"] +
  dat2$dummy_miss * coefs["dummy_miss"]

wtpa <- c(exp(pred1) / -coefs['dummy_miss'],
          exp(pred2) / -coefs['dummy_miss'])
mean(wtpa)
wtpa.visit <- -1 / coefs['dummy_miss']




preds <- c(pred1, pred2)
exp(sum(preds))





fitted(mod)
str(mod)

length

coefs <- coef(mod)
vcm <- mod$hessian
k <- length(coef)

wtpa <- exp()
krCI
