#' Function to compare AIC and LL values
#' 
#' Function to compare AIC and LL values
#' 
#' @param risk_coefficient Risk coefficient
#' @param net_cost Net cost adjustment
#' @param seed Seed
#' @param nhauls Number of hauls
#' @param years Years to evaluate
#' @param nports Number of ports
#' @param dir Path to directory
#' @param ncores Number of cores to use
#' @export


compare_aic <- function(risk_coefficient, net_cost,
  seed, nhauls, years = 2009:2014, nports = 4,
  ncores = 6){

  #----------------------------------------------------------
  #Determine directory based on system type
  if(Sys.info()['sysname'] == 'Darwin'){
    the_directory <- "/Volumes/udrive/"
    udrive_files <- list.files(the_directory)  
  }
  
  if(Sys.info()['sysname'] != 'Darwin'){  
    the_directory <- "//udrive.uw.edu//udrive//"
    udrive_files <- list.files(the_directory)    
  }
  if(length(udrive_files) == 0) stop('check udrive connection')

  dir <- the_directory
  #----------------------------------------------------------
  #Now read in the coefficients
  #By risk coefficient
  coefs_names <- udrive_files[grep(paste0('coefs', risk_coefficient, "_"),
    udrive_files)]
  
  #Then filter by net_cost
  coefs_names <- coefs_names[grep(net_cost, coefs_names)]

  #Filter by seed 
  coefs_names <- coefs_names[grep(as.character(seed), coefs_names)]  

  #Add in number of hauls
  coefs_names <- coefs_names[grep(as.character(nhauls), coefs_names)]  

  #Filter by years
  yrz <- paste(years, collapse = "|")
  coefs_names <- coefs_names[which(grepl(yrz, coefs_names))  ]

  #number of ports, defaults to 4
  coefs_names <- coefs_names[grep(as.character(nports), coefs_names)]  
  mod_names1 <- gsub("coefs", "EUR_runs", coefs_names)
  mod_names2 <- gsub("coefs", "AST_runs", coefs_names)
  mod_names3 <- gsub("coefs", "CHA_runs", coefs_names)
  mod_names4 <- gsub("coefs", "NEW_runs", coefs_names)
  mod_names <- c(mod_names1, mod_names2, mod_names3,
    mod_names4)

  #Remove Ports
  mod_names <- gsub("nports4_", "", mod_names)

  if(sum(mod_names %in% udrive_files) != length(mod_names)) {
    print('missing files')
    browser()
  }
  
  #----------------------------------------------------------
  #Load the model files and extract LLs and AIC  
  library(doParallel)
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  the_function <- function(input_file, dir){
    load(paste0(dir, input_file))
    outs <- c(mod$logLik, AIC(mod$logLik))
    return(outs)
  }

  start_time <- Sys.time()
  lls <- foreach::foreach(ii = 1:length(mod_names),
    .packages = c('ch4')) %dopar%
    the_function(input_file = mod_names[ii], dir = dir)
  
  # start_time <- Sys.time()
  # lls <- mclapply(mod_names, FUN = function(xx){
  #   load(paste0(dir, xx))
  #   outs <- c(mod$logLik, AIC(mod$logLik))
  #   return(outs)
  # }, mc.cores = ncores)
  run_time <- Sys.time() - start_time
  print(run_time)  

  lls1 <- ldply(lls)
  names(lls1) <- c('logLik', "AIC")
  lls1$models <- mod_names
  lls1$risk_coefficient <- risk_coefficient
  lls1$nhauls <- nhauls
  lls1$net_cost <- n  et_cost
  lls1$seed <- seed
  file_name <- paste0("loglikes_riskc_", risk_coefficient, 
    "_seed_", seed, "_netcost_", net_cost, "_nhauls_", nhauls, ".Rdata")
  stopCluster(cl)
  save(lls1, file = paste0(the_directory, file_name))
  return(lls1)
}