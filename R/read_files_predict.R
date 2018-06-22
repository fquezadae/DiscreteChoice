#' Read files and calculate model predictions
#' Function to read in model results and generate figures
#' 
#' @param the_args Argument List
#' @param data_in Data in for sampling
#' @param return_filenames Option to only return the filenames and not sample
#' @export

read_files_predict <- function(the_args, data_in = tows_clust,
    return_filenames = FALSE){

    #First generate choices based on ports and focus_year    
    combs <- expand.grid(unlist(the_args$port), 2011:2014, stringsAsFactors = F,
        KEEP.OUT.ATTRS = F)
    names(combs) <- c('port', "year")

    pps <- lapply(1:nrow(combs), FUN = function(xx){
      tp <- combs[xx, "port"]
      ty <- combs[xx, "year"]
      
      ff <- get_filenames(the_args = a_l, years = ty, ports = tp)
      ff1 <- paste0("model_runs//", ff, ".Rdata") 

      if(return_filenames == TRUE) return(ff)

      choices1 <- sampled_rums(data_in = data_in, the_port = tp,
        min_year = the_args$m_y, max_year = ty, risk_coefficient = the_args$r_c,
        ndays = the_args$dyz, focus_year = ty, nhauls_sampled = the_args$nhauls_sampled,
        seed = the_args$seed, ncores = 6, rev_scale = the_args$r_s,
        model_type = "no_bycatch", net_cost = the_args$n_c,
        habit_distance = the_args$h_d, return_hauls = T)

      #Read in model
      load(ff1)
      mod1 <- mod
      rm(mod)
      preds <- pred_metrics(choices = choices1, mod = mod1)
      args_out <- the_args
      args_out$preds <- preds
      args_out$filename <- ff
    
      #Format the output
      rm_inds <- which(names(args_out) %in% c("ports", "quota_species", "preds"))
      args_out <- args_out[-rm_inds]
      names(args_out)
      aodf <- data.frame(matrix(unlist(args_out), nrow = 1), stringsAsFactors = F)
      names(aodf) <- names(args_out)
      aodf$score1 <- preds[1]
      aodf$score2 <- preds[2]
      aodf$score3 <- preds[3]
      aodf$score4 <- preds[4]
      aodf$f_y <- ty
      return(aodf)
    })

    pps <- ldply(pps)
    return(pps)
}