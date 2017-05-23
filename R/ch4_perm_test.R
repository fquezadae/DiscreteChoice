#' Permutation Test

#' Function to conduct permutation test on different columns

#' @param input Input data frame
#' @param column Column to permute
#' @param ndraws Number of draws; defaults to 1000
#' @param gb Character string of things to group by for the parse statements
#' @param summ Character string of things to group by for the parse statements
#' @param seed Random sampling seed

#' @export
#' @examples
#' dd <- ch4_perm_test(input = top100_clusts, column = 'ntows', ndraws = 50)

ch4_perm_test <- function(input, column, ndraws = 1000, gb = "dyear, unq_clust",
  summ = 'length(species)', clust_cat = "unq_clust", seed = 12345){  

  #Check that column is actually a column
  if(column %in% names(input) == FALSE) stop("column has to be a column in input")

  # eval((parse(text = paste0("temp <- input %>% arrange(desc(", column, "))"))))
  
  #Perm data frame, used to permute column of interest
  #Build up function evaluation 
  perm_call <- paste0("perm <- input %>% group_by(", gb, ") %>% summarize(",
        column, " = ", summ, ")")
  eval(parse(text = perm_call))
  
  if(column %in% names(input) == FALSE) stop("column has to be a column in input")
  
  #Define number of unique clusters, using clust_cat
  unq_call <- paste0("nclusts <- input %>% group_by(", clust_cat, ") %>% distinct()")
  eval(parse(text = unq_call))
  nclusts <- nclusts %>% as.data.frame

  # nclusts <- length(unique(input$unq_clust))
  # the_clusts <- unique(perm$unq_clust)
  ncols <- ncol(nclusts)
  nclusts[ii, ncols[1]] 
  nclusts[ii, ncols[2]]

  #Construct the filtering statement
  filt_statement <- paste(paste0(names(nclusts), " == nclusts[ii, ", 1:ncols, "]"), collapse = ", ")
  p_vals <- rep(999, nrow(nclusts))

  #Set the seed
  set.seed(seed)

  for(ii in 1:nrow(nclusts)){
    #Filter the data to permutate
    eval(parse(text = paste0("temp <- perm %>% filter(", filt_statement, ")"  )))
    
    #Move to next value if number of years isn't 6
    if(length(temp$dyear) != 6) next
    
    #Calculate empirical before/after difference
    eval(parse(text = paste0("bef <- mean(temp$",
                  column,
                  "[1:3])")))
    eval(parse(text = paste0("aft <- mean(temp$",
                 column,
                 "[4:6])")))
    emp_out <- aft - bef
    
    #Run the resampling function
    resamp <- sapply(1:ndraws, FUN = function(x){
       eval(parse(text = paste0("draw <- sample(temp$",
                            column,
                            ", replace = F)")))
       bef <- mean(draw[1:3])
       aft <- mean(draw[4:6])
       out <- aft - bef
       return(out)
    })

    #Calculate p-value
    p_vals[ii] <- length(which(resamp <= emp_out)) / length(resamp)
  }

  sigs <- data.frame(p_vals = p_vals, sig = "999")
  sigs$sig <- as.numeric(sigs$sig)
  sigs$sig <- 'no change'
  sigs[(sigs$p_vals <= .05), "sig"] <- 'sig decrease'
  sigs[(sigs$p_vals >= .95), "sig"] <- 'sig increase'
  sigs[(sigs$p_vals == 999), "sig"] <- 'not enough years'
  
  sigs <- cbind(sigs, nclusts)
  
  #Combine with clust_tows
  output <- inner_join(input, sigs, by = names(input)[which(names(input) %in% names(sigs))])
  
  #Modify column names
  names(output)[which(names(output) %in% "p_vals")] <- paste0("p_vals_", column)
  names(output)[which(names(output) %in% "sig")] <- paste0("sig_", column)
  
  return(output)
}





# ch4_perm_test("ntows")
# #--------------------------------------------------------------------------------
#   #CHANGES IN TOWS IN CLUSTERS
#   #before after changes with permutation test
  
#   #look at top 100 clusters; ntows column is number of unique hauls
#   clust_tows <- clust_tows %>% arrange(desc(ntows))
#   top100 <- unique(clust_tows$unq_clust)[1:100]
#   ntow_perm <- clust_tows %>% group_by(dyear, unq_clust) %>% summarize(ntows = length(species)) 
  
#   p_vals <- rep(999, 100)
  
#   for(ii in 1:100){
#     temp <- ntow_perm %>% filter(unq_clust == top100[ii], dyear > 2007)
#     if(length(temp$dyear) != 6) next
#     bef <- mean(temp$ntows[1:3])
#     aft <- mean(temp$ntows[4:6])
#     emp_out <- aft - bef
    
#     resamp <- sapply(1:1000, FUN = function(x){
#       draw <- sample(temp$ntows, replace = F)
#       bef <- mean(draw[1:3])
#       aft <- mean(draw[4:6])
#       out <- aft - bef
#       return(out)
#     })
  
#     #To look at specific distributions
#     # hist(resamp)  
#     # abline(v = emp_out, lwd = 2, lty = 2, col = 'red')  
  
#     #p_value of emp_out, which things had decreases?
#     p_vals[ii] <- length(which(resamp <= emp_out)) / length(resamp)
#   }
  
#   sigs <- data.frame(p_vals = p_vals, sig = "999")
#   sigs$sig <- as.numeric(sigs$sig)
#   sigs$sig <- 'no change'
#   sigs[(sigs$p_vals <= .05), "sig"] <- 'sig decrease'
#   sigs[(sigs$p_vals >= .95), "sig"] <- 'sig increase'
#   sigs[(sigs$p_vals == 999), "sig"] <- 'not enough years'
#   sigs$unq_clust <- top100
  
#   #Combine with clust_tows
#   top100_clusts <- inner_join(clust_tows, sigs, by = c('unq_clust'))
#   top100_clusts <- plyr::rename(top100_clusts, c("p_vals" = 'p_vals_tows', 'sig' = 'sig_tows'))
#   