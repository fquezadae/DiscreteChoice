#' Function for simple permutations
#' 
#' Simple being that the data have to be in a specific format 
#' 
#' @param input The data to permute
#' @param perm_column Column for permutation
#' @param nperms Number of permutations/resamples
#' @param seed Seed for resampling
#' @param crit Criterion for evaluating significance
#' @param column Column of years by default, whatever index to permute
#' @param index Values in column that are indices of permutation

#' @export
#Specify index for aggregation
simple_permute <- function(input, perm_column, nperms = 1000, seed = 12345,
                           crit = "<=", column = 'set_year', index = c(2007,2010)){

    
  #Define "before" and "after" indices
  # inds <- which(input[, column] %in% index)
  
  # befs <- input[inds[1]:inds[2], perm_column]
  # afts <- input[(inds[2] + 1):nrow(input), perm_column]
  befs <- input %>% filter(when == 'before')
  afts <- input %>% filter(when == 'after')
  emp_diff <- mean(afts[, perm_column]) - mean(befs[, perm_column])
  
  #--------
  #start resampling
  set.seed(seed)
  temp1 <- input
  
  samps <- sapply(1:nperms, FUN = function(x){
    sample(1:nrow(temp1))
    
    samp_inds <- sample(1:nrow(temp1))
    temp1[, perm_column] <- input[samp_inds, perm_column]
    
    befs <- temp1[, ] %>% filter(when == 'before')
    afts <- temp1[, ] %>% filter(when == 'after')

    emp_diff <- mean(afts[, perm_column]) - mean(befs[, perm_column])
    return(emp_diff)
  })
  
  #--------
  #Evaluate Significance
  p_val <- eval(parse(text = paste("sum(emp_diff", crit, "samps) / length(samps)")))
  
  #Format the output to add statement of p-value and statement of signficance
  sig_statement <- "not significant"

  if(p_val >= 0.95 | p_val <= 0.05) sig_statement <- 'significant' 
  # if(p_val >= 0.95 | p_val <= 0.05) sig_statement <- 'significant' 
  if(p_val < 0.95 & p_val > 0.05 & emp_diff < 0) sig_statement <- 'not significant' 
  if(emp_diff > 0) sig_statement <- paste0(sig_statement, " increase")
  if(emp_diff < 0) sig_statement <- paste0(sig_statement, " decrease")

  # if(nrow(input) != inds[2] * 2) sig_statement <- paste0(sig_statement, "; not enough years")
  
  input$p_val <- p_val
  input$sig <- sig_statement
  names(input)[which(names(input) == 'p_val')] <- paste0(names(input)[which(names(input) == 'p_val')], "_", perm_column)
  names(input)[which(names(input) == 'sig')] <- paste0(names(input)[which(names(input) == 'sig')], "_", perm_column)
  
  return(list(output = input, samps = samps))
  
}

  





