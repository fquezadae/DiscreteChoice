#'Load Tow Data

#'Load WC Logbook Data

#'@export
#'@examples
#'load_tow_data()

load_tow_data <- function(){
  load('/Users/peterkuriyama/School/Research/ch2_vms/output/wc_data_expanded_tows.Rdata')

  ####################################################
  # #Load Port data and rename
  port_codes <- read.csv("/Users/peterkuriyama/School/Research/ch2_vms/data/port_codes.csv", stringsAsFactors = FALSE)
  port_codes <- plyr::rename(port_codes, c('Pcid' = 'text_id', 'Agid' = 'state',
    'Agency' = 'number_id', 'Port.Agency.Description' = 'description'))

  # #identify missing ports
  # #Add in ports that I know are missing
  added_ports <- data.frame(text_id = c("", "", "", "", "", "", "", "", "", "", "", ""),
                     state = c("O", "O", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"),
                     number_id = c("02", "46", "WES", "ORE", "BEL", "N.B", "SEA", "BLA",
                                   "P.A", "ILW", "ANA", "CAL"),
                     description = c("ASTORIA", "dont know", "WESTPORT", "dont know ORE", 
                                     "BELLINGHAM", "NORTH BEND", "SEATTLE", "BLAINE", 
                                     "PORT ANGELES", "ILWACO", "ANACORTES", "dont know CAL"))
  port_codes <- rbind(port_codes, added_ports)
  port_codes$state_port <- paste(port_codes$state, port_codes$number_id)

  #Add dport and rport codes
  wc_data$state_dport <- paste(wc_data$agid, wc_data$dport)
  wc_data$state_rport <- paste(wc_data$agid, wc_data$rport)

  ##rport 
  test <- data.frame("state_port" = paste(wc_data$agid, wc_data$rport))
  test$state_port <- as.character(test$state_port)
  thing <- dplyr::inner_join(x = test, y = port_codes[, c('state_port', 'description')], by = 'state_port')
  wc_data$rport_desc <- thing$description

  ##dport
  test <- data.frame("state_port" = paste(wc_data$agid, wc_data$dport))
  test$state_port <- as.character(test$state_port)
  thing <- dplyr::inner_join(test, port_codes[, c('state_port', 'description')], by = 'state_port')
  wc_data$dport_desc <- thing$description

  # wc_data <- wc_data[-grep("\\.", row.names(wc_data)), ]

  #Bin the data by effort to assign individual tows to spatial locations
  bh <- ggplot(wc_data, aes(x = -long, y = lat, group = tow_year)) + 
    stat_bin2d(binwidth = c(.0909, .11))

  binned <- ggplot_build(bh)$data[[1]]
  binned$unq <- paste(binned$xbin, binned$ybin)
  binned$id <- 1:nrow(binned)

  #Assign Years to binned
  binned$group <- as.character(binned$group)
  binned$group <- as.factor(binned$group)

  binned %>% group_by(group) %>% summarize(nlocs = length(unique(unq)), 
    tot_locs = length(unique(binned$unq)),
    perc_locs = nlocs / tot_locs) -> yrz
  yrz$year <- 2008:2013
  merge_yrz <- yrz %>% select(group, year) %>% as.data.frame
  binned <- left_join(binned, merge_yrz, by = 'group')

  #----------------------------
  #Don't run this because it takes too long
  #Now assign the tows to individual cells
  # found <- vector('list', length = nrow(binned))

  # for(ii in 1:length(found)){
  #   if(ii %% 100 == 0) print(ii)
  #   temp <- binned[ii, ]

  #   find_these <- which(-wc_data$long <= temp$xmax & -wc_data$long >= temp$xmin &
  #     wc_data$lat <= temp$ymax & wc_data$lat >= temp$ymin & 
  #     wc_data$tow_year == temp$year) 

  #   ifelse(length(find_these) == temp$count, found[[ii]] <- find_these, stop(print(ii)))

  # }

  # save(found, file = 'output/tow_assignments.Rdata')
  #----------------------------
  load('output/tow_assignments.Rdata')

  names(found) <- 1:length(found)
  found1 <- melt(found)
  names(found1) <- c('row', 'id_by_year')

  #Add in IDs for each site by year
  wc_data[found1$row, 'id_by_year'] <- found1$id_by_year

  #Add in IDs for each wite without year
  unq_sites <- data.frame(unq = unique(binned$unq), 
    id_no_year = 1:length(unique(binned$unq)))
  unq_sites$unq <- as.character(unq_sites$unq)

  binned <- inner_join(binned, unq_sites, by = 'unq')

  merge_ids <- binned %>% select(id, id_no_year)
  names(merge_ids)[1] <- 'id_by_year'

  #Maintain row names in case I want to remove duplicated rows at some point
  wc_data$row_name <- row.names(wc_data)

  wc_data$id_by_year <- as.integer(wc_data$id_by_year)
  wc_data <- inner_join(wc_data, merge_ids, by = "id_by_year")

  return(wc_data)  
}


