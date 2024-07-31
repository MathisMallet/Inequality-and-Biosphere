
library(ineq)

subtype_save <- list(X.1..Energía. = "Energy", X.2..IPPU. = "IP&PU", X.3..Agricultura. = "Agriculture", X.4..UTCUTS. = "Land_Use", X.5..Residuos. = "Waste", X.Balance. = "Total")

region_GHG_line <- function(name, region_index, ldf ) {
  corresponding_index <- region_index[[name]]
  subdata <- ldf[[name]]
  GHG_subdata <- data.frame(
    Region = substr(name, start = 1, stop = nchar(name)-4),
    Region_number = corresponding_index
  )
  subdata_years <- subdata[["X.Sector."]]
  indexes <- rownames(subdata)
  for (type_index in 2:7) {
    subdata_colname <- colnames(subdata)[[type_index]]
    for (index in indexes) {
      new_col_name <- paste0("GHG","-",subtype_save[[subdata_colname]],"-",subdata_years[[as.integer(index)]])
      new_col_value <- subdata[index,subdata_colname]
      new_col_value <- gsub(",", ".", new_col_value)
      new_col_value <- as.numeric(new_col_value)
      GHG_subdata[new_col_name] <- new_col_value
    }
  }
  return(GHG_subdata)
}

region_IN_line <- function(region_index, data, year) {
  if (region_index == 8){
    regional_data <- data[data$r %in% c(8,16),]
  } else if (region_index == 1){
    regional_data <- data[data$r %in% c(1,15),]
  } else if (region_index == 10){
    regional_data <- data[data$r %in% c(10,14),]
  } else {
    regional_data <- data[data$r == region_index,]
  }
  
  regional_mean <- mean(regional_data$ypchaj, na.rm = TRUE)
  na_count <- sum(is.na(regional_data$ypchaj))
  total_count <- length(regional_data$ypchaj)
  print(paste("Number of missing values for region n",region_index,":", na_count, "out of", total_count, na_count/total_count*100, "%"))
  regional_deciles <- quantile(na.omit(regional_data$ypchaj), probs = seq(0,1, by = 0.1))
  S8020 <- regional_deciles["80%"] / regional_deciles["20%"]
  P9010 <- regional_deciles["90%"] / regional_deciles["10%"]
  P9050 <- regional_deciles["90%"] / regional_deciles["50%"]
  P5010 <- regional_deciles["50%"] / regional_deciles["10%"]
  Palma <- regional_deciles["90%"] / regional_deciles["40%"]
  Gini <- Gini(regional_data$ypchaj) 
  #Add P9950 and P50001
  regional_subdata <- data.frame(
  Region_number = region_index
  )
  regional_subdata[paste0("I-S8020-",year)] <- S8020
  regional_subdata[paste0("I-P9010-",year)] <- P9010
  regional_subdata[paste0("I-P9050-",year)] <- P9050
  regional_subdata[paste0("I-P5010-",year)] <- P5010
  regional_subdata[paste0("I-Palma-",year)] <- Palma
  regional_subdata[paste0("I-Gini-",year)] <- Gini
  regional_subdata[paste0("W-Mean-",year)] <- regional_mean
  return(regional_subdata)
}

region_IN_data <- function(data, year) {
  region_indexes = unique(data$r)
  cpt <- 0
  for (region_index in region_indexes) {
    #print(region_index)
    if (region_index <= 13) {
      cpt <- cpt + 1
      new_line <- region_IN_line(region_index, data, year)
      if (cpt > 1) {
        IN_data <- merge(IN_data, new_line, all = TRUE)
      }else {
        IN_data <- new_line
      }
    }
  }
  return(IN_data)
}

pop_to_odata[,paste0("D8",substr(year,3,4))]ld_regions <- function(data){
  sum_of_8and16 <- data[data$r == 8, 2] + data[data$r == 16,2]
  sum_of_1and15 <- data[data$r == 1, 2] + data[data$r == 15, 2]
  sum_of_10and14 <- data[data$r == 10, 2] + data[data$r == 14, 2]
  
  data[data$r == 8, -1] <- sum_of_8and16
  data[data$r == 1, -1] <- sum_of_1and15
  data[data$r == 10, -1] <- sum_of_10and14
  data <- data[!(data$r %in% c(16,15,14)) ,]
  
  return(data)
}

get_year_columns <- function(data, year){
  names  = colnames(data)
  year_columns_name = c()
  for (name in names){
    split_name <- strsplit(name, "-")[[1]]
    name_year <- split_name[3]
    if (name_year == as.character(year)){
      year_columns_name = c(year_columns_name, name)
    }
  }
  return(year_columns_name)
}

multiple_plot <- function(data, unit = "kT CO2 eq"){
  region_index = data$Region_number
  Years = c(1990:2020)
  cpt = 0
  print(region_index)
  for (i in region_index){
    if (cpt == 0) {
      cpt = cpt + 1
      plot(Years, data[data$Region_number == i,(ncol(data) - length(Years) + 1):ncol(data)], type = "o", main = "Regional GHG emissions", xlab = "years", ylab = unit, col = rainbow(length(region_index))[cpt], xlim = c(1990, 2023), ylim = c(min(data[,(ncol(data) - length(Years) + 1):ncol(data)]), max(data[,(ncol(data) - length(Years) + 1):ncol(data)])))
      print(rainbow(length(region_index))[cpt])
    } else {
      cpt = cpt + 1
      points(Years, data[data$Region_number == i,(ncol(data) - length(Years) + 1):ncol(data)], type = "o", col = rainbow(length(region_index))[cpt])
    }
  }
  legend("topright", legend = region_index, col = rainbow(length(region_index)), pch = 1)
}
