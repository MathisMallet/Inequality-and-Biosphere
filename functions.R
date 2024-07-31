

condense_values <- function(data) {
  value_names <- colnames(data)[11:ncol(data)] # Select tvalues <- list() # Initialize the value list
  label <- character() # Initialize the label list
  
  for (i in 1:nrow(data)) { # Iterate over all the rows of the dataframe
    if (!is.na(data$sous_champ[i])) { # Check if a sub_label exists
      label[i] <- paste(data$no_indic[i], data$variable[i], data$sous_champ[i], sep = ".") # Construct the full label
    } else {
      label[i] <- paste(data$no_indic[i], data$variable[i], sep = ".") # Construct the full label without the sub_label
    }
  }
  
  return(label = label)
}

list_years <- function(data) {
  name_years <- colnames(data)[11:ncol(data)] # Get the name of the years columns
  years <- as.integer(substring(name_years, 2)) # Extract the integer values
  
  return(list(name_years = name_years, years = years))
}

data_labeling <- function(data) {
  # get the list of the values of each line over the years as well as a list of all the labels condensed
  labels <- condense_values(data)
  
  # get a list of all the available years as well as the name of the corresponding columns
  years <- list_years(data)$years
  name_years <- list_years(data)$name_years
  
  data$label <- labels # add the column of the full labels
  
  return(list(
    data = data,
    label_unique = unique(data$label),
    codgeo_unique = unique(data$codgeo),
    years = years,
    name_years = name_years
  ))
}


existence_grid <- function(data, label, codgeos, name_years, years) {
  mask <- data$label %in% label
  data <- data[mask, ]
  t_len <- length(years)
  geo_len <- length(codgeos)
  presence <- matrix(0, nrow = geo_len, ncol = t_len)
  
  for (i in 1:length(name_years)) {
    for (j in 1:length(codgeos)) {
      mask <- data$codgeo %in% codgeos[j]
      temp_data <- data[mask, ]
      value <- temp_data[[name_years[i]]][1]
      if (!is.na(value)) {
        presence[j, i] <- 1
      }
    }
  }
  
  return(presence)
}

plot_all_existences <- function(data, labels, codgeos, name_years, years) {
  par(mfrow=c(length(labels), 1), mar=c(5, 4, 4, 2)) # Set multiple plots layout
  #dev.new(width = 750, height = 3000, unit = "px")
  
  for (label in labels) {
    presence <- existence_grid(data, label, codgeos, name_years, years) # Get the presence grid
    
    # Plotting
    image(1:length(name_years), 1:length(codgeos), t(presence), xlab = "Years", ylab = "Regions codgeo", main = label)
    
    # Adding labels to axes
    axis(1, at = 1:length(name_years), labels = name_years, las = 2)
    axis(2, at = 1:length(codgeos), labels = codgeos, las = 2)
  }
}

save_all_existences <- function(data, labels, codgeos, name_years, years) {
  
  pdf("existence.pdf",         # File name
      width = 8, height = 30, # Width and height in inches
      bg = "white",          # Background color
      colormodel = "cmyk")    # Color model (cmyk is required for most publications)
      #paper = "A4")          # Paper size
  
  par(mfrow=c(length(labels), 1), mar=c(5, 4, 4, 2)) # Set multiple plots layout
  #dev.new(width = 750, height = 3000, unit = "px")
  
  for (label in labels) {
    presence <- existence_grid(data, label, codgeos, name_years, years) # Get the presence grid
    
    # Plotting
    image(1:length(name_years), 1:length(codgeos), t(presence), xlab = "Years", ylab = "Regions codgeo", main = label, xaxt= "n", yaxt = "n")
    # # Vertical grid  
    # axis(1,
    #      at = 1:length(name_years),
    #      tck = 1, lty = 1, col = "gray")
    # 
    # # Horizontal grid
    # axis(2,
    #      at = 1:length(codgeos),
    #      tck = 1, lty = 1, col = "black")
    # Adding labels to axes
    axis(1, at = 1:length(name_years), labels = name_years, las = 2)
    axis(2, at = 1:length(codgeos), labels = codgeos, las = 2, lty = 1, col = "black", tck = 1, cex.axis = 0.7)
  }
  dev.off() 
}

label_year_generator <- function(year) {
  year_label <- paste0("A", year)  # Create the year label
  return(year_label)
}

closest_value <- function(data, year, label, codgeo) {
  mask <- data$codgeo %in% codgeo & data$label %in% label  # Keep only the corresponding values
  temp_data <- subset(data, mask) # Update the dataframe
  
  test_value <- TRUE  # TRUE if no value has been found yet
  cpt <- 0  # Algebraic difference from the targeted year
  
  while (test_value) {  # Test if a value has been found
    year_label <- label_year_generator(year - cpt)  # Get the anterior year label
    value <- temp_data[[year_label]][1]  # Extract the associated value
    if (!is.na(value)) {  # Test if it is not missing
      test_value <- FALSE  # Save that we found a value
    } else if (year + cpt <= 2023) {  # Test if we're not too far in the future
      year_label <- label_year_generator(year + cpt)  # Get the posterior year label
      value <- temp_data[[year_label]][1]  # Extract the associated value
      if (!is.na(value)) {  # Test if it is not missing
        test_value <- FALSE  # Save that we found a value
      }
    }
    cpt <- cpt + 1  # If no value is found, increase the distance to the targeted year
  }
  
  return(value)
}

format_static <- function(data, target_year, labels, codgeos) {
  matrix <- matrix(nrow = length(codgeos), ncol = length(labels))  # Initialize the PCA matrix
  
  for (i in 1:length(codgeos)) {  # Iterate over the locations
    matrix_line <- vector()  # Create an empty location line
    for (label in labels) {  # Iterate over the parameters
      value <- closest_value(data, target_year, label, codgeos[i])  # Get the closest available value to the targeted year
      matrix_line <- c(matrix_line, value)  # Add this value to the matrix line
    }
    matrix[i, ] <- matrix_line  # Add the location line to the full matrix
  }
  
  return(matrix)
}

factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

binomial_coefficient <- function(n, k) {
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

scatter_from_data <- function(dataset,i,j){
  x_name <- colnames(dataset)[i]
  y_name <- colnames(dataset)[j]
  x <- dataset[,x_name]
  y <- dataset[,y_name]
  plot(x,y, xlab = x_name, ylab = y_name, cex.lab = 1.2)
}


trilemma_scatter <- function(dataset){
  no_indic <- unique(colnames(dataset))  # Get the indicators labels
  nb = binomial_coefficient(length(no_indic), 2) # == 3 it's a bit overkill ^^
  par(mfrow = c(1, nb))
  scatter_from_data(dataset,1,2)
  scatter_from_data(dataset,2,3)
  scatter_from_data(dataset,3,1)
}

density_from_data <- function(dataset, i, j){
  x_name <- colnames(dataset)[i]
  y_name <- colnames(dataset)[j]
  return (ggplot(dataset, aes(x = .data[[x_name]], y = .data[[y_name]] )) +
            stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
            geom_density2d() +
            labs(
              x = x_name,
              y = y_name) +
            theme_minimal() +
            theme(legend.title = element_text(size = 20),  # Adjust legend title size
                  legend.text = element_text(size = 15),   # Adjust legend label size
                  axis.title.x = element_text(size = 14),# Set size of x-axis labels
                  axis.title.y = element_text(size = 14), # Set size of y-axis labels
                  axis.text.x = element_text(size = 15),# Set size of x-axis labels
                  axis.text.y = element_text(size = 15)) +# Set size of y-axis labels
            scale_fill_distiller(palette= "Spectral", direction=-1)+
            scale_color_viridis_c()
    )
}

trilemma_density <- function(dataset){
  par(mfrow = c(1,3))
  p1 <- density_from_data(dataset,1,2)
  p2 <-density_from_data(dataset,2,3)
  p3 <-density_from_data(dataset,3,1)
  plot <- ggarrange(p1, p2, p3, ncols =3, nrow = 1) 
  ggsave("test.png", plot = plot, width = 25, height = 5)
}



