


#' Return all columns' names that contain type, subtype and year if specified (with names under the format "type-subtype-year")
#'
#' @param data the data.frame from which we select the columns
#' @param type the firt element in the name of the columns
#' @param subtype the second element in the name of the columns
#' @param year the third element in the name of the columns
#'
#' @return a list of the selected names of columns
#' @export
#'
#' @examples
select_columns <- function(data, type = "empty", subtype = "empty", year = "empty") {
  full_names = colnames(data[,3:ncol(data)]) #full_names contains all names of columns except for the first 2
  subset = c() 
  for (name in full_names){
    split_name <- strsplit(name, "-")[[1]]
    name_type <- split_name[1]
    name_subtype <- split_name[2]
    name_year <- as.character(split_name[3])
    if (type %in% c("empty",name_type)){
      if (subtype %in% c("empty", name_subtype)){
        if (year %in% c("empty", name_year)){
          subset <- c(subset, name) #add the colname only if it passes all 3 tests
        }
      }
    }
  }
  return(subset)
}

filter_strings_with_substrings <- function(strings, substrings) {
  filtered_strings <- strings[sapply(strings, function(s) any(sapply(substrings, grepl, x = s)))]
  return(filtered_strings)
}

data_to_plot_one_region <- function(dataset, region_number, type1, subtype1, type2, subtype2, years){
  dataset <- dataset[dataset$Region_number == region_number,]
  columns1 <- select_columns(dataset, type = type1, subtype = subtype1)
  columns2 <- select_columns(dataset, type = type2, subtype = subtype2)
  x <- c()
  y <- c()
  for (year in years) {
    x_column <- filter_strings_with_substrings(strings = columns1, substrings = c(year))
    value <- dataset[,x_column]
    x <- c(x, value)
    y_column <- filter_strings_with_substrings(strings = columns2, substrings = c(year))
    value <- dataset[,y_column]
    y <- c(y, value)
  }
  subdata <- data.frame(
    x = x,
    y = y,
    z = as.integer(years)
  )
  subdata <- subdata[order(subdata$z),]
  colnames(subdata) <- c(substring(x_column, 1, nchar(x_column) - 5), substring(y_column, 1, nchar(y_column) - 5), "year")
  return(subdata)
}

data_to_plot <- function(dataset, type1, subtype1, type2, subtype2, years){
  list_of_datasets <- list()
  for (region_number in dataset$Region_number){
    new_dataset <- data_to_plot_one_region(dataset = dataset, region_number = region_number, type1 = type1, subtype1 = subtype1, type2 = type2, subtype2 = subtype2, years = years)
    list_of_datasets[[as.character(region_number)]] <- new_dataset
  }
  return(list_of_datasets)
}

#test <- data_to_plot(dataset = dataset, type1 = "GHG", subtype1 = "X.Balance.", type2 = "I", subtype2 = "Gini", years = common_years)

graph_from_datasets <- function(datasets){
  nb <- length(datasets)
  names <- colnames(datasets[[1]])
  graph <- ggplot() +
    labs(
      x = names[1],
      y = names[2],
      color = names[3]) +
    theme_minimal() +
    theme(legend.title = element_text(size = 20),  # Adjust legend title size
          legend.text = element_text(size = 15),   # Adjust legend label size
          axis.title.x = element_text(size = 20),# Set size of x-axis labels
          axis.title.y = element_text(size = 20), # Set size of y-axis labels
          axis.text.x = element_text(size = 15),# Set size of x-axis labels
          axis.text.y = element_text(size = 15)) +# Set size of y-axis labels
    scale_color_viridis_c()
  #scale_color_gradient(low = "blue", high = "red") 
  for (i in 1:nb) {
    graph <- graph + 
      geom_point(data = datasets[[i]], aes(x = .data[[names[1]]], y = .data[[names[2]]], color = .data[[names[3]]])) +
      geom_path(data = datasets[[i]], aes(x = .data[[names[1]]], y = .data[[names[2]]], color = .data[[names[3]]])) 
  }
  return(graph)
}
#graph_from_datasets(test)

get_only_indi <- function(list_of_type1){
  list_of_type1 <- substr(list_of_type1, 1, nchar(list_of_type1) - 5)
  list_of_type1 <- unique(list_of_type1)
  return(list_of_type1)
}

plots_from_data <- function(data, type1, type2){
  list_of_type1 <- select_columns(data = data, type = type1)
  list_of_type1 <- get_only_indi(list_of_type1)
  list_of_type2 <- select_columns(data = data, type = type2)
  list_of_type2 <- get_only_indi(list_of_type2)
  list_of_plots <- list()
  n_x <- length(list_of_type1)
  n_y <- length(list_of_type2)
  pb1 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                        max = n_y, # Maximum value of the progress bar
                        style = 3,    # Progress bar style (also available style = 1 and style = 2)
                        width = 50,   # Progress bar width. Defaults to getOption("width")
                        char = ">")   # Character used to create the bar
  cpt <-1
  
  for (name2 in list_of_type2){
    split_name2 <- strsplit(name2, "-")[[1]]
    for (name1 in list_of_type1) {
      split_name1 <- strsplit(name1, "-")[[1]]
      subdatasets <- data_to_plot(dataset = data, type1 = split_name1[1], subtype1 = split_name1[2], type2 = split_name2[1], subtype2 = split_name2[2], years = common_years)
      new_plot <- graph_from_datasets(subdatasets)
      list_of_plots <- append(list_of_plots, list(new_plot))
    }
    cpt <- cpt + 1
    setTxtProgressBar(pb1, cpt)
  }
  #beep(2)#8
  return(list(list_of_plots, n_x, n_y))
}

#test <- plots_from_data(dataset, "GHG", "I")

scatter_data <- function(data, type1, type2, sup = ""){
  result <- plots_from_data(data, type1, type2)
  list_of_plots <- result[[1]]
  n_x <- result[[2]]
  n_y <- result[[3]]
  combined_plot <- plot_grid(plotlist = list_of_plots, ncol = n_x, align = "hv")
  ggsave(paste0(type1, "-", type2,sup,".png"), plot = combined_plot, width = 7*n_x, height = 5*n_y)
}

density_from_datasets <- function(datasets){
  nb <- length(datasets)
  names <- colnames(datasets[[1]])
  merged_data <- bind_rows(datasets)
  graph <- ggplot(data = merged_data, aes(x = .data[[names[1]]], y = .data[[names[2]]])) +
    stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
    geom_density2d() +
    labs(
      x = names[1],
      y = names[2],
      color = names[3]) +
    theme_minimal() +
    theme(legend.title = element_text(size = 20),  # Adjust legend title size
          legend.text = element_text(size = 15),   # Adjust legend label size
          axis.title.x = element_text(size = 20),# Set size of x-axis labels
          axis.title.y = element_text(size = 20), # Set size of y-axis labels
          axis.text.x = element_text(size = 15),# Set size of x-axis labels
          axis.text.y = element_text(size = 15)) +# Set size of y-axis labels
    scale_fill_distiller(palette= "Spectral", direction=-1)+
    scale_color_viridis_c()
  return(graph)
}

density_plots_from_data <- function(data, type1, type2){
  list_of_type1 <- select_columns(data = data, type = type1)
  list_of_type1 <- get_only_indi(list_of_type1)
  list_of_type2 <- select_columns(data = data, type = type2)
  list_of_type2 <- get_only_indi(list_of_type2)
  list_of_plots <- list()
  n_x <- length(list_of_type1)
  n_y <- length(list_of_type2)
  pb1 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                        max = n_y, # Maximum value of the progress bar
                        style = 3,    # Progress bar style (also available style = 1 and style = 2)
                        width = 50,   # Progress bar width. Defaults to getOption("width")
                        char = ">")   # Character used to create the bar
  cpt <-1
  
  for (name2 in list_of_type2){
    split_name2 <- strsplit(name2, "-")[[1]]
    for (name1 in list_of_type1) {
      split_name1 <- strsplit(name1, "-")[[1]]
      subdatasets <- data_to_plot(dataset = data, type1 = split_name1[1], subtype1 = split_name1[2], type2 = split_name2[1], subtype2 = split_name2[2], years = common_years)
      new_plot <- density_from_datasets(subdatasets)
      list_of_plots <- append(list_of_plots, list(new_plot))
    }
    cpt <- cpt + 1
    setTxtProgressBar(pb1, cpt)
  }
  #beep(2)#8
  return(list(list_of_plots, n_x, n_y))
}

scatter_density_data <- function(data, type1, type2, sup = ""){
  result <- density_plots_from_data(data, type1, type2)
  list_of_plots <- result[[1]]
  n_x <- result[[2]]
  n_y <- result[[3]]
  combined_plot <- plot_grid(plotlist = list_of_plots, ncol = n_x, align = "hv")
  ggsave(paste0("density-",type1, "-", type2,sup,".png"), plot = combined_plot, width = 7*n_x, height = 5*n_y)
}

plot_variance <- function(res.pca){
  eigen <- res.pca$eig
  eigen <- as.data.frame(eigen)
  eigen <- rowid_to_column(eigen, "RowIndex")
  eigen[2:4] <- eigen[2:4]/100
  ggplot(data = eigen , aes(x = .data[["RowIndex"]], y = .data[["percentage of variance"]])) +
    geom_col(color = "grey25") +
    geom_point(data = eigen, aes(.data[["RowIndex"]], .data[["cumulative percentage of variance"]])) +
    geom_line(data = eigen, aes(.data[["RowIndex"]], .data[["cumulative percentage of variance"]])) +
    labs(x = "Principal components", y = "Explained variance") +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_light(base_size = 10)
}

convert_dim_to_PCA <- function(dim){
  split_dim <- strsplit(dim, "\\.")
  PCA_str <- paste0("PC ",split_dim[[1]][2])
  return(PCA_str)
}

plot_PCs_ind <- function(PCA, dim1 = 1, dim2 = 2, color = FALSE, group = c()) {
  coord <- PCA$ind$coord
  coord <- as.data.frame(coord)
  coord <- select(coord, any_of(c(dim1, dim2)))
  names <- colnames(coord)
  colnames(coord)[colnames(coord) == names[1]] <- convert_dim_to_PCA(names[1])
  colnames(coord)[colnames(coord) == names[2]] <- convert_dim_to_PCA(names[2])
  names <- colnames(coord)
  coord <- rowid_to_column(coord, "Region_number")
  if (color) {
    coord["group"] <- as.character(group)
  }
  plot <- ggplot(coord, aes(.data[[names[1]]], .data[[names[2]]])) +
    coord_equal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
    theme_light(base_size = 10)
  if (!(color)){
    plot <- plot + 
      geom_text(aes(label = Region_number), size = 3)
  } else {
    number_groups <- length(unique(group))
    palette <- brewer.pal(number_groups, "Dark2")
    plot <- plot +
      geom_text(aes(label = Region_number, color = group), size = 3) +
      scale_color_manual(values = palette, guide = guide_legend(ncol = 1, title.position = "top"), breaks =  c(1:number_groups)) + 
      theme(legend.position = "none")
  }
  return(plot)
}

modif_PCs_var <- function(PCA, dim1 = 1, dim2 = 2){
  coord <- PCA$var$coord
  coord <- as.data.frame(coord)
  coord <- select(coord, any_of(c(dim1, dim2)))
  names <- colnames(coord)
  row_names <- rownames(coord)
  coord["RowNames"] <- row_names
  split_list <- strsplit(row_names, "-")
  transposed_list <- transpose(split_list)
  coord["type"] <- unlist(transposed_list[[1]])
  coord["subtype"] <- unlist(transposed_list[[2]])
  combined_label <- unlist(Map(function(x, y) paste(x, y, sep = "-"), transposed_list[[1]], transposed_list[[2]]))
  coord["label"] <- combined_label
  coord["year"] <- as.integer(transposed_list[[3]])
  return(coord)
}

plot_PCs_var <- function(PCA, dim1 = 1, dim2 = 2) {
  coord <- modif_PCs_var(PCA = PCA, dim1 = dim1, dim2 = dim2)
  names <- colnames(coord)
  colnames(coord)[colnames(coord) == names[1]] <- convert_dim_to_PCA(names[1])
  colnames(coord)[colnames(coord) == names[2]] <- convert_dim_to_PCA(names[2])
  names <- colnames(coord)
  number <- length(unique(coord$label))
  number_GHG <- length(unique(coord[coord$type == "GHG",]$label))
  number_I <- length(unique(coord[coord$type == "I",]$label))
  number_W <- length(unique(coord[coord$type == "W",]$label))
  number_GI <- length(unique(coord[coord$type == "GI",]$label))
  palette <- c(brewer.pal(number_GHG, "Dark2"), brewer.pal(number_I, "Dark2"), brewer.pal(number_GI, "Dark2"),brewer.pal(number_W, "Dark2"))
  plot_var <- ggplot(coord, size = type) +
    geom_segment(aes(xend = .data[[names[1]]], yend = .data[[names[2]]],
                     x = 0, y = 0,
                     color = label,
                     alpha = as.numeric(year),
                     linetype = type),
                 arrow = arrow(length = unit(0.2,"cm"))) +
    scale_linetype_manual(values = c("solid","dashed","twodash", "longdash"), guide = guide_legend(ncol = 1, title.position = "top")) +
    scale_color_manual(values = palette,guide = guide_legend(ncol = 4, title.position = "top")) +
    scale_alpha("year", guide = guide_legend(ncol = 1, title.position = "top"), range = c(0.2,1)) +
    #scale_size_manual(values = c(0.01,1,5),breaks = unique(coord$type)) +
    coord_equal()+
    theme_light(base_size = 8)+
    labs(x = names[1],
         y = names[2])
  return(plot_var)
}

combined_PCA_plot <- function(PCA, dim1 = 1, dim2 = 2, color = FALSE, group = c()) {
  plot_ind <- plot_PCs_ind(PCA = PCA, dim1 = dim1, dim2 = dim2, color = color, group = group)
  plot_var <- plot_PCs_var(PCA = PCA, dim1 = dim1, dim2 = dim2)
  combined_plot <- (plot_ind|plot_var) +
    plot_layout(guide = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal",
          legend.title = element_text(size = 10),  # Adjust legend title size
          legend.text = element_text(size = 10),   # Adjust legend label size
          legend.key.size = unit(1, "lines"),# Adjust legend key size
          axis.text.x = element_text(size = 10),# Set size of x-axis labels
          axis.text.y = element_text(size = 10), # Set size of y-axis labels
          axis.title.x = element_text(size = 10),# Set size of x-axis labels
          axis.title.y = element_text(size = 10)) # Set size of y-axis labels
  return(combined_plot)
}

sorted_columns <- function(dataset, year = FALSE){
  names <- colnames(dataset)
  split_names <- strsplit(x = names[3:length(names)],split = "-")
  split_names <- do.call(rbind, split_names)
  if (year){
    sorted_indices <- order(split_names[,3],split_names[,1],split_names[,2])
  } else {
    sorted_indices <- order(split_names[,1],split_names[,2],split_names[,3])
  }
  sorted_split_names <- split_names[sorted_indices,]
  if (year){
    group_name <- unique(sorted_split_names[,3])
  } else {
    group_name <- unique(paste0(sorted_split_names[,1],"-",sorted_split_names[,2]))
  }
  sorted_split_names <- paste0(sorted_split_names[,1], "-", sorted_split_names[,2],"-", sorted_split_names[,3])
  return(list(sorted_columns = sorted_split_names, group_name = group_name))
}


#' This function turns the dataframe from an hcpc analysis cluster analysis (FactoMineR library) result into
#' a matrix of the v.test values over time for each significant variable.
#'
#' @param data One cluster analysis result form the hcpc() calculation.
#' @param common_years the common years of the variables
#'
#' @return a dataframe of the v.test values over time for each significant variable.
#' @export
#'
#' @examples
var_to_var_over_time <- function(data, common_years){
  names <- rownames(data)
  split_names <- strsplit(x = names,split = "-")
  split_names <- do.call(rbind, split_names)
  sorted_indices <- order(split_names[,1],split_names[,2],split_names[,3])
  sorted_split_names <- split_names[sorted_indices,]
  var <- paste0(split_names[,1], "-", split_names[,2])
  sorted_var <- paste0(sorted_split_names[,1], "-", sorted_split_names[,2])
  unique_var <- unique(sorted_var)
  zero_matrix <- matrix(NA, nrow = length(unique_var), ncol = length(common_years))
  var_over_time <- as.data.frame(zero_matrix)
  colnames(var_over_time) <- as.character(common_years)
  rownames(var_over_time) <- unique_var
  cpt <- 1
  for (name in names){
    value <- data[name,"v.test"]
    var_over_time[var[cpt],split_names[,3][cpt]] <- value
    cpt <- cpt + 1
  }
  return(var_over_time)
}


#' Plot the matrix for th ith cluster of it's variables of influence
#'
#' @param res.hcpc the result from hcpc() 
#' @param common_years the common years of the study
#' @param i th number of the considered cluster
#'
#' @return the heatmap graph
#' @export
#'
#' @examples
plot_clusters_var <- function(res.hcpc, common_years, i){
  data <- res.hcpc$desc.var$quanti[[i]]
  data <- var_to_var_over_time(data, common_years)
  #print(data)
  data <- rownames_to_column(data, var = "variables")
  melted_data <- melt(data, id.vars = "variables", variable.name = "year", value.name = "Z")
  
  plot <- ggplot(melted_data, aes(x = year, y = variables, fill= Z)) + 
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1)+
    scale_fill_continuous(
      na.value = "seashell2",
      type = "viridis",
      limits = c(-3,3),
      oob = scales::squish)+
    geom_text(aes(label = round(Z, digits = 1)), color = "white", size = 4) +
    ggtitle(paste0("Cluster n° ",as.character(i))) +
    theme_minimal()
  return(plot)
}
