install.packages("moments")
install.packages(c("dplyr", "ggplot2", "corrplot", "caret"))
install.packages("GGally")
install.packages("fmsb")

library(readxl)  
library(ggplot2)
library(dplyr)   
library(tidyr)   
library(moments) 

data <- read.csv("E:/desktop/Data Science/Final/Final Assessment1/shanghai_ranking_2024.csv", header = TRUE, sep = ",")

head(data)

summary(data)

data$Rank[1:100] <- as.character(1:100)

str(data)

convert_to_numeric <- function(value) {
  value <- gsub(" Jun", "", value)
  value <- trimws(value)
  if (grepl("-", value)) {
    range_values <- as.numeric(unlist(strsplit(value, "-")))
    return(min(range_values))
  } else {
    return(as.integer(value))
  }
}

data[['National.Regional.Rank']] <- sapply(data[['National.Regional.Rank']], convert_to_numeric)
data$National.Regional.Rank <- as.character(data$National.Regional.Rank)
data <- data[!is.na(data$N.S), ]

numeric_cols <- sapply(data, is.numeric)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



for (col in names(data)[numeric_cols]) {
  
  skewness_value <- skewness(data[[col]], na.rm = TRUE)
  mean_val <- mean(data[[col]], na.rm = TRUE)
  median_val <- median(data[[col]], na.rm = TRUE)
  mode_val <- get_mode(data[[col]])
  
  cat("Mean of ", col, mean_val,"\n")
  cat("Median of ", col, median_val,"\n")
  cat("Mode of ", col, mode_val,"\n")
  
  sd_val <- sd(data[[col]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha = 0.5) +
    geom_density(color = "red", size = 1) +
    geom_vline(xintercept = mean_val, color = "green", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_val, color = "orange", linetype = "dashed", size = 1) +
    geom_vline(xintercept = mode_val, color = "purple", linetype = "dashed", size = 1) +
    labs(title = paste("Line Histogram of", col, "\nSkewness:", round(skewness_value, 2)), 
         x = col, y = "Density") +
    theme_minimal() +
    annotate("text", x = mean_val, y = 0, label = paste("Mean:", round(mean_val, 2)), color = "green", vjust = -1) +
    annotate("text", x = median_val, y = 0, label = paste("Median:", round(median_val, 2)), color = "orange", vjust = -1) +
    annotate("text", x = mode_val, y = 0, label = paste("Mode:", mode_val), color = "purple", vjust = -1)
  
  x_seq <- seq(min(data[[col]], na.rm = TRUE), max(data[[col]], na.rm = TRUE), length.out = 100)
  y_seq <- dnorm(x_seq, mean = mean_val, sd = sd_val)
  
  p <- p + geom_line(data = data.frame(x = x_seq, y = y_seq), aes(x = x, y = y), color = "black", size = 1) +
    labs(subtitle = "Overlayed Normal Distribution")
  
  print(p)
  print(x_seq)
  print(y_seq)

  readline(prompt = "Press [Enter] to see the next plot...")
}

data_z_scores <- data.frame(data, Z_Scores = z_scores)
print(head(data_z_scores))





if ("National.Regional.Rank" %in% names(data)) {
  bar_plot <- ggplot(data, aes(x = National.Regional.Rank)) +
    geom_bar(fill = "blue") +
    labs(title = "Bar Graph of National Regional Rank", x = "National Regional Rank", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(bar_plot)
}

if ("Rank" %in% names(data)) {
  bar_plot <- ggplot(data, aes(x = Rank)) +
    geom_bar(fill = "blue") +
    labs(title = "Bar Graph of Rank", x = "Rank", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(bar_plot)
}



for (col in names(data)[numeric_cols]) {
  box_plot <- ggplot(data, aes_string(x = col)) +  
    geom_boxplot(fill = "blue", color = "black") +
    labs(title = paste("Boxplot of", col), x = col, y = "Value") +
    theme_minimal() +
    theme(axis.title.x = element_blank())  
  
  print(box_plot)
  
  readline(prompt = "Press [Enter] to see the next plot...")
}




print(pearson_corr)

for (i in 1:(length(numeric_cols) - 1)) {
  for (j in (i + 1):length(numeric_cols)) {
    col_x <- names(data)[numeric_cols][i]
    col_y <- names(data)[numeric_cols][j]
    
    scatter_plot <- ggplot(data, aes_string(x = col_x, y = col_y)) +
      geom_point(color = "blue", alpha = 0.6) +
      labs(title = paste("Scatter Plot of", col_y, "vs", col_x), 
           x = col_x, y = col_y) +
      theme_minimal()
    
    print(scatter_plot)
    
    readline(prompt = "Press [Enter] to see the next plot...")
  }
}



library(GGally)  
library(ggplot2)

print(pearson_corr)

numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]

ggpairs(numeric_data) + theme_minimal()

pairs(numeric_data)





for (col in names(data)[numeric_cols]) {
  violin_plot <- ggplot(data, aes_string(x = 100, y = col)) +  
    geom_violin(fill = "blue", color = "black", alpha = 0.6) +
    geom_boxplot(width = 0.1, color = "pink", outlier.shape = NA) +  
    labs(title = paste("Violin Plot of", col), x = "", y = col) +
    theme_minimal() +
    theme(axis.title.x = element_blank())  
  
  print(violin_plot)
  
  iqr_col <- IQR(data[[col]], na.rm = TRUE)
  cat("Interquartile Range (IQR) for ", col , iqr_col, "\n")
  median_col <- median(data[[col]], na.rm = TRUE)
  cat("Median for ", col , median_col, "\n")
  
  readline(prompt = "Press [Enter] to see the next plot...")
}




valid_ranges <- c("101-150", "151-200", "201-300", "301-400", "401-500","501-600","601-700", "701-800","801-900","901-1000")

for (col in names(data)[numeric_cols]) {
  data_filtered <- data[data$Rank %in% valid_ranges, ]  
  
  if (col %in% names(data_filtered)) {
    line_graph <- ggplot(data_filtered, aes_string(x = "Rank", y = col)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = paste("Line Graph of", col, "vs Rank for Selected Ranges"), 
           x = "Rank", y = col) +
      theme_minimal()
    
    print(line_graph)
  } else {
    warning(paste(col, "is not in the filtered dataset."))
  }
  
  readline(prompt = "Press [Enter] to see the next plot...")
}



data$Alumni <- as.numeric(as.character(data$Alumni))
data$Award <- as.numeric(as.character(data$Award))
data$Hici <- as.numeric(as.character(data$Hici))
data$N.S <- as.numeric(as.character(data$N.S))
data$PUB <- as.numeric(as.character(data$PUB))

radar_data[is.na(radar_data)] <- 0

data <- na.omit(data)

library(fmsb)  

plot_radar_with_values <- function(radar_data, university_name) {
  
  max_min <- data.frame(
    Alumni = c(max(data$Alumni, na.rm = TRUE), 0),
    Award = c(max(data$Award, na.rm = TRUE), 0),
    Hici = c(max(data$Hici, na.rm = TRUE), 0),
    N.S = c(max(data$N.S, na.rm = TRUE), 0),
    PUB = c(max(data$PUB, na.rm = TRUE), 0)
  )
  
  radar_data <- rbind(max_min, radar_data)  
  
  radarchart(radar_data, axistype = 1,
             pcol = "blue", pfcol = rgb(0.2, 0.5, 0.8, 0.5), 
             plwd = 2, plty = 1,
             title = paste("Radar Chart for", university_name))  
  
  values <- radar_data[3,]
  
  num_axes <- ncol(radar_data)
  angles <- seq(0, 2 * pi, length.out = num_axes + 1)[1:num_axes]  
  
  for (j in 1:num_axes) {
    x <- 1.2 * cos(angles[j])  
    y <- 1.2 * sin(angles[j])  
    text(x, y, labels = round(as.numeric(values[j]), 2), col = "navy", cex = 1.2)  
  }
}

for (i in 1:nrow(data)) {
  
  radar_data <- as.data.frame(t(as.numeric(data[i, c("Alumni", "Award", "Hici", "N.S", "PUB")])))
  
  colnames(radar_data) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  
  if (any(is.na(radar_data))) {
    cat("Skipping row", i, "due to missing data\n")
    next
  }
  
  plot_radar_with_values(radar_data, data$University_Name[i])
  
  readline(prompt = "Press [Enter] to see the next plot...")
}



harvard_data <- data[data$University == "Harvard University", c("Alumni", "Award", "Hici", "N.S", "PUB")]
oxford_data <- data[data$University == "University of Oxford", c("Alumni", "Award", "Hici", "N.S", "PUB")]

if (nrow(harvard_data) > 0 && nrow(oxford_data) > 0) {
  
  radar_data_harvard <- as.data.frame(t(as.numeric(harvard_data)))
  radar_data_oxford <- as.data.frame(t(as.numeric(oxford_data)))
  
  
  colnames(radar_data_harvard) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  colnames(radar_data_oxford) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  
  
  plot_radar_comparison(radar_data_harvard, radar_data_oxford, "Harvard University", "University of Oxford")
} else {
  cat("One or both universities do not have valid data.\n")
}




plot_radar_comparison_three <- function(data1, data2, data3, uni1_name, uni2_name, uni3_name) {
  
  radar_data <- rbind(data1, data2, data3)
  
  max_min <- data.frame(
    Alumni = c(max(data$Alumni, na.rm = TRUE), 0),
    Award = c(max(data$Award, na.rm = TRUE), 0),
    Hici = c(max(data$Hici, na.rm = TRUE), 0),
    N.S = c(max(data$N.S, na.rm = TRUE), 0),
    PUB = c(max(data$PUB, na.rm = TRUE), 0)
  )
  
  radar_data <- rbind(max_min, radar_data)  
  
  radarchart(radar_data, axistype = 1,
             pcol = c("blue", "red", "green"), 
             pfcol = c(rgb(0.2, 0.5, 0.8, 0.5), rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5)), 
             plwd = 2, plty = 1,
             title = paste("Radar Chart Comparison:", uni1_name, ",", uni2_name, "and", uni3_name))  
  
  values1 <- radar_data[3,]
  values2 <- radar_data[4,]
  values3 <- radar_data[5,]
  
  num_axes <- ncol(radar_data)
  angles <- seq(0, 2 * pi, length.out = num_axes + 1)[1:num_axes] 
  
  for (j in 1:num_axes) {
    x <- 1.2 * cos(angles[j])
    y <- 1.2 * sin(angles[j])
    text(x, y, labels = round(as.numeric(values1[j]), 2), col = "navy", cex = 1.2)
  }
  
  
  for (j in 1:num_axes) {
    x <- 1.2 * cos(angles[j]) + 0.1  
    y <- 1.2 * sin(angles[j]) + 0.1  
    text(x, y, labels = round(as.numeric(values2[j]), 2), col = "red", cex = 1.2)
  }
  
  
  for (j in 1:num_axes) {
    x <- 1.2 * cos(angles[j]) - 0.1  
    y <- 1.2 * sin(angles[j]) - 0.1  
    text(x, y, labels = round(as.numeric(values3[j]), 2), col = "green", cex = 1.2)
  }
}




princeton_data <- data[data$University == "Princeton University", c("Alumni", "Award", "Hici", "N.S", "PUB")]
oxford_data <- data[data$University == "University of Oxford", c("Alumni", "Award", "Hici", "N.S", "PUB")]
columbia_data <- data[data$University == "Columbia University", c("Alumni", "Award", "Hici", "N.S", "PUB")]

if (nrow(princeton_data) > 0 && nrow(oxford_data) > 0 && nrow(columbia_data) > 0) {
  
  radar_data_princeton <- as.data.frame(t(as.numeric(princeton_data)))
  radar_data_oxford <- as.data.frame(t(as.numeric(oxford_data)))
  radar_data_columbia <- as.data.frame(t(as.numeric(columbia_data)))
  
  colnames(radar_data_princeton) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  colnames(radar_data_oxford) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  colnames(radar_data_columbia) <- c("Alumni", "Award", "Hici", "N.S", "PUB")
  
  
  plot_radar_comparison_three(radar_data_princeton, radar_data_oxford, radar_data_columbia, 
                              "Princeton University", "University of Oxford", "Columbia University")
} else {
  cat("One or more universities do not have valid data.\n")
}
