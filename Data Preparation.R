install.packages("readxl")
install.packages("ggplot2")
install.packages("colorspace")
install.packages("dplyr")
install.packages("ROSE")
library(readxl)
mainData <- read_excel("E:/desktop/Data Science/MID Project/Abir/Midterm_Project_Dataset_section(B).xlsx")
mainData

numberOfColumn<-ncol(mainData)
numberOfRow<-nrow(mainData)
cat("number Of Columns: ", numberOfColumn,"\n")
cat("number Of Rows: ", numberOfRow,"\n")

str(mainData)

unique(mainData$age)
unique(mainData$who)

realData <- mainData
realData$who <- ifelse(substr(tolower(realData$who), 1, 1) == "m", "man",
                       ifelse(substr(tolower(realData$who), 1, 1) == "w", "woman",
                              ifelse(substr(tolower(realData$who), 1, 1) == "c", "child",
                                     NA)))
unique(realData$who)


realData$Gender <- factor(realData$Gender,
                          levels = c("male", "female"),
                          labels = c(1, 0))
realData$Gender

realData$class <- factor(realData$class,
                         levels = c("First", "Second", "Third"),
                         labels = c(1, 2, 3))
realData$class

realData$who <- factor(realData$who,
                       levels = c("child", "man", "woman"),
                       labels = c(0, 1, 2))

realData

summary(realData)

na_counts <- colSums(is.na(realData))
print(na_counts)

barplot(na_counts, 
        main = "Missing Values Count per Column", 
        xlab = "Columns", 
        ylab = "Missing Values", 
        col = "skyblue", 
        las = 2)


cleanData1 <- na.omit(realData, cols = "")
na_counts <- colSums(is.na(cleanData1))
print(na_counts)
barplot(na_counts, names.arg = names(na_counts),
        ylab = "Number of Missing Values", col = "red",cex.names = 0.9,
        main = "Missing Values per Attribute", las =2)


cleanData2 <- mainData

rows_to_discard <- apply(mainData, 1, function(row) {
  sum(!is.na(row)) == 1 && !is.na(row["survived"])
})


cleanData2 <- mainData[!rows_to_discard, ]
cleanData2$fare <- as.numeric(cleanData2$fare)
cleanData2

mode_Gender <- names(sort(table(cleanData2$Gender), decreasing = TRUE))[1]
cleanData2$Gender[is.na(cleanData2$Gender)] <- mode_Gender

mode_class <- names(sort(table(cleanData2$class), decreasing = TRUE))[1]
cleanData2$class[is.na(cleanData2$class)] <- mode_class

cleanData2

na_counts <- colSums(is.na(cleanData2))
print(na_counts)
barplot(na_counts, names.arg = names(na_counts),
        ylab = "Number of Missing Values", col = "red",cex.names = 0.9,
        main = "Missing Values per Attribute", las =2)



numberOfRowMain<-nrow(mainData)
numberOfRow<-nrow(cleanData2)
cat("number Of Rows: ", numberOfRowMain,"\n")
cat("number Of Rows: ", numberOfRow,"\n")

for(col_name in names(cleanData2)) {
  if(is.numeric(cleanData2[[col_name]])) {
    column_mean <- mean(cleanData2[[col_name]], na.rm = TRUE)
    
    cleanData2[[col_name]][is.na(cleanData2[[col_name]])] <- column_mean
    cleanData2[[col_name]] <- round(cleanData2[[col_name]], digits = 0)
  }
}

na_counts <- colSums(is.na(cleanData2))
print(na_counts)
barplot(na_counts, names.arg = names(na_counts),
        ylab = "Number of Missing Values", col = "red",cex.names = 0.9,
        main = "Missing Values per Attribute", las =2)


cleanData2$who <- ifelse(substr(tolower(cleanData2$who), 1, 1) == "m", "man",
                       ifelse(substr(tolower(cleanData2$who), 1, 1) == "w", "woman",
                              ifelse(substr(tolower(cleanData2$who), 1, 1) == "c", "child",
                                     NA)))
unique(cleanData2$who)
cleanData2

getMode <- function(v) {
  tabulated <- table(v)
  mode_value <- names(sort(tabulated, decreasing = TRUE))[1]
  return(as.numeric(mode_value)) 
}

means <- sapply(cleanData2, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
medians <- sapply(cleanData2, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA)
modes <- sapply(cleanData2, function(x) if(is.numeric(x) || is.factor(x) || is.character(x)) getMode(x) else NA)

stat_values <- rbind(means, medians, modes)

row_names <- c("Mean", "Median", "Mode")
rownames(stat_values) <- row_names

barplot(stat_values, beside = TRUE, 
        col = c("green", "orange", "brown"),
        legend.text = row_names, args.legend = list(x = "topright", cex = 0.7),
        cex.names = 0.7)



cleanData2$Gender <- factor(cleanData2$Gender,
                          levels = c("male", "female"),
                          labels = c(1, 0))
cleanData2$Gender

cleanData2$class <- factor(cleanData2$class,
                         levels = c("First", "Second", "Third"),
                         labels = c(1, 2, 3))
cleanData2$class

cleanData2$who <- factor(cleanData2$who,
                       levels = c("child", "man", "woman"),
                       labels = c(0, 1, 2))

cleanData2

summary(cleanData2)


library(ggplot2)

boxplot(cleanData2$Gender, main = "Gender")
boxplot(cleanData2$age, main = "age" )
boxplot(cleanData2$sibsp, main = "sibsp" )
boxplot(cleanData2$parch, main = "parch" )
boxplot(cleanData2$fare, main = "fare")
boxplot(cleanData2$class, main = "class" )
boxplot(cleanData2$who, main = "who")
boxplot(cleanData2$alone, main = "alone" )
boxplot(cleanData2$survived, main = "survived" )

age_mean <- round(mean(cleanData2$age, na.rm = TRUE))
age_outliers <- boxplot.stats(cleanData2$age)$out
age_outliers
cleanData2$age[cleanData2$age %in% age_outliers] <- age_mean
boxplot(cleanData2$age, main = "age")
age_outliers <- boxplot.stats(cleanData2$age)$out
age_outliers

fare_mean <- round(mean(cleanData2$fare, na.rm = TRUE))
fare_outliers <- boxplot.stats(cleanData2$fare)$out
fare_outliers
cleanData2$fare[cleanData2$fare %in% fare_outliers] <- fare_mean
boxplot(cleanData2$fare, main = "fare")
fare_outliers <- boxplot.stats(cleanData2$fare)$out
fare_outliers

realData<- cleanData2

min_age <- min(realData$age, na.rm = TRUE)
max_age <- max(realData$age, na.rm = TRUE)

realData$age <- (realData$age - min_age) / (max_age - min_age)

min_fare <- min(realData$fare, na.rm = TRUE)
max_fare <- max(realData$fare, na.rm = TRUE)

realData$fare <- (realData$fare - min_fare) / (max_fare - min_fare)



realData$Gender <- factor(realData$Gender,
                            levels = c(0, 1),
                            labels = c("female", "male"))

realData$who <- factor(realData$who,
                         labels = c("child", "man", "woman"),
                         levels = c(0, 1, 2))




library(ggplot2)

mean_age <- mean(realData$age, na.rm = TRUE)
median_age <- median(realData$age, na.rm = TRUE)
mode_age <- as.numeric(names(sort(table(realData$age), decreasing = TRUE))[1])

plot_data <- data.frame(age = realData$age)

ggplot(plot_data, aes(x = age)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = mean_age, col = "green", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = median_age, col = "blue", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mode_age, col = "red", linetype = "dashed", size = 1) +    
  labs(title = "Density Plot of Age with Central Tendency Measures",
       x = "Age", y = "Density") +
  theme_minimal() +
  annotate("text", x = mean_age, y = 0.02, label = "Mean", col = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = median_age, y = 0.02, label = "Median", col = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_age, y = 0.02, label = "Mode", col = "red", angle = 90, vjust = -0.5)


mean_fare <- mean(realData$fare, na.rm = TRUE)
median_fare <- median(realData$fare, na.rm = TRUE)
mode_fare <- as.numeric(names(sort(table(realData$fare), decreasing = TRUE))[1])

plot_data <- data.frame(fare = realData$fare)

ggplot(plot_data, aes(x = fare)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = mean_fare, col = "green", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = median_fare, col = "blue", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mode_fare, col = "red", linetype = "dashed", size = 1) +    
  labs(title = "Density Plot of Fare with Central Tendency Measures",
       x = "Fare", y = "Density") +
  theme_minimal() +
  annotate("text", x = mean_fare, y = 0.02, label = "Mean", col = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = median_fare, y = 0.02, label = "Median", col = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_fare, y = 0.02, label = "Mode", col = "red", angle = 90, vjust = -0.5)





iqr_age <- IQR(realData$age, na.rm = TRUE)
iqr_fare <- IQR(realData$fare, na.rm = TRUE)
cat("Interquartile Range (IQR) for Age: ", iqr_age, "\n")
cat("Interquartile Range (IQR) for Fare: ", iqr_fare, "\n")

sd_age <- sd(realData$age, na.rm = TRUE)
sd_fare <- sd(realData$fare, na.rm = TRUE)
cat("Standard Deviation for Age: ", sd_age, "\n")
cat("Standard Deviation for Fare: ", sd_fare, "\n")

var_age <- var(realData$age, na.rm = TRUE)
var_fare <- var(realData$fare, na.rm = TRUE)
cat("Variance for Age: ", var_age, "\n")
cat("Variance for Fare: ", var_fare, "\n")



library(dplyr)
library(ROSE)

class_distribution <- table(realData$survived)
print(class_distribution)

if (class_distribution[1] > class_distribution[2]) {
  majority <- filter(realData, survived == 0)  
  minority <- filter(realData, survived == 1)   
} else {
  majority <- filter(realData, survived == 1)   
  minority <- filter(realData, survived == 0)   
}

set.seed(123)
oversampled_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)

oversampled_data <- bind_rows(majority, oversampled_minority)

table(oversampled_data$survived)



