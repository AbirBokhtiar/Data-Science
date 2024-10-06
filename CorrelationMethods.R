install.packages(c("dplyr", "ggplot2", "corrplot", "caret"))

library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)


data <- read.csv("E:/desktop/Data Science/Final/Final Assessment1/shanghai_ranking_2024.csv",header=TRUE, sep=",")

data
summary(data)
str(data)

inconsistent_formats <- data[!grepl("^[0-9]+$", data$National.Regional.Rank), ]
print("Inconsistent formats:")
print(inconsistent_formats)

data$Rank <- 1:1000

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

data$Alumni <- as.integer(data$Alumni)
data$Award <- as.integer(data$Award)
data$Hici <- as.integer(data$Hici)
data$N.S <- as.integer(data$N.S)
data <- data[!is.na(data$N.S), ]
data$PUB <- as.integer(data$PUB)


pearson_corr <- cor(data %>% select_if(is.numeric), method = "pearson")
corrplot(pearson_corr, method = "circle", title = "Pearson Correlation")


spearman_corr <- cor(data %>% select_if(is.numeric), method = "spearman")
corrplot(spearman_corr, method = "circle", title = "Spearman Correlation")


kendall_corr <- cor(data %>% select_if(is.numeric), method = "kendall")
corrplot(kendall_corr, method = "circle", title = "Kendall Correlation")




RankvsRank <- cor(data$Rank, data$Rank, method = "pearson")
print(RankvsRank)

NRRvsRank <- cor(data$Rank, data$National.Regional.Rank, method = "pearson")
print(NRRvsRank)

AlumnivsRank <- cor(data$Alumni, data$Rank, method = "pearson")
print(AlumnivsRank)

AwardvsRank <- cor(data$Award, data$Rank, method = "pearson")
print(AwardvsRank)

HicivsRank <- cor(data$Hici, data$Rank, method = "pearson")
print(HicivsRank)

N.SvsRank <- cor(data$N.S, data$Rank, method = "pearson")
print(N.SvsRank)

PUBvsRank <- cor(data$PUB, data$Rank, method = "pearson")
print(PUBvsRank)

PCPvsRank <- cor(data$PCP, data$Rank, method = "pearson")
print(PCPvsRank)



NRR1 <- cor(data$Rank, data$National.Regional.Rank, method = "pearson")
print(NRR1)

NRR2 <- cor(data$National.Regional.Rank, data$National.Regional.Rank, method = "pearson")
print(NRR2)

NRR3 <- cor(data$Alumni, data$National.Regional.Rank, method = "pearson")
print(NRR3)

NRR4 <- cor(data$Award, data$National.Regional.Rank, method = "pearson")
print(NRR4)

NRR5 <- cor(data$Hici, data$National.Regional.Rank, method = "pearson")
print(NRR5)

NRR6 <- cor(data$N.S, data$National.Regional.Rank, method = "pearson")
print(NRR6)

NRR7 <- cor(data$PUB, data$National.Regional.Rank, method = "pearson")
print(NRR7)

NRR8 <- cor(data$PCP, data$National.Regional.Rank, method = "pearson")
print(NRR8)



Alumni1 <- cor(data$Alumni, data$Rank, method = "pearson")
print(Alumni1)

Alumni2 <- cor(data$National.Regional.Rank, data$Alumni, method = "pearson")
print(Alumni2)

Alumni3 <- cor(data$Alumni, data$Alumni, method = "pearson")
print(Alumni3)

Alumni4 <- cor(data$Award, data$Alumni, method = "pearson")
print(Alumni4)

Alumni5 <- cor(data$Hici, data$Alumni, method = "pearson")
print(Alumni5)

Alumni6 <- cor(data$N.S, data$Alumni, method = "pearson")
print(Alumni6)

Alumni7 <- cor(data$PUB, data$Alumni, method = "pearson")
print(Alumni7)

Alumni8 <- cor(data$PCP, data$Alumni, method = "pearson")
print(Alumni8)



Award1 <- cor(data$Rank, data$Award, method = "pearson")
print(Award1)

Award2 <- cor(data$National.Regional.Rank, data$Award, method = "pearson")
print(Award2)

Award3 <- cor(data$Alumni, data$Award, method = "pearson")
print(Award3)

Award4 <- cor(data$Award, data$Award, method = "pearson")
print(Award4)

Award5 <- cor(data$Hici, data$Award, method = "pearson")
print(Award5)

Award6 <- cor(data$N.S, data$Award, method = "pearson")
print(Award6)

Award7 <- cor(data$PUB, data$Award, method = "pearson")
print(Award7)

Award8 <- cor(data$PCP, data$Award, method = "pearson")
print(Award8)



Hici1 <- cor(data$Rank, data$Hici, method = "pearson")
print(Hici1)

Hici2 <- cor(data$National.Regional.Rank, data$Hici, method = "pearson")
print(Hici2)

Hici3 <- cor(data$Alumni, data$Hici, method = "pearson")
print(Hici3)

Hici4 <- cor(data$Award, data$Hici, method = "pearson")
print(Hici4)

Hici5 <- cor(data$Hici, data$Hici, method = "pearson")
print(Hici5)

Hici6 <- cor(data$N.S, data$Hici, method = "pearson")
print(Hici6)

Hici7 <- cor(data$PUB, data$Hici, method = "pearson")
print(Hici7)

Hici8 <- cor(data$PCP, data$Hici, method = "pearson")
print(Hici8)



N.S1 <- cor(data$Rank, data$N.S, method = "pearson")
print(N.S1)

N.S2 <- cor(data$National.Regional.Rank, data$N.S, method = "pearson")
print(N.S2)

N.S3 <- cor(data$Alumni, data$N.S, method = "pearson")
print(N.S3)

N.S4 <- cor(data$Award, data$N.S, method = "pearson")
print(N.S4)

N.S5 <- cor(data$Hici, data$N.S, method = "pearson")
print(N.S5)

N.S6 <- cor(data$N.S, data$N.S, method = "pearson")
print(N.S6)

N.S7 <- cor(data$PUB, data$N.S, method = "pearson")
print(N.S7)

N.S8 <- cor(data$PCP, data$N.S, method = "pearson")
print(N.S8)



PUB1 <- cor(data$Rank, data$PUB, method = "pearson")
print(PUB1)

PUB2 <- cor(data$National.Regional.Rank, data$PUB, method = "pearson")
print(PUB2)

PUB3 <- cor(data$Alumni, data$PUB, method = "pearson")
print(PUB3)

PUB4 <- cor(data$Award, data$PUB, method = "pearson")
print(PUB4)

PUB5 <- cor(data$Hici, data$PUB, method = "pearson")
print(PUB5)

PUB6 <- cor(data$N.S, data$PUB, method = "pearson")
print(PUB6)

PUB7 <- cor(data$PUB, data$PUB, method = "pearson")
print(PUB7)

PUB8 <- cor(data$PCP, data$PUB, method = "pearson")
print(PUB8)



PCP1 <- cor(data$Rank, data$PCP, method = "pearson")
print(PCP1)

PCP2 <- cor(data$National.Regional.Rank, data$PCP, method = "pearson")
print(PCP2)

PCP3 <- cor(data$Alumni, data$PCP, method = "pearson")
print(PCP3)

PCP4 <- cor(data$Award, data$PCP, method = "pearson")
print(PCP4)

PCP5 <- cor(data$Hici, data$PCP, method = "pearson")
print(PCP5)

PCP6 <- cor(data$N.S, data$PCP, method = "pearson")
print(PCP6)

PCP7 <- cor(data$PUB, data$PCP, method = "pearson")
print(PCP7)

PCP8 <- cor(data$PCP, data$PCP, method = "pearson")
print(PCP8)


corrMat <- cbind(RankvsRank, NRRvsRank, AlumnivsRank, AwardvsRank, HicivsRank, N.SvsRank, PUBvsRank, PCPvsRank)

col_names <- c("Rank","NRR", "Alumni", "Award", "Hici", "N.s", "PUB", "PCP")

colnames(corrMat) <- col_names
corrMat <- rbind(corrMat, cbind(NRR1, NRR2, NRR3, NRR4, NRR5, NRR6, NRR7, NRR8))
corrMat <- rbind(corrMat, cbind(Alumni1, Alumni2, Alumni3, Alumni4, Alumni5, Alumni6, Alumni7, Alumni8))
corrMat <- rbind(corrMat, cbind(Award1, Award2, Award3, Award4, Award5, Award6, Award7, Award8))
corrMat <- rbind(corrMat, cbind(Hici1, Hici2, Hici3, Hici4, Hici5, Hici6, Hici7, Hici8))
corrMat <- rbind(corrMat, cbind(N.S1, N.S2, N.S3, N.S4, N.S5, N.S6, N.S7, N.S8))
corrMat <- rbind(corrMat, cbind(PUB1, PUB2, PUB3, PUB4, PUB5, PUB6, PUB7, PUB8))
corrMat <- rbind(corrMat, cbind(PCP1, PCP2, PCP3, PCP4, PCP5, PCP6, PCP7, PCP8))
row_names <- c("Rank", "NRR", "Alumni", "Award", "Hici", "N.s", "PUB", "PCP")
rownames(corrMat) <- row_names




RankvsRank <- cor(data$Rank, data$Rank, method = "spearman")
print(RankvsRank)

NRRvsRank <- cor(data$Rank, data$National.Regional.Rank, method = "spearman")
print(NRRvsRank)

AlumnivsRank <- cor(data$Alumni, data$Rank, method = "spearman")
print(AlumnivsRank)

AwardvsRank <- cor(data$Award, data$Rank, method = "spearman")
print(AwardvsRank)

HicivsRank <- cor(data$Hici, data$Rank, method = "spearman")
print(HicivsRank)

N.SvsRank <- cor(data$N.S, data$Rank, method = "spearman")
print(N.SvsRank)

PUBvsRank <- cor(data$PUB, data$Rank, method = "spearman")
print(PUBvsRank)

PCPvsRank <- cor(data$PCP, data$Rank, method = "spearman")
print(PCPvsRank)



NRR1 <- cor(data$Rank, data$National.Regional.Rank, method = "spearman")
print(NRR1)

NRR2 <- cor(data$National.Regional.Rank, data$National.Regional.Rank, method = "spearman")
print(NRR2)

NRR3 <- cor(data$Alumni, data$National.Regional.Rank, method = "spearman")
print(NRR3)

NRR4 <- cor(data$Award, data$National.Regional.Rank, method = "spearman")
print(NRR4)

NRR5 <- cor(data$Hici, data$National.Regional.Rank, method = "spearman")
print(NRR5)

NRR6 <- cor(data$N.S, data$National.Regional.Rank, method = "spearman")
print(NRR6)

NRR7 <- cor(data$PUB, data$National.Regional.Rank, method = "spearman")
print(NRR7)

NRR8 <- cor(data$PCP, data$National.Regional.Rank, method = "spearman")
print(NRR8)



Alumni1 <- cor(data$Alumni, data$Rank, method = "spearman")
print(Alumni1)

Alumni2 <- cor(data$National.Regional.Rank, data$Alumni, method = "spearman")
print(Alumni2)

Alumni3 <- cor(data$Alumni, data$Alumni, method = "spearman")
print(Alumni3)

Alumni4 <- cor(data$Award, data$Alumni, method = "spearman")
print(Alumni4)

Alumni5 <- cor(data$Hici, data$Alumni, method = "spearman")
print(Alumni5)

Alumni6 <- cor(data$N.S, data$Alumni, method = "spearman")
print(Alumni6)

Alumni7 <- cor(data$PUB, data$Alumni, method = "spearman")
print(Alumni7)

Alumni8 <- cor(data$PCP, data$Alumni, method = "spearman")
print(Alumni8)



Award1 <- cor(data$Rank, data$Award, method = "spearman")
print(Award1)

Award2 <- cor(data$National.Regional.Rank, data$Award, method = "spearman")
print(Award2)

Award3 <- cor(data$Alumni, data$Award, method = "spearman")
print(Award3)

Award4 <- cor(data$Award, data$Award, method = "spearman")
print(Award4)

Award5 <- cor(data$Hici, data$Award, method = "spearman")
print(Award5)

Award6 <- cor(data$N.S, data$Award, method = "spearman")
print(Award6)

Award7 <- cor(data$PUB, data$Award, method = "spearman")
print(Award7)

Award8 <- cor(data$PCP, data$Award, method = "spearman")
print(Award8)



Hici1 <- cor(data$Rank, data$Hici, method = "spearman")
print(Hici1)

Hici2 <- cor(data$National.Regional.Rank, data$Hici, method = "spearman")
print(Hici2)

Hici3 <- cor(data$Alumni, data$Hici, method = "spearman")
print(Hici3)

Hici4 <- cor(data$Award, data$Hici, method = "spearman")
print(Hici4)

Hici5 <- cor(data$Hici, data$Hici, method = "spearman")
print(Hici5)

Hici6 <- cor(data$N.S, data$Hici, method = "spearman")
print(Hici6)

Hici7 <- cor(data$PUB, data$Hici, method = "spearman")
print(Hici7)

Hici8 <- cor(data$PCP, data$Hici, method = "spearman")
print(Hici8)



N.S1 <- cor(data$Rank, data$N.S, method = "spearman")
print(N.S1)

N.S2 <- cor(data$National.Regional.Rank, data$N.S, method = "spearman")
print(N.S2)

N.S3 <- cor(data$Alumni, data$N.S, method = "spearman")
print(N.S3)

N.S4 <- cor(data$Award, data$N.S, method = "spearman")
print(N.S4)

N.S5 <- cor(data$Hici, data$N.S, method = "spearman")
print(N.S5)

N.S6 <- cor(data$N.S, data$N.S, method = "spearman")
print(N.S6)

N.S7 <- cor(data$PUB, data$N.S, method = "spearman")
print(N.S7)

N.S8 <- cor(data$PCP, data$N.S, method = "spearman")
print(N.S8)



PUB1 <- cor(data$Rank, data$PUB, method = "spearman")
print(PUB1)

PUB2 <- cor(data$National.Regional.Rank, data$PUB, method = "spearman")
print(PUB2)

PUB3 <- cor(data$Alumni, data$PUB, method = "spearman")
print(PUB3)

PUB4 <- cor(data$Award, data$PUB, method = "spearman")
print(PUB4)

PUB5 <- cor(data$Hici, data$PUB, method = "spearman")
print(PUB5)

PUB6 <- cor(data$N.S, data$PUB, method = "spearman")
print(PUB6)

PUB7 <- cor(data$PUB, data$PUB, method = "spearman")
print(PUB7)

PUB8 <- cor(data$PCP, data$PUB, method = "spearman")
print(PUB8)



PCP1 <- cor(data$Rank, data$PCP, method = "spearman")
print(PCP1)

PCP2 <- cor(data$National.Regional.Rank, data$PCP, method = "spearman")
print(PCP2)

PCP3 <- cor(data$Alumni, data$PCP, method = "spearman")
print(PCP3)

PCP4 <- cor(data$Award, data$PCP, method = "spearman")
print(PCP4)

PCP5 <- cor(data$Hici, data$PCP, method = "spearman")
print(PCP5)

PCP6 <- cor(data$N.S, data$PCP, method = "spearman")
print(PCP6)

PCP7 <- cor(data$PUB, data$PCP, method = "spearman")
print(PCP7)

PCP8 <- cor(data$PCP, data$PCP, method = "spearman")
print(PCP8)


corrMat2 <- cbind(RankvsRank, NRRvsRank, AlumnivsRank, AwardvsRank, HicivsRank, N.SvsRank, PUBvsRank, PCPvsRank)

col_names <- c("Rank","NRR", "Alumni", "Award", "Hici", "N.s", "PUB", "PCP")

colnames(corrMat2) <- col_names
corrMat2 <- rbind(corrMat2, cbind(NRR1, NRR2, NRR3, NRR4, NRR5, NRR6, NRR7, NRR8))
corrMat2 <- rbind(corrMat2, cbind(Alumni1, Alumni2, Alumni3, Alumni4, Alumni5, Alumni6, Alumni7, Alumni8))
corrMat2 <- rbind(corrMat2, cbind(Award1, Award2, Award3, Award4, Award5, Award6, Award7, Award8))
corrMat2 <- rbind(corrMat2, cbind(Hici1, Hici2, Hici3, Hici4, Hici5, Hici6, Hici7, Hici8))
corrMat2 <- rbind(corrMat2, cbind(N.S1, N.S2, N.S3, N.S4, N.S5, N.S6, N.S7, N.S8))
corrMat2 <- rbind(corrMat2, cbind(PUB1, PUB2, PUB3, PUB4, PUB5, PUB6, PUB7, PUB8))
corrMat2 <- rbind(corrMat2, cbind(PCP1, PCP2, PCP3, PCP4, PCP5, PCP6, PCP7, PCP8))
row_names <- c("Rank", "NRR", "Alumni", "Award", "Hici", "N.s", "PUB", "PCP")
rownames(corrMat2) <- row_names




library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)

numeric_columns <- c("Alumni", "Award", "Hici", "N.S", "PUB", "Rank", "National.Regional.Rank")  # Add other numeric columns as needed
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)

correlation_matrix <- cor(data %>% select_if(is.numeric), method = "pearson")

melted_corr <- melt(correlation_matrix)

ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Attributes", y = "Attributes")




ggplot(data, aes(x=Rank, y=Hici)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Scatter plot of Alumni vs. Award",
       x="Rank", y="Hici")


ggplot(data, aes(x=Alumni, y=Award)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Scatter plot of Alumni vs. Award",
       x="Alumni", y="Award")
