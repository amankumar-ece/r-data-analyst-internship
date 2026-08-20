library(MASS)
data(survey)
str(survey)
colSums(is.na(survey))
survey_clean <- survey

survey_clean$Wr.Hnd[is.na(survey_clean$Wr.Hnd)] <- median(survey_clean$Wr.Hnd, na.rm = TRUE)
survey_clean$NW.Hnd[is.na(survey_clean$NW.Hnd)] <- median(survey_clean$NW.Hnd, na.rm = TRUE)
survey_clean$Pulse[is.na(survey_clean$Pulse)] <- median(survey_clean$Pulse, na.rm = TRUE)
survey_clean$Height[is.na(survey_clean$Height)] <- median(survey_clean$Height, na.rm = TRUE)
colSums(is.na(survey_clean))
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

survey_clean$Sex[is.na(survey_clean$Sex)]     <- get_mode(survey_clean$Sex)
survey_clean$W.Hnd[is.na(survey_clean$W.Hnd)] <- get_mode(survey_clean$W.Hnd)
survey_clean$Clap[is.na(survey_clean$Clap)]   <- get_mode(survey_clean$Clap)
survey_clean$Smoke[is.na(survey_clean$Smoke)] <- get_mode(survey_clean$Smoke)
survey_clean$M.I[is.na(survey_clean$M.I)]     <- get_mode(survey_clean$M.I)
colSums(is.na(survey_clean))

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  sum(x < lower | x > upper)
}

detect_outliers(survey_clean$Wr.Hnd)
detect_outliers(survey_clean$NW.Hnd)
detect_outliers(survey_clean$Pulse)
detect_outliers(survey_clean$Height)
detect_outliers(survey_clean$Age)

cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

survey_clean$Wr.Hnd <- cap_outliers(survey_clean$Wr.Hnd)
survey_clean$NW.Hnd <- cap_outliers(survey_clean$NW.Hnd)
survey_clean$Pulse  <- cap_outliers(survey_clean$Pulse)
survey_clean$Height <- cap_outliers(survey_clean$Height)
survey_clean$Age    <- cap_outliers(survey_clean$Age)

detect_outliers(survey_clean$Pulse)

survey_clean$Wr.Hnd_z <- as.numeric(scale(survey_clean$Wr.Hnd))
survey_clean$NW.Hnd_z <- as.numeric(scale(survey_clean$NW.Hnd))
survey_clean$Pulse_z  <- as.numeric(scale(survey_clean$Pulse))
survey_clean$Height_z <- as.numeric(scale(survey_clean$Height))
survey_clean$Age_z    <- as.numeric(scale(survey_clean$Age))

mean(survey_clean$Height_z)
sd(survey_clean$Height_z)

survey_clean$Sex_enc   <- as.integer(factor(survey_clean$Sex))
survey_clean$W.Hnd_enc <- as.integer(factor(survey_clean$W.Hnd))
survey_clean$M.I_enc   <- as.integer(factor(survey_clean$M.I))

onehot_exer  <- model.matrix(~ Exer - 1, data = survey_clean)
onehot_smoke <- model.matrix(~ Smoke - 1, data = survey_clean)

survey_clean <- cbind(survey_clean, onehot_exer, onehot_smoke)

names(survey_clean)

write.csv(survey_clean, "Outputs/survey_cleaned.csv", row.names = FALSE)

summary(survey_clean[, c("Wr.Hnd","NW.Hnd","Pulse","Height","Age")])

png("plots/height_histogram.png", width = 800, height = 550)
hist(survey_clean$Height,
     main = "Distribution of Student Height",
     xlab = "Height (cm)",
     col = "steelblue",
     breaks = 15)
dev.off()
png("plots/height_by_sex_boxplot.png", width = 800, height = 550)
boxplot(Height ~ Sex, data = survey_clean,
        main = "Height by Sex",
        xlab = "Sex", ylab = "Height (cm)",
        col = c("orange", "steelblue"))
dev.off()

png("plots/smoking_status_bar.png", width = 800, height = 550)
barplot(table(survey_clean$Smoke),
        main = "Smoking Status of Students",
        col = "tomato", ylab = "Count")
dev.off()

png("plots/handspan_scatter.png", width = 800, height = 550)
plot(survey_clean$Wr.Hnd, survey_clean$NW.Hnd,
     main = "Writing Hand Span vs Non-Writing Hand Span",
     xlab = "Writing hand span (cm)", ylab = "Non-writing hand span (cm)",
     pch = 19, col = "darkblue")
abline(lm(NW.Hnd ~ Wr.Hnd, data = survey_clean), col = "red", lwd = 2)
dev.off()

p1 <- ggplot(survey_clean, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Student Height",
       x = "Height (cm)", y = "Count") +
  theme_minimal()

ggsave("plots/gg_height_histogram.png", plot = p1, width = 8, height = 5.5, dpi = 100)

p2 <- ggplot(survey_clean, aes(x = Sex, y = Height, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Height Distribution by Sex",
       x = "Sex", y = "Height (cm)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots/gg_height_by_sex.png", plot = p2, width = 8, height = 5.5, dpi = 100)

p3 <- ggplot(survey_clean, aes(x = Wr.Hnd, y = NW.Hnd, color = Sex)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Writing vs Non-Writing Hand Span by Sex",
       x = "Writing hand span (cm)", y = "Non-writing hand span (cm)") +
  theme_minimal()

ggsave("plots/gg_handspan_by_sex.png", plot = p3, width = 8, height = 5.5, dpi = 100)

p4 <- ggplot(survey_clean, aes(x = Exer, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Exercise Frequency by Sex",
       x = "Exercise Level", y = "Count") +
  theme_minimal()

ggsave("plots/gg_exercise_by_sex.png", plot = p4, width = 8, height = 5.5, dpi = 100)

cor_matrix <- cor(survey_clean[, c("Wr.Hnd","NW.Hnd","Pulse","Height","Age")])
cor_melted <- melt(cor_matrix)

p5 <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0) +
  labs(title = "Correlation Matrix of Numeric Variables", x = "", y = "", fill = "Correlation") +
  theme_minimal()

ggsave("plots/gg_correlation_heatmap.png", plot = p5, width = 7, height = 6, dpi = 100)