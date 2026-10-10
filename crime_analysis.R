# =====================================================================
#  Crime Data Analysis and Visualization Using R
#  MCA - R Programming Mini Project
#  Author : Sutharssan (717825Z155)
# =====================================================================
#  Stages
#   1. Dataset Import & Understanding
#   2. Data Cleaning
#   3. Exploratory Data Analysis & Visualization
#   4. Data Splitting
#   5. Model Training  (Logistic Regression, Decision Tree, Random Forest)
#   6. Model Comparison & Key Findings
# ---------------------------------------------------------------------
#  How to run : open the project folder in RStudio, set it as working
#  directory (Session > Set Working Directory > To Source File Location)
#  and press Source.
# =====================================================================

# ---- 0. Packages -----------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "corrplot",
          "caret", "ranger", "rpart")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
set.seed(123)
dir.create("charts", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# helper to save every chart as PNG
save_chart <- function(plot, name, w = 9, h = 5.5) {
  ggsave(file.path("charts", paste0(name, ".png")), plot,
         width = w, height = h, dpi = 120, bg = "white")
}
my_theme <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", colour = "#1F3A5F"))
pal <- c("#1F3A5F", "#C0392B", "#2E86AB", "#F39C12", "#27AE60",
         "#8E44AD", "#16A085", "#D35400", "#7F8C8D", "#2C3E50", "#E74C3C")

# =====================================================================
# STAGE 1 : DATASET IMPORT & UNDERSTANDING
# =====================================================================
crime <- read.csv("data/crime_data.csv", stringsAsFactors = FALSE,
                  na.strings = c("", "NA"))

head(crime)                     # first six records
dim(crime)                      # rows and columns
names(crime)                    # column names
str(crime)                      # data types
summary(crime)                  # statistical summary

# =====================================================================
# STAGE 2 : DATA CLEANING
# =====================================================================
# 2.1 Missing values
na_summary <- colSums(is.na(crime))
na_summary

# 2.2 Duplicate rows
dup_count <- sum(duplicated(crime))
cat("Duplicate rows:", dup_count, "\n")
crime <- crime[!duplicated(crime), ]

# 2.3 Fix inconsistent text (spaces / upper / lower case)
crime$State       <- tools::toTitleCase(tolower(trimws(crime$State)))
crime$City        <- trimws(crime$City)
crime$Crime_Type  <- tools::toTitleCase(tolower(trimws(crime$Crime_Type)))
crime$Case_Status <- trimws(crime$Case_Status)
cat("Unique crime types :", length(unique(crime$Crime_Type)), "\n")
cat("Unique states      :", length(unique(crime$State)), "\n")

# 2.4 Impossible ages (outliers)  -> treat as missing
bad_age <- sum(crime$Victim_Age < 1 | crime$Victim_Age > 100, na.rm = TRUE)
cat("Impossible age values:", bad_age, "\n")
crime$Victim_Age[crime$Victim_Age < 1 | crime$Victim_Age > 100] <- NA

# 2.5 Handle missing values
crime$Victim_Age[is.na(crime$Victim_Age)] <- median(crime$Victim_Age, na.rm = TRUE)
gender_mode <- names(which.max(table(crime$Victim_Gender)))
crime$Victim_Gender[is.na(crime$Victim_Gender)] <- gender_mode
crime$City[is.na(crime$City)] <- "Unknown"

# 2.6 Feature engineering
crime$Date  <- as.Date(crime$Date)
crime$Year  <- as.integer(format(crime$Date, "%Y"))
crime$Month <- factor(format(crime$Date, "%b"), levels = month.abb)
crime$Hour  <- as.integer(substr(crime$Time, 1, 2))
crime$TimeOfDay <- cut(crime$Hour, breaks = c(-1, 5, 11, 17, 21, 23),
                       labels = c("Night", "Morning", "Afternoon", "Evening", "Night2"))
crime$TimeOfDay <- factor(ifelse(crime$TimeOfDay == "Night2", "Night",
                                 as.character(crime$TimeOfDay)),
                          levels = c("Morning", "Afternoon", "Evening", "Night"))
crime$AgeGroup  <- cut(crime$Victim_Age, breaks = c(0, 17, 30, 45, 60, 100),
                       labels = c("0-17", "18-30", "31-45", "46-60", "60+"))
crime$Solved    <- ifelse(crime$Case_Status == "Solved", 1, 0)

cat("Missing values after cleaning:", sum(is.na(crime)), "\n")
cat("Rows after cleaning:", nrow(crime), "\n")
write.csv(crime, "output/crime_data_cleaned.csv", row.names = FALSE)

# =====================================================================
# STAGE 3 : EXPLORATORY DATA ANALYSIS & VISUALIZATION
# =====================================================================
# 3.1 Overall crime-type distribution
type_tbl <- crime %>% count(Crime_Type, sort = TRUE)
print(type_tbl)
p1 <- ggplot(type_tbl, aes(reorder(Crime_Type, n), n)) +
  geom_col(fill = "#1F3A5F") + coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.8) +
  expand_limits(y = max(type_tbl$n) * 1.1) +
  labs(title = "Crime Count by Crime Type", x = NULL, y = "Number of Cases") +
  my_theme
save_chart(p1, "01_crime_type_distribution")

# 3.2 Year-wise trend
year_tbl <- crime %>% count(Year)
print(year_tbl)
p2 <- ggplot(year_tbl, aes(Year, n)) +
  geom_line(colour = "#C0392B", linewidth = 1.3) +
  geom_point(colour = "#C0392B", size = 3.5) +
  geom_text(aes(label = n), vjust = -1, size = 3.8) +
  scale_x_continuous(breaks = year_tbl$Year) +
  expand_limits(y = max(year_tbl$n) * 1.15) +
  labs(title = "Year-wise Crime Trend (2019 - 2025)", x = "Year", y = "Number of Cases") +
  my_theme
save_chart(p2, "02_yearly_trend")

# 3.3 Year-wise trend for each crime type
p3 <- crime %>% count(Year, Crime_Type) %>%
  ggplot(aes(Year, n, colour = Crime_Type)) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(title = "Crime Trend by Crime Type", x = "Year", y = "Cases", colour = "Crime Type") +
  my_theme
save_chart(p3, "03_trend_by_crime_type", 10, 5.5)

# 3.4 Monthly pattern
p4 <- crime %>% count(Month) %>%
  ggplot(aes(Month, n)) +
  geom_col(fill = "#2E86AB") +
  labs(title = "Month-wise Crime Pattern", x = "Month", y = "Number of Cases") +
  my_theme
save_chart(p4, "04_monthly_pattern")

# 3.5 State-wise analysis
state_tbl <- crime %>% count(State, sort = TRUE)
print(state_tbl)
p5 <- ggplot(state_tbl, aes(reorder(State, n), n)) +
  geom_col(fill = "#C0392B") + coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.8) +
  expand_limits(y = max(state_tbl$n) * 1.1) +
  labs(title = "State-wise Crime Count", x = NULL, y = "Number of Cases") +
  my_theme
save_chart(p5, "05_statewise_crime")

# 3.6 Top 10 cities
city_tbl <- crime %>% filter(City != "Unknown") %>% count(City, sort = TRUE) %>% head(10)
print(city_tbl)
p6 <- ggplot(city_tbl, aes(reorder(City, n), n)) +
  geom_col(fill = "#27AE60") + coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.8) +
  expand_limits(y = max(city_tbl$n) * 1.1) +
  labs(title = "Top 10 Cities by Crime Count", x = NULL, y = "Number of Cases") +
  my_theme
save_chart(p6, "06_top_cities")

# 3.7 State x Crime-type heat map
p7 <- crime %>% count(State, Crime_Type) %>%
  ggplot(aes(Crime_Type, State, fill = n)) +
  geom_tile(colour = "white") + geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "#FDEDEC", high = "#922B21") +
  labs(title = "State vs Crime Type Heat Map", x = NULL, y = NULL, fill = "Cases") +
  my_theme + theme(axis.text.x = element_text(angle = 40, hjust = 1))
save_chart(p7, "07_state_crime_heatmap", 10, 6)

# 3.8 Victim age distribution
cat("Victim age summary:\n"); print(summary(crime$Victim_Age))
p8 <- ggplot(crime, aes(Victim_Age)) +
  geom_histogram(bins = 25, fill = "#8E44AD", colour = "white") +
  labs(title = "Distribution of Victim Age", x = "Age (years)", y = "Count") + my_theme
save_chart(p8, "08_victim_age_histogram")

# 3.9 Victim age group and gender
p9 <- crime %>% count(AgeGroup, Victim_Gender) %>%
  ggplot(aes(AgeGroup, n, fill = Victim_Gender)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Female = "#C0392B", Male = "#1F3A5F")) +
  labs(title = "Victims by Age Group and Gender", x = "Age Group", y = "Victims", fill = "Gender") +
  my_theme
save_chart(p9, "09_age_group_gender")

# 3.10 Gender by crime type
gender_tbl <- crime %>% count(Crime_Type, Victim_Gender) %>%
  group_by(Crime_Type) %>% mutate(pct = n / sum(n) * 100)
female_order <- gender_tbl %>% filter(Victim_Gender == "Female") %>% arrange(pct)
gender_tbl$Crime_Type <- factor(gender_tbl$Crime_Type, levels = female_order$Crime_Type)
p10 <- ggplot(gender_tbl, aes(Crime_Type, pct, fill = Victim_Gender)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c(Female = "#C0392B", Male = "#1F3A5F")) +
  labs(title = "Victim Gender Share within each Crime Type", x = NULL, y = "Percentage", fill = "Gender") +
  my_theme
save_chart(p10, "10_gender_by_crime")

# 3.11 Victim age by crime type (box plot - outlier check)
p11 <- ggplot(crime, aes(reorder(Crime_Type, Victim_Age, median), Victim_Age)) +
  geom_boxplot(fill = "#AED6F1") + coord_flip() +
  labs(title = "Boxplot of Victim Age by Crime Type", x = NULL, y = "Victim Age") + my_theme
save_chart(p11, "11_age_boxplot")

# 3.12 Crime by hour of day
p12 <- crime %>% count(Hour) %>%
  ggplot(aes(Hour, n)) + geom_col(fill = "#F39C12") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "Crimes by Hour of the Day", x = "Hour (0-23)", y = "Number of Cases") + my_theme
save_chart(p12, "12_hourly_pattern")

# 3.13 Solved vs Unsolved
status_tbl <- crime %>% count(Case_Status) %>% mutate(pct = round(n / sum(n) * 100, 1))
print(status_tbl)
p13 <- ggplot(status_tbl, aes(Case_Status, n, fill = Case_Status)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), vjust = -0.5, size = 4.2) +
  scale_fill_manual(values = c(Solved = "#27AE60", Unsolved = "#C0392B")) +
  expand_limits(y = max(status_tbl$n) * 1.12) +
  labs(title = "Solved vs Unsolved Cases", x = NULL, y = "Number of Cases") +
  my_theme + theme(legend.position = "none")
save_chart(p13, "13_solved_vs_unsolved")

# 3.14 Solve rate by crime type
solve_type <- crime %>% group_by(Crime_Type) %>%
  summarise(Cases = n(), Solve_Rate = round(mean(Solved) * 100, 1)) %>%
  arrange(desc(Solve_Rate))
print(solve_type)
p14 <- ggplot(solve_type, aes(reorder(Crime_Type, Solve_Rate), Solve_Rate)) +
  geom_col(fill = "#16A085") + coord_flip() +
  geom_text(aes(label = paste0(Solve_Rate, "%")), hjust = -0.15, size = 3.8) +
  expand_limits(y = 100) +
  labs(title = "Case Solve Rate by Crime Type", x = NULL, y = "Solve Rate (%)") + my_theme
save_chart(p14, "14_solve_rate_by_crime")

# 3.15 Solve rate by state
solve_state <- crime %>% group_by(State) %>%
  summarise(Solve_Rate = round(mean(Solved) * 100, 1)) %>% arrange(desc(Solve_Rate))
print(solve_state)
p15 <- ggplot(solve_state, aes(reorder(State, Solve_Rate), Solve_Rate)) +
  geom_col(fill = "#2C3E50") + coord_flip() +
  geom_text(aes(label = paste0(Solve_Rate, "%")), hjust = -0.15, size = 3.8) +
  expand_limits(y = 100) +
  labs(title = "Case Solve Rate by State", x = NULL, y = "Solve Rate (%)") + my_theme
save_chart(p15, "15_solve_rate_by_state")

# 3.16 Weapon usage
p16 <- crime %>% count(Weapon_Used, sort = TRUE) %>%
  ggplot(aes(reorder(Weapon_Used, n), n)) + geom_col(fill = "#D35400") + coord_flip() +
  labs(title = "Weapon / Mode Used", x = NULL, y = "Number of Cases") + my_theme
save_chart(p16, "16_weapon_used")

# 3.17 Correlation matrix of numeric features
num_vars <- crime[, c("Year", "Hour", "Victim_Age", "Victims_Count", "Solved")]
cor_mat  <- cor(num_vars, use = "pairwise.complete.obs")
print(round(cor_mat, 3))
png("charts/17_correlation_matrix.png", width = 900, height = 700, res = 120)
corrplot(cor_mat, method = "color", addCoef.col = "black",
         tl.col = "black", tl.cex = 0.9, number.cex = 0.9,
         title = "Correlation Matrix", mar = c(0, 0, 2, 0))
dev.off()

# 3.18 Statistical test : is solve-rate related to crime type?
chi <- chisq.test(table(crime$Crime_Type, crime$Case_Status))
print(chi)

# =====================================================================
# STAGE 4 : DATA SPLITTING  (80 % train / 20 % test)
# =====================================================================
model_df <- crime %>%
  select(Case_Status, Crime_Type, State, Weapon_Used, Victim_Age,
         Victim_Gender, Victims_Count, Hour, Year)
model_df$Case_Status   <- factor(model_df$Case_Status, levels = c("Unsolved", "Solved"))
model_df$Crime_Type    <- factor(model_df$Crime_Type)
model_df$State         <- factor(model_df$State)
model_df$Weapon_Used   <- factor(model_df$Weapon_Used)
model_df$Victim_Gender <- factor(model_df$Victim_Gender)

trainIndex <- createDataPartition(model_df$Case_Status, p = 0.8, list = FALSE)
train <- model_df[trainIndex, ]
test  <- model_df[-trainIndex, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")

# helper : evaluation metrics from a confusion matrix
evaluate <- function(pred, actual, name) {
  cm <- confusionMatrix(pred, actual, positive = "Solved")
  cat("\n-----", name, "-----\n"); print(cm$table)
  data.frame(Model = name,
             Accuracy  = round(unname(cm$overall["Accuracy"]), 4),
             Precision = round(unname(cm$byClass["Precision"]), 4),
             Recall    = round(unname(cm$byClass["Recall"]), 4),
             F1        = round(unname(cm$byClass["F1"]), 4))
}

# =====================================================================
# STAGE 5 : MODEL TRAINING
# =====================================================================
# ---- 5.1 Logistic Regression (Generalized Linear Model) ----
glm_model <- glm(Case_Status ~ ., data = train, family = binomial)
print(summary(glm_model))
glm_prob  <- predict(glm_model, newdata = test, type = "response")
glm_pred  <- factor(ifelse(glm_prob > 0.5, "Solved", "Unsolved"),
                    levels = c("Unsolved", "Solved"))
res_glm <- evaluate(glm_pred, test$Case_Status, "Logistic Regression")

# ---- 5.2 Decision Tree ----
tree_model <- rpart(Case_Status ~ ., data = train, method = "class",
                    control = rpart.control(cp = 0.005, maxdepth = 5))
tree_pred  <- predict(tree_model, newdata = test, type = "class")
res_tree <- evaluate(tree_pred, test$Case_Status, "Decision Tree")
png("charts/18_decision_tree.png", width = 1100, height = 700, res = 120)
plot(tree_model, uniform = TRUE, margin = 0.1, main = "Decision Tree - Solved vs Unsolved")
text(tree_model, use.n = TRUE, cex = 0.7)
dev.off()

# ---- 5.3 Random Forest ----
rf_model <- ranger(Case_Status ~ ., data = train, num.trees = 300,
                   importance = "impurity")
rf_pred  <- predict(rf_model, data = test)$predictions
res_rf   <- evaluate(rf_pred, test$Case_Status, "Random Forest")

# ---- 5.4 Variable importance (Random Forest) ----
imp <- data.frame(Variable = names(rf_model$variable.importance),
                  Importance = as.numeric(rf_model$variable.importance)) %>%
  arrange(desc(Importance))
print(imp)
p19 <- ggplot(imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col(fill = "#1F3A5F") + coord_flip() +
  labs(title = "Random Forest - Variable Importance", x = NULL, y = "Importance") + my_theme
save_chart(p19, "19_variable_importance")

# =====================================================================
# STAGE 6 : MODEL COMPARISON
# =====================================================================
baseline <- round(max(prop.table(table(test$Case_Status))), 4)
cat("\nBaseline accuracy (always predict majority class):", baseline, "\n")
results <- rbind(res_glm, res_tree, res_rf)
print(results)
write.csv(results, "output/model_comparison.csv", row.names = FALSE)

p20 <- results %>% pivot_longer(-Model, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(Model, Value, fill = Metric)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = baseline, linetype = "dashed", colour = "#C0392B") +
  scale_fill_manual(values = c("#1F3A5F", "#2E86AB", "#F39C12", "#27AE60")) +
  labs(title = "Model Comparison (dashed line = baseline accuracy)", x = NULL, y = "Score") +
  my_theme
save_chart(p20, "20_model_comparison", 10, 5.5)

cat("\n=== PROJECT COMPLETED : charts saved in /charts, tables in /output ===\n")
