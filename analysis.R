library(ggplot2)
library(patchwork)
library(tidyr)
library(ggdist)
library(GGally)
library(pheatmap)
library(pROC)

# Data Preparation

stroop_data <- data.frame(
  seen = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  W = c(126,118,61,69,57,78,114,81,73,93,116,156,90,120,99,113,103,123,86,99,
        102,120,128,100,95,80,98,111,101,102,100,112,82,72,72,89,108,88,116,
        100,99,93,100,110,100,106,115,120,97),
  C = c(86,76,66,48,59,64,61,85,57,50,92,70,66,73,68,110,78,61,65,77,77,74,
        100,89,61,55,92,90,85,78,66,78,84,63,65,71,46,70,83,69,70,63,93,76,
        83,71,112,87,82),
  CW = c(64,54,44,32,42,53,41,47,33,45,49,45,48,49,44,47,52,28,42,51,54,53,
         56,56,37,36,51,52,45,51,48,55,37,46,47,49,29,49,67,39,43,36,62,56,
         36,49,66,54,41)
)

head(stroop_data)
str(stroop_data)

summary(stroop_data$seen)
summary(stroop_data$W)
summary(stroop_data$C)
summary(stroop_data$CW)

sum(is.na(stroop_data))

stroop_data$seen <- as.factor(stroop_data$seen)

str(stroop_data)

# Normality tests
shapiro.test(stroop_data$W)
shapiro.test(stroop_data$C)
shapiro.test(stroop_data$CW)

# Boxplot of all three Stroop scores
boxplot(stroop_data$W, stroop_data$C, stroop_data$CW,
        names = c("W", "C", "CW"),
        col = c("skyblue", "pink", "lightgreen"))

par(mfrow = c(1, 3))
barplot(stroop_data$W, main = "W", col = "skyblue")
barplot(stroop_data$C, main = "C", col = "pink")
barplot(stroop_data$CW, main = "CW", col = "lightgreen")

p1 <- ggplot(stroop_data, aes(x = W)) +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("W") +
  theme_minimal()

p2 <- ggplot(stroop_data, aes(x = C)) +
  geom_bar(fill = "pink", color = "black") +
  ggtitle("C") +
  theme_minimal()

p3 <- ggplot(stroop_data, aes(x = CW)) +
  geom_bar(fill = "lightgreen", color = "black") +
  ggtitle("CW") +
  theme_minimal()

p1 + p2 + p3

stroop_data$ID <- 1:nrow(stroop_data)

stroop_long <- pivot_longer(stroop_data, cols = c(W, C, CW),
                            names_to = "Grupa", values_to = "Wynik")
stroop_long$Grupa <- factor(stroop_long$Grupa, levels = c("W", "C", "CW"))

ggplot(stroop_long, aes(x = Grupa, y = Wynik, group = ID)) +
  geom_line(alpha = 0.3, color = "gray") +
  geom_point(aes(color = Grupa), size = 3) +
  theme_minimal() +
  labs(title = "individual patterns across groups")

ggplot(stroop_long, aes(x = Grupa, y = Wynik, fill = Grupa)) +
  stat_halfeye(adjust = .5, width = .6, .width = 0,
               justification = -.3, point_colour = NA) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.5) +
  stat_dots(side = "left", justification = 1.1, binwidth = .1) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggpairs(stroop_data[, c("W", "C", "CW")],
        title = "correlations between groups")

macierz_danych <- as.matrix(stroop_data[, c("W", "C", "CW")])
pheatmap(macierz_danych,
         main = "patterns between W, C, CW",
         color = colorRampPalette(c("blue", "white", "red"))(50),
         scale = "column",
         clustering_distance_rows = "euclidean")

(p1 <- ggplot(stroop_data, aes(x = W, y = C, color = as.factor(seen))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"),
                       labels = c("No", "Yes")) +
    labs(title = "C vs W", x = "W", y = "C", color = "seen") +
    theme_minimal() +
    theme(legend.position = "top", legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5, face = "bold")))

(p2 <- ggplot(stroop_data, aes(x = W, y = CW, color = as.factor(seen))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"),
                       labels = c("No", "Yes")) +
    labs(title = "CW vs W", x = "W", y = "CW", color = "seen") +
    theme_minimal() +
    theme(legend.position = "top", legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5, face = "bold")))

(p3 <- ggplot(stroop_data, aes(x = C, y = CW, color = as.factor(seen))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"),
                       labels = c("No", "Yes")) +
    labs(title = "CW vs C", x = "C", y = "CW", color = "seen") +
    theme_minimal() +
    theme(legend.position = "top", legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5, face = "bold")))

p1 + p2 + p3

stroop_data$seen <- factor(stroop_data$seen, levels = c(0, 1),
                           labels = c("No", "Yes"))

b1 <- ggplot(stroop_data, aes(x = seen, y = W, fill = seen)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4")) +
  labs(title = "W by Gorilla Seen", x = "seen", y = "value") +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold"))

b2 <- ggplot(stroop_data, aes(x = seen, y = C, fill = seen)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4")) +
  labs(title = "C by Gorilla Seen", x = "seen", y = "value") +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold"))

b3 <- ggplot(stroop_data, aes(x = seen, y = CW, fill = seen)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4")) +
  labs(title = "CW by Gorilla Seen", x = "seen", y = "value") +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold"))

b1 + b2 + b3

# Exploratory Analysis

stroop_data$seen <- ifelse(stroop_data$seen == "Yes", 1, 0)

str(stroop_data)
summary(stroop_data)

stroop_data$seen_f <- factor(stroop_data$seen,
                             levels = c(0, 1),
                             labels = c("not seen", "seen"))

# Normality tests per group (seen vs not seen)
# The t-test assumes normality within each group separately
by(stroop_data$W, stroop_data$seen_f, shapiro.test)
by(stroop_data$C, stroop_data$seen_f, shapiro.test)
by(stroop_data$CW, stroop_data$seen_f, shapiro.test)

table(stroop_data$seen_f)
prop.table(table(stroop_data$seen_f))
aggregate(cbind(W, C, CW) ~ seen_f, data = stroop_data, mean)
aggregate(cbind(W, C, CW) ~ seen_f, data = stroop_data, sd)

par(mfrow = c(1, 3))

boxplot(W ~ seen_f, data = stroop_data,
        main = "W by Gorilla Seen",
        xlab = "Gorilla Seen",
        ylab = "W score")

boxplot(C ~ seen_f, data = stroop_data,
        main = "C by Gorilla Seen",
        xlab = "Gorilla Seen",
        ylab = "C score")

boxplot(CW ~ seen_f, data = stroop_data,
        main = "CW by Gorilla Seen",
        xlab = "Gorilla Seen",
        ylab = "CW score")

par(mfrow = c(1, 1))

# Welch Two-Sample t-tests (exploratory analysis)
# Welch t-test does not assume equal variances between groups,
# making it appropriate here without requiring homogeneity of variance testing.

# W score: seen vs not seen
t_W <- t.test(W ~ seen_f, data = stroop_data, var.equal = FALSE)
t_W

# C score: seen vs not seen
t_C <- t.test(C ~ seen_f, data = stroop_data, var.equal = FALSE)
t_C

# CW score: seen vs not seen
t_CW <- t.test(CW ~ seen_f, data = stroop_data, var.equal = FALSE)
t_CW

# Summary of Welch t-test results
cat("Welch t-test results:\n")
cat(sprintf("  W:  t = %.3f, df = %.2f, p-value = %.4f\n",
            t_W$statistic, t_W$parameter, t_W$p.value))
cat(sprintf("  C:  t = %.3f, df = %.2f, p-value = %.4f\n",
            t_C$statistic, t_C$parameter, t_C$p.value))
cat(sprintf("  CW: t = %.3f, df = %.2f, p-value = %.4f\n",
            t_CW$statistic, t_CW$parameter, t_CW$p.value))

# Performing Logistic Regression

model <- glm(seen ~ W + C + CW, data = stroop_data, family = binomial)
summary(model)

#
AIC(model)
stroop_data$prob <- predict(model, type = "response")
roc_obj <- roc(stroop_data$seen, stroop_data$prob)
auc(roc_obj)
plot(roc_obj,
     col = "violet", 
     lwd = 4, 
     print.auc = TRUE)

# other models and AIC comparison
model_W <- glm(seen ~ W, data = stroop_data, family = binomial)
model_C <- glm(seen ~ C, data = stroop_data, family = binomial)
model_CW <- glm(seen ~ CW, data = stroop_data, family = binomial)
model_CCW <- glm(seen ~ C + CW, data = stroop_data, family = binomial)
model_WCW <- glm(seen ~ W + CW, data = stroop_data, family = binomial)
model_WC <- glm(seen ~ W + C, data = stroop_data, family = binomial)
model_null <- glm(seen ~ 1, data = stroop_data, family = binomial)

model_interaction <- glm(seen ~ W + C * CW, data = stroop_data, family = binomial)
summary(model_interaction)

(model_comparison <- data.frame(
  Model = c("Null", "W", "C", "CW", "C + CW", "W + CW", "W + C", "W + C + CW", "W + C * CW"),
  AIC = c(AIC(model_null), AIC(model_W), AIC(model_C), AIC(model_CW), AIC(model_CCW),
          AIC(model_WCW), AIC(model_WC), AIC(model), AIC(model_interaction))
))

# AUC comparison
roc_W <- roc(stroop_data$seen, predict(model_W, type = "response"))
roc_C <- roc(stroop_data$seen, predict(model_C, type = "response"))
roc_CW <- roc(stroop_data$seen, predict(model_CW, type = "response"))
roc_CCW <- roc(stroop_data$seen, predict(model_CCW, type = "response"))
roc_WCW <- roc(stroop_data$seen, predict(model_WCW, type = "response"))
roc_WC <- roc(stroop_data$seen, predict(model_WC, type = "response"))
roc_null <- roc(stroop_data$seen, predict(model_null, type = "response"))
roc_interaction <- roc(stroop_data$seen, predict(model_interaction, type = "response"))

auc_values <- c(
  "Null"         = as.numeric(auc(roc_null)),
  "W"            = as.numeric(auc(roc_W)),
  "C"            = as.numeric(auc(roc_C)),
  "CW"           = as.numeric(auc(roc_CW)),
  "C + CW"     = as.numeric(auc(roc_CCW)),
  "W + CW"     = as.numeric(auc(roc_WCW)),
  "W + C"      = as.numeric(auc(roc_WC)),
  "W + C + CW" = as.numeric(auc(roc(stroop_data$seen, predict(model, type = "response")))),
  "W + C * CW" = as.numeric(auc(roc_interaction))
)

(auc_comparison <- data.frame(
  Model = names(auc_values), 
  AUC = as.numeric(auc_values)
))


# Forest plot 
coef_data <- data.frame(
  predictor = c("W (Word)", "C (Color)", "CW (Color-Word)"),
  estimate  = coef(model)[c("W", "C", "CW")],
  lower     = confint(model)[c("W", "C", "CW"), 1],
  upper     = confint(model)[c("W", "C", "CW"), 2]
)

ggplot(coef_data, aes(x = estimate, y = predictor)) +
  geom_point(size = 3, color = "#2E5F8A") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2,
                 color = "#2E5F8A") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Forest Plot of Logistic Regression Coefficients",
       x = "Coefficient Estimate (log-odds)",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

