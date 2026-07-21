# ================================
# - FINAL R SCRIPT
# ================================

# --- Data ---
year <- c("2014-15","2015-16","2016-17","2017-18","2018-19",
          "2019-20","2020-21","2021-22","2022-23","2023-24","2024-25")

cpi <- c(6.2,5.1,4.3,3.8,3.9,4.6,6.4,5.9,6.8,5.6,5.2)
wpi <- c(1.8,-2.9,2.1,3.4,4.8,2.2,1.6,11.5,8.7,0.9,2.4)

df <- data.frame(Year = year, CPI = cpi, WPI = wpi)

# --- View Data ---
print(df)

# ================================
# 1. DESCRIPTIVE STATISTICS
# ================================
cat("========== Descriptive Statistics ==========\n")

cat("\nCPI:\n")
print(summary(df$CPI))
cat("Standard Deviation:", sd(df$CPI), "\n")

cat("\nWPI:\n")
print(summary(df$WPI))
cat("Standard Deviation:", sd(df$WPI), "\n")

# ================================
# 2. CORRELATION
# ================================
cat("\n========== Correlation ==========\n")
correlation <- cor(df$CPI, df$WPI)
cat("Correlation (CPI vs WPI):", correlation, "\n")

# ================================
# 3. REGRESSION MODEL
# ================================
cat("\n========== Regression ==========\n")
model <- lm(WPI ~ CPI, data = df)

summary(model)

# Regression Equation
coef_vals <- coef(model)
cat("\nRegression Equation:\n")
cat("WPI =", round(coef_vals[1],3), "+", round(coef_vals[2],3), "* CPI\n")

# ================================
# 4. SCATTER PLOT + REGRESSION
# ================================
plot(df$CPI, df$WPI,
     main = "WPI vs CPI (India)",
     xlab = "CPI Inflation (%)",
     ylab = "WPI Inflation (%)",
     pch = 19)

abline(model, lwd = 2)

text(df$CPI, df$WPI, labels = df$Year, pos = 3, cex = 0.7)

# ================================
# 5. DIAGNOSTIC PLOTS
# ================================
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

# ================================
# 6. FITTED VALUES & RESIDUALS
# ================================
df$Fitted <- round(fitted(model), 3)
df$Residuals <- round(residuals(model), 3)

cat("\n========== Fitted Values & Residuals ==========\n")
print(df)

# ================================
# END OF SCRIPT
# ================================
cat("\nAnalysis Complete\n")