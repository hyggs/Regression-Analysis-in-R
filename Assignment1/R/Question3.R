price_difference <- c(-0.05, 0.25, 0.60, 0.00, 0.25, 0.20, 0.15, 0.05, -0.15, 0.15,
                      0.20, 0.10, 0.40, 0.45, 0.35, 0.30, 0.50, 0.50, 0.40, -0.05,
                      -0.05, -0.10, 0.20, 0.10, 0.50, 0.60, -0.05, 0.00, 0.05, 0.65)
demand <- c(7.38, 8.51, 9.52, 7.50, 9.33, 8.28, 8.75, 7.87, 7.10, 8.00, 7.89, 8.15,
            9.10, 8.86, 8.90, 8.87, 9.26, 9.00, 8.70, 7.95, 7.65, 7.27, 8.00, 8.50,
            8.75, 9.21, 8.27, 7.67, 7.93, 9.26)
lm(demand ~ price_difference)

average_y <- sum(demand) / length(demand)

total_variaton <- sum((demand - average_y)^2)
total_variaton
explained_variation <- sum((7.821 + price_difference * 2.585 - average_y)^2)
explained_variation
SSE <- sum((demand - (7.821 + price_difference * 2.585))^2)
SSE
(cor(price_difference, demand, method = "pearson"))^2
r <- sqrt(explained_variation / total_variaton)
r
r^2

s_squared <- SSE / (length(demand) - 2)
s_squared
s <- sqrt(s_squared)
s

model = lm(demand ~ price_difference)
summary(model)

plot(price_difference, demand, col = "black", main = "demand(y) versus price difference(x)")
abline(lm(demand ~ price_difference))

anova(model)

confint(model)

new <- data.frame(price_difference = 0.6)
predict(model, new, se.fit = TRUE, interval = "confidence", level = 0.95)

