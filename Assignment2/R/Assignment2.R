t <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
sales <- c(21.74, 26.41, 26.44, 26.40, 23.91, 27.06, 26.63, 26.98, 28.26, 27.87,
           28.40, 30.16, 32.61, 30.33, 30.18, 33.33)
model <- lm(sales ~ t)
resid(model)
plot(t, resid(model), col = "blue", main = "residuals versus time")


#plot(t, sales, col="blue", main="sales versus time")
#summary(fit)

library(car)
durbinWatsonTest(model)

yearly_income <- c(8, 10, 12, 14, 16, 18, 20)
clothing_expenditure <- c(6.47, 6.17, 7.40, 10.57, 11.93, 10.30, 14.67)
model <- lm(clothing_expenditure ~ yearly_income)
coef(model)

SSE <- sum(clothing_expenditure^2) - 
  (0.2967857 * sum(clothing_expenditure) + 0.6676786 * sum(clothing_expenditure * yearly_income))
s <- sqrt(SSE / (length(yearly_income) - 2))
s

resid(model)
rstandard(model)
plot(yearly_income, resid(model), col = "blue", main = "residuals versus x, the yearly income")
plot(predict(model), resid(model), col = "blue", main = "residuals versus x, the predicted clothing expenditure")


x1 <- c(1008, 1290, 860, 912, 1204, 1204, 1764, 1600, 1255, 3600, 864, 720, 1008,
        1950, 2086, 2011, 1465, 1232, 1736, 1296, 1996, 1874, 1580, 1920, 1430,
        1486, 1008, 1282, 1134, 2400, 1701, 1020, 1053, 1728, 416, 1040, 1496, 
        1936, 1904, 1080, 1768, 1503, 1736, 1695, 2186, 888, 1120, 1400, 2165,
        2353, 1536, 1972, 1120, 1664, 925, 1288, 1400, 1376, 2038, 1572, 1545,
        1993, 1130)
x2 <- c(5, 6, 8, 5, 6, 5, 8, 7, 5, 10, 5, 4, 6, 8, 7, 9, 6, 5, 7, 6, 7, 5, 5, 5,
        9, 6, 5, 5, 5, 9, 5, 6, 5, 6, 3, 5, 6, 8, 7, 5, 8, 6, 7, 6, 8, 5, 6, 5, 7,
        8, 6, 8, 5, 7, 5, 5, 5, 6, 12, 6, 6, 6, 5)
x3 <- c(2, 3, 2, 3, 3, 3, 4, 3, 3, 5, 3, 2, 3, 3, 3, 4, 3, 2, 3, 3, 3, 2, 3, 3, 3,
        3, 2, 3, 2, 4, 3, 3, 2, 3, 1, 2, 3, 4, 4, 2, 4, 3, 3, 3, 4, 2, 3, 3, 3, 4,
        3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3, 3, 2)
x4 <- c(35, 36, 36, 41, 40, 10, 64, 19, 16, 17, 37, 41, 35, 52, 12, 76, 102, 69,
        67, 11, 9, 14, 11, 14, 16, 27, 35, 20, 74, 15, 15, 16, 24, 26, 42, 9, 30,
        39, 32, 24, 74, 14, 16, 12, 12, 34, 29, 33, 2, 15, 36, 37, 27, 79, 20, 2,
        2, 103, 62, 29, 9, 4, 21)
y <- c(53.5, 49.0, 50.5, 49.9, 52.0, 55.0, 80.5, 86.0, 69.0, 149.0, 46.0, 38.0,
       49.5, 105.0, 152.5, 85.0, 60.0, 58.5, 101.0, 79.4, 125.0, 87.9, 80.0, 94.0,
       74.0, 69.0, 63.0, 67.5, 35.0, 142.5, 92.2, 56.0, 63.0, 60.0, 34.0, 52.0,
       75.0, 93.0, 60.0, 73.0, 71.0, 83.0, 90.0, 83.0, 115.0, 50.0, 55.2, 61.0,
       147.0, 210.0, 60.0, 100.0, 44.5, 55.0, 53.4, 65.0, 73.0, 40.0, 141.0, 68.0,
       139.0, 140.0, 55.0)
model <- lm(y ~ x1 + x2 + x3 + x4)
summary(model)
qt(0.975, 58)
qf(0.95, 4, 58)
y_bar <- sum(y) / length(y)
total_variaton <- sum(y^2) - length(y) * (y_bar^2)
R_squared <- summary(model)$r.squared
explained_variation <- total_variaton * R_squared
explained_variation
unexplained_variation <- total_variaton - explained_variation
unexplained_variation
sqrt(unexplained_variation / (length(y) - 5))
confint(model)

