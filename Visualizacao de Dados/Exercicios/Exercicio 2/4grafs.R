

# Base de dados -----------------------------------------------------------
base <- read.csv("Table11_1 - Sheet1.csv")

par(mar = rep(2, 4), mfrow = c(2,2))

# Boxplot -----------------------------------------------------------------

# Hidrocarbono
# boxplot(base$hidrocarbono, horizontal = T, main = "Boxplot Hidrocarbono", xlab = "Valor", col = "lightblue")
# Pureza
boxplot(base$pureza, horizontal = T, main = "Boxplot Pureza", xlab = "Valor", col = "lightgreen")


# Dispersão ---------------------------------------------------------------

plot(base, main = "Dispersão entre Hidrocarbono e Pureza")
abline(lm(base$pureza~base$hidrocarbono), col = 2)
text(1, 92, "reta de regressão")


# Histograma --------------------------------------------------------------

# Hidrocarbono
# shapiro.test(base$hidrocarbono)
hist(base$hidrocarbono, freq = F, density = 60, col = "lightblue", 
     main = "Histograma de Hidrocarbono com a curva da normal", xlab = "Hidrocarbono")
curve(dnorm(x, mean = mean(base$hidrocarbono), sd = sd(base$hidrocarbono)), 
      add = T, lty = 2, lwd = 2)
text(1.5, 2, paste("Shapiro Test \n p-value =", round(shapiro.test(base$hidrocarbono)$p.value, 4)))

# Pureza
# shapiro.test(base$pureza)
hist(base$pureza, freq = F, density = 60, col = "lightgreen",
     main = "Histograma de Pureza com a curva da normal", xlab = "Pureza")
curve(dnorm(x, mean = mean(base$pureza), sd = sd(base$pureza)), 
      add = T, lty = 2, lwd = 2)
text(98, 0.1, paste("Shapiro Test \n p-value =", round(shapiro.test(base$pureza)$p.value, 4)))


