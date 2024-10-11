# install.packages(c("mclust", "mclustAddons", "data.table", "ggplot2", "quantmod"))

library(mclust)
library(mclustAddons)
library(data.table)
library(ggplot2)
theme_set(theme_bw())

GOLD = quantmod::getSymbols("GC=F", src = "yahoo", auto.assign = FALSE)
GOLD = quantmod::dailyReturn(GOLD, type = "log")
plot(GOLD)

data = as.data.table(GOLD[,1])
data = setNames(data, c("date", "log.returns"))
data = data[year(date) == "2023",]
data

# single-Gaussian model
GMM1 = GMMlogreturn(data$log.returns, G = 1)
summary(GMM1)

# Gaussian mixture model
GMM = GMMlogreturn(data$log.returns)
summary(GMM)

ggplot(data = data.frame(G = 1:nrow(GMM$BIC), 
                         BIC = GMM$BIC[,"V"]),
       aes(x = G, y = BIC)) +
  geom_line() +
  geom_point(col = theme_get()$panel.background$fill, size = 5) +
  geom_point() +
  scale_x_continuous(name = "Number of GMM components", 
                     breaks = 1:nrow(GMM$BIC))

y0 = extendrange(GMM$data, f = 0.1)
y0 = seq(min(y0), max(y0), length = 1000)
ggplot(data = data.frame(y = GMM$data[,1]),
       aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "grey",
                 col = theme_get()$panel.background$fill,
                 bins = 21) +
  geom_rug() +
  geom_line(data = data.table(x = y0,
                              dens = predict(GMM1, what = "dens", newdata = y0)),
            aes(x, dens, color = "Gaussian"), lwd = 1) +
  geom_line(data = data.table(x = y0,
                              dens = predict(GMM, what = "dens", newdata = y0)),
            aes(x, dens, color = "GMM"), lwd = 1) +
  labs(x = "Gold price log-returns", y = "Density") +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_color_manual(name = NULL, values = c("red3", "steelblue"))

