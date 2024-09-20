# install.packages(c("mclust", "mclustAddons", "data.table", "ggplot2", "quantmod", "patchwork"))
 
library(mclust)
library(mclustAddons)
library(data.table)
library(ggplot2)
library(patchwork)

MIB = quantmod::getSymbols("FTSEMIB.MI", src = "yahoo", auto.assign = FALSE)
MIB = quantmod::dailyReturn(MIB, type = "log")
plot(MIB)

data = as.data.table(MIB[,1])
data = setNames(data, c("date", "log.returns"))
data = data[date >= "2016-01-01",]
data[, year := year(date)]

years = sort(unique(data$year))
GMM = vector("list", length = length(years))
for(t in seq(years))
{
  y = data[year == years[t], log.returns]
  GMM[[t]]  = GMMlogreturn(y)
}

tab = data.table(Year = years,
                 Nobs = as.integer(NA),
                 G = as.integer(NA),
                 Mean = as.double(NA),
                 StDev = as.double(NA),
                 Skewness = as.double(NA),
                 Kurtosis = as.double(NA),
                 VaR = as.double(NA),
                 ES = as.double(NA),
                 Entropy = as.double(NA), 
                 EntGauss = as.double(NA))
plots = vector("list", length = length(years))

for(t in seq(years))
{
  print(summary(GMM[[t]]))
  DT = data[year == years[t]]
  ylim = max(abs(range(data$log.returns)))*c(-1,1) # common y-axis range
  date_breaks = seq.Date(from = min(DT$date), to = max(DT$date), by = "1 month")
  #
  plot1 = ggplot(data = DT, aes(x = date, y = log.returns)) +
    geom_segment(aes(yend = 0)) +
    scale_x_date(name = NULL, expand = c(0.01,0),
                 date_labels = "%b", breaks = date_breaks) +
    scale_y_continuous(name = "log-returns", limits = ylim) +
    labs(subtitle = years[t])
  y0 = layer_scales(plot1)$y$get_limits()
  y0 = seq(min(y0), max(y0), length = 200)
  plot2 = ggplot(data = DT, aes(x = log.returns)) +
    geom_histogram(aes(y = after_stat(density)),
                   fill = "grey",
                   col = theme_get()$panel.background$fill,
                   bins = nclass.numpy(DT$log.returns)) +
    geom_rug() +
    geom_line(data = data.table(x = y0,
                                dens = predict(GMM[[t]], what = "dens",
                                               newdata = y0)),
              aes(x, dens)) +
    labs(x = NULL, y = NULL) +
    lims(x = ylim) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    coord_flip()
  plot = plot1 + plot2 +
    plot_layout(widths = c(0.8, 0.2))
  #
  tab$Nobs[t] = GMM[[t]]$n
  tab$G[t] = GMM[[t]]$G
  tab$Mean[t] = GMM[[t]]$marginalParameters$mean
  tab$StDev[t] = GMM[[t]]$marginalParameters$sd
  tab$Skewness[t] = GMM[[t]]$marginalParameters$skewness
  tab$Kurtosis[t] = GMM[[t]]$marginalParameters$kurtosis
  tab$VaR[t] = GMM[[t]]$marginalParameters$VaR
  tab$ES[t] = GMM[[t]]$marginalParameters$ES
  tab$Entropy[t] = GMM[[t]]$Entropy
  tab$EntGauss[t] = EntropyGauss(Var(GMM[[t]]$data))
  plots[[t]] = plot
}
print(tab, digits = 4)

wrap_plots(plots, ncol = 3, nrow = 3)

dtab = melt(tab[, .(Year, Entropy, EntGauss)], 
            id.vars = "Year",
            variable.name = "Method", 
            value.name = "Entropy")
levels(dtab$Method) = c("GMM", "Gaussian")

ggplot(data = dtab,
       aes(x = Year, y = Entropy, color = Method, shape = Method)) +
  geom_point() +
  scale_color_manual(name = NULL, values = c("steelblue", "red3")) +
  scale_shape_manual(name = NULL, values = c(19, 0)) +
  scale_x_continuous(name = NULL, breaks = years) +
  scale_y_continuous(sec.axis = sec_axis(function(ent) 1/sqrt(2*pi) * exp(ent - 1/2),
                                         name = "Std deviation"))

