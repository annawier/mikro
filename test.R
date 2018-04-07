x <- rnorm(10^3, 0, 1)

png("plot.png")
  plot(x, sin(x))
dev.off(0)
