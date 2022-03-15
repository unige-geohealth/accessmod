
library(lattice)

result <- data.frame(slope = -20:20)

result$speed <- sapply(result$slope, function(x) {
  s <- system(paste("node bicycleSpeed.js 30", x), intern = T)
  as.numeric(s)
})

png(file = "bicycleSpeedBySlope.png", bg = "transparent", width = 800, height = 400)
xyplot(speed ~ slope, result,
  main = "Bicycle speed by slope ( flat=30km/h )", xlab = "Slope [%]", ylab = "Speed [km/h]",
  panel = function(x, y) {
    panel.xyplot(x, y, type = "l")
    panel.abline(30)
  }
)
dev.off()
