#========Atmospheric Lapse Rate===
Elevation <- c(180,305,381,488,549,640,762,883)
Elevation <- Elevation /1000
Temperature <- c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)

plot(Elevation, Temperature, xlab = 'Elevation(km)', ylab = 'Temperature(degrees C)')
fit <- lm(Temperature~Elevation)
summary(fit)
abline(fit, lwd = 2, col = "blue")