mplot <- function(x,title){
plot(x,
     xlab = "Year", ylab = "Voulme", ylim = c(0, 160000000), 
     bty = "l", xlim = c(2012, 2024.5), xaxt = 'n', main = title, lwd = 2) 
axis(1, at = seq(2012, 2024.5, 1), labels = format(seq(2012, 2024.5, 1)))
lines(data.ts, col = "green", lty = 1, lwd = 3)
lines(c(2021, 2021), c(0, 1500000000))
lines(c(2023, 2023), c(0, 1500000000))
text(2015, 140000000, "Training")
text(2022, 140000000, "Validation")
text(2024, 140000000, "Future")
arrows(2012, 130000000, 2020.9, 130000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021.1, 130000000, 2022.9, 130000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1,130000000, 2024.5,130000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
}