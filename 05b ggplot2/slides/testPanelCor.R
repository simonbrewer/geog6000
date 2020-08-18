data(iris)
require(lattice)
require(RColorBrewer)

pp = splom(~iris[,1:4], upper.panel = panel.splom,
      lower.panel = function(x, y, ...) {
        panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y) * 4 + 5)])
        cpl <- current.panel.limits()
        panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y),2), font=2)
      }, type = c("g", "p", "smooth"))

      
#      scales = list(x = list( draw = TRUE, cex=0.1)), type = c("g", "p", "smooth"),layout =           c(1, 1), pscales=0, pch=".",
#      main="correlation between the weather variables after removing district F.E and yearly trends")

print(pp)