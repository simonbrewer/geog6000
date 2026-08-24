heads <- 0

for (i in 1:100) {
    x <- runif(1)
    if (x >= 0.5) {
        heads <- heads + 1
    }
}
tails <- 100 - heads
print(paste("Heads:", heads))
print(paste("Tails:", tails))