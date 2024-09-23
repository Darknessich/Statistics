data = list(
    data.frame(
        N = rnorm(10),
        C = rcauchy(10),
        t = rt(10, 3, 0),
        P = rpois(10, 10),
        U = runif(10, -sqrt(3), sqrt(3))
        ),
    data.frame(
        N = rnorm(50),
        C = rcauchy(50),
        t = rt(50, 3, 0),
        P = rpois(50, 10),
        U = runif(50, -sqrt(3), sqrt(3))
        ),
    data.frame(
        N = rnorm(1000),
        C = rcauchy(1000),
        t = rt(1000, 3, 0),
        P = rpois(1000, 10),
        U = runif(1000, -sqrt(3), sqrt(3))
        )
)

images = c(
    "Lab1/images/hist/normal.png",
    "Lab1/images/hist/cauchy.png",
    "Lab1/images/hist/student.png",
    "Lab1/images/hist/poisson.png",
    "Lab1/images/hist/uniform.png"
)

dfuncs = list(
    dnorm,
    dcauchy,
    {function (x) dt(x, 3, 0)},
    {function (x) dpois(x, 10)},
    {function (x) dunif(x, -sqrt(3), sqrt(3))}
)

names = c("Normal", "Cauchy", "Student", "Poisson", "Uniform")
sizes = c(10, 50, 1000)

steps = c(0.01, 0.01, 0.01, 1, 0.01)

for (i in 1:5) {
    png(images[i], width=1200, height=400)
    par(mfrow = c(1, 3))
    for (j in 1:3) {
        breaks = (log(sizes[j]) + 1)**2
        arr <- data[[j]][ ,i]
        x <- seq(min(arr), max(arr), steps[i])
        hist(arr, col="white", breaks=breaks, freq=F, main=paste(names[i], " n = ", sizes[j]), xlab=names[i])
        lines(x, dfuncs[[i]](x))
    }
    dev.off()
}