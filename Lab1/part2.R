df <- data.frame(
    name=character(),
    size=numeric(0),
    mean=numeric(0),
    median=numeric(0),
    zR=numeric(0),
    zQ=numeric(0),
    ztr=numeric(0)
)

funcs = list(
    rnorm,
    rcauchy,
    {function (n) rt(n, 3, 0)},
    {function (n) rpois(n, 10)},
    {function (n) runif(n, -sqrt(3), sqrt(3))}
)

names = c("normal", "cauchy", "student","poisson", "uniform")
sizes = c(10, 100, 1000)

for (i in 1:(length(names))) {
    for (size in sizes) {
        for (j in 1:1000) {
            sample = funcs[[i]](size)
            means = mean(sample)
            meds = median(sample)
            zR = 0.5 * (min(sample) + max(sample))
            zQ = 0.5 * sum(quantile(sample, probs = c(0.25, 0.75)))
            r = round(size / 4)
            ztr = (1/(size - 2 * r)) * sum(sample[(r+1):(size-r)])
            df[nrow(df) + 1, ] <- list(names[i], size, means, meds, zR, zQ, ztr)
        }
    }
}

table <- aggregate(. ~ name + size, df, {function (x) c(mean(x),sd(x))})
View(table)