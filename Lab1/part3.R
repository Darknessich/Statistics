dfuncs = list(
    rnorm,
    rcauchy,
    {function (x) rt(x, 3, 0)},
    {function (x) rpois(x, 10)},
    {function (x) runif(x, -sqrt(3), sqrt(3))}
)

images = c(
    "Lab1/images/tukey/normal.png",
    "Lab1/images/tukey/cauchy.png",
    "Lab1/images/tukey/student.png",
    "Lab1/images/tukey/poisson.png",
    "Lab1/images/tukey/uniform.png"
)

names = c("Normal", "Cauchy", "Student", "Poisson", "Uniform")

for (i in 1:(length(dfuncs))) {
    png(images[i], width=1200, height=400)
    boxplot(dfuncs[[i]](20), dfuncs[[i]](100), col="white", horizontal=T, names=c(20, 100), main=names[i])
    dev.off()
}