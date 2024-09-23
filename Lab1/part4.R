library(e1071) # kurtosis

normalMeanRange <- function(x, a) {
  nx = length(x)
  xmean = mean(x)
  xsd = sd(x)
  delta = (xsd * qt(1 - 0.5 * a, nx - 1, 0)) / sqrt(nx - 1)
  return(
    list(
      min=(xmean - delta),
      max=(xmean + delta)
    )
  )
}

normalSdRange <- function(x, a) {
  nx = length(x)
  xnsd = sd(x) * sqrt(nx)
  return(
    list(
      min=(xnsd / sqrt(qchisq(1 - 0.5 * a, nx - 1, 0))),
      max=(xnsd / sqrt(qchisq(0.5 * a, nx - 1, 0)))
    )
  )
}

arbitraryMeanRange <- function(x, a) {
  nx = length(x)
  xmean = mean(x)
  xsd = sd(x)
  delta = (xsd * qnorm(1 - 0.5 * a)) / sqrt(nx)
  return(
    list(
      min=(xmean - delta),
      max=(xmean + delta)
    )
  )
}

arbitrarySdRange <- function(x, a) {
  nx = length(x)
  xsd = sd(x)
  ex = kurtosis(x)
  delta = 0.5*qnorm(1 - 0.5 * a) * sqrt((ex + 2) / nx)
  return(
    list(
      min=(xsd * (1 - delta)),
      max=(xsd * (1 + delta))
    )
  )
}

funcs = list(
    list(dist=rnorm, mrange=normalMeanRange, srange=normalSdRange),
    list(dist=rcauchy, mrange=arbitraryMeanRange, srange=arbitrarySdRange),
    list(dist={function (x) rt(x, 3, 0)}, mrange=arbitraryMeanRange, srange=arbitrarySdRange),
    list(dist={function (x) rpois(x, 10)}, mrange=arbitraryMeanRange, srange=arbitrarySdRange),
    list(dist={function (x) runif(x, -sqrt(3), sqrt(3))}, mrange=arbitraryMeanRange, srange=arbitrarySdRange)
)

images = c(
    "Lab1/images/ranges/normal.png",
    "Lab1/images/ranges/cauchy.png",
    "Lab1/images/ranges/student.png",
    "Lab1/images/ranges/poisson.png",
    "Lab1/images/ranges/uniform.png"
)

names = c(
  "Normal",
  "Cauchy",
  "Student",
  "Poisson",
  "Uniform"
)

df = data.frame(name=character(), size=numeric(0), mmin=numeric(0), mmax=numeric(0), smin=numeric(0), smax=numeric(0))

a = 0.05
h = 0.5
for (i in 1:length(funcs)) {
    sample20 = funcs[[i]]$dist(20)
    sample100 = funcs[[i]]$dist(100)
    m20 = funcs[[i]]$mrange(sample20, a)
    m100 = funcs[[i]]$mrange(sample100, a)
    s20 = funcs[[i]]$srange(sample20, a)
    s100 = funcs[[i]]$srange(sample100, a)

    mxmin = min(c(m20$min, m100$min))
    mxmax = max(c(m20$max, m100$max))
    delta = mxmax - mxmin
    mxmin = mxmin - delta * 0.1
    mxmax = mxmax + delta * 0.1

    sxmin = min(c(s20$min, s100$min))
    sxmax = max(c(s20$max, s100$max))
    delta = sxmax - sxmin
    sxmin = sxmin - delta * 0.1
    sxmax = sxmax + delta * 0.1

    png(images[i], width=1600, height=400)
    par(mfrow = c(1, 4))

    hist(sample20, col="wheat1", freq=F, border="wheat1", ylim=c(0, h), breaks=10, main=paste(names[i], " n = ", 20), xlab=names[i])
    lines(c(m20$min, m20$max), c(h, h), type="h", col="mediumpurple", lwd = 3)
    lines(c(m20$min - s20$max, m20$max + s20$max), c(h, h), type="h", col="royalblue", lwd = 3)
    legend(min(sample20), h, c(paste(names[i], " hist n = ", 20), "min mu, max mu", "min mu - max s, max mu + max s"), fill=c("wheat1", "mediumpurple", "royalblue"))

    hist(sample100, col="wheat1", freq=F, border="wheat1", ylim=c(0, h), breaks=25, main=paste(names[i], " n = ", 100), xlab=names[i])
    lines(c(m100$min, m100$max), c(h, h), type="h", col="mediumpurple", lwd = 3)
    lines(c(m100$min - s100$max, m100$max + s100$max), c(h, h), type="h", col="royalblue", lwd = 3)
    legend(min(sample100), h, c(paste(names[i], " hist n = ", 100), "min mu, max mu", "min mu - max s, max mu + max s"), fill=c("wheat1", "mediumpurple", "royalblue"))

    plot(c(m20$min, m20$max), c(20, 20), type="b", col="mediumpurple", lwd = 3, ylim=c(0, 120), xlim=c(mxmin, mxmax), xlab=names[i], ylab="size", main="Mean interval")
    lines(c(m100$min, m100$max), c(100, 100), type="b", col="royalblue", lwd = 3)
    legend(mxmin, 120, c(paste("mean interval n = ", 20), paste("mean interval n = ", 100)), fill=c("mediumpurple", "royalblue"))

    plot(c(s20$min, s20$max), c(20, 20), type="b", col="mediumpurple", lwd = 3, ylim=c(0, 120), xlim=c(sxmin, sxmax), xlab=names[i], ylab="size", main="Sigma interval")
    lines(c(s100$min, s100$max), c(100, 100), type="b", col="royalblue", lwd = 3)
    legend(sxmin, 120, c(paste("sigma interval n = ", 20), paste("sigma interval n = ", 100)), fill=c("mediumpurple", "royalblue"))

    dev.off()

    df[nrow(df) + 1, ] <- list(names[i], 20, m20$min, m20$max, s20$min, s20$max)
    df[nrow(df) + 1, ] <- list(names[i], 100, m100$min, m100$max, s100$min, s100$max)
}

# View(df)