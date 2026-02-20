# Install packages if not already installed
required_packages <- c(
  "startupmsg",
  "sfsmisc",
  "distr",
  "RcmdrPlugin.TeachStat",
  "abind",
  "e1071"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    print(pkg)
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

G <- AbscontDistribution(
  d = function(y) {
    sapply(y, function(x) {
      d <- if (x < 0) {
        0
      } else if (x <= 1) {
        10 * (1 - x)^9
      } else {
        0
      }
      return(d)
    })
  },
  low1 = 0,
  up1 = 1,
  withStand = TRUE
)

distr::q(G)(c(0.01, 0.5, 0.99), lower.tail = TRUE)

local({
  .D <- G
  .fr <- c(-Inf)
  .to <- c(0.3689416526)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
  plotRegions(
    .D,
    regions = list(c(-Inf, 0.3689416526)),
    col = c("#BEBEBE"),
    legend = FALSE,
    to.draw.arg = 1,
    mfColRow = FALSE,
    xlab = "x",
    ylab = "Densidad",
    main = paste("Generic Distribution:  Distribution=G")
  )
  legend(
    "topright",
    legend = c(paste("P(", .fr, " < X <= ", .to, ")=", round(.p, 4), sep = "")),
    col = c("#BEBEBE"),
    pch = 15,
    pt.cex = 2.5,
    inset = 0.02
  )
})
local({
  .D <- G
  .fr <- c(-Inf)
  .to <- c(1)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
  plotRegions(
    .D,
    regions = list(c(-Inf, 1)),
    col = c("#BEBEBE"),
    legend = FALSE,
    to.draw.arg = 1,
    mfColRow = FALSE,
    xlab = "x",
    ylab = "Densidad",
    main = paste("Generic Distribution:  Distribution=G")
  )
  legend(
    "topright",
    legend = c(paste("P(", .fr, " < X <= ", .to, ")=", round(.p, 4), sep = "")),
    col = c("#BEBEBE"),
    pch = 15,
    pt.cex = 2.5,
    inset = 0.02
  )
})
local({
  .D <- G
  opar <- par(mfrow = c(1, 2))
  plotRegions(
    .D,
    to.draw.arg = 2,
    pch.u = "",
    cex.points = par("cex"),
    verticals = FALSE,
    mfColRow = FALSE,
    xlab = "x",
    ylab = "Probabilidad Acumulada",
    main = paste("Generic Distribution:  Distribution=G")
  )
  plotRegions(
    .D,
    to.draw.arg = 1,
    mfColRow = FALSE,
    xlab = "x",
    ylab = "Densidad",
    main = paste("Generic Distribution:  Distribution=G")
  )
  par(opar)
})
characRV(
  G,
  charact = c("expectation", "median", "sd", "moment", "cmoment"),
  moment = 10,
  cmoment = 5
)
GenericSamples <- as.data.frame(matrix(r(G)(100 * 1), ncol = 1))
rownames(GenericSamples) <- paste("sample", 1:100, sep = "")
colnames(GenericSamples) <- "obs"

numSummary(
  GenericSamples[, "obs"],
  statistics = c("mean", "sd", "quantiles"),
  quantiles = c(0, 0.25, 0.5, 0.75, 0.99, 1)
)
