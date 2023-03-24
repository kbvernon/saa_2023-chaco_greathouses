
library(here)
library(qrcode)

bob <- qr_code("https://github.com/kbvernon/saa_2023-chaco_greathouses")

png(here("figures", "github-qrcode.png"), bg = "transparent")

plot(bob, col = c(ghc(cinnamon2), "transparent"))

dev.off()
