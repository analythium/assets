library(qrcode)
png("analythium-QRcode.png")
qrcode_gen("https://analythium.io/")
dev.off()
