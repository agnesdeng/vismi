library(mixgb)
set.seed(2023)
withNA.df <- createNA(data = nhanes3_newborn, var.names = c("HSHSIZER", "HSAGEIR", "HSSEX", "DMARETHN", "HYD1"), p = 0.1)
colSums(is.na(withNA.df))

mixgb.data<- mixgb(data = withNA.df, m = 5)
range(mixgb.data[[1]]$HSAGEIR)


library(vismi)

setwd("/Users/agnes/Desktop/vismi")
devtools::load_all()
devtools::document()

p<-plot_bar(
  imputation.list = mixgb.data, var.name = "HSAGEIR",
  original.data = withNA.df
)
library(ggplot2)
p <- p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  strip.text = element_text(size = 23, face = "plain"),
  axis.title.x = element_text(size = 26, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
  axis.title.y = element_text(size = 26, margin = margin(0, r = 5, 0, l = 0)),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 21),
  panel.spacing.x = unit(0.4, "cm")
)


dir.path<-"/Users/agnes/Desktop/PhDthesis/Latex/figures/vismi"

jpeg(
  filename = file.path(dir.path, "/hsageir12.jpeg"),
  width = 16, height = 4, units = "in", res = 300, pointsize = 12
)
print(p)
# Turn off the device
dev.off()
