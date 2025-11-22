
devtools::load_all()
devtools::document()
devtools::check()


plot1D(
  imputation.list = imp_nhanes3_newborn, var.name = "HSHSIZER",
  original.data =na_nhanes3_newborn,int.plot = "bar"
)

#How many overall downloads
library(cranlogs)
mls <- cran_downloads(packages="mixgb", from = "2022-06-07", to = Sys.Date()-1)
mls
sum(mls[,2])


#Cumulative
cumulative <- cumsum(mls[,2])
mls2 <- cbind(mls,cumulative)

library(ggplot2)
#Plot
gr1 <- ggplot(mls2, aes(date, cumulative)) +
  geom_line(colour = "blue",size=1)
gr1 + xlab("Time") + ylab("Nr. of downloads") +
  labs(title = paste0("mixgb cumulative downloads until ", Sys.Date()-1))

