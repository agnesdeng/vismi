library(devtools)
devtools::document()
devtools::load_all()
devtools::check()
devtools::build()

data(iris)


set.seed(2022)
withNA.df=createNA(iris,p=0.1)



n=nrow(withNA.df)
idx=sample(1:n, size = round(0.7*n), replace=FALSE)

trainNA.df=withNA.df[idx,]
testNA.df=withNA.df[-idx,]



##########overimpute using mixgb
iris.obj<-overimpute_mixgb(train.data=trainNA.df,test.data=testNA.df,p=0.5,m=10)

iris.obj$params$addNA.m


trainNA.df$Sepal.Length

obj=iris.obj
train.data=trainNA.df
test.data=testNA.df
var.name="Sepal.Length"
var.name="Species"

plot1D.entries(obj=iris.obj,var.name="Sepal.Length", train.data=trainNA.df,test.data=testNA.df)
plot1D.entries(obj=iris.obj,var.name="Species", train.data=trainNA.df,test.data=testNA.df)

plot1D.entriesV2(obj=iris.obj,var.name="Sepal.Length", train.data=trainNA.df,test.data=testNA.df)
plot1D.entriesV2(obj=iris.obj,var.name="Species", train.data=trainNA.df,test.data=testNA.df)

