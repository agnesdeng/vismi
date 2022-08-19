library(devtools)
devtools::document()
devtools::load_all()

data(nhanes3_newborn)

set.seed(2022)
#withNA.df=createNA(nhanes3_newborn,p=0.1)



n=nrow(nhanes3_newborn)
idx=sample(1:n, size = round(0.7*n), replace=FALSE)

trainNA.df=nhanes3_newborn[idx,]
testNA.df=nhanes3_newborn[-idx,]


#train.data=trainNA.df
#test.data=testNA.df
#p=0.3
#m=10

##########overimpute using mixgb
#overimpute the whole dataset using mixgb
whole.obj<-overimpute_mixgb(train.data=nhanes3_newborn,p=0.3,m=10)

#overimpute the training set, and use the trained imputer to overimpute test set
train_test.obj<-overimpute_mixgb(train.data=trainNA.df,test.data=testNA.df,p=0.3,m=10)

obj=whole.obj
obj=train_test.obj
var.name="BMPHEAD"
train.data=trainNA.df
test.data=testNA.df
var.name="HYD1"


obj=train_test.obj
var.name="HYD1"
train.data=trainNA.df
test.data=testNA.df
n.entries=10

plot1D.entries(obj=whole.obj,var.name="BMPHEAD", train.data=nhanes3_newborn)
plot1D.entries(obj=train_test.obj,var.name="BMPHEAD", train.data=trainNA.df,test.data=testNA.df,n.entries = 10)
plot1D.entries(obj=train_test.obj,var.name="HYD1", train.data=trainNA.df,test.data=testNA.df,n.entries = 10)


str(nhanes3_newborn)

obj=nhanes3_newborn.obj
var.name="HSSEX"
var.name="BMPHEAD"
var.name="HYD1"
train.data=trainNA.df
test.data=testNA.df

plot1D.entries(obj=nhanes3_newborn.obj,var.name="HSSEX", train.data=trainNA.df,test.data=testNA.df)
plot1D.entries(obj=nhanes3_newborn.obj,var.name="DMARETHN", train.data=trainNA.df,test.data=testNA.df)
plot1D.entries(obj=nhanes3_newborn.obj,var.name="BMPHEAD", train.data=trainNA.df,test.data=testNA.df)
plot1D.entries(obj=nhanes3_newborn.obj,var.name="HYD1", train.data=trainNA.df,test.data=testNA.df)


plot1D.entriesV2(obj=nhanes3_newborn.obj,var.name="HSSEX", train.data=trainNA.df,test.data=testNA.df)
plot1D.entriesV2(obj=nhanes3_newborn.obj,var.name="DMARETHN", train.data=trainNA.df,test.data=testNA.df)
plot1D.entriesV2(obj=nhanes3_newborn.obj,var.name="BMPHEAD", train.data=trainNA.df,test.data=testNA.df)
plot1D.entriesV2(obj=nhanes3_newborn.obj,var.name="HYD1", train.data=trainNA.df,test.data=testNA.df)

###
plot1D.imputations(obj=nhanes3_newborn.obj,var.name="BMPHEAD", train.data=trainNA.df,test.data=testNA.df)
plot1D.imputations(obj=nhanes3_newborn.obj,var.name="HYD1", train.data=trainNA.df,test.data=testNA.df)


obj=train_test.obj


var.x="BMPHEAD"
var.y="BMPRECUM"
train.data=trainNA.df
test.data=testNA.df
n.entries=10

colnames(nhanes3_newborn)

var.x="HSSEX"
var.y="HYD1"

