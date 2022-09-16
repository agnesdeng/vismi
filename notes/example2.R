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



obj=train_test.obj
var.name="BMPHEAD"
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
var.name="BMPHEAD"
var.name="HYD1"
train.data=trainNA.df
test.data=testNA.df

plot1D.imputationsV2(obj=train_test.obj,var.name="BMPHEAD", train.data=trainNA.df,test.data=testNA.df)

obj=train_test.obj


var.x="BMPHEAD"
var.y="BMPRECUM"
train.data=trainNA.df
test.data=testNA.df
n.entries=10

colnames(nhanes3_newborn)

var.x="HSSEX"
var.y="HYD1"



#trelliscopejs

library(trelliscopejs)
library(ggplot2)
library(gapminder)


qplot(year, lifeExp, data = subset(gapminder, continent == "Europe")) +
  facet_wrap(~ country + continent) +
  theme_bw()


qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_wrap(~ country + continent)

qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)


###




obj=whole.obj
train.data=nhanes3_newborn
Names <- obj$params$Names
Types <- obj$params$Types
m <- obj$params$m



addNA.m <- obj$params$addNA.m

imputed.traindata <- obj$imputed.traindata

var.names = c("BMPHEAD", "BMPRECUM", "BMPSB1", "BMPSB2","BMPTR1", "BMPTR2","BMPWT","DMPPIR")



var.name="BMPHEAD"
# train data --------------------------------------------------------------
imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[,var.name]])



#imputed <- lapply(imputation.list, function(dt) dt[na.combine, c(var.x, var.y, con.fac), with = FALSE])

Nrow <- length(imputed[[1]])

L <- unlist(imputed)
Mat <- matrix(L, nrow = Nrow, ncol = m)

# original observed values for this variable
if(is.data.table(train.data)| is_tibble(train.data)){
  #data.table
  train.data <- as.data.frame(train.data)
}

#data.frame
True <- train.data[, var.name][addNA.m[, var.name]]

combine.df <- cbind.data.frame(Mat, True)
colnames(combine.df) <- c(paste0("m", 1:m), "True")
long.df <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = var.name)


sub.title <- paste(paste("Distribution of", m, "imputed values"),paste("in variable",var.name),sep="\n")

#blue
long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
colfunc <- colorRampPalette(c("#0c71ff","#bcd8ff"))
traincolor<-c(colfunc(m),"gray40")


ggplot(data=long.df,aes(.data[[var.name]],color=set))+
  geom_density()+
  scale_color_manual(values=traincolor)

  # true values as points and bars
  P1 <- ggplot(data = long.df, aes(x = .data[[var.name]])) +
    geom_density_ridges(alpha = 0.8, aes(y = set, fill = set)) +
    scale_fill_manual(values = traincolor, guide = guide_legend(reverse = TRUE)) +
    guides(fill="none")+
    theme_ridges(center_axis_labels = TRUE) +
    labs(title = "Training Data", subtitle = sub.title, y = "Imputed sets")

na.idx <- which(is.na(original.data[[var.name]]))
imp.l <- lapply(imputation.list, function(dt) dt[[var.name]][na.idx])
N.imp <- length(imp.l)
M <- paste("m", 1:N.imp, sep = "")
names(imp.l) <- M
imp.df <- do.call(cbind.data.frame, imp.l)
imp.dt<-as.data.table(imp.df)
#imp.df$obs <- 1:nrow(imp.df)
imp.dt = melt(imp.dt, measure.vars = M, variable.name = "m.set", value.name = var.name, value.factor = TRUE)
#imp.df <- tidyr::pivot_longer(data = imp.df, cols=everything(), names_to = "m.set", values_to = var.name)
#imp.df <- tidyr::pivot_longer(data = imp.df, cols =!obs, names_to = "m.set", values_to = var.name)
#imp.df$m.set <- factor(imp.df$m.set, levels = M)

# Observed
observed <- original.data[[var.name]][-na.idx]
N.obs <- length(observed)
N.mis<-length(na.idx)
#observed.df <- data.frame(1:N.obs, rep("Observed", N.obs), observed)
#colnames(observed.df) <- c("obs", "m.set", var.name)



if (class(imp.dt[[var.name]])[1] != class(original.data[[var.name]])[1]) {
  # Special case: original ordinal-> imputed as integer
  observed <- fac2int(observed)
}

if(!is.null(true.data)){
  #with true.data
  true<-true.data[[var.name]][na.idx]
  if (class(imp.dt[[var.name]])[1] != class(original.data[[var.name]])[1]) {
    true<-fac2int(true)
  }
  other.dt<-data.table(m.set=c(rep("Observed",N.obs),rep("MaskedTrue",N.mis)),var.name=c(observed,true))
  all.dt<- rbind(other.dt,imp.dt,use.names=FALSE)
  all.dt[,obs:=c(1:N.obs,rep(1:N.mis,N.imp+1))]
  if (is.null(color.pal)) {
    color.pal <- c("gray40","gray20", scales::hue_pal()(N.imp))
  }
}else{
  #without true.data
  other.dt<-data.table(m.set=c(rep("Observed",N.obs)),var.name=observed)
  all.dt<- rbind(other.dt,imp.dt,use.names=FALSE)
  all.dt[,obs:=c(1:N.obs,rep(1:N.mis,N.imp))]
  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }
}

setnames(all.dt,"var.name",var.name)
#colnames(all.dt)[2]<-var.name
return(list("all.dt"=all.dt,"color.pal"=color.pal))




