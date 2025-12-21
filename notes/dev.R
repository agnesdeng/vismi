library(mixgb)
library(plotly)
set.seed(2025)
devtools::load_all()
devtools::document()

save(newborn,file="/Users/agnes/Desktop/vismi/data/newborn.rda",compress = "xz")
imp_newborn <- mixgb(data = newborn, m = 5, maxit=5)
save(imp_newborn,file="/Users/agnes/Desktop/vismi/data/imp_newborn.rda",compress = "xz")

#imp_newborn
#show_var(imp_newborn,var.name="BMPHEAD",original.data = newborn)

data(newborn)
devtools::load_all()
unlink("NAMESPACE")
devtools::document()

devtools::load_all()
devtools::document()

devtools::check()



# overimputation ---------------------------------------------------------
devtools::load_all()
devtools::document()



obj<-overimpute(data=newborn,p=0.2,m=5,test_ratio=0.2,method="mixgb")
#obj2<-overimpute(data=newborn,p=0.2,m=5,test_ratio=0,method="mixgb")
my_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

fig<-vismi(obj=obj,x="BMPHEAD", y= "BMPRECUM", alpha=1, point_size=0.2, train_color_pal = my_pal, test_color_pal = my_pal)
fig

fig<-vismi(obj=obj,x="BMPHEAD", y= "BMPRECUM", xlim=c(40,45))
fig


fig<-vismi(obj=obj,x="BMPHEAD", y= "BMPRECUM", ylim=c(50,90))
fig

fig<-vismi(obj=obj,x="BMPHEAD", y= "HSSEX", alpha=0.5, point_size=0.2, boxpoints="all")
fig

fig<-vismi(obj=obj,x="HSSEX", y= "BMPHEAD", alpha=0.5, point_size=0.2, boxpoints="all")
fig
fig<-vismi(obj=obj,x="HSSEX", y= "BMPHEAD", alpha=0.5, point_size=0.2, boxpoints="all", ylim=c(40,50))
fig


fig<-vismi(obj=obj,x="BMPHEAD", y= "HSSEX")
fig

fig<-vismi(obj=obj,x="DMARETHN", y= "HSSEX")
fig

fig<-vismi(obj=obj,x="HYD1", y= "HSSEX" , ylim=c(0,200))
fig
fig<-vismi(obj=obj,y="HYD1", x= "HSSEX" , xlim=c(0,200))
fig


fig<-vismi(obj=obj,x="HSAGEIR", y= "DMARETHN", integerAsFactor=TRUE,width=0.9)
fig

devtools::load_all()
devtools::document()

fig<-vismi(obj=obj,x="BMPHEAD",num_plot="qq", point_size=0.5,xlim=c(-2,2))
fig
fig<-vismi(obj=obj,x="BMPHEAD",num_plot="qqline", point_size=0.5,xlim=c(-2,2))
fig
fig<-vismi(obj=obj,x="BMPHEAD",num_plot="density", linewidth=0.5, ylim=c(0,0.4))
fig
fig2<-vismi(obj=obj2,x="BMPHEAD",num_plot="qqline",linewidth=1)
fig2
fig<-vismi(obj=obj,x="BMPHEAD",num_plot="ridge",alpha=1)
fig
fig<-vismi(obj=obj,x="BMPHEAD",num_plot="ridge", xlim=c(40,45))
fig
fig<-vismi(obj=obj,x="BMPHEAD",num_plot="ridge",alpha=0.8)
fig
#1d fac
fig<-vismi(obj=obj,x="HYD1",fac_plot="dodge",alpha=0.8)
fig
fig<-vismi(obj=obj,x="HYD1",fac_plot="dodge", ylim=c(0,180))
fig

fig<-vismi(obj=obj,x="HYD1",fac_plot="bar",ylim=c(0,100))
fig
class(fig)
plot(fig)
overimp1D_qq(obj,x="BMPHEAD")


class(obj)
vismi2.overimp(obj,x="BMPHEAD",num.plot="qq")
vismi2(obj,x="BMPHEAD",num.plot="qq")

obj


num.plot="qq"
x="BMPHEAD"
y=NULL
z=NULL
num.plot="qq"
obj$imputed_test
str(newborn)


overimp1D_qq(obj=obj, x="BMPHEAD", point_size = 1, ylim = NULL)

# inspection --------------------------------------------------------------



tools::showNonASCIIfile("R/ggplot_marginal_helpers.R")

data(imp_newborn)


my_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")



integerAsFactor <-TRUE

data=newborn
imp_list=imp_newborn
y="BMPHEAD"

x="HSSEX"
z="HFF1"

x="BMPHEAD"
y=NULL
z=NULL

interactive=FALSE
alpha=1
point_size=NULL
color_pal=NULL
marginal_x=NULL
marginal_y=NULL



vismi(data=newborn,imp_list=imp_newborn,x="HSHSIZER",y="BMPRECUM", z="DMARETHN", color_pal=my_pal,interactive=FALSE,integerAsFactor = T)

vismi(data=newborn,imp_list=imp_newborn,y="HSAGEIR",x="HSHSIZER", z="BMPRECUM",interactive=TRUE,integerAsFactor = F)




vismi(data=newborn,imp_list=imp_newborn,x="DMARETHN",y="HSHSIZER", z="BMPRECUM",interactive=FALSE,integerAsFactor = T)

vismi(data=newborn,imp_list=imp_newborn,x="DMARETHN",y="HSHSIZER", z="BMPRECUM",interactive=FALSE)

vismi(data=newborn,imp_list=imp_newborn,x="DMARETHN",y="BMPRECUM", z="HFF1",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM",y="HSSEX", z="HFF1",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1", z="BMPRECUM",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM",y="HFF1", z="HSSEX",interactive=FALSE)

#2fac1num
devtools::load_all()
devtools::document()
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1", z="BMPRECUM",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1", z="BMPRECUM",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="HSSEX",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,y="BMPRECUM", x="HSSEX",interactive=TRUE)

#3num
devtools::load_all()
devtools::document()
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="BMPHEAD",z="BMPSB1",color_pal=my_pal,interactive=TRUE)

#1fac2num
devtools::load_all()
devtools::document()
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="BMPHEAD",z="HYD1",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="HSHSIZER",z="HFF1",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="HFF1", z="BMPRECUM",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="BMPHEAD",z="HFF1",interactive=FALSE)



#2D
devtools::load_all()
devtools::document()
#2fac
vismi(data=newborn,imp_list=imp_newborn,x="HFF1", y="HSSEX",interactive=TRUE, color_pal=my_pal)
vismi(data=newborn,imp_list=imp_newborn,x="HFF1", y="HSSEX",interactive=FALSE)
#2num
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="BMPHEAD",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="BMPHEAD",interactive=FALSE)
#1fac1nun
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="HSSEX",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="BMPRECUM", interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPRECUM", y="HSSEX",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="BMPRECUM", interactive=FALSE)




vismi(data=newborn,imp_list=imp_newborn,x="HFF1", y="HSSEX",interactive=TRUE)

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="HSSEX",interactive=FALSE,width=0.5)

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="HSSEX",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1",interactive=FALSE)



vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1",interactive=FALSE,alpha=0.9,width=0.2)

vismi(data=newborn,imp_list=imp_newborn,x="HSSEX",y="HFF1",interactive=TRUE)


vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=FALSE, point_size=3,alpha=0.8)

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="HFF1",interactive=TRUE, alpha=0.5,boxpoints=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=TRUE, alpha=0.5,boxpoints=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=TRUE, alpha=0.5,boxpoints="all")
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=TRUE, alpha=0.5,boxpoints="outliers")

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=FALSE, alpha=0.8,boxpoints=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=FALSE, alpha=0.5,boxpoints="all")
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=FALSE, alpha=0.8,boxpoints="outliers")

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=TRUE, boxpoints="all",point_size=3,alpha=0.2)



#2D

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="HFF1",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="HFF1",y="BMPHEAD",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="hist",marginal_y="hist")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="hist",marginal_y="hist",nbins=10)



vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="box",marginal_y="box")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="rug",marginal_y="rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="box+rug",marginal_y="box+rug")

devtools::load_all()
devtools::document()
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE,marginal_x="box+rug",marginal_y=NULL)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE,marginal_x=NULL,marginal_y="box+rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE,marginal_x="box+rug",marginal_y="box+rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE,marginal_x="box",marginal_y="box")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE,marginal_x="rug",marginal_y="rug",point_size = 1,alpha=0.2)

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_y="rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=TRUE,marginal_x="rug",marginal_y="rug")

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",y="BMPRECUM",interactive=FALSE)


#1D
devtools::load_all()
devtools::document()
#1num
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=TRUE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=TRUE,nbins=NULL,marginal_x="box+rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=TRUE,nbins=NULL,marginal_x="box")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=TRUE,marginal_x="rug")

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE)
#1fac

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",interactive=TRUE)

vismi(data=newborn,imp_list=imp_newborn,x="HFF1",interactive=FALSE)




vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE,nbins=30)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE,nbins=NULL)

vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE)
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE,marginal_x="rug")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE,marginal_x="box")
vismi(data=newborn,imp_list=imp_newborn,x="BMPHEAD",interactive=FALSE,marginal_x="box+rug")





