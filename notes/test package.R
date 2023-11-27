devtools::document()
devtools::load_all()

library(mixgb)

data(nhanes3_newborn)
set.seed(2022)

n=nrow(nhanes3_newborn)
idx=sample(1:n, size = round(0.7*n), replace=FALSE)

trainNA.df=nhanes3_newborn[idx,]
testNA.df=nhanes3_newborn[-idx,]


#overimpute the whole dataset using mixgb
mixgb.obj<-overimpute(train.data=nhanes3_newborn,p=0.3,m=10, method = "mixgb")




train.data=nhanes3_newborn
test.data=NULL
p = 0.3
m = 2
maxit = 1
ordinalAsInteger = FALSE
pmm.type = NULL
pmm.k = 5
pmm.link = "prob"
initial.num = "normal"
initial.int = "mode"
initial.fac = "mode"
save.models = FALSE
save.vars = colnames(train.data)
save.models.folder = NULL
verbose = F
xgb.params = list()
nrounds = 10
early_stopping_rounds = NULL
print_every_n = 10L
xgboost_verbose = 0

train.obj <- mixgb(data = addNA.df, m = m, maxit = maxit, ordinalAsInteger = ordinalAsInteger,
                   pmm.type = pmm.type, pmm.k = pmm.k, pmm.link = pmm.link,
                   initial.num = initial.num, initial.int = initial.int, initial.fac = initial.fac,
                   save.models = save.models, save.vars = save.vars, save.models.folder = save.models.folder,
                   verbose = verbose,
                   xgb.params = xgb.params,
                   nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
                   print_every_n = print_every_n, xgboost_verbose = xgboost_verbose)

#overimpute the whole dataset using mixgb
mice.obj<-overimpute(train.data=nhanes3_newborn,p=0.3,m=10, method = "mice")
