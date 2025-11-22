
devtools::load_all()
devtools::document()
devtools::check()


plot1D(
  imputation.list = imp_nhanes3_newborn, var.name = "HSHSIZER",
  original.data =na_nhanes3_newborn,int.plot = "bar"
)
