#create package
library(usethis)
usethis::create_package(path = "C:/Users/agnes/Desktop/phd-thesis/my-packages/miae")

library(devtools)
library(usethis)

#set github
use_git()
use_gpl3_license()

#create function inside R folder
use_r("createNA")

load_all()
document()
check()
install()

library(vismi)

use_readme_rmd()

createNA(data=iris,p=0.3)



