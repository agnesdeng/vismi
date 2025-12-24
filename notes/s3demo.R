# 1. Define the Generic with a neutral name like 'target'
#'@export
myfun <- function(target, ...) {
  UseMethod("myfun")
}

# 2. Method for the Data Frame (where 'target' is your data)
#'@export
myfun.data.frame <- function(target, imp_list, x = NULL, y = NULL, ...) {
  message("Using data.frame logic...")
  # Here, 'target' is your data, and 'x' is your x-axis variable
  print(paste("Data rows:", nrow(target)))
  print(paste("X-axis variable:", x))
}

# 3. Method for the Object (where 'target' is your obj)
#'@export
myfun.MyModelClass <- function(target, x = NULL, num.plot = "qq", ...) {
  message("Using object logic...")
  # Here, 'target' is your obj, and 'x' is your x-axis variable
  print(paste("Model result:", target$result))
  print(paste("X-axis variable:", x))
}


if(D==1){
  # Call the function
  overimp1D_qq(obj = obj, x = vars[1])
}

