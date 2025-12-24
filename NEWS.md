
# vismi 1.0.0 (planned)
## Breaking changes version
- Submit to CRAN


# vismi 0.9.0
### Deprecations 
- Deprecated `trelliscopejs_con()` and `trelliscopejs_cat()`. 
### New Features
-  `ts_overimp()` Support showing all overimputation 1D plots for all numeric and factors variables through `trelliscopejs`


# vismi 0.8.0
### New Features
- subset imputed datasets for plotting via `imp_idx` or `m` argument in `vismi()` function.
### Bug Fixes
- Misalignment issue of using plotly subplots in `.plotly_box_facet()`.
### Refactoring
- Refactored other functions.


# vismi 0.7.0
### Refactoring
- Refactored all overimputation visualisation functions for 1D & 2D.

# vismi 0.6.0
### Refactoring
- `overimpute()` function refactored to improve code readability and maintainability.
- Added S3 methods for visualising overimputation object.
- Added `vismi.data.frame` method. 
  - `vismi(data, imp_list, x, y, z,...)` visualises multiple imputed datasets for up to 3 variables.
- Added `vismi.overimp` method.
  - `vismi(obj, x, y, z,...)` visualises overimputation results

# vismi 0.5.0
### New Features
- Added 1D, 2D, and 3D interactive inspection visualisation using `plotly`.

# vismi 0.4.0
### New Features
- Added more features for static visualisation using`ggplot2`. 

# vismi 0.3.0
### Refactoring 
- Added preprocessing function
- Default parameters handling 

# vismi 0.2.0
### Refactoring 
- A high-level main function `vismi()` will be used to call all other inspection functions.

# vismi 0.1.0
### Initial Release
- First development version releases on Github.
- As illustrated in my PhD thesis.


