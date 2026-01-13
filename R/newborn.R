#' NHANES III (1988-1994) newborn data
#'
#' This dataset is extracted from the NHANES III (1988-1994) for the age class \code{Newborn (under 1 year)}. Please note that this example dataset only contains selected variables and is for demonstration purposes only.
#' @docType  data
#' @usage data(newborn)
#' @format A data frame of 2107 rows and 16 variables, adapted from the \code{nhanes3} dataset. Nine variables contain missing values. Variable names and factor levels have been renamed for clarity and easier interpretation.
#' \describe{
#'  \item{household_size}{Household size. An integer variable ranging from 1 to 10. Original variable name \code{HSHSIZER} in the \code{nhanes3} dataset.}
#'  \item{age_months}{Age at interview (screener), in months. An integer variable ranging from 2 to 11. Original variable name \code{HSAGEIR} in the \code{nhanes3} dataset.}
#'  \item{sex}{Sex of the subject. A factor variable with levels \code{Male} and \code{Female}. Original variable name \code{HSSEX} in the \code{nhanes3} dataset.}
#'  \item{race}{Race of the subject. A factor variable with levels \code{White}, \code{Black}, and \code{Other}. Original variable name \code{DMARACER} in the \code{nhanes3} dataset.}
#'  \item{ethnicity}{Ethnicity of the subject. A factor variable with levels \code{Mexican-American}, \code{Other Hispanic}, and \code{Not Hispanic}. Original variable name \code{DMAETHNR} in the \code{nhanes3} dataset.}
#'  \item{race_ethinicity}{Combined race–ethnicity classification. A factor variable with levels \code{Non-Hispanic White}, \code{Non-Hispanic Black}, \code{Mexican-American}, and \code{Other}. Original variable name \code{DMARETHN} in the \code{nhanes3} dataset.}
#'  \item{head_circumference_cm}{Head circumference, in centimetres. Numeric. Original variable name \code{BMPHEAD} in the \code{nhanes3} dataset.}
#'  \item{recumbent_length_cm}{Recumbent length, in centimetres. Numeric. Original variable name \code{BMPRECUM} in the \code{nhanes3} dataset.}
#'  \item{first_subscapular_skinfold_mm}{First subscapular skinfold thickness, in millimetres. Numeric. Original variable name \code{BMPSB1} in the \code{nhanes3} dataset.}
#'  \item{second_subscapular_skinfold_mm}{Second subscapular skinfold thickness, in millimetres. Numeric. Original variable name \code{BMPSB2} in the \code{nhanes3} dataset.}
#'  \item{first_triceps_skinfold_mm}{First triceps skinfold thickness, in millimetres. Numeric. Original variable name \code{BMPTR1} in the \code{nhanes3} dataset.}
#'  \item{second_triceps_skinfold_mm}{Second triceps skinfold thickness, in millimetres. Numeric. Original variable name \code{BMPTR2} in the \code{nhanes3} dataset.}
#'  \item{weight_kg}{Body weight, in kilograms. Numeric. Original variable name \code{BMPWT} in the \code{nhanes3} dataset.}
#'  \item{poverty_income_ratio}{Poverty income ratio. Numeric. Original variable name \code{DMPPIR} in the \code{nhanes3} dataset.}
#'  \item{smoke}{Whether anyone living in the household smokes cigarettes inside the home. A factor variable with levels \code{Yes} and \code{No}. Original variable name \code{HFF1} in the \code{nhanes3} dataset.}
#'  \item{health}{General health status of the subject. An ordered factor with levels \code{Excellent}, \code{Very Good}, \code{Good}, \code{Fair}, and \code{Poor}. Original variable name \code{HYD1} in the \code{nhanes3} dataset.}
#' }
#'
#' @references U.S. Department of Health and Human Services
#' (DHHS).  National Center for Health Statistics.  Third National
#' Health and Nutrition Examination Survey (NHANES III, 1988-1994):
#' Multiply Imputed Data Set. CD-ROM, Series 11, No. 7A.
#' Hyattsville, MD: Centers for Disease Control and Prevention,
#' 2001. Includes access software: Adobe Systems, Inc. Acrobat
#' Reader version 4.
#' @source \url{https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx}
"newborn"
