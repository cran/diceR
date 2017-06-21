## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	collapse = TRUE,
	comment = "#>",
	fig.align = "center",
	fig.width = 6,
	fig.height = 4.5
)

## ----install, eval=FALSE-------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("AlineTalhouk/diceR")

## ----load----------------------------------------------------------------
library(diceR)
library(dplyr)
library(ggplot2)
library(knitr)
data(hgsc)
hgsc <- hgsc[1:100, 1:50]

## ----consensus_cluster, results='hide'-----------------------------------
CC <- consensus_cluster(hgsc, nk = 3:4, p.item = 0.8, reps = 5,
                        algorithms = c("hc", "pam", "diana"))

## ----consensus_cluster_biclust-------------------------------------------
str(CC)
kable(head(CC[, , "DIANA_Euclidean", "3"]))

## ----impute_knn----------------------------------------------------------
CC <- apply(CC, 2:4, impute_knn, data = hgsc, seed = 1)
CC_imputed <- impute_missing(CC, hgsc, nk = 4)
sum(is.na(CC))
sum(is.na(CC_imputed))

## ----consensus_matrix----------------------------------------------------
pam.4 <- CC[, , "PAM_Euclidean", "4", drop = FALSE]
cm <- consensus_matrix(pam.4)
dim(cm)

## ----graph_heatmap-------------------------------------------------------
hm <- graph_heatmap(pam.4)

## ----consensus_combine, results='hide'-----------------------------------
ccomb_matrix <- consensus_combine(CC, element = "matrix")
ccomb_class <- consensus_combine(CC, element = "class")

## ----consensus_combine_str-----------------------------------------------
str(ccomb_matrix, max.level = 2)
kable(head(ccomb_class$`4`))

## ----consensus_combine_2, results='hide'---------------------------------
CC2 <- consensus_cluster(hgsc, nk = 3:4, p.item = 0.8, reps = 5,
                         algorithms = "km")
ccomb_class2 <- consensus_combine(CC, CC2, element = "class")

## ----consensus_combine_2_str---------------------------------------------
kable(head(ccomb_class2$`4`))

## ----consensus_evaluate, results='hide'----------------------------------
ccomp <- consensus_evaluate(hgsc, CC, CC2, plot = FALSE)

## ----consensus_evaluate_kable--------------------------------------------
kable(ccomp$internal)

## ----consensus_evaluate_trim, results='hide'-----------------------------
ctrim <- consensus_evaluate(hgsc, CC, CC2, trim = TRUE, reweigh = FALSE, n = 2)

## ----consensus_evaluate_trim_str-----------------------------------------
str(ctrim, max.level = 2)

