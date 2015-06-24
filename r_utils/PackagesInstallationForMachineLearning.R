# Install R packages for Machine Learning Projects
# To use this file can excute the source command as shown here
# -- source("PackagesInstallationForMachineLearning.r")
# -- bli@groupon.com, 2014-03-20
# 
# To check if we got the right packages
# --  print(require(PACKAGE_NAME))
# To load installed packages, use the following two r base functions 
# --  library(package_name), 
# --  require(package_name), return true or flase


## R packages for basic Machine Learning
## adopted from <Machine Learning For Hackers>

install.packages(c("arm","glmnet"))
#   arm, packages for doing multilevel/hierarchical regression model
#   glmnet, lasso and eleastic-net regularized generalized linear models

install.packages("lme4") 
# provides functions for creating linear and generalized mixed effects model

packname = "igraph"; 
install.packages(packname)
# package "igraph" provides routines for simple graphs and network analysis.
# Used for representing social networks

install.packages(c("lubridate", "RCurl","reshape","RJSONIO"))
# lubridate, provides convenience function to making working with dates in R
#
# RCurl, provides R interfaces to the libcurl library for interacting with the
#   http protocol. Used to import raw data from the Web.
#
# reshape, a set of tools used to manipulate, aggregate, and manage data in R
#
# RJSONIO, reading and writing Java Script Object Notation(JSON)
#   used to parse data from web-based APIs

install.packages("tm") 
# a collection  of functions for performing for text mining in R. Used to
# work with unstructured text data

install.packages("ggplot2") #for graphics

## R packages for Refund Projects####
## data_science@groupon.com 
## adopted from Guoxian's and others' previous works

#
# All Java Related APIs
# 

# Packages: DBI, RJDBC, 
install.packages("rJava")

# Higher level java APIs
install.packages('xlsx')

# API for .xlsx excel files
#       equired xlsxjars files are also installedlled


