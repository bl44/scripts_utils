#
 #getAnywhere
 #scale.default
 ##  File Description
 ##
 ##  Copyright© 2015
 ##  Groupon Inc
 ##  Author  :  Addhyan (adpandey@groupon.com) 
 ##  Version :  1.0
 ##																			  
 ## --------------------------------------------------------------------------------------------------------------------
 ##  As an administrator, ensure 777 or atleast 666 permission in each folder 
 ##  Code runs in phases, sequential throughout with no recurssion or parallel or dynamic programming.
 ##  Tested on R version R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
 ##  Platform: x86_64-apple-darwin10.8.0 (64-bit)
 ##  Read comments for more detailed descriptions
 ##  TODO: Hast tag TODO
 ##  NOTE: Hast tag NOTE
 ## -------------------------------------------------------------------------------------------------------------------
 
#################### FILE DESCRIPTION ####################
#
#  @file utils.R
#  @projectName Hero Model Intl / Category Price Mix Analysis / Engagement Analysis / LTV 
#  @brief functions' definition used by various projects
#################### END OF FILE DESCRIPTION ####################
#

# NOTE: '=' is used for assignment in and outside functions, in all environments, scopes and or workspaces.  In order to change the assignment use 'formatR' package. 
# tidy.source(replace.assign=TRUE, "uniVariate.R") will give you the required result. 

#Short description
#1. getVarsByClass
#Def: Get list of columns with specific data type. 

#2. printPrecisionAccuracy
#Def: Print precision, accuracy, recall and all the confusion matrix parameters of a model, based on actual, predicted values, various threhold values and model object. 

#3. change_class
#Def: Change/Convert the datatype of a vector

#4. confusion.matrix
#Def: Print the confusion matrix based on actual and predicted vectors. 

#5. auc 
#Def: Print the area under the curve value based on actual and predicted values. 

#6. rsqcalc
#Def: Calculate R square value based on actual and predicted values

#7. adjrsq 
#Def: Calculate adjusted R square value based on actual and predicted values

#8. filterOutlier 
#Def: Returns the input data after processing it i.i; after removing the outlier based on lower and upper bound percentile values

#9. lm_eqn 
#Def: Creates a linear equation based on x and y values from a dataset provided as an input to a ggplot function.

#10. multiplot 
#Def: Sourced from the internet, this function put many ggplot in a grid. 

#11. importance_rpart
#Def: Sourced from the internet, this function outputs variable names and their importance based on gini index. 
#TODO : add variable importance based on stepwise quickTreeFormation method. 

#12. dataPrep4Clust 
#Def: Prepares data for pca and clustering analysis. 

#13. score_fun
#Def: Score the data using model object, applies round on the scored data and return the entire dataset. 

#14. analyseModelInBins
#Def: Breaks down the data (actual and predicted values) into bins and prints the important stats.

#get all the variables from a dataset with specific classes mentioned in "classesToFind"
#getVarsByClass(inputData = data, classesToFind = c("character", "factor"))
getVarsByClass <- function (inputData = NULL, vars = NULL, classesToFind = c("character", "factor"), ...){
    if(is.null(inputData)) {
        print("Please provide an inputData, returning NULL")
        return (NULL)
    }
    if(is.null(vars)){
        print("Checking all the columns in inputData")
        vars = c(); vars = names(inputData)
    }
    varsToChange = as.data.frame(sapply(inputData[, vars], class)) ;
    varsToChange$varName = row.names(varsToChange)  ;
    names(varsToChange) = c('Class', 'varName') ;
    varsToChange = varsToChange[which(varsToChange$Class %in% classesToFind),'varName']
    return (varsToChange)
}

#prints precision, accuracy, recall, tp, fp, tn, fn rates based on various threshold values
printPrecisionAccuracy <- function(modelObject = NULL, testData = NULL, dv = NULL, interval = seq(0.1, 30, 0.1), predDV = NULL){
  dv = testData[,dv]
  obs = ifelse(dv == 0, 0, 1)
  if(is.null(modelObject)){
  	pred_dv = as.numeric(newData[,predDV])
  } else {
    pred_dv = as.numeric(predict(modelObject, newdata = testData))
  }
  a  = count(obs == 0)
  total_os = a[a$x=='TRUE','freq']
  result = c()
  for (i in interval){
    pred = ifelse(pred_dv > i, 1, 0)
    total_0s_predicted = as.numeric(as.data.frame(count(pred==0))[which( as.data.frame(count(pred==0))$x==TRUE),'freq'])
    test = as.data.frame(cbind(obs, pred))
    test$xor = !xor(test$obs, test$pred)
    tp = count(test$obs==0 & test$pred==0)
    if(nrow(tp) > 1) {
      tempResults = c() #tempResults
      cm = table(pred, obs) ; cm = confusionMatrix(cm) ; accuracy = as.numeric(cm$overall[1]*100) ; byClass = as.data.frame(cm$byClass) ; cm = as.data.frame(cm$table)
      byClass$parameter = row.names(byClass) ; names(byClass)[1] = "value" ; 
      precision = 100*byClass[which(byClass$parameter == "Pos Pred Value"), "value"] ; 
      recall = 100*byClass[which(byClass$parameter == "Sensitivity"), "value"] ; 
      tpr = recall ; fpr = cm[(cm$obs==1 & cm$pred == 0), "Freq"] / sum(cm[which(cm$obs==1) ,"Freq"]) * 100 ; fnr = 100 * cm[(cm$obs==0 & cm$pred == 1), "Freq"] / sum(cm[which(cm$obs==0) ,"Freq"]) ;
      tnr = 100*byClass[which(byClass$parameter == "Specificity"), "value"] ; 
      tempResults = c(i, tpr, fpr, fnr, tnr, precision, recall, accuracy)
      names(tempResults) = c('threshold', 'tpr', 'fpr', 'fnr', 'tnr', 'precision', 'recall', 'accuracy')
      result = rbind(result, tempResults)
    }
  }
  return(result)
}

#takes data, changes class of required features, returns back the new data
change_class <- function(newdata, change_class_to, features_to_change, ...){
	if(change_class_to == "character")
		newdata[,features_to_change]<-lapply(features_to_change, function(x) as.character(newdata[,x]))
	if(change_class_to == "factor")
		newdata[,features_to_change]<-lapply(features_to_change, function(x) as.factor(newdata[,x]))
	if(change_class_to == "numeric")
		newdata[,features_to_change]<-lapply(features_to_change, function(x) as.numeric(newdata[,x]))
	if(change_class_to == "Date")
		newdata[,features_to_change]<-lapply(features_to_change, function(x) as.Date(newdata[,x]))
	return(newdata)
}

#prints confusion matrix in old-school fashion, takes observation, predicted values and threshold as inputa parameters
confusion.matrix <- function(obs,pred,threshold=0.5){
	#apply the threshold to the prediction
	if (threshold==0) {
		pred[which(pred>threshold)] = 1; pred[which(pred<=threshold)] = 0
	} else {
		pred[which(pred>=threshold)] = 1; pred[which(pred<threshold)] = 0
	}
	#return the confusion matrix
	mat = table(pred=factor(pred,levels=c(0,1)),obs=factor(obs,levels=c(0,1)))
	#attr(mat,'class') = 'confusion.matrix'
	return(mat)
}

#prints the value of area under the curve
# for more details: http://www.statsblogs.com/tag/auc/
auc <- function(dv,pred_dv){
	#input checks
	if (length(dv)!=length(pred_dv)) stop('this requires the same number of Observed & pred_dvicted values')	
	#deal with NAs
	if (length(which(is.na(c(dv,pred_dv))))>0) {
		na = union(which(is.na(dv)),which(is.na(pred_dv)))
		warning(length(na),' data points removed due to missing data')
		dv = dv[-na]; pred_dv = pred_dv[-na]
	}
	#define the n's and do checks
	n = length(dv); if (length(which(dv %in% c(0,1)))!=n) stop('Observed values must be 0 or 1') #ensure Observed are values 0 or 1
	n1 = as.double(length(which(dv==1))); n0 = as.double(length(which(dv==0)))
	if (n1==0 || n1==n) return( NaN ) #if all Observed 1's or 0's return NaN
	###calculating AUC
	pred_dv0 = pred_dv[which(dv==0)]
	pred_dv1 = pred_dv[which(dv==1)]
	ranks = rank(pred_dv,ties.method='average')#define ranks
	ranks0 = ranks[which(dv==0)]
	ranks1 = ranks[which(dv==1)]
	U = n0*n1 + (n0*(n0+1))/2 - sum(ranks0) #calc U stat
	AUC = U/(n0*n1) #estimate AUC
	if (AUC<.5) AUC = 1-AUC
	#return the auc value
	return(AUC)
}

#Calculates R square for linear regression model
rsqcalc <- function(dv,pred_dv)
{
   rsq = cor(dv,pred_dv)^2
   return(rsq)
}

#Calculates adjusted r squared for linear model
adjrsq <- function(dv,pred_dv,n_regressors,data_size)
{
  rsq = rsqcalc(dv = dv, pred_dv = pred_dv)
  adjrsqv = 1-(1-rsq)*(data_size-1)/(data_size-n_regressors-1)
  return(adjrsqv)

}

#removes outlier from a model based on lower and upper bound values
filterOutlier <- function(data, lb = 5, ub = 95, var_name = NULL) {
    lb_val = quantile(data[,var_name], lb/100)
    ub_val = quantile(data[,var_name], ub/100)
	result = data[which(data[,var_name]<=ub_val & data[,var_name]>=lb_val), ]
    result
}

#generates linear equation for plots (ggplot)
lm_eqn <- function(m, dig = 3) {
  l <- list(a = round(coef(m)[1], digits = dig),
      b = round(abs(coef(m)[2]), digits = dig),
      r2 = round(summary(m)$r.squared, digits = dig));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}

#print multiple plots in the same file
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}

#generate var imp vector from a rpart model
importance_rpart <- function(fit)
{
  if(is.null(fit)) {
    stop('Please provide correct rpart model object, returning NULL')
  }
  if(class(fit) != 'rpart'){
    stop('Please provide correct rpart model object, returning NULL')
  }
  ff <- fit$frame
  fpri <- which(ff$var != "<leaf>")  # points to primary splits in ff
  spri <- 1 + cumsum(c(0, 1 + ff$ncompete[fpri] + ff$nsurrogate[fpri]))
  spri <- spri[seq_along(fpri)] # points to primaries in the splits matrix
  nsurr <- ff$nsurrogate[fpri]  # number of surrogates each has
  sname <- vector("list", length(fpri))
  sval <- sname
    ## The importance for primary splits needs to be scaled
    ## It was a printout choice for the anova method to list % improvement in
    ##  the sum of squares, an importance calculation needs the total SS.
    ## All the other methods report an unscaled change.
  scaled.imp <- if (fit$method == "anova")
  fit$splits[spri, "improve"] * ff$dev[fpri]
  else fit$splits[spri, "improve"]

  sdim <- rownames(fit$splits)
  for (i in seq_along(fpri)) {
    ## points to surrogates
    if (nsurr[i] > 0L) {
      indx <- spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
      sname[[i]] <- sdim[indx]
      sval[[i]] <- scaled.imp[i] * fit$splits[indx, "adj"]
    }
  }

    import <- tapply(c(scaled.imp, unlist(sval)),
                     c(as.character(ff$var[fpri]), unlist(sname)),
                     sum)
    sort(c(import), decreasing = TRUE) # a named vector
}

#takes data, variable names, i/e (include/exclude) as input parameters, returns a scaled dataset
#by default includeORexclude is set as include
#dataPrep4Clust (inputData = data)
#dataPrep4Clust (inputData = data, vars = c("user_key", "subs_year_key", "es_f30", "is_fb", "app_es_f30", "goods_cohort"), includeORexclude = 'i')

dataPrep4Clust <- function(inputData = NULL, vars = NULL, includeORexclude = 'i', ...){
  pca_data = inputData; 
  if(is.null(inputData)) { 
    stop('Please provide inputdata, returning NULL')
  }
  if(class(inputData) != 'data.frame') { 
    stop('Class of inputData is not data.frame, please provide a data with correct class, returning NULL')
  }
  if(is.null(vars)){
    print ('Running PCA on the entire dataset since vars is not provided')
    vars = names(inputData)
  } else {
    if(includeORexclude == 'e')
      vars = setdiff(names(inputData), vars)
  } 
  varsToRemove = getVarsByClass(inputData = pca_data, classesToFind = c("character", "factor"))
  if(length(varsToRemove) > 0) {
    cat("Removing variables of class character and factor since PCA can only be run over a numeric dataset", "\n")
  }
  if(length(intersect(vars, names(inputData))) > 0){
    pca_data = pca_data[, vars]    
    pca_data = pca_data[, setdiff(vars, varsToRemove)]
	if(length(ncol(pca_data)) == 0) { 
	  print("Dataset only had categorical variables which were filtered out, please provide correct dataset, returning NULL") ;
      return (NULL) ; 
	}
  } else{ 
    stop('List of variables provided are not present in the inputData, returning NULL')
  }  
  cat("checking for NAs and 0 standard deviation condition", "\n")
  cols_stage_1 = names(pca_data); pca_data = pca_data[,!apply(is.na(pca_data), 2, any)]; cols_stage_2 = names(pca_data)
  if(length(cols_stage_1) - length(cols_stage_2) > 0) {
    cat('Removing variables with NAs, these variables are:', '\n')
    print(setdiff(cols_stage_1, cols_stage_2))
  }
  constantVars = as.data.frame(sapply(pca_data, FUN = sd))
  constantVars$varName = row.names(constantVars) ; names(constantVars)[1] = 'sdVal'
  if(length(constantVars[which(constantVars$sdVal == 0), "varName"]) > 0) {
    cat('Removing variables with 0 standard deviation, these variables are:', '\n')
    print(constantVars[which(constantVars$sdVal == 0), "varName"])
    pca_data = pca_data[, constantVars[which(constantVars$sdVal != 0), "varName"]]
  }
  if (length(names(pca_data)) == 0) {
    stop('All the variables are filtered out, please provide correct dataset, returning NULL')
  }
  pca_data <- scale(pca_data)
  return(pca_data)
}

#This returns the input data with addition column of scored values based on other columns and provided model object. It applies round on the scored value. 
#score_fun(do_round = 0, dig = 0, leads_data, fit_rpart_4_rf, "score_rpart_4_rf")
score_fun = function(do_round = 0, dig = 0, newdata, model_obj, pred_dv, type_gbm = NULL, n.trees = NULL, ...){
	if(class(model_obj)=='gbm'){
		newdata[,pred_dv] = predict(model_obj, newdata = newdata, type = type_gbm, n.trees = n.trees) 
	}else{ 
		newdata[,pred_dv] = predict(model_obj, newdata = newdata) 
	}
	if( (class(newdata[,pred_dv]) %in% c('integer', 'numeric', 'factor', 'matrix')) & do_round == 1){
		newdata[,pred_dv] = round(newdata[,pred_dv], dig)
	}
	return(newdata) 
}

#analyseModelInBins(dv = data$dv, pred.dv=data$pred_dv, nbins = 5)
#analyseModelInBins takes observed and predicted data points as input and returns a matrix with all the relevant stats
analyseModelInBins <- function(dv, pred.dv, nbins = 15, ties.method="min", breaks = NULL){
  if (is.null(dv) | is.null(pred.dv)) stop ('Please provide Observed and Predicted Values')
  x = data.frame(dv, pred.dv)
  cat ("Correlation b/w actual and predicted value is :", cor(x)[2], "\n")
  g = nbins - floor(rank(x[,"dv"], ties.method = ties.method )*nbins/(nrow(x)+1));
  y=split(x,g)
  f = function(z) c(range(z[,1]), range(z[,2]), quantile(z[,1], 0.25), quantile(z[,2], 0.25), mean(z[,1]), mean(z[,2]), median (z[,1]), median (z[,2]), mean(z[,2]-z[,1]), cor(z[,2],z[,1]), length(z[,2]))
  y3 = data.frame(do.call("rbind",lapply(y,f)))
  names(y3) = c("min_dv", "max_dv", "min_pred", "max_pred", "25p_dv", "25p_pred", "avg_dv", "avg_pred", "median_dv", "median_pred", "avg_pred_dv", "cor", "dps")
  y3
}
