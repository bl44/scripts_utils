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
 ##  Requires R packages "rpart", "randomForest"
 ##  Tested on R version R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
 ##  Platform: x86_64-apple-darwin10.8.0 (64-bit)
 ##  Read comments for more detailed descriptions
 ##  TODO: Hast tag TODO
 ##  NOTE: Hast tag NOTE
 ## -------------------------------------------------------------------------------------------------------------------
 
#################### FILE DESCRIPTION ####################
#
#  @file modelToSQL.R
#  @projectName Engagement Analysis / LTV
#  @brief converts R model object to Teradata SQL conditions, can be easily expanded to HIVE queries as well, but would require more ds efforts
#################### END OF FILE DESCRIPTION ####################
#

# NOTE: '=' is used for assignment in and outside functions, in all environments, scopes and or workspaces.  In order to change the assignment use 'formatR' package. 
# NOTE: Currently only rpart and randomForest are implemented
# tidy.source(replace.assign=TRUE, "uniVariate.R") will give you the required result. 

#treeToSQL(modelObj = fit_rpart, pred_dv_name = "pred_es", outputPath = NULL, outputFileName = NULL)
#@modelObj - model object with class rpart
#@pred_dv_name - name of the predicted value 
#@version - model Version
#@outputPath - place where do you want to save the resulting SQL
#@outputFile - name of the resulting SQL file
#@modelName - name of the model
#This work is very similar to that of one mentioned here: http://www.teradata.com/Partners/Zementis/ {Didn't know about this b4 implementing}

treeToSQL <- function(modelObj, pred_dv_name = "pred_es_next_month", outputPath = "/Users/adpandey/Desktop/", outputFileName = NULL, modelName = "treeModel", version = "v_0", fallBack = "NULL"){
  if (!inherits(modelObj, "rpart")) stop("Not a legitimate rpart tree")
  frm     <- modelObj$frame
  if (class(modelObj$y) != "numeric") {
    dvClass <- "varchar(31)"
  } else {
    dvClass <- "float"
  }
  if (nrow(frm) == 1){
    print ("Model only has a root, which looks like :")  
    print (modelObj)
    if(is.null(outputPath) || is.null(outputFileName)){
      return (paste("case when 1 = 1 then ", as.numeric(frm$yval), " else ", fallBack, " end as ", pred_dv_name, sep = ""))  
    } else{
      outputFileName = ifelse(is.null(outputFileName), paste(outputPath, "/", modelName, "_", version, sep = ""), paste(outputPath, "/", outputFileName, sep = ""))
      cature.output(paste("case when 1 = 1 then ", as.numeric(frm$yval), " else ", fallBack, " end as ", pred_dv_name, sep = ""), file = outputFileName)
      return (paste("case when 1 = 1 then ", as.numeric(frm$yval), " else ", fallBack, " end as ", pred_dv_name, sep = ""))  
    }  
  }
  names   <- row.names(frm)
  ds.size <- modelObj$frame[1,]$n
  sql = c()
  for (i in 1:nrow(frm)){
    if (frm[i,1] == "<leaf>"){
      pth <- path.rpart(modelObj, nodes=as.numeric(names[i]), print.it=FALSE, pretty = 0)
      pth <- unlist(pth)[-1]
      if (length(pth) != 0)
      {
        rules = c(); newRule = c()
        for (p in (1:length(pth)))
        {
          f <- unlist(strsplit(pth[p], "<|>=|="))[[1]]
          o <- ifelse(length(grep("<", pth[p]))>0, "lessThen",
               ifelse(length(grep(">=", pth[p]))>0, "greaterOrEqual",
               ifelse(length(grep("=", pth[p]))>0, "equal", "DONTKNOW")))
          v <- unlist(strsplit(pth[p], "<|>=|="))[[2]]
          if(o == 'equal'){
            newRule = as.character(paste(f, " in ('", gsub(",", "','", v), "')", sep = ""))
          } else{
            newRule = as.character(pth[p])
          }
          rules = c(rules, newRule)
        }
      }
      case_condition = paste(rules, collapse = " and ")
      terminal_val = ifelse(dvClass == "varchar(31)", as.character(frm[i,]$yval), as.numeric(frm[i,]$yval))
      case_condition = paste(" when ", case_condition, " then ", terminal_val, sep = "")
      sql = c(sql, case_condition)
    }
  }
  if(is.null(outputPath) || is.null(outputFileName)){
    return (paste("case ", paste(sql, collapse = " "), " else ", fallBack, " end as ", pred_dv_name, sep = ""))  
  } else{
    outputFileName = ifelse(is.null(outputFileName), paste(outputPath, "/", modelName, "_", version, sep = ""), paste(outputPath, "/", outputFileName, sep = ""))
    cature.output(paste("case ", paste(sql, collapse = " "), " else ", fallBack," end as ", pred_dv_name, sep = ""), file = outputFileName)
    return (paste("case ", paste(sql, collapse = " "), " else ", fallBack, " end as ", pred_dv_name, sep = ""))  
  }
}

sdecimal2binary <- function(x)
{
  return(rev(sdecimal2binary.smallEndian(x)))
}

sdecimal2binary.smallEndian <- function(x)
{
  if (x==0) return(0)
  if (x<0) stop("Sorry, the input must be positive")
  dec <- x
  n <- floor(log(x)/log(2))
  bin <- c(1)
  dec <- dec - 2 ^ n
  while(n > 0)
  {
    if (dec >= 2 ^ (n-1)) {bin <- c(bin,1); dec <- dec - 2 ^ (n-1)}
    else bin <- c(bin,0)
    n <- n - 1
  }
  return(bin)
}


#print_equation_for_a_tree(modelObj = fit_rf, currentTree = 2, n_row = 1, fallBackRule = FALSE)
#Recursively drills down the tree to form equation out of each row
#Delivers equation for each tree of randomForest model object and the current_tree which is a data.frame obtained from the object
#n_row = nodeID for which equation needs to be printed
  # Let's say that the nodeID is a leaf node, then equation = else "DV value"
  # If not a leaf node then recursively go to the left and then the right node just like preorder tree traversal "NLR"
#If any of the values are null then should the predictions be NULL or on the basis of decision till that level? fallBackRule = TRUE/FALSE takes care of that. 
print_equation_for_a_tree <- function(modelObj, currentTree, n_row, fallBackRule = FALSE) {
  tree_row = currentTree[n_row,]
  tr.vars <- attr(modelObj$terms, "dataClasses")
  var.names <- names(tr.vars)
  splitVar <- as.character(tree_row[,"split var"])
  splitPoint <- tree_row[,"split point"]
  #Added fall back mechanism
  if(fallBackRule){
    predVal <- tree_row[,"prediction"]
    predVal <- ifelse(is.na(a[1,"prediction"]), "null", predVal)
  } else {
    predVal = "null"
  }
  if(tree_row[,"status"] != -1) { #means it's not a terminal node, now since it's not a terminal node print down
    # the equation for this row
    if(is.numeric(unlist(modelObj$forest$xlevels[splitVar]))) {
      cat(paste(" case when", gsub("[.]","_",splitVar), "is null then ", predVal,
      " when", gsub("[.]","_",splitVar), "<=", splitPoint, "then ")) 
      print_equation_for_a_tree(modelObj, currentTree, tree_row[,"left daughter"], fallBackRule = fallBackRule)
      cat(" else ")
      print_equation_for_a_tree(modelObj, currentTree, tree_row[,"right daughter"], fallBackRule = fallBackRule)
      cat("end ")
    } else {      
#    NOTE: Original implementation can be found in pmml.randomForest.R
      var.class <- tr.vars[splitVar]
      node.var <- var.names[splitVar]
      var.levels <- levels(eval(modelObj$call$data)[[splitVar]])
      bins <- sdecimal2binary(splitPoint)
      bins <- c(bins, rep(0, length(var.levels)-length(bins)))
      node.value <- var.levels[bins==1]
      cat(paste(" case when ", gsub("[.]","_",splitVar), " in ('",
      paste(node.value, sep="", collapse="', '"),  #FIXME replace quotes dependant on var type
      "') then ", sep=""))
      print_equation_for_a_tree(modelObj, currentTree, tree_row[,"left daughter"], fallBackRule = fallBackRule)
      cat(paste(" when ", gsub("[.]","_",splitVar), " in ('",
      paste(setdiff(var.levels, node.value), sep="", collapse="', '"),
      "') then ", sep=""))
      print_equation_for_a_tree(modelObj, currentTree, tree_row[,"right daughter"], fallBackRule = fallBackRule)
      cat(paste(" else null end ", sep=""))
    }
  } else { 
    # handling leaf node according to DV being numeric or categorical value
    if (is.numeric(currentTree$prediction)) {
      cat(paste(tree_row[,"prediction"], " ", sep=""))
    } else {
      cat(paste("'", tree_row[,"prediction"], "' ", sep=""))
    }
  }
}

#rf2SQL(modelObj = fit_rf, uid = "user_key", outputPath = "/Users/adpandey/Desktop/", outputFile = "RFmodel.sql", idvs_table = "datascience_lab.adp_mees_running_d", pred_dv_name = "pred")
#@modelObj - model object with class randomForest
#@uid - unique id for each row of the final table
#@outputPath - place where do you want to save the resulting SQL
#@outputFile - name of the resulting SQL file
#@idvs_table - inputTable with all the independent variables
#@pred_dv_name - name of the predicted value 
#@fallBackRule - What to do in case of NULL values? Should fall back at the current value (fallBackRule = TRUE) or output NULL (fallBackRule = FALSE)

rf2SQL <- function (modelObj, uid = "user_key", uidDataType = "BIGINT", outputPath = "/Users/adpandey/Desktop/", outputFile = "RFmodel.sql", idvs_table = "datascience_lab.adp_mees_running_d", pred_dv_name = "pred", outputTable = "datascience_lab.rf_predictions", forceReg = 0, ntrees = NULL, fallBackRule = FALSE) {
  library(randomForest)
  
  if(is.null(modelObj)) { 
    stop ("Model object is missing, please provide!")
    return
  }

  if (!("randomForest" %in% class(modelObj))) {
    stop ("Expected a randomForest object")
    return
  }

  sink(paste(outputPath, "/", outputFile, sep = ""), type="output")
  sql = c()
  
  dv = NULL
  vars = attr(modelObj$terms, "dataClasses")
  dv$name = names(vars)
  dv$class = vars
  names(dv$class) <- names(vars)
  dvTypeIsFactor <- dv$class[[dv$name[1]]] == "factor"

  if (modelObj$type == "classification") {
    #for class character, factor and ordered
    dvClass <- "varchar(31)"
  } else {
    #case of numeric or integer
    if(!dvTypeIsFactor){
      dvClass <- "float"
    }
  }

  if (forceReg == 1) {
    dvClass <- "float"
  }   
    
  cat(paste("create multiset table ", outputTable, " (\n", "\t",uid, " ", uidDataType," not null,\n", "\t",pred_dv_name," ",dvClass,"\n",") primary index(", uid,");\n\n","create multiset volatile table all_tree_scores (\n","\t",uid, " ", uidDataType," not null,\n","\t",pred_dv_name," ",dvClass,"\n",") on commit preserve rows;\n\n",sep=""))   
  
  if(is.null(ntrees)) { 
    ntrees = modelObj$ntree
  }
  
  for (nth_tree in 1:(ntrees)) {
    cat(paste("insert into all_tree_scores\nselect ",uid,",", sep=""))
    #getTree is a function to get a specific tree from randomForest model object as a data.frame
    #the structure of data frame is:
    #left daughter || right daughter || split var || split point || status || prediction
    #In PMML format, this will be "Segment id = nth_tree or the value of k"
    #row number of this dataframe is the node ID and when status == -1 then it's a terminal node and value at the 'prediction' column is the terminal node value
    print_equation_for_a_tree(modelObj, getTree(modelObj,k=nth_tree,labelVar=TRUE), 1, fallBackRule = fallBackRule)
    cat(paste("as tree",nth_tree,"\nfrom ",idvs_table,";\n\n", sep=""))
  }
  
  if (forceReg == 0) {
    if (modelObj$type == "classification") {
      cat(paste("insert into ",outputTable," \n ",
                paste("select \n    a.", uid, ", \n    a.", pred_dv_name, "\n", sep = ""),
                paste(" from (select ",uid," as ", uid, " , ",  pred_dv_name, " , count(*) as cnt from all_tree_scores"," where ",pred_dv_name," is not null "," group by ",uid,", ",pred_dv_name,") a\n",
                "  inner join (select ",uid,", MAX(cnt) as cnt ", " from (select ",uid," as ",uid,",", pred_dv_name,", count(*) as cnt from all_tree_scores"," where ",pred_dv_name," is not null "," group by ",uid,", ",pred_dv_name,") cc ", " group by ",uid,") b\n",
                "    on a.",uid," = b.",uid," and a.cnt = b.cnt;\n\n", sep="")))
    } else {
      cat(paste("insert into ",outputTable," select ",uid,", AVG(",pred_dv_name,") ", "from all_tree_scores ", "where ",pred_dv_name," is not null ", "group by ",uid,";\n", sep=""))
    }
  }
  else{
    cat(paste("insert into ",outputTable," select ",uid,", AVG(",pred_dv_name,") ", "from all_tree_scores ", "where ",pred_dv_name," is not null ", "group by ",uid,";\n", sep=""))  
  }
    cat("drop table all_tree_scores;\n\n")
  
  sink()
}

#Testing the Code
#library(MASS) ; library(randomForest) ; library(pmml) ; library(rpart)
#data(fgl) ; str(fgl) ; set.seed(1) ; s <- sample(dim(fgl)[1], 10) ; test <- fgl[s, ] ; train <- fgl[-s, ]
#fit <- randomForest(RI ~ ., data = train, importance = T, ntree=4,proximity=TRUE, nodesize = 10)
#capture.output(pmml(fit), file = "/Users/adpandey/Desktop/test.xml")
#source("/Users/adpandey/Desktop/ES/utils/modelToSQL.R")
#rf2SQL(modelObj = fit, uid = "user_key", outputPath = "/Users/adpandey/Desktop/", outputFile = "RFmodel.sql", idvs_table = "datascience_lab.adp_mees_running_d", pred_dv_name = "pred", outputTable = "datascience_lab.rf_predictions")
#fit <- rpart(RI ~ ., data = train)
#treeToSQL(modelObj = fit_rpart, pred_dv_name = "pred_es", outputPath = NULL, outputFileName = NULL, fallBack = NULL)

#source("http://scg.sdsu.edu/wp-content/uploads/2013/09/dataprep.r")
#fit2 <-randomForest(income~capital_gain+relationship+marital+race,data=data$train, mtry=2, ntree=4, keep.forest=TRUE, importance=TRUE,test=data$val, nodesize = 5)
#rf2SQL(modelObj = fit2, uid = "user_key", outputPath = "/Users/adpandey/Desktop/", outputFile = "RFmodel.sql", idvs_table = "datascience_lab.adp_mees_running_d", pred_dv_name = "pred", outputTable = "datascience_lab.rf_predictions")

