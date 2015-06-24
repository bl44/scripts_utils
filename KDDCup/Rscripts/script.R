# all the combined_data in the functions below assume that this data is a merge between log_train and truth_train
max_series_len=function(combined_data){
  max_len=-1
  num = length(unique(merged_labels[merged_labels$class==1,]$enrollment_id))
  for(i in 1:num){
    id=unique(merged_labels[merged_labels$class==1,]$enrollment_id)[i]
    #print(id)
    if(max_len<length(merged_labels[merged_labels$class==1,][merged_labels$enrollment_id==id,]$event)){
      max_len=length(merged_labels[merged_labels$class==1,][merged_labels$enrollment_id==id,]$event)
      cat(sprintf("%d %d",id,max_len))
    }
  }
  return(max_len)
}

max_series_len2=function(combined_data){
  max_len=-1
  uniq = unique(merged_labels$enrollment_id)
  for(id in uniq){
    #id=unique(merged_labels[merged_labels$class==1,]$enrollment_id)[i]
    print(id)
    if(max_len<length(merged_labels[merged_labels$enrollment_id==id,]$event)){
      max_len=length(merged_labels[merged_labels$enrollment_id==id,]$event)
      cat(sprintf("%d %d",id,max_len))
    }
  }
  return(max_len)
}

plot_series=function(combined_data,num_sample){
  uniq = unique(merged_labels$enrollment_id)
  uniq_sampled = uniq[sample(length(uniq),num_sample)]
  id0=uniq_sampled[1]
  id0_data=merged_labels[merged_labels$enrollment_id==id0,]$event
  print(length(id0_data))
  color=ifelse(unique(merged_labels[merged_labels$enrollment_id==id0,]$class)[1]==1,"red","blue")
  max_len=-1
  for(id in uniq_sampled){
    if(max_len<length(merged_labels[merged_labels$enrollment_id==id,]$event)){
      max_len=length(merged_labels[merged_labels$enrollment_id==id,]$event)
    }
  }
  #,ylim=levels(merged_labels$event)
  plot(y=id0_data,x=1:length(id0_data),xlim=c(1,max_len),type="n")
  print("After first")
  #print(unique(merged_labels[merged_labels$enrollment_id==id0,]$class)[1])
  print(color)
  for(id in uniq_sampled){
    id_data = merged_labels[merged_labels$enrollment_id==id,]$event
    color=ifelse(unique(merged_labels[merged_labels$enrollment_id==id,]$class)[1]==1,"red","blue")
    lines(y=id_data,x=1:length(id_data),type="l",col=color)
    print(color)
  }
}

fit_logistic_event_counts = function(merged_log_train_events_counts){
  glmfit2 = glm(class ~ events_count, data=merged_log_train_events_counts,family="binomial")
  prob=predict(glmfit2,type=c("response"))
  merged_log_train_events_counts$prob = prob
  g = roc(class ~ prob, data=merged_log_train_events_counts)
  plot(g)
}

fit_logistic_event_counts_split = function(merged_log_train_events_counts){
  smp_size <- floor(0.70 * nrow(merged_log_train_events_counts))
  train_ind <- sample(seq_len(nrow(merged_log_train_events_counts)), size = smp_size)
  train <- merged_log_train_events_counts[train_ind, ]
  test <- merged_log_train_events_counts[-train_ind, ]
  glmfit2 = glm(class ~ events_count, data=train,family="binomial")
  prob=predict(glmfit2,newdata=test,type=c("response"))
  test$prob = prob
  g = roc(class ~ prob, data=test)
  plot(g)
  return (test)
}

fit_logistic_course_id = function(merged_enrollment){
  glmfit2 = glm(class ~ course_id, data=merged_enrollment,family="binomial")
  prob=predict(glmfit2,type=c("response"))
  merged_enrollment$prob = prob
  g = roc(class ~ prob, data=merged_enrollment)
  plot(g)
}

fit_logistic_course_id_event_counts = function(merged_enrollment_log_train_events_counts){
  glmfit2 = glm(class ~ course_id+events_count, data=merged_enrollment_log_train_events_counts,family="binomial")
  prob=predict(glmfit2,type=c("response"))
  merged_enrollment_log_train_events_counts$prob = prob
  g = roc(class ~ prob, data=merged_enrollment_log_train_events_counts)
  plot(g)
}

fit_logistic_glmnet(){
  glmfit2 = cv.glmnet(x=model.matrix(~ course_id+events_count ,merged_enrollment_log_train_events_counts), y=merged_enrollment_log_train_events_counts[,c("class")], family="binomial", type.measure="auc")
  coeff(glmfit2,s="lambda.min")
  prob=predict(glmfit2,type=c("response"), newx=model.matrix(~ course_id+events_count , merged_enrollment_log_train_events_counts),s="lambda.min")
  merged_enrollment_log_train_events_counts$prob = prob
  g = roc(class ~ prob, data=merged_enrollment_log_train_events_counts)
  plot(g)
}

get_events_count=function(log_train,truth_train){
  log_train_enrollment = log_train$enrollment_id
  log_train_enrollment_counts = table(log_train_enrollment)
  log_train_enrollment_counts = as.data.frame(log_train_enrollment_counts)
  colnames(log_train_enrollment_counts) = c("enrollment_id","events_count")
  log_train_enrollment_counts$enrollment_id = as.numeric(as.character(log_train_enrollment_counts$enrollment_id))
  merged_log_train_events_counts = merge(x=log_train_enrollment_counts,y=truth_train)
  return(merged_log_train_events_counts)
}

get_train_test_score = function(log_train,truth_train){
  merged_log_train_events_counts = get_events_count(log_train,truth_train)
  test=fit_logistic_event_counts_split(merged_log_train_events_counts)
  return(test)
}

get_test_score = function(log_train,truth_train,log_test){
  merged_log_train_events_counts = get_events_count(log_train,truth_train)
  glmfit2=glm(class ~ events_count, data=merged_log_train_events_counts,family="binomial")
  log_test_event_counts=get_events_count_noClass(log_test)
  prob=predict(glmfit2,newdata=log_test_event_counts,type=c("response"))
  log_test_event_counts$prob = prob
  write.table(log_test_event_counts[,c("enrollment_id","prob")], file="test_out.csv", row.names=FALSE, col.names=FALSE, sep=",")
  return(test)
}

get_events_count_noClass=function(log_train){
  log_train_enrollment = log_train$enrollment_id
  log_train_enrollment_counts = table(log_train_enrollment)
  log_train_enrollment_counts = as.data.frame(log_train_enrollment_counts)
  colnames(log_train_enrollment_counts) = c("enrollment_id","events_count")
  log_train_enrollment_counts$enrollment_id = as.numeric(as.character(log_train_enrollment_counts$enrollment_id))
  return(log_train_enrollment_counts)
}

get_test_score_eventCount_maxDuration=function(log_train,log_test,truth_train){
  merged_log_train_events_counts = get_events_count(log_train,truth_train)
  merged_log_train_maxDuration = get_max_duartion(log_train,truth_train)
  merged_log_train_events_counts_maxDuration = merge(merged_log_train_events_counts,merged_log_train_maxDuration[,c(1,2)])
  glmfit2 = glm(class ~ maxTimeDiff + events_count, data=merged_log_train_events_counts_maxDuration,family="binomial")
  log_test_event_counts=get_events_count_noClass(log_test)
  log_test_maxDuration=get_maxDuration_noClass(log_test)
  merged_log_test_events_counts_maxDuration = merge(log_test_event_counts,log_test_maxDuration[,c("enrollment_id","maxTimeDiff")])
  prob=predict(glmfit2,newdata=merged_log_test_events_counts_maxDuration,type=c("response"))
  merged_log_test_events_counts_maxDuration$prob = prob
  write.table(merged_log_test_events_counts_maxDuration[,c("enrollment_id","prob")], file="test_out_eventCount_maxDuration.csv", row.names=FALSE, col.names=FALSE, sep=",")
  return(test)
}

get_maxDuration_noClass=function(log_train){
  log_train$time = as.character(log_train$time)
  log_train$time = gsub("T"," ",log_train$time)
  log_train$time=strptime(log_train$time,format="%Y-%m-%d %H:%M:%S")
  DT_log_train = data.table(log_train)
  DT_log_train$time = as.POSIXct(DT_log_train$time)
  log_train_maxDuration=DT_log_train[,.(maxTimeDiff=as.numeric(difftime(max(time),min(time)))),by=enrollment_id]
  log_train_maxDuration = data.frame(log_train_maxDuration)
  return(log_train_maxDuration)
}

get_max_duartion=function(log_train,truth_train){
  log_train$time = as.character(log_train$time)
  log_train$time = gsub("T"," ",log_train$time)
  log_train$time=strptime(log_train$time,format="%Y-%m-%d %H:%M:%S")
  DT_log_train = data.table(log_train)
  DT_log_train$time = as.POSIXct(DT_log_train$time)
  log_train_maxDuration=DT_log_train[,.(maxTimeDiff=as.numeric(difftime(max(time),min(time)))),by=enrollment_id]
  merged_log_train_maxDuration = merge(log_train_maxDuration,truth_train,by=intersect(names(truth_train),names(log_train_maxDuration)))
  merged_log_train_maxDuration = data.frame(merged_log_train_maxDuration)
  return(merged_log_train_maxDuration)
}

get_featured_event_and_category_count=function(log_train,object_,truth_train){
  object_missing=unique(log_train[log_train$object %in% c(setdiff(unique(log_train$object), unique(object_$module_id))),c("course_id","object")])
  object_missing_full=data.frame(course_id=object_missing$course_id, module_id=object_missing$object, category=object_missing$object, children=NA, start=NA)
  object_missing_full$start = factor(object_missing_full$start)
  object_missing_full$children = factor(object_missing_full$children)
  object_full = rbind(object_,object_missing_full)
  merge_log_module_type = merge(x=log_train[,c("enrollment_id","event","object","course_id")], y=unique(object_full[,c("module_id","category","course_id")]), by.x=c("object","course_id"), by.y=c("module_id","course_id"))
  merge_log_module_type_labels = merge(merge_log_module_type,truth_train)
  merge_log_module_type_counts_labels = ddply(merge_log_module_type_labels, .(enrollment_id,category,class), summarize, category_count=length(enrollment_id))
  merge_log_module_type_counts_labels_featured = dcast(merge_log_module_type_counts_labels, enrollment_id+class ~ category, value.var="category_count")
  merged_log_train_events_counts_category_count_labels = merge(x=merged_enrollment_log_train_events_counts[,c("enrollment_id","class","events_count")], y=merge_log_module_type_counts_labels_featured, by.x=c("enrollment_id","class"), by.y=c("enrollment_id","class"))
  return(merged_log_train_events_counts_category_count_labels)
}

get_glmnet_event_and_category_count=function(log_train,object_,truth_train){
#   object_missing=unique(log_train[log_train$object %in% c(setdiff(unique(log_train$object), unique(object_$module_id))),c("course_id","object")])
#   object_missing_full=data.frame(course_id=object_missing$course_id, module_id=object_missing$object, category=object_missing$object, children=NA, start=NA)
#   object_missing_full$start = factor(object_missing_full$start)
#   object_missing_full$children = factor(object_missing_full$children)
#   object_full = rbind(object_,object_missing_full)
#   merge_log_module_type = merge(x=log_train[,c("enrollment_id","event","object","course_id")], y=unique(object_full[,c("module_id","category","course_id")]), by.x=c("object","course_id"), by.y=c("module_id","course_id"))
#   merge_log_module_type_labels = merge(merge_log_module_type,truth_train)
#   merge_log_module_type_counts_labels = ddply(merge_log_module_type_labels, .(enrollment_id,category,class), summarize, category_count=length(enrollment_id))
#   merge_log_module_type_counts_labels_featured = dcast(merge_log_module_type_counts_labels, enrollment_id+class ~ category, value.var="category_count")
#   merged_log_train_events_counts_category_count_labels = merge(x=merged_enrollment_log_train_events_counts[,c("enrollment_id","class","events_count")], y=merge_log_module_type_counts_labels_featured, by.x=c("enrollment_id","class"), by.y=c("enrollment_id","class"))
  merged_log_train_events_counts_category_count_labels = get_featured_event_and_category_count(log_train,object_,truth_train)
  glmfit4 = cv.glmnet(x=data.matrix(merged_log_train_events_counts_category_count_labels[,-c(1,2)]), y=merged_log_train_events_counts_category_count_labels[,c("class")], family="binomial", type.measure="auc")
  prob=predict(glmfit4,type=c("response"), newx=data.matrix(merged_log_train_events_counts_category_count_labels[,-c(1,2)]),s="lambda.min")
  merged_log_train_events_counts_category_count_labels$prob = prob
  g = roc(class ~ prob, data=merged_log_train_events_counts_category_count_labels)
  plot(g)
}

get_gbm_even_and_category_count=function(log_train,object_,truth_train){
  merged_log_train_events_counts_category_count_labels = get_featured_event_and_category_count(log_train,object_,truth_train)
  inTraining <- createDataPartition(merged_log_train_events_counts_category_count_labels[,2:178]$class, p = .4, list = FALSE)
  training_event_category = merged_log_train_events_counts_category_count_labels[inTraining,2:178]
  test_event_category = merged_log_train_events_counts_category_count_labels[-inTraining,2:178]
  training_event_category$class = as.factor(training_event_category$class)
  test_event_category$class = as.factor(test_event_category$class)
  # the below is important for probabilistic prediction
  levels(training_event_category$class) = c("NO","YES")
  levels(test_event_category$class) = c("NO","YES")
  gbmGrid <-  expand.grid(interaction.depth = c(3,6,9), n.trees = c(2,7,12)*50, shrinkage = c(0.01,0.05,0.1), n.minobsinnode = 10)
  # expand the above gbmGrid; have more trees and tree depth
  fitControl <- trainControl(method = "cv", number = 5, repeats = 5, classProbs = TRUE)
  registerDoMC(cores=8)
  gbmFit <- train(class ~ ., data = training_event_category, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = "ROC")
  # , allowParallel=TRUE is not a good idea above
}

# for parallel execution:
# library(doMC)
# registerDoMC(cores=8)
