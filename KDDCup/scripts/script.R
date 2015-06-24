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

fit_logistic_glmnet = function(){
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

load_data = function(env = parent.frame()){
  env$log_train_new = read.csv("KDDdata09Jun15/log_train.csv", header=T)
  env$log_test_new = read.csv("KDDdata09Jun15/log_test.csv", header=T)
  env$enrollment_train_new = read.csv("KDDdata09Jun15/enrollment_train.csv", header=T)
  env$enrollment_test_new = read.csv("KDDdata09Jun15/enrollment_test.csv", header=T)
  env$object_new = read.csv("KDDdata09Jun15/object.csv", header=T)
  env$truth_train = read.csv("KDDdata09Jun15/truth_train.csv", header=F)
  env$max_and_min_course_time = read.csv("max_and_min_course_time.csv", header=T)
}

get_train_session_and_time_features=function(env = parent.frame(), saveFile=F){
  train_log_enrollment_new = merge(log_train_new, enrollment_train_new, by=c("enrollment_id"))
  train_log_enrollment_new$time = as.character(train_log_enrollment_new$time)
  train_log_enrollment_new$time = gsub("T"," ", train_log_enrollment_new$time)
  train_log_enrollment_new$time = strptime(train_log_enrollment_new$time,format="%Y-%m-%d %H:%M:%S")
  train_log_enrollment_new$time = as.POSIXct(train_log_enrollment_new$time)
  train_log_enrollment_new = data.table(train_log_enrollment_new)
  train_log_enrollment_new_max_course_time = train_log_enrollment_new[,(max_course_time := max(time)),by=course_id]
  colnames(train_log_enrollment_new_max_course_time)[8] = "max_course_time"
  train_log_enrollment_new_max_diff = train_log_enrollment_new_max_course_time[,.(max_time_diff = as.numeric(difftime(max_course_time, max(time), units="days"))),by=enrollment_id]
  train_log_enrollment_new_max_diff = unique(train_log_enrollment_new_max_diff)
  train_log_enrollment_new_course_time_max_diff = merge(train_log_enrollment_new_max_course_time,train_log_enrollment_new_max_diff,by=c("enrollment_id"))
  train_log_enrollment_new_course_time_max_diff_session_diff = train_log_enrollment_new_course_time_max_diff[,min_course_time:=as.POSIXct(min(time)),by="course_id"]
  train_log_enrollment_new_course_time_max_diff_session_diff = train_log_enrollment_new_course_time_max_diff[,prev_time:=as.POSIXct(c(0,time[-.N]), origin="1970-01-01 00:00.00"),by="enrollment_id"]
  train_log_enrollment_new_course_time_max_diff_session_diff = train_log_enrollment_new_course_time_max_diff_session_diff[,min_time_diff_days := as.numeric(difftime(min(time), min_course_time, units="days")),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff = train_log_enrollment_new_course_time_max_diff_session_diff[,session_delta:= as.numeric(difftime(time, prev_time, units="days"))]
  setkey(train_log_enrollment_new_course_time_max_diff_session_diff,enrollment_id,time)
  object_new_noChild = unique(object_new[,c("course_id","module_id","category","start")])
  colnames(test_log_enrollment_new_course_time_max_diff_session_diff)[6] = "module_id"
  train_log_enrollment_new_course_time_max_diff_session_diff_object = merge(train_log_enrollment_new_course_time_max_diff_session_diff,object_new_noChild,by=c("course_id","module_id"),all.x=T)
  setkey(train_log_enrollment_new_course_time_max_diff_session_diff_object,enrollment_id,time)
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,total_num_logs_noDeDups:= length(event),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_10days:=length(session_delta_days[session_delta_days>=10 & session_delta_days<31]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_9days:=length(session_delta_days[session_delta_days>=9 & session_delta_days<10]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_8days:=length(session_delta_days[session_delta_days>=8 & session_delta_days<9]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_7days:=length(session_delta_days[session_delta_days>=7 & session_delta_days<8]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_6days:=length(session_delta_days[session_delta_days>=6 & session_delta_days<7]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_5days:=length(session_delta_days[session_delta_days>=5 & session_delta_days<6]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_4days:=length(session_delta_days[session_delta_days>=4 & session_delta_days<5]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_3days:=length(session_delta_days[session_delta_days>=3 & session_delta_days<4]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_2days:=length(session_delta_days[session_delta_days>=2 & session_delta_days<3]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_1days:=length(session_delta_days[session_delta_days>=1 & session_delta_days<2]),by=enrollment_id]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[session_delta_days>31] = 0 #  this is for the first log action for every user sinc eit was set to 1970
#   > str(train_log_enrollment_new_course_time_max_diff_session_diff_object)
#   Classes ‘data.table’ and 'data.frame':  8157277 obs. of  26 variables:
#     $ course_id              : Factor w/ 39 levels "1pvLqtotBsKv7QSOsLicJDQMHx3lui6d",..: NA 16 16 16 16 16 16 16 16 16 ...
#   $ module_id              : Factor w/ 5890 levels "005x5aALZuJd7pV5KWd240POzjkEPql7",..: NA 350 4140 194 4266 4266 4266 4266 4266 4266 ...
#   $ enrollment_id          : int  0 1 1 1 1 1 1 1 1 1 ...
#   $ time                   : POSIXct, format: "1970-01-01 00:00:00" "2014-06-14 09:38:39" ...
#   $ source                 : Factor w/ 2 levels "browser","server": NA 2 2 2 1 1 1 1 1 1 ...
#   $ event                  : Factor w/ 7 levels "access","discussion",..: NA 1 1 1 5 5 5 5 5 5 ...
#   $ username               : Factor w/ 79186 levels "00038q9llTDdhWUJPTqFNEuMEA6pdK5n",..: NA 12472 12472 12472 12472 12472 12472 12472 12472 12472 ...
#   $ max_course_time        : POSIXct, format: "1970-01-01 00:00:00" "2014-07-11 23:59:46" ...
#   $ max_time_diff_days     : num  0 0.588 0.588 0.588 0.588 ...
#   $ prev_time              : POSIXct, format: "1970-01-01 00:00:00" "2014-06-14 09:38:29" ...
#   $ min_course_time        : POSIXct, format: "1970-01-01 00:00:00" "2014-06-12 00:01:46" ...
#   $ min_time_diff_days     : num  0 2.4 2.4 2.4 2.4 ...
#   $ session_delta_days     : num  0 0.000116 0 0.000104 0.002095 ...
#   $ category               : Factor w/ 15 levels "about","chapter",..: NA NA 12 12 11 11 11 11 11 11 ...
#   $ start                  : Factor w/ 421 levels "1999-09-04T00:00:00",..: NA NA 22 213 421 421 421 421 421 421 ...
#   $ total_num_logs_noDeDups: int  0 314 314 314 314 314 314 314 314 314 ...
#   $ session_delta_10days   : int  0 0 0 0 0 0 0 0 0 0 ...
#   $ session_delta_9days    : int  0 0 0 0 0 0 0 0 0 0 ...
#   $ session_delta_8days    : int  0 0 0 0 0 0 0 0 0 0 ...
#   $ session_delta_7days    : int  0 0 0 0 0 0 0 0 0 0 ...
#   $ session_delta_6days    : int  0 1 1 1 1 1 1 1 1 1 ...
#   $ session_delta_5days    : int  0 2 2 2 2 2 2 2 2 2 ...
#   $ session_delta_4days    : int  0 2 2 2 2 2 2 2 2 2 ...
#   $ session_delta_3days    : int  0 3 3 3 3 3 3 3 3 3 ...
#   $ session_delta_2days    : int  0 5 5 5 5 5 5 5 5 5 ...
#   $ session_delta_1days    : int  0 3 3 3 3 3 3 3 3 3 ...
#   - attr(*, ".internal.selfref")=<externalptr> 
#     - attr(*, "index")= atomic  
#   ..- attr(*, "enrollment_id")= int  1 315 603 702 1335 1358 1837 1934 2061 2524 ...
#   train_log_enrollment_new_course_time_max_diff_session_diff_object$time = as.POSIXct(train_log_enrollment_new_course_time_max_diff_session_diff_object$time, format="%Y-%m-%d %H:%M:%S")
#   train_log_enrollment_new_course_time_max_diff_session_diff_object$min_course_time = as.POSIXct(train_log_enrollment_new_course_time_max_diff_session_diff_object$min_course_time, format="%Y-%m-%d %H:%M:%S")
#   train_log_enrollment_new_course_time_max_diff_session_diff_object$max_course_time = as.POSIXct(train_log_enrollment_new_course_time_max_diff_session_diff_object$max_course_time, format="%Y-%m-%d %H:%M:%S")
#   train_log_enrollment_new_course_time_max_diff_session_diff_object$prev_time = as.POSIXct(train_log_enrollment_new_course_time_max_diff_session_diff_object$prev_time, format="%Y-%m-%d %H:%M:%S")
  # note first calculate the timediffs and then do the dedup
  train_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_days[train_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_day>31]=0
  train_log_enrollment_new_course_time_max_diff_session_diff_object[session_delta_days>=1,session_num:=1:.N,by=enrollment_id]
  setkey(train_log_enrollment_new_course_time_max_diff_session_diff_object,enrollment_id,time)
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,filled_session_num:=train_log_enrollment_new_course_time_max_diff_session_diff_object[!is.na(session_num)][train_log_enrollment_new_course_time_max_diff_session_diff_object, session_num, roll=+Inf, allow.cartesian=TRUE]]
  train_log_enrollment_new_course_time_max_diff_session_diff_object$filled_session_num[is.na(train_log_enrollment_new_course_time_max_diff_session_diff_object$filled_session_num)] <- 0
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,session_duartion:=sum(session_delta_days[session_delta_days<1]),by=list(enrollment_id,filled_session_num)]
  train_log_enrollment_new_course_time_max_diff_session_diff_object$category = as.character(train_log_enrollment_new_course_time_max_diff_session_diff_object$category)
  train_log_enrollment_new_course_time_max_diff_session_diff_object$category[is.na(train_log_enrollment_new_course_time_max_diff_session_diff_object$category)] <- c("DUMMY_CATEGORY")
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,category_count:=nrow(.SD),by=list(enrollment_id,category)]
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,event_count:=nrow(.SD),by=list(enrollment_id,event)] # NOTE: it deosn't take into account the source of the event 
  train_log_enrollment_new_course_time_max_diff_session_diff_object[,course_count:=1]
  if(saveFile){
    write.csv(train_log_enrollment_new_course_time_max_diff_session_diff_object, file="train_log_enrollment_new_course_time_max_diff_session_delta_1to10_duration_event_count_category_count_courseid.csv", row.names=F)
  }
  #train_max_min_timediff_sessions_durations_expanded = train_log_enrollment_new_course_time_max_diff_session_diff_object[,c(1,3,6,8,9,11,12,14,16:26,28:32), with=F]
  train_max_min_timediff_sessions_durations_expanded = train_log_enrollment_new_course_time_max_diff_session_diff_object[,c("enrollment_id", "course_id", "max_course_time", "max_time_diff_days", "min_course_time", "min_time_diff_days", "total_num_logs_noDeDups", "session_delta_10days", "session_delta_9days", "session_delta_8days", "session_delta_7days", "session_delta_6days", "session_delta_5days", "session_delta_4days", "session_delta_3days", "session_delta_2days", "session_delta_1days","session_duartion", "category_count", "course_count", "event_count", "event", "category", "filled_session_num"), with=F]
  # c("enrollment_id", "course_id", "max_course_time", "max_time_diff_days", "min_course_time", "min_time_diff_days", "total_num_logs_noDeDups", "session_delta_10days", "session_delta_9days", "session_delta_8days", "session_delta_7days", "session_delta_6days", "session_delta_5days", "session_delta_4days", "session_delta_3days", "session_delta_2days", "session_delta_1days","session_duartion", "category_count", "course_count", "event_count", "event", "category", "filled_session_num")
  train_max_min_timediff_sessions_durations_expanded = unique(train_max_min_timediff_sessions_durations_expanded)
  setkey(train_max_min_timediff_sessions_durations_expanded)  # this sets all the cols as keys
  subset_event_count = dcast.data.table(train_max_min_timediff_sessions_durations_expanded, enrollment_id~event, fun=mean, value.var="event_count", fill=0)
  subset_category_count = dcast.data.table(train_max_min_timediff_sessions_durations_expanded, enrollment_id~category, fun=mean, value.var="category_count", fill=0)
  subset_session_num_duration = dcast.data.table(train_max_min_timediff_sessions_durations_expanded, enrollment_id~filled_session_num, fun=mean, value.var="session_duartion", fill=0)
  subset_course_count = dcast.data.table(train_max_min_timediff_sessions_durations_expanded, enrollment_id~course_id, fun=mean, value.var="course_count", fill=0)
  colnames(subset_session_num_duration)[2:17] <- paste("session",colnames(subset_session_num_duration)[2:17],sep="_")
  train_max_min_timediff_sessions_durations_expanded_categoryCount = merge(train_max_min_timediff_sessions_durations_expanded, subset_category_count, by=c("enrollment_id"))  
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount = merge(train_max_min_timediff_sessions_durations_expanded_categoryCount, subset_course_count, by=c("enrollment_id"))
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount = merge(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount, subset_event_count, by=c("enrollment_id"))
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, session_duartion:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, category_count:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[,course_count:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, event_count:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount = merge(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount, subset_session_num_duration, by=c("enrollment_id"))
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = merge(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount, truth_train, by=c("enrollment_id"))
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,event:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,category:=NULL]
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,filled_session_num:=NULL]
  setkey(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = unique(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  if(saveFile){
    write.csv(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label, file="train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label.csv", row.names=F)
  }
  train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = data.frame(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  inTraining <- createDataPartition(train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,2:86]$class, p = .7, list = FALSE)
  training = train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[inTraining,2:86]
  test = train_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[-inTraining,2:86]
  training$class = as.factor(training$class)
  test$class = as.factor(test$class)
  levels(training$class) = c("NO","YES")
  levels(test$class) = c("NO","YES")
  gbmGrid <-  expand.grid(interaction.depth = c(3,6,9), n.trees = c(2,7,12,17,22,27,32)*50, shrinkage = c(0.1), n.minobsinnode = 10)
  fitControl <- trainControl(method = "cv", number = 5, repeats = 5, classProbs = TRUE)
  #fitControl <- trainControl(method = "cv", number = 5, summaryFunction=twoClassSummary, repeats = 5, classProbs = TRUE)
  gbmFit <- train(class ~ ., data = training, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = "ROC")
  prob  = predict(gbmFit, newdata = test, type = "prob")[2]
  levels(test$class) = c(0,1)
  test$prob = prob$YES
  g = roc(class ~ prob, data=test)
  plot(g)
  return(train_log_enrollment_new_course_time_max_diff)
}

get_test_session_and_time_features=function(env = parent.frame(), saveFile=T){
  env$test_log_enrollment_new = merge(log_test_new, enrollment_test_new, by=c("enrollment_id"))
  print("test_log_enrollment_new done")
  flush.console()
  env$test_log_enrollment_new$time = as.character(test_log_enrollment_new$time)
  env$test_log_enrollment_new$time = gsub("T"," ", test_log_enrollment_new$time)
  env$test_log_enrollment_new$time = strptime(test_log_enrollment_new$time,format="%Y-%m-%d %H:%M:%S")
  env$test_log_enrollment_new$time = as.POSIXct(test_log_enrollment_new$time)
  print("test_log_enrollment_new$time done")
  flush.console()
  #test_log_enrollment_new = data.table(test_log_enrollment_new) # This is giving error in merge below
  #test_log_enrollment_new_max_course_time = test_log_enrollment_new[,(max_course_time := max(time)),by=course_id]
  #colnames(test_log_enrollment_new_max_course_time)[8] = "max_course_time"
  #test_log_enrollment_new_max_course_time = data.table(test_log_enrollment_new_max_course_time)
  env$test_log_enrollment_new_max_course_time  = merge(test_log_enrollment_new, max_and_min_course_time,by=c("course_id"))
  env$test_log_enrollment_new_max_course_time = data.table(test_log_enrollment_new_max_course_time) # very important because it makes sure that prev_time is calculated correctly as it ensures sorting 
  env$test_log_enrollment_new_max_diff = test_log_enrollment_new_max_course_time[,.(max_time_diff_days = as.numeric(difftime(max_course_time, max(time), units="days"))),by=enrollment_id]
  env$test_log_enrollment_new_max_diff = unique(test_log_enrollment_new_max_diff)
  env$test_log_enrollment_new_course_time_max_diff = merge(test_log_enrollment_new_max_course_time,test_log_enrollment_new_max_diff,by=c("enrollment_id"))
  print("test_log_enrollment_new_course_time_max_diff done")
  flush.console()
  #test_log_enrollment_new_course_time_max_diff_session_diff = test_log_enrollment_new_course_time_max_diff[,min_course_time:=as.POSIXct(min(time)),by="course_id"]
  # We dont need the above as we have already merged max_and_min_course_time
  setkey(test_log_enrollment_new_course_time_max_diff,enrollment_id,time)
  env$test_log_enrollment_new_course_time_max_diff_session_diff = test_log_enrollment_new_course_time_max_diff[,prev_time:=as.POSIXct(c(0,time[-.N]), origin="1970-01-01 00:00.00"),by="enrollment_id"]
  print("test_log_enrollment_new_course_time_max_diff_session_diff$prev_time done")
  flush.console()
  # We dont need to calculate min_time_diff_days again
  env$test_log_enrollment_new_course_time_max_diff_session_diff[,min_time_diff_days := as.numeric(difftime(min(time), min_course_time, units="days")),by=enrollment_id]
  print("test_log_enrollment_new_course_time_max_diff_session_diff$min_time_diff_days done")
  flush.console()
  env$test_log_enrollment_new_course_time_max_diff_session_diff[,session_delta_days:= as.numeric(difftime(time, prev_time, units="days"))]
  print("test_log_enrollment_new_course_time_max_diff_session_diff$session_delta_days done")
  flush.console()
  # By this time check if we have any bad prev_time or negative session_delta
  # subset(test_log_enrollment_new_course_time_max_diff_session_diff, !is.na(session_delta) & session_delta<0)[1:1] # enrollment_id=19
  # test_log_enrollment_new_course_time_max_diff_session_diff[test_log_enrollment_new_course_time_max_diff_session_diff$enrollment_id==22806][1:3]
  # setkey(test_log_enrollment_new_course_time_max_diff_session_diff,enrollment_id,time) # this needs to go before we calulate prev_time
  env$object_new_noChild = unique(object_new[,c("course_id","module_id","category","start")])
  # colnames(test_log_enrollment_new_course_time_max_diff_session_diff)[6] = "module_id"
  setnames(test_log_enrollment_new_course_time_max_diff_session_diff,old="object",new="module_id")
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object = merge(test_log_enrollment_new_course_time_max_diff_session_diff,object_new_noChild,by=c("course_id","module_id"),all.x=T)
  print("test_log_enrollment_new_course_time_max_diff_session_diff_object done")
  flush.console()
  setkey(test_log_enrollment_new_course_time_max_diff_session_diff_object,enrollment_id,time)
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,total_num_logs_noDeDups:= length(event),by=enrollment_id]
  print("total_num_logs_noDeDups done")
  flush.console()
  #colnames(test_log_enrollment_new_course_time_max_diff_session_diff_object)[13] = "session_delta_days"
  #test_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_days[test_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_days>31] = 0
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_10days:=length(session_delta_days[session_delta_days>=10 & session_delta_days<31]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_9days:=length(session_delta_days[session_delta_days>=9 & session_delta_days<10]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_8days:=length(session_delta_days[session_delta_days>=8 & session_delta_days<9]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_7days:=length(session_delta_days[session_delta_days>=7 & session_delta_days<8]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_6days:=length(session_delta_days[session_delta_days>=6 & session_delta_days<7]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_5days:=length(session_delta_days[session_delta_days>=5 & session_delta_days<6]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_4days:=length(session_delta_days[session_delta_days>=4 & session_delta_days<5]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_3days:=length(session_delta_days[session_delta_days>=3 & session_delta_days<4]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_2days:=length(session_delta_days[session_delta_days>=2 & session_delta_days<3]),by=enrollment_id]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_delta_1days:=length(session_delta_days[session_delta_days>=1 & session_delta_days<2]),by=enrollment_id]
  print("session_delta_1days done")
  flush.console()
  #test_log_enrollment_new_course_time_max_diff_session_diff_object[session_delta_days>31] = 0 #  this is for the first log action for every user sinc eit was set to 1970
  #   > str(test_log_enrollment_new_course_time_max_diff_session_diff_object)
  #   Classes ‘data.table’ and 'data.frame':  8157277 obs. of  26 variables:
  #     $ course_id              : Factor w/ 39 levels "1pvLqtotBsKv7QSOsLicJDQMHx3lui6d",..: NA 16 16 16 16 16 16 16 16 16 ...
  #   $ module_id              : Factor w/ 5890 levels "005x5aALZuJd7pV5KWd240POzjkEPql7",..: NA 350 4140 194 4266 4266 4266 4266 4266 4266 ...
  #   $ enrollment_id          : int  0 1 1 1 1 1 1 1 1 1 ...
  #   $ time                   : POSIXct, format: "1970-01-01 00:00:00" "2014-06-14 09:38:39" ...
  #   $ source                 : Factor w/ 2 levels "browser","server": NA 2 2 2 1 1 1 1 1 1 ...
  #   $ event                  : Factor w/ 7 levels "access","discussion",..: NA 1 1 1 5 5 5 5 5 5 ...
  #   $ username               : Factor w/ 79186 levels "00038q9llTDdhWUJPTqFNEuMEA6pdK5n",..: NA 12472 12472 12472 12472 12472 12472 12472 12472 12472 ...
  #   $ max_course_time        : POSIXct, format: "1970-01-01 00:00:00" "2014-07-11 23:59:46" ...
  #   $ max_time_diff_days     : num  0 0.588 0.588 0.588 0.588 ...
  #   $ prev_time              : POSIXct, format: "1970-01-01 00:00:00" "2014-06-14 09:38:29" ...
  #   $ min_course_time        : POSIXct, format: "1970-01-01 00:00:00" "2014-06-12 00:01:46" ...
  #   $ min_time_diff_days     : num  0 2.4 2.4 2.4 2.4 ...
  #   $ session_delta_days     : num  0 0.000116 0 0.000104 0.002095 ...
  #   $ category               : Factor w/ 15 levels "about","chapter",..: NA NA 12 12 11 11 11 11 11 11 ...
  #   $ start                  : Factor w/ 421 levels "1999-09-04T00:00:00",..: NA NA 22 213 421 421 421 421 421 421 ...
  #   $ total_num_logs_noDeDups: int  0 314 314 314 314 314 314 314 314 314 ...
  #   $ session_delta_10days   : int  0 0 0 0 0 0 0 0 0 0 ...
  #   $ session_delta_9days    : int  0 0 0 0 0 0 0 0 0 0 ...
  #   $ session_delta_8days    : int  0 0 0 0 0 0 0 0 0 0 ...
  #   $ session_delta_7days    : int  0 0 0 0 0 0 0 0 0 0 ...
  #   $ session_delta_6days    : int  0 1 1 1 1 1 1 1 1 1 ...
  #   $ session_delta_5days    : int  0 2 2 2 2 2 2 2 2 2 ...
  #   $ session_delta_4days    : int  0 2 2 2 2 2 2 2 2 2 ...
  #   $ session_delta_3days    : int  0 3 3 3 3 3 3 3 3 3 ...
  #   $ session_delta_2days    : int  0 5 5 5 5 5 5 5 5 5 ...
  #   $ session_delta_1days    : int  0 3 3 3 3 3 3 3 3 3 ...
  #   - attr(*, ".internal.selfref")=<externalptr> 
  #     - attr(*, "index")= atomic  
  #   ..- attr(*, "enrollment_id")= int  1 315 603 702 1335 1358 1837 1934 2061 2524 ...
  #   env$test_log_enrollment_new_course_time_max_diff_session_diff_object$time = as.POSIXct(test_log_enrollment_new_course_time_max_diff_session_diff_object$time, format="%Y-%m-%d %H:%M:%S")
  #   env$test_log_enrollment_new_course_time_max_diff_session_diff_object$min_course_time = as.POSIXct(test_log_enrollment_new_course_time_max_diff_session_diff_object$min_course_time, format="%Y-%m-%d %H:%M:%S")
  #   env$test_log_enrollment_new_course_time_max_diff_session_diff_object$max_course_time = as.POSIXct(test_log_enrollment_new_course_time_max_diff_session_diff_object$max_course_time, format="%Y-%m-%d %H:%M:%S")
  #   env$test_log_enrollment_new_course_time_max_diff_session_diff_object$prev_time = as.POSIXct(test_log_enrollment_new_course_time_max_diff_session_diff_object$prev_time, format="%Y-%m-%d %H:%M:%S")
  # note first calculate the timediffs and then do the dedup
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_days[test_log_enrollment_new_course_time_max_diff_session_diff_object$session_delta_day>31]=0
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[session_delta_days>=1,session_num:=1:.N,by=enrollment_id]
  print("session_num done")
  flush.console()
  setkey(test_log_enrollment_new_course_time_max_diff_session_diff_object,enrollment_id,time)
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,filled_session_num:=test_log_enrollment_new_course_time_max_diff_session_diff_object[!is.na(session_num)][test_log_enrollment_new_course_time_max_diff_session_diff_object, session_num, roll=+Inf, allow.cartesian=TRUE]]
  print("filled_session_num done")
  flush.console()
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object$filled_session_num[is.na(test_log_enrollment_new_course_time_max_diff_session_diff_object$filled_session_num)] <- 0
  print("filled_session_num 0 done")
  flush.console()
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,session_duartion:=sum(session_delta_days[session_delta_days<1]),by=list(enrollment_id,filled_session_num)]
  print("session_duartion done")
  flush.console()
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object$category = as.character(test_log_enrollment_new_course_time_max_diff_session_diff_object$category)
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object$category[is.na(test_log_enrollment_new_course_time_max_diff_session_diff_object$category)] <- c("DUMMY_CATEGORY")
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,category_count:=nrow(.SD),by=list(enrollment_id,category)]
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,event_count:=nrow(.SD),by=list(enrollment_id,event)] # NOTE: it deosn't take into account the source of the event 
  env$test_log_enrollment_new_course_time_max_diff_session_diff_object[,course_count:=1]
  print("category_count,event_count,course_count done")
  flush.console()
  fileName = "test_log_enrollment_new_course_time_max_diff_session_delta_1to10_duration_event_count_category_count_courseid.csv"
  fileName = gsub(".csv",paste("_",gsub(" ","T", as.character(Sys.time())),".csv",sep=""),fileName)
  write.csv(test_log_enrollment_new_course_time_max_diff_session_diff_object, file=fileName, row.names=F)
  print("test_log_enrollment_new_course_time_max_diff_session_delta_1to10_duration_event_count_category_count_courseid.csv writing done")
  flush.console()
  #test_max_min_timediff_sessions_durations_expanded = test_log_enrollment_new_course_time_max_diff_session_diff_object[,c(1,3,6,8,9,11,12,14,16:26,28:32), with=F]
  env$test_max_min_timediff_sessions_durations_expanded = test_log_enrollment_new_course_time_max_diff_session_diff_object[,c("enrollment_id", "course_id", "max_course_time", "max_time_diff_days", "min_course_time", "min_time_diff_days", "total_num_logs_noDeDups", "session_delta_10days", "session_delta_9days", "session_delta_8days", "session_delta_7days", "session_delta_6days", "session_delta_5days", "session_delta_4days", "session_delta_3days", "session_delta_2days", "session_delta_1days","session_duartion", "category_count", "course_count", "event_count", "event", "category", "filled_session_num"), with=F]
  # c("enrollment_id", "course_id", "max_course_time", "max_time_diff_days", "min_course_time", "min_time_diff_days", "total_num_logs_noDeDups", "session_delta_10days", "session_delta_9days", "session_delta_8days", "session_delta_7days", "session_delta_6days", "session_delta_5days", "session_delta_4days", "session_delta_3days", "session_delta_2days", "session_delta_1days","session_duartion", "category_count", "course_count", "event_count", "event", "category", "filled_session_num")
  # "session_duartion", "category_count", "course_count", "event_count", "event", "category","filled_session_num"
  env$test_max_min_timediff_sessions_durations_expanded = unique(test_max_min_timediff_sessions_durations_expanded)
  setkey(test_max_min_timediff_sessions_durations_expanded)  # this sets all the cols as keys
  env$test_subset_event_count = dcast.data.table(test_max_min_timediff_sessions_durations_expanded, enrollment_id~event, fun=mean, value.var="event_count", fill=0)
  env$test_subset_category_count = dcast.data.table(test_max_min_timediff_sessions_durations_expanded, enrollment_id~category, fun=mean, value.var="category_count", fill=0)
  env$test_subset_session_num_duration = dcast.data.table(test_max_min_timediff_sessions_durations_expanded, enrollment_id~filled_session_num, fun=mean, value.var="session_duartion", fill=0)
  env$test_subset_course_count = dcast.data.table(test_max_min_timediff_sessions_durations_expanded, enrollment_id~course_id, fun=mean, value.var="course_count", fill=0)
  colnames(test_subset_session_num_duration)
  #colnames(test_subset_session_num_duration)[2:17] <- paste("session",colnames(test_subset_session_num_duration)[2:17],sep="_")
  setnames(test_subset_session_num_duration, old=paste(c(0:15)), new=paste("session",colnames(test_subset_session_num_duration)[2:17],sep="_"))
  print("session_num renaming done")
  flush.console()
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount = merge(test_max_min_timediff_sessions_durations_expanded, test_subset_category_count, by=c("enrollment_id"))  
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount = merge(test_max_min_timediff_sessions_durations_expanded_categoryCount, test_subset_course_count, by=c("enrollment_id"))
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount = merge(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount, test_subset_event_count, by=c("enrollment_id"))
  print("category_count,event_count,course_count expansion done")
  flush.console()
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, session_duartion:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, category_count:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[,course_count:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount[, event_count:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = merge(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount, test_subset_session_num_duration, by=c("enrollment_id"))
  print("session expansion done")
  flush.console()
  #test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = merge(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount, truth_test, by=c("enrollment_id"))
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,event:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,category:=NULL]
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,filled_session_num:=NULL]
  setkey(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = unique(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  fileName = "test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount.csv"
  #if(file.exists(fileName)){
  fileName = gsub(".csv",paste("_",gsub(" ","T", as.character(Sys.time())),".csv",sep=""),fileName)
  #}
  write.csv(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label, file=fileName, row.names=F)
  print("test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount.csv writing done")
  flush.console()
  env$test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label = data.frame(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label)
  #intesting <- createDataPartition(test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,2:86]$class, p = .7, list = FALSE)
  env$testing_main = test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[,2:86]
  #test = test_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount_label[-intesting,2:86]
  env$testing_main$class = as.factor(testing_main$class)
  #test$class = as.factor(test$class)
  levels(testing_main$class) = c("NO","YES")
  #levels(test$class) = c("NO","YES")
  #gbmGrid <-  expand.grid(interaction.depth = c(3,6,9), n.trees = c(2,7,12,17,22,27,32)*50, shrinkage = c(0.1), n.minobsinnode = 10)
  #fitControl <- testControl(method = "cv", number = 5, repeats = 5, classProbs = TRUE)
  #fitControl <- testControl(method = "cv", number = 5, summaryFunction=twoClassSummary, repeats = 5, classProbs = TRUE)
  #gbmFit <- test(class ~ ., data = testing, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = "ROC")
  env$probTest  = predict(env$gbmFit, newdata = env$testing_main, type = "prob")[2]
  print("predictions done")
  flush.console()
  levels(testing_main$class) = c(0,1)
  testing_main$probTest = probTest$YES
  #g = roc(class ~ prob, data=testing_main)
  #plot(g)
  fileName = "test_out_max_min_timediff_sessions_durations_expanded_categoryCount_courseCount_eventCount_sessionCount.csv"
  #if(file.exists(fileName)){
  fileName = gsub(".csv",paste("_",gsub(" ","T", as.character(Sys.time())),".csv",sep=""),fileName)
  #}
  write.table(testing_main[,c("enrollment_id","probTest")], file="", row.names=FALSE, col.names=FALSE, sep=",")
  print("write table testing_main done")
  flush.console()
  return(test_log_enrollment_new_course_time_max_diff)
}

test_concepts_fun = function(env=parent.frame()){
  env$test_max_min_timediff_sessions_durations_expanded = test_log_enrollment_new_course_time_max_diff_session_diff_object[,c("enrollment_id", "course_id", "max_course_time", "max_time_diff_days", "min_course_time", "min_time_diff_days", "total_num_logs_noDeDups", "session_delta_10days", "session_delta_9days", "session_delta_8days", "session_delta_7days", "session_delta_6days", "session_delta_5days", "session_delta_4days", "session_delta_3days", "session_delta_2days", "session_delta_1days","session_duartion", "category_count", "course_count", "event_count", "event", "category", "filled_session_num"), with=F]
  env$test_subset_session_num_duration = dcast.data.table(test_max_min_timediff_sessions_durations_expanded, enrollment_id~filled_session_num, fun=mean, value.var="session_duartion", fill=0)
}

# for parallel execution:
# library(stringr)
# library(doMC)
# registerDoMC(cores=8)
# library(pROC)
# library("gbm")
# library("caret")
# library("caret")
# library(doMC)
# library("corrplot")
# library(FactoMineR)
# library(plyr)
# library(data.table)

