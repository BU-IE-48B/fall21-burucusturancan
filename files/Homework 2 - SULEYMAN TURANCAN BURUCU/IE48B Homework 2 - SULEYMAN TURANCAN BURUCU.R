require(genlasso)
require(data.table)
require(ggplot2)
require(repr)
require(TSrepr)
require(stats)
require(rpart)
require(tibble)
require(rattle)
require(zoo)
require(Metrics)
setwd("D:/Lectures/(2021-2022) Lectures Senior/IE48B/Homeworks/2")

#QUESTION - 1
train_dataset_raw = read.table("CBF_TRAIN.txt") #30 tane time series yani row, 129 tane de column var.

train_dataset_usable = train_dataset_raw[order(train_dataset_raw$V1),]
train_dataset_usable_decreased = train_dataset_usable[,-1]

out1 = trendfilter(as.numeric(train_dataset_usable_decreased[1,]), ord=0)
cv1 = cv.trendfilter(out1, k=10)

par(mfrow = c(1,2))
plot(out1, lambda=cv1$lambda.min, main="Minimal CV error") 
plot(out1, lambda=cv1$lambda.1se, main="One standard error rule")
cv1$lambda.min#overfit oldugu için bu iyi durmuyor, zaten deger de 0 a yakin baya
cv1$lambda.1se#overfit olmadigi için bu iyi duruyor

out2 = trendfilter(as.numeric(train_dataset_usable_decreased[2,]), ord=0)
cv2 = cv.trendfilter(out2, k=10)

out3 = trendfilter(as.numeric(train_dataset_usable_decreased[3,]), ord=0)
cv3 = cv.trendfilter(out3, k=10)

out4 = trendfilter(as.numeric(train_dataset_usable_decreased[4,]), ord=0)
cv4 = cv.trendfilter(out4, k=10)

out5 = trendfilter(as.numeric(train_dataset_usable_decreased[5,]), ord=0)
cv5 = cv.trendfilter(out5, k=10)

out6 = trendfilter(as.numeric(train_dataset_usable_decreased[6,]), ord=0)
cv6 = cv.trendfilter(out6, k=10)

out7 = trendfilter(as.numeric(train_dataset_usable_decreased[7,]), ord=0)
cv7 = cv.trendfilter(out7, k=10)

out8 = trendfilter(as.numeric(train_dataset_usable_decreased[8,]), ord=0)
cv8 = cv.trendfilter(out8, k=10)

out9 = trendfilter(as.numeric(train_dataset_usable_decreased[9,]), ord=0)
cv9 = cv.trendfilter(out9, k=10)

out10 = trendfilter(as.numeric(train_dataset_usable_decreased[10,]), ord=0)
cv10 = cv.trendfilter(out10, k=10)

out11 = trendfilter(as.numeric(train_dataset_usable_decreased[11,]), ord=0)
cv11 = cv.trendfilter(out11, k=10)

out12 = trendfilter(as.numeric(train_dataset_usable_decreased[12,]), ord=0)
cv12 = cv.trendfilter(out12, k=10)

out13 = trendfilter(as.numeric(train_dataset_usable_decreased[13,]), ord=0)
cv13 = cv.trendfilter(out13, k=10)

out14 = trendfilter(as.numeric(train_dataset_usable_decreased[14,]), ord=0)
cv14 = cv.trendfilter(out14, k=10)

out15 = trendfilter(as.numeric(train_dataset_usable_decreased[15,]), ord=0)
cv15 = cv.trendfilter(out15, k=10)

out16 = trendfilter(as.numeric(train_dataset_usable_decreased[16,]), ord=0)
cv16 = cv.trendfilter(out16, k=10)

out17 = trendfilter(as.numeric(train_dataset_usable_decreased[17,]), ord=0)
cv17 = cv.trendfilter(out17, k=10)

out18 = trendfilter(as.numeric(train_dataset_usable_decreased[18,]), ord=0)
cv18 = cv.trendfilter(out18, k=10)

out19 = trendfilter(as.numeric(train_dataset_usable_decreased[19,]), ord=0)
cv19 = cv.trendfilter(out19, k=10)

out20 = trendfilter(as.numeric(train_dataset_usable_decreased[20,]), ord=0)
cv20 = cv.trendfilter(out20, k=10)

out21 = trendfilter(as.numeric(train_dataset_usable_decreased[21,]), ord=0)
cv21 = cv.trendfilter(out21, k=10)

out22 = trendfilter(as.numeric(train_dataset_usable_decreased[22,]), ord=0)
cv22 = cv.trendfilter(out22, k=10)

out23 = trendfilter(as.numeric(train_dataset_usable_decreased[23,]), ord=0)
cv23 = cv.trendfilter(out23, k=10)

out24 = trendfilter(as.numeric(train_dataset_usable_decreased[24,]), ord=0)
cv24 = cv.trendfilter(out24, k=10)

out25 = trendfilter(as.numeric(train_dataset_usable_decreased[25,]), ord=0)
cv25 = cv.trendfilter(out25, k=10)

out26 = trendfilter(as.numeric(train_dataset_usable_decreased[26,]), ord=0)
cv26 = cv.trendfilter(out26, k=10)

out27 = trendfilter(as.numeric(train_dataset_usable_decreased[27,]), ord=0)
cv27 = cv.trendfilter(out27, k=10)

out28 = trendfilter(as.numeric(train_dataset_usable_decreased[28,]), ord=0)
cv28 = cv.trendfilter(out28, k=10)

out29 = trendfilter(as.numeric(train_dataset_usable_decreased[29,]), ord=0)
cv29 = cv.trendfilter(out29, k=10)

out30 = trendfilter(as.numeric(train_dataset_usable_decreased[30,]), ord=0)
cv30 = cv.trendfilter(out30, k=10)

all_cv_values = c(cv1$lambda.1se,cv2$lambda.1se,cv3$lambda.1se,cv4$lambda.1se,
                  cv5$lambda.1se,cv6$lambda.1se,cv7$lambda.1se,cv8$lambda.1se,
                  cv9$lambda.1se,cv10$lambda.1se,cv11$lambda.1se,cv12$lambda.1se,
                  cv13$lambda.1se,cv14$lambda.1se,cv15$lambda.1se,cv16$lambda.1se,
                  cv17$lambda.1se,cv18$lambda.1se,cv19$lambda.1se,cv20$lambda.1se,
                  cv21$lambda.1se,cv22$lambda.1se,cv23$lambda.1se,cv24$lambda.1se,
                  cv25$lambda.1se,cv26$lambda.1se,cv27$lambda.1se,cv28$lambda.1se,
                  cv29$lambda.1se,cv30$lambda.1se)
time_series = c(1:30)

t = cbind(time_series, all_cv_values)
colnames(t) = c("Time Series", "Best Lambda Values")
t

fused_lasso_1_predicted = out1$fit[,match(all_cv_values[1], out1$lambda)]
fused_lasso_2_predicted = out2$fit[,match(all_cv_values[2], out2$lambda)]
fused_lasso_3_predicted = out3$fit[,match(all_cv_values[3], out3$lambda)]
fused_lasso_4_predicted = out4$fit[,match(all_cv_values[4], out4$lambda)]
fused_lasso_5_predicted = out5$fit[,match(all_cv_values[5], out5$lambda)]
fused_lasso_6_predicted = out6$fit[,match(all_cv_values[6], out6$lambda)]
fused_lasso_7_predicted = out7$fit[,match(all_cv_values[7], out7$lambda)]
fused_lasso_8_predicted = out8$fit[,match(all_cv_values[8], out8$lambda)]
fused_lasso_9_predicted = out9$fit[,match(all_cv_values[9], out9$lambda)]
fused_lasso_10_predicted = out10$fit[,match(all_cv_values[10], out10$lambda)]
fused_lasso_11_predicted = out11$fit[,match(all_cv_values[11], out11$lambda)]
fused_lasso_12_predicted = out12$fit[,match(all_cv_values[12], out12$lambda)]
fused_lasso_13_predicted = out13$fit[,match(all_cv_values[13], out13$lambda)]
fused_lasso_14_predicted = out14$fit[,match(all_cv_values[14], out14$lambda)]
fused_lasso_15_predicted = out15$fit[,match(all_cv_values[15], out15$lambda)]
fused_lasso_16_predicted = out16$fit[,match(all_cv_values[16], out16$lambda)]
fused_lasso_17_predicted = out17$fit[,match(all_cv_values[17], out17$lambda)]
fused_lasso_18_predicted = out18$fit[,match(all_cv_values[18], out18$lambda)]
fused_lasso_19_predicted = out19$fit[,match(all_cv_values[19], out19$lambda)]
fused_lasso_20_predicted = out20$fit[,match(all_cv_values[20], out20$lambda)]
fused_lasso_21_predicted = out21$fit[,match(all_cv_values[21], out21$lambda)]
fused_lasso_22_predicted = out22$fit[,match(all_cv_values[22], out22$lambda)]
fused_lasso_23_predicted = out23$fit[,match(all_cv_values[23], out23$lambda)]
fused_lasso_24_predicted = out24$fit[,match(all_cv_values[24], out24$lambda)]
fused_lasso_25_predicted = out25$fit[,match(all_cv_values[25], out25$lambda)]
fused_lasso_26_predicted = out26$fit[,match(all_cv_values[26], out26$lambda)]
fused_lasso_27_predicted = out27$fit[,match(all_cv_values[27], out27$lambda)]
fused_lasso_28_predicted = out28$fit[,match(all_cv_values[28], out28$lambda)]
fused_lasso_29_predicted = out29$fit[,match(all_cv_values[29], out29$lambda)]
fused_lasso_30_predicted = out30$fit[,match(all_cv_values[30], out30$lambda)]

#QUESTION - 2
all_cv_values
time_index_raw = c(0:128)
time_dt = t(data.table(time_index = c(0:128)))
train_dataset_usable_with_time = rbind(time_dt, train_dataset_usable)
setnames(train_dataset_usable_with_time,'V1','class')


time_index = c(1:128)
time_index_column = data.table(c(1:128))

selected_dt1 = train_dataset_usable_with_time[2:11,]


value1_1 = as.numeric(selected_dt1[1,-1])
tree_fit1_1=rpart(value1_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_1)


tree1_1_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_1_values[,tree1_1_predictions:=predict(tree_fit1_1,tree1_1_values)]
colnames(tree1_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree1_1_values = cbind(time_index_column, tree1_1_values)
data_plot1_1 = melt(tree1_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_1 = ggplot(data_plot1_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_1






value1_2 = as.numeric(selected_dt1[1,-1])
tree_fit1_2=rpart(value1_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_2)


tree1_2_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_2_values[,tree1_2_predictions:=predict(tree_fit1_2,tree1_2_values)]
colnames(tree1_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_2_values = cbind(time_index_column, tree1_2_values)
data_plot1_2 = melt(tree1_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_2 = ggplot(data_plot1_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_2







value1_3 = as.numeric(selected_dt1[1,-1])
tree_fit1_3=rpart(value1_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_3)


tree1_3_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_3_values[,tree1_3_predictions:=predict(tree_fit1_3,tree1_3_values)]
colnames(tree1_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_3_values = cbind(time_index_column, tree1_3_values)
data_plot1_3 = melt(tree1_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_3 = ggplot(data_plot1_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_3







value1_4 = as.numeric(selected_dt1[1,-1])
tree_fit1_4=rpart(value1_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_4)


tree1_4_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_4_values[,tree1_4_predictions:=predict(tree_fit1_4,tree1_4_values)]
colnames(tree1_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_4_values = cbind(time_index_column, tree1_4_values)
data_plot1_4 = melt(tree1_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_4 = ggplot(data_plot1_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_4








value1_5 = as.numeric(selected_dt1[1,-1])
tree_fit1_5=rpart(value1_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_5)


tree1_5_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_5_values[,tree1_5_predictions:=predict(tree_fit1_5,tree1_5_values)]
colnames(tree1_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_5_values = cbind(time_index_column, tree1_5_values)
data_plot1_5 = melt(tree1_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_5 = ggplot(data_plot1_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_5








value1_6 = as.numeric(selected_dt1[1,-1])
tree_fit1_6=rpart(value1_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_6)


tree1_6_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_6_values[,tree1_6_predictions:=predict(tree_fit1_6,tree1_6_values)]
colnames(tree1_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_6_values = cbind(time_index_column, tree1_6_values)
data_plot1_6 = melt(tree1_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_6 = ggplot(data_plot1_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_6






value1_7 = as.numeric(selected_dt1[1,-1])
tree_fit1_7=rpart(value1_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_7)


tree1_7_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_7_values[,tree1_7_predictions:=predict(tree_fit1_7,tree1_7_values)]
colnames(tree1_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_7_values = cbind(time_index_column, tree1_7_values)
data_plot1_7 = melt(tree1_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_7 = ggplot(data_plot1_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_7




value1_8 = as.numeric(selected_dt1[1,-1])
tree_fit1_8=rpart(value1_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit1_8)


tree1_8_values = as.data.table(t(data.table(selected_dt1[1,-1])))
tree1_8_values[,tree1_8_predictions:=predict(tree_fit1_8,tree1_8_values)]
colnames(tree1_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree1_8_values = cbind(time_index_column, tree1_8_values)
data_plot1_8 = melt(tree1_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted1_8 = ggplot(data_plot1_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted1_8

par(mfrow = c(1,2))

minimize = function(num1,num2,num3,num4,num5,num6,num7,num8)
{
  if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num1)
  {
    a = c(1,num1)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num2)
  {
    a = c(2,num2)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num3)
  {
    a = c(3,num3)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num4)
  {
    a = c(4,num4)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num5)
  {
    a = c(5,num5)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num6)
  {
    a = c(6,num6)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num7)
  {
    a = c(7,num7)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
  else if (min(num1,num2,num3,num4,num5,num6,num7,num8)==num8)
  {
    a = c(8,num8)
    b = c("Best maxdepth parameter value", "CV Error Value")
    c = data.table(t(a))
    colnames(c) <- b
    return(c)
  }
}

minimize(tree_fit1_1$cptable[length(tree_fit1_1$cptable[, "xerror"]), "xerror"],
         tree_fit1_2$cptable[length(tree_fit1_2$cptable[, "xerror"]), "xerror"],
         tree_fit1_3$cptable[length(tree_fit1_3$cptable[, "xerror"]), "xerror"],
         tree_fit1_4$cptable[length(tree_fit1_4$cptable[, "xerror"]), "xerror"],
         tree_fit1_5$cptable[length(tree_fit1_5$cptable[, "xerror"]), "xerror"],
         tree_fit1_6$cptable[length(tree_fit1_6$cptable[, "xerror"]), "xerror"],
         tree_fit1_7$cptable[length(tree_fit1_7$cptable[, "xerror"]), "xerror"],
         tree_fit1_8$cptable[length(tree_fit1_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted1_4

#maxdepth = 4 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 3

#############################


value2_1 = as.numeric(selected_dt1[2,-1])
tree_fit2_1=rpart(value2_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_1)


tree2_1_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_1_values[,tree2_1_predictions:=predict(tree_fit2_1,tree2_1_values)]
colnames(tree2_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree2_1_values = cbind(time_index_column, tree2_1_values)
data_plot2_1 = melt(tree2_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_1 = ggplot(data_plot2_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_1







value2_2 = as.numeric(selected_dt1[2,-1])
tree_fit2_2=rpart(value2_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_2)


tree2_2_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_2_values[,tree2_2_predictions:=predict(tree_fit2_2,tree2_2_values)]
colnames(tree2_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_2_values = cbind(time_index_column, tree2_2_values)
data_plot2_2 = melt(tree2_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_2 = ggplot(data_plot2_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_2







value2_3 = as.numeric(selected_dt1[2,-1])
tree_fit2_3=rpart(value2_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_3)


tree2_3_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_3_values[,tree2_3_predictions:=predict(tree_fit2_3,tree2_3_values)]
colnames(tree2_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_3_values = cbind(time_index_column, tree2_3_values)
data_plot2_3 = melt(tree2_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_3 = ggplot(data_plot2_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_3







value2_4 = as.numeric(selected_dt1[2,-1])
tree_fit2_4=rpart(value2_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_4)


tree2_4_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_4_values[,tree2_4_predictions:=predict(tree_fit2_4,tree2_4_values)]
colnames(tree2_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_4_values = cbind(time_index_column, tree2_4_values)
data_plot2_4 = melt(tree2_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_4 = ggplot(data_plot2_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_4








value2_5 = as.numeric(selected_dt1[2,-1])
tree_fit2_5=rpart(value2_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_5)


tree2_5_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_5_values[,tree2_5_predictions:=predict(tree_fit2_5,tree2_5_values)]
colnames(tree2_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_5_values = cbind(time_index_column, tree2_5_values)
data_plot2_5 = melt(tree2_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_5 = ggplot(data_plot2_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_5








value2_6 = as.numeric(selected_dt1[2,-1])
tree_fit2_6=rpart(value2_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_6)


tree2_6_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_6_values[,tree2_6_predictions:=predict(tree_fit2_6,tree2_6_values)]
colnames(tree2_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_6_values = cbind(time_index_column, tree2_6_values)
data_plot2_6 = melt(tree2_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_6 = ggplot(data_plot2_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_6






value2_7 = as.numeric(selected_dt1[2,-1])
tree_fit2_7=rpart(value2_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_7)


tree2_7_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_7_values[,tree2_7_predictions:=predict(tree_fit2_7,tree2_7_values)]
colnames(tree2_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_7_values = cbind(time_index_column, tree2_7_values)
data_plot2_7 = melt(tree2_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_7 = ggplot(data_plot2_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_7




value2_8 = as.numeric(selected_dt1[2,-1])
tree_fit2_8=rpart(value2_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit2_8)


tree2_8_values = as.data.table(t(data.table(selected_dt1[2,-1])))
tree2_8_values[,tree2_8_predictions:=predict(tree_fit2_8,tree2_8_values)]
colnames(tree2_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree2_8_values = cbind(time_index_column, tree2_8_values)
data_plot2_8 = melt(tree2_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted2_8 = ggplot(data_plot2_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted2_8




minimize(tree_fit2_1$cptable[length(tree_fit2_1$cptable[, "xerror"]), "xerror"],
         tree_fit2_2$cptable[length(tree_fit2_2$cptable[, "xerror"]), "xerror"],
         tree_fit2_3$cptable[length(tree_fit2_3$cptable[, "xerror"]), "xerror"],
         tree_fit2_4$cptable[length(tree_fit2_4$cptable[, "xerror"]), "xerror"],
         tree_fit2_5$cptable[length(tree_fit2_5$cptable[, "xerror"]), "xerror"],
         tree_fit2_6$cptable[length(tree_fit2_6$cptable[, "xerror"]), "xerror"],
         tree_fit2_7$cptable[length(tree_fit2_7$cptable[, "xerror"]), "xerror"],
         tree_fit2_8$cptable[length(tree_fit2_8$cptable[, "xerror"]), "xerror"])

exact_vs_predicted2_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5



###################################

value3_1 = as.numeric(selected_dt1[3,-1])
tree_fit3_1=rpart(value3_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_1)


tree3_1_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_1_values[,tree3_1_predictions:=predict(tree_fit3_1,tree3_1_values)]
colnames(tree3_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree3_1_values = cbind(time_index_column, tree3_1_values)
data_plot3_1 = melt(tree3_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_1 = ggplot(data_plot3_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_1







value3_2 = as.numeric(selected_dt1[3,-1])
tree_fit3_2=rpart(value3_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_2)


tree3_2_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_2_values[,tree3_2_predictions:=predict(tree_fit3_2,tree3_2_values)]
colnames(tree3_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_2_values = cbind(time_index_column, tree3_2_values)
data_plot3_2 = melt(tree3_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_2 = ggplot(data_plot3_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_2







value3_3 = as.numeric(selected_dt1[3,-1])
tree_fit3_3=rpart(value3_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_3)


tree3_3_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_3_values[,tree3_3_predictions:=predict(tree_fit3_3,tree3_3_values)]
colnames(tree3_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_3_values = cbind(time_index_column, tree3_3_values)
data_plot3_3 = melt(tree3_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_3 = ggplot(data_plot3_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_3







value3_4 = as.numeric(selected_dt1[3,-1])
tree_fit3_4=rpart(value3_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_4)


tree3_4_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_4_values[,tree3_4_predictions:=predict(tree_fit3_4,tree3_4_values)]
colnames(tree3_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_4_values = cbind(time_index_column, tree3_4_values)
data_plot3_4 = melt(tree3_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_4 = ggplot(data_plot3_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_4








value3_5 = as.numeric(selected_dt1[3,-1])
tree_fit3_5=rpart(value3_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_5)


tree3_5_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_5_values[,tree3_5_predictions:=predict(tree_fit3_5,tree3_5_values)]
colnames(tree3_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_5_values = cbind(time_index_column, tree3_5_values)
data_plot3_5 = melt(tree3_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_5 = ggplot(data_plot3_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_5








value3_6 = as.numeric(selected_dt1[3,-1])
tree_fit3_6=rpart(value3_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_6)


tree3_6_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_6_values[,tree3_6_predictions:=predict(tree_fit3_6,tree3_6_values)]
colnames(tree3_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_6_values = cbind(time_index_column, tree3_6_values)
data_plot3_6 = melt(tree3_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_6 = ggplot(data_plot3_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_6






value3_7 = as.numeric(selected_dt1[3,-1])
tree_fit3_7=rpart(value3_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_7)


tree3_7_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_7_values[,tree3_7_predictions:=predict(tree_fit3_7,tree3_7_values)]
colnames(tree3_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_7_values = cbind(time_index_column, tree3_7_values)
data_plot3_7 = melt(tree3_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_7 = ggplot(data_plot3_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_7




value3_8 = as.numeric(selected_dt1[3,-1])
tree_fit3_8=rpart(value3_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit3_8)


tree3_8_values = as.data.table(t(data.table(selected_dt1[3,-1])))
tree3_8_values[,tree3_8_predictions:=predict(tree_fit3_8,tree3_8_values)]
colnames(tree3_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree3_8_values = cbind(time_index_column, tree3_8_values)
data_plot3_8 = melt(tree3_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted3_8 = ggplot(data_plot3_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted3_8




minimize(tree_fit3_1$cptable[length(tree_fit3_1$cptable[, "xerror"]), "xerror"],
         tree_fit3_2$cptable[length(tree_fit3_2$cptable[, "xerror"]), "xerror"],
         tree_fit3_3$cptable[length(tree_fit3_3$cptable[, "xerror"]), "xerror"],
         tree_fit3_4$cptable[length(tree_fit3_4$cptable[, "xerror"]), "xerror"],
         tree_fit3_5$cptable[length(tree_fit3_5$cptable[, "xerror"]), "xerror"],
         tree_fit3_6$cptable[length(tree_fit3_6$cptable[, "xerror"]), "xerror"],
         tree_fit3_7$cptable[length(tree_fit3_7$cptable[, "xerror"]), "xerror"],
         tree_fit3_8$cptable[length(tree_fit3_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted3_2
#maxdepth = 2 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 2


####################################

value4_1 = as.numeric(selected_dt1[4,-1])
tree_fit4_1=rpart(value4_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_1)


tree4_1_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_1_values[,tree4_1_predictions:=predict(tree_fit4_1,tree4_1_values)]
colnames(tree4_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree4_1_values = cbind(time_index_column, tree4_1_values)
data_plot4_1 = melt(tree4_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_1 = ggplot(data_plot4_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_1







value4_2 = as.numeric(selected_dt1[4,-1])
tree_fit4_2=rpart(value4_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_2)


tree4_2_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_2_values[,tree4_2_predictions:=predict(tree_fit4_2,tree4_2_values)]
colnames(tree4_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_2_values = cbind(time_index_column, tree4_2_values)
data_plot4_2 = melt(tree4_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_2 = ggplot(data_plot4_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_2







value4_3 = as.numeric(selected_dt1[4,-1])
tree_fit4_3=rpart(value4_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_3)


tree4_3_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_3_values[,tree4_3_predictions:=predict(tree_fit4_3,tree4_3_values)]
colnames(tree4_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_3_values = cbind(time_index_column, tree4_3_values)
data_plot4_3 = melt(tree4_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_3 = ggplot(data_plot4_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_3







value4_4 = as.numeric(selected_dt1[4,-1])
tree_fit4_4=rpart(value4_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_4)


tree4_4_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_4_values[,tree4_4_predictions:=predict(tree_fit4_4,tree4_4_values)]
colnames(tree4_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_4_values = cbind(time_index_column, tree4_4_values)
data_plot4_4 = melt(tree4_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_4 = ggplot(data_plot4_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_4








value4_5 = as.numeric(selected_dt1[4,-1])
tree_fit4_5=rpart(value4_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_5)


tree4_5_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_5_values[,tree4_5_predictions:=predict(tree_fit4_5,tree4_5_values)]
colnames(tree4_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_5_values = cbind(time_index_column, tree4_5_values)
data_plot4_5 = melt(tree4_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_5 = ggplot(data_plot4_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_5








value4_6 = as.numeric(selected_dt1[4,-1])
tree_fit4_6=rpart(value4_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_6)


tree4_6_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_6_values[,tree4_6_predictions:=predict(tree_fit4_6,tree4_6_values)]
colnames(tree4_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_6_values = cbind(time_index_column, tree4_6_values)
data_plot4_6 = melt(tree4_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_6 = ggplot(data_plot4_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_6






value4_7 = as.numeric(selected_dt1[4,-1])
tree_fit4_7=rpart(value4_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_7)


tree4_7_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_7_values[,tree4_7_predictions:=predict(tree_fit4_7,tree4_7_values)]
colnames(tree4_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_7_values = cbind(time_index_column, tree4_7_values)
data_plot4_7 = melt(tree4_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_7 = ggplot(data_plot4_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_7




value4_8 = as.numeric(selected_dt1[4,-1])
tree_fit4_8=rpart(value4_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit4_8)


tree4_8_values = as.data.table(t(data.table(selected_dt1[4,-1])))
tree4_8_values[,tree4_8_predictions:=predict(tree_fit4_8,tree4_8_values)]
colnames(tree4_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree4_8_values = cbind(time_index_column, tree4_8_values)
data_plot4_8 = melt(tree4_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted4_8 = ggplot(data_plot4_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted4_8




minimize(tree_fit4_1$cptable[length(tree_fit4_1$cptable[, "xerror"]), "xerror"],
         tree_fit4_2$cptable[length(tree_fit4_2$cptable[, "xerror"]), "xerror"],
         tree_fit4_3$cptable[length(tree_fit4_3$cptable[, "xerror"]), "xerror"],
         tree_fit4_4$cptable[length(tree_fit4_4$cptable[, "xerror"]), "xerror"],
         tree_fit4_5$cptable[length(tree_fit4_5$cptable[, "xerror"]), "xerror"],
         tree_fit4_6$cptable[length(tree_fit4_6$cptable[, "xerror"]), "xerror"],
         tree_fit4_7$cptable[length(tree_fit4_7$cptable[, "xerror"]), "xerror"],
         tree_fit4_8$cptable[length(tree_fit4_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted4_2
#maxdepth = 2 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 2



###################################

value5_1 = as.numeric(selected_dt1[5,-1])
tree_fit5_1=rpart(value5_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_1)


tree5_1_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_1_values[,tree5_1_predictions:=predict(tree_fit5_1,tree5_1_values)]
colnames(tree5_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree5_1_values = cbind(time_index_column, tree5_1_values)
data_plot5_1 = melt(tree5_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_1 = ggplot(data_plot5_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_1







value5_2 = as.numeric(selected_dt1[5,-1])
tree_fit5_2=rpart(value5_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_2)


tree5_2_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_2_values[,tree5_2_predictions:=predict(tree_fit5_2,tree5_2_values)]
colnames(tree5_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_2_values = cbind(time_index_column, tree5_2_values)
data_plot5_2 = melt(tree5_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_2 = ggplot(data_plot5_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_2







value5_3 = as.numeric(selected_dt1[5,-1])
tree_fit5_3=rpart(value5_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_3)


tree5_3_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_3_values[,tree5_3_predictions:=predict(tree_fit5_3,tree5_3_values)]
colnames(tree5_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_3_values = cbind(time_index_column, tree5_3_values)
data_plot5_3 = melt(tree5_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_3 = ggplot(data_plot5_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_3







value5_4 = as.numeric(selected_dt1[5,-1])
tree_fit5_4=rpart(value5_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_4)


tree5_4_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_4_values[,tree5_4_predictions:=predict(tree_fit5_4,tree5_4_values)]
colnames(tree5_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_4_values = cbind(time_index_column, tree5_4_values)
data_plot5_4 = melt(tree5_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_4 = ggplot(data_plot5_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_4








value5_5 = as.numeric(selected_dt1[5,-1])
tree_fit5_5=rpart(value5_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_5)


tree5_5_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_5_values[,tree5_5_predictions:=predict(tree_fit5_5,tree5_5_values)]
colnames(tree5_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_5_values = cbind(time_index_column, tree5_5_values)
data_plot5_5 = melt(tree5_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_5 = ggplot(data_plot5_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_5








value5_6 = as.numeric(selected_dt1[5,-1])
tree_fit5_6=rpart(value5_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_6)


tree5_6_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_6_values[,tree5_6_predictions:=predict(tree_fit5_6,tree5_6_values)]
colnames(tree5_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_6_values = cbind(time_index_column, tree5_6_values)
data_plot5_6 = melt(tree5_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_6 = ggplot(data_plot5_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_6






value5_7 = as.numeric(selected_dt1[5,-1])
tree_fit5_7=rpart(value5_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_7)


tree5_7_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_7_values[,tree5_7_predictions:=predict(tree_fit5_7,tree5_7_values)]
colnames(tree5_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_7_values = cbind(time_index_column, tree5_7_values)
data_plot5_7 = melt(tree5_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_7 = ggplot(data_plot5_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_7




value5_8 = as.numeric(selected_dt1[5,-1])
tree_fit5_8=rpart(value5_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit5_8)


tree5_8_values = as.data.table(t(data.table(selected_dt1[5,-1])))
tree5_8_values[,tree5_8_predictions:=predict(tree_fit5_8,tree5_8_values)]
colnames(tree5_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree5_8_values = cbind(time_index_column, tree5_8_values)
data_plot5_8 = melt(tree5_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted5_8 = ggplot(data_plot5_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted5_8



minimize(tree_fit5_1$cptable[length(tree_fit5_1$cptable[, "xerror"]), "xerror"],
         tree_fit5_2$cptable[length(tree_fit5_2$cptable[, "xerror"]), "xerror"],
         tree_fit5_3$cptable[length(tree_fit5_3$cptable[, "xerror"]), "xerror"],
         tree_fit5_4$cptable[length(tree_fit5_4$cptable[, "xerror"]), "xerror"],
         tree_fit5_5$cptable[length(tree_fit5_5$cptable[, "xerror"]), "xerror"],
         tree_fit5_6$cptable[length(tree_fit5_6$cptable[, "xerror"]), "xerror"],
         tree_fit5_7$cptable[length(tree_fit5_7$cptable[, "xerror"]), "xerror"],
         tree_fit5_8$cptable[length(tree_fit5_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted5_2

#maxdepth = 2 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 2





#########################


value6_1 = as.numeric(selected_dt1[6,-1])
tree_fit6_1=rpart(value6_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_1)


tree6_1_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_1_values[,tree6_1_predictions:=predict(tree_fit6_1,tree6_1_values)]
colnames(tree6_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree6_1_values = cbind(time_index_column, tree6_1_values)
data_plot6_1 = melt(tree6_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_1 = ggplot(data_plot6_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_1







value6_2 = as.numeric(selected_dt1[6,-1])
tree_fit6_2=rpart(value6_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_2)


tree6_2_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_2_values[,tree6_2_predictions:=predict(tree_fit6_2,tree6_2_values)]
colnames(tree6_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_2_values = cbind(time_index_column, tree6_2_values)
data_plot6_2 = melt(tree6_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_2 = ggplot(data_plot6_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_2







value6_3 = as.numeric(selected_dt1[6,-1])
tree_fit6_3=rpart(value6_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_3)


tree6_3_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_3_values[,tree6_3_predictions:=predict(tree_fit6_3,tree6_3_values)]
colnames(tree6_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_3_values = cbind(time_index_column, tree6_3_values)
data_plot6_3 = melt(tree6_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_3 = ggplot(data_plot6_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_3







value6_4 = as.numeric(selected_dt1[6,-1])
tree_fit6_4=rpart(value6_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_4)


tree6_4_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_4_values[,tree6_4_predictions:=predict(tree_fit6_4,tree6_4_values)]
colnames(tree6_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_4_values = cbind(time_index_column, tree6_4_values)
data_plot6_4 = melt(tree6_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_4 = ggplot(data_plot6_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_4








value6_5 = as.numeric(selected_dt1[6,-1])
tree_fit6_5=rpart(value6_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_5)


tree6_5_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_5_values[,tree6_5_predictions:=predict(tree_fit6_5,tree6_5_values)]
colnames(tree6_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_5_values = cbind(time_index_column, tree6_5_values)
data_plot6_5 = melt(tree6_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_5 = ggplot(data_plot6_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_5








value6_6 = as.numeric(selected_dt1[6,-1])
tree_fit6_6=rpart(value6_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_6)


tree6_6_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_6_values[,tree6_6_predictions:=predict(tree_fit6_6,tree6_6_values)]
colnames(tree6_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_6_values = cbind(time_index_column, tree6_6_values)
data_plot6_6 = melt(tree6_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_6 = ggplot(data_plot6_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_6






value6_7 = as.numeric(selected_dt1[6,-1])
tree_fit6_7=rpart(value6_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_7)


tree6_7_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_7_values[,tree6_7_predictions:=predict(tree_fit6_7,tree6_7_values)]
colnames(tree6_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_7_values = cbind(time_index_column, tree6_7_values)
data_plot6_7 = melt(tree6_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_7 = ggplot(data_plot6_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_7




value6_8 = as.numeric(selected_dt1[6,-1])
tree_fit6_8=rpart(value6_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit6_8)


tree6_8_values = as.data.table(t(data.table(selected_dt1[6,-1])))
tree6_8_values[,tree6_8_predictions:=predict(tree_fit6_8,tree6_8_values)]
colnames(tree6_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree6_8_values = cbind(time_index_column, tree6_8_values)
data_plot6_8 = melt(tree6_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted6_8 = ggplot(data_plot6_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted6_8




minimize(tree_fit6_1$cptable[length(tree_fit6_1$cptable[, "xerror"]), "xerror"],
         tree_fit6_2$cptable[length(tree_fit6_2$cptable[, "xerror"]), "xerror"],
         tree_fit6_3$cptable[length(tree_fit6_3$cptable[, "xerror"]), "xerror"],
         tree_fit6_4$cptable[length(tree_fit6_4$cptable[, "xerror"]), "xerror"],
         tree_fit6_5$cptable[length(tree_fit6_5$cptable[, "xerror"]), "xerror"],
         tree_fit6_6$cptable[length(tree_fit6_6$cptable[, "xerror"]), "xerror"],
         tree_fit6_7$cptable[length(tree_fit6_7$cptable[, "xerror"]), "xerror"],
         tree_fit6_8$cptable[length(tree_fit6_8$cptable[, "xerror"]), "xerror"])

exact_vs_predicted6_2
#maxdepth = 2 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 2


################################################

value7_1 = as.numeric(selected_dt1[7,-1])
tree_fit7_1=rpart(value7_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_1)


tree7_1_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_1_values[,tree7_1_predictions:=predict(tree_fit7_1,tree7_1_values)]
colnames(tree7_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree7_1_values = cbind(time_index_column, tree7_1_values)
data_plot7_1 = melt(tree7_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_1 = ggplot(data_plot7_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_1







value7_2 = as.numeric(selected_dt1[7,-1])
tree_fit7_2=rpart(value7_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_2)


tree7_2_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_2_values[,tree7_2_predictions:=predict(tree_fit7_2,tree7_2_values)]
colnames(tree7_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_2_values = cbind(time_index_column, tree7_2_values)
data_plot7_2 = melt(tree7_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_2 = ggplot(data_plot7_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_2







value7_3 = as.numeric(selected_dt1[7,-1])
tree_fit7_3=rpart(value7_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_3)


tree7_3_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_3_values[,tree7_3_predictions:=predict(tree_fit7_3,tree7_3_values)]
colnames(tree7_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_3_values = cbind(time_index_column, tree7_3_values)
data_plot7_3 = melt(tree7_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_3 = ggplot(data_plot7_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_3







value7_4 = as.numeric(selected_dt1[7,-1])
tree_fit7_4=rpart(value7_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_4)


tree7_4_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_4_values[,tree7_4_predictions:=predict(tree_fit7_4,tree7_4_values)]
colnames(tree7_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_4_values = cbind(time_index_column, tree7_4_values)
data_plot7_4 = melt(tree7_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_4 = ggplot(data_plot7_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_4








value7_5 = as.numeric(selected_dt1[7,-1])
tree_fit7_5=rpart(value7_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_5)


tree7_5_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_5_values[,tree7_5_predictions:=predict(tree_fit7_5,tree7_5_values)]
colnames(tree7_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_5_values = cbind(time_index_column, tree7_5_values)
data_plot7_5 = melt(tree7_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_5 = ggplot(data_plot7_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_5








value7_6 = as.numeric(selected_dt1[7,-1])
tree_fit7_6=rpart(value7_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_6)


tree7_6_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_6_values[,tree7_6_predictions:=predict(tree_fit7_6,tree7_6_values)]
colnames(tree7_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_6_values = cbind(time_index_column, tree7_6_values)
data_plot7_6 = melt(tree7_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_6 = ggplot(data_plot7_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_6






value7_7 = as.numeric(selected_dt1[7,-1])
tree_fit7_7=rpart(value7_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_7)


tree7_7_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_7_values[,tree7_7_predictions:=predict(tree_fit7_7,tree7_7_values)]
colnames(tree7_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_7_values = cbind(time_index_column, tree7_7_values)
data_plot7_7 = melt(tree7_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_7 = ggplot(data_plot7_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_7




value7_8 = as.numeric(selected_dt1[7,-1])
tree_fit7_8=rpart(value7_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit7_8)


tree7_8_values = as.data.table(t(data.table(selected_dt1[7,-1])))
tree7_8_values[,tree7_8_predictions:=predict(tree_fit7_8,tree7_8_values)]
colnames(tree7_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree7_8_values = cbind(time_index_column, tree7_8_values)
data_plot7_8 = melt(tree7_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted7_8 = ggplot(data_plot7_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted7_8



minimize(tree_fit7_1$cptable[length(tree_fit7_1$cptable[, "xerror"]), "xerror"],
         tree_fit7_2$cptable[length(tree_fit7_2$cptable[, "xerror"]), "xerror"],
         tree_fit7_3$cptable[length(tree_fit7_3$cptable[, "xerror"]), "xerror"],
         tree_fit7_4$cptable[length(tree_fit7_4$cptable[, "xerror"]), "xerror"],
         tree_fit7_5$cptable[length(tree_fit7_5$cptable[, "xerror"]), "xerror"],
         tree_fit7_6$cptable[length(tree_fit7_6$cptable[, "xerror"]), "xerror"],
         tree_fit7_7$cptable[length(tree_fit7_7$cptable[, "xerror"]), "xerror"],
         tree_fit7_8$cptable[length(tree_fit7_8$cptable[, "xerror"]), "xerror"])


exact_vs_predicted7_8
#maxdepth = 8 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 8



##################################


value8_1 = as.numeric(selected_dt1[8,-1])
tree_fit8_1=rpart(value8_1~time_index,selected_dt1,
                  control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_1)


tree8_1_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_1_values[,tree8_1_predictions:=predict(tree_fit8_1,tree8_1_values)]
colnames(tree8_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree8_1_values = cbind(time_index_column, tree8_1_values)
data_plot8_1 = melt(tree8_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_1 = ggplot(data_plot8_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_1







value8_2 = as.numeric(selected_dt1[8,-1])
tree_fit8_2=rpart(value8_2~time_index,selected_dt1,
                  control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_2)


tree8_2_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_2_values[,tree8_2_predictions:=predict(tree_fit8_2,tree8_2_values)]
colnames(tree8_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_2_values = cbind(time_index_column, tree8_2_values)
data_plot8_2 = melt(tree8_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_2 = ggplot(data_plot8_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_2







value8_3 = as.numeric(selected_dt1[8,-1])
tree_fit8_3=rpart(value8_3~time_index,selected_dt1,
                  control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_3)


tree8_3_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_3_values[,tree8_3_predictions:=predict(tree_fit8_3,tree8_3_values)]
colnames(tree8_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_3_values = cbind(time_index_column, tree8_3_values)
data_plot8_3 = melt(tree8_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_3 = ggplot(data_plot8_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_3







value8_4 = as.numeric(selected_dt1[8,-1])
tree_fit8_4=rpart(value8_4~time_index,selected_dt1,
                  control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_4)


tree8_4_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_4_values[,tree8_4_predictions:=predict(tree_fit8_4,tree8_4_values)]
colnames(tree8_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_4_values = cbind(time_index_column, tree8_4_values)
data_plot8_4 = melt(tree8_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_4 = ggplot(data_plot8_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_4








value8_5 = as.numeric(selected_dt1[8,-1])
tree_fit8_5=rpart(value8_5~time_index,selected_dt1,
                  control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_5)


tree8_5_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_5_values[,tree8_5_predictions:=predict(tree_fit8_5,tree8_5_values)]
colnames(tree8_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_5_values = cbind(time_index_column, tree8_5_values)
data_plot8_5 = melt(tree8_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_5 = ggplot(data_plot8_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_5








value8_6 = as.numeric(selected_dt1[8,-1])
tree_fit8_6=rpart(value8_6~time_index,selected_dt1,
                  control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_6)


tree8_6_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_6_values[,tree8_6_predictions:=predict(tree_fit8_6,tree8_6_values)]
colnames(tree8_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_6_values = cbind(time_index_column, tree8_6_values)
data_plot8_6 = melt(tree8_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_6 = ggplot(data_plot8_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_6






value8_7 = as.numeric(selected_dt1[8,-1])
tree_fit8_7=rpart(value8_7~time_index,selected_dt1,
                  control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_7)


tree8_7_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_7_values[,tree8_7_predictions:=predict(tree_fit8_7,tree8_7_values)]
colnames(tree8_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_7_values = cbind(time_index_column, tree8_7_values)
data_plot8_7 = melt(tree8_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_7 = ggplot(data_plot8_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_7




value8_8 = as.numeric(selected_dt1[8,-1])
tree_fit8_8=rpart(value8_8~time_index,selected_dt1,
                  control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit8_8)


tree8_8_values = as.data.table(t(data.table(selected_dt1[8,-1])))
tree8_8_values[,tree8_8_predictions:=predict(tree_fit8_8,tree8_8_values)]
colnames(tree8_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree8_8_values = cbind(time_index_column, tree8_8_values)
data_plot8_8 = melt(tree8_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted8_8 = ggplot(data_plot8_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted8_8




minimize(tree_fit8_1$cptable[length(tree_fit8_1$cptable[, "xerror"]), "xerror"],
         tree_fit8_2$cptable[length(tree_fit8_2$cptable[, "xerror"]), "xerror"],
         tree_fit8_3$cptable[length(tree_fit8_3$cptable[, "xerror"]), "xerror"],
         tree_fit8_4$cptable[length(tree_fit8_4$cptable[, "xerror"]), "xerror"],
         tree_fit8_5$cptable[length(tree_fit8_5$cptable[, "xerror"]), "xerror"],
         tree_fit8_6$cptable[length(tree_fit8_6$cptable[, "xerror"]), "xerror"],
         tree_fit8_7$cptable[length(tree_fit8_7$cptable[, "xerror"]), "xerror"],
         tree_fit8_8$cptable[length(tree_fit8_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted8_2

#maxdepth = 2 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 2


###################################

value9_1 = as.numeric(selected_dt1[9,-1])
tree_fit9_1=rpart(value9_1~time_index,selected_dt1,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_1)


tree9_1_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_1_values[,tree9_1_predictions:=predict(tree_fit9_1,tree9_1_values)]
colnames(tree9_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree9_1_values = cbind(time_index_column, tree9_1_values)
data_plot9_1 = melt(tree9_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_1 = ggplot(data_plot9_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_1







value9_2 = as.numeric(selected_dt1[9,-1])
tree_fit9_2=rpart(value9_2~time_index,selected_dt1,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_2)


tree9_2_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_2_values[,tree9_2_predictions:=predict(tree_fit9_2,tree9_2_values)]
colnames(tree9_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_2_values = cbind(time_index_column, tree9_2_values)
data_plot9_2 = melt(tree9_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_2 = ggplot(data_plot9_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_2







value9_3 = as.numeric(selected_dt1[9,-1])
tree_fit9_3=rpart(value9_3~time_index,selected_dt1,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_3)


tree9_3_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_3_values[,tree9_3_predictions:=predict(tree_fit9_3,tree9_3_values)]
colnames(tree9_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_3_values = cbind(time_index_column, tree9_3_values)
data_plot9_3 = melt(tree9_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_3 = ggplot(data_plot9_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_3







value9_4 = as.numeric(selected_dt1[9,-1])
tree_fit9_4=rpart(value9_4~time_index,selected_dt1,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_4)


tree9_4_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_4_values[,tree9_4_predictions:=predict(tree_fit9_4,tree9_4_values)]
colnames(tree9_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_4_values = cbind(time_index_column, tree9_4_values)
data_plot9_4 = melt(tree9_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_4 = ggplot(data_plot9_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_4








value9_5 = as.numeric(selected_dt1[9,-1])
tree_fit9_5=rpart(value9_5~time_index,selected_dt1,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_5)


tree9_5_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_5_values[,tree9_5_predictions:=predict(tree_fit9_5,tree9_5_values)]
colnames(tree9_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_5_values = cbind(time_index_column, tree9_5_values)
data_plot9_5 = melt(tree9_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_5 = ggplot(data_plot9_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_5








value9_6 = as.numeric(selected_dt1[9,-1])
tree_fit9_6=rpart(value9_6~time_index,selected_dt1,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_6)


tree9_6_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_6_values[,tree9_6_predictions:=predict(tree_fit9_6,tree9_6_values)]
colnames(tree9_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_6_values = cbind(time_index_column, tree9_6_values)
data_plot9_6 = melt(tree9_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_6 = ggplot(data_plot9_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_6






value9_7 = as.numeric(selected_dt1[9,-1])
tree_fit9_7=rpart(value9_7~time_index,selected_dt1,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_7)


tree9_7_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_7_values[,tree9_7_predictions:=predict(tree_fit9_7,tree9_7_values)]
colnames(tree9_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_7_values = cbind(time_index_column, tree9_7_values)
data_plot9_7 = melt(tree9_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_7 = ggplot(data_plot9_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_7




value9_8 = as.numeric(selected_dt1[9,-1])
tree_fit9_8=rpart(value9_8~time_index,selected_dt1,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit9_8)


tree9_8_values = as.data.table(t(data.table(selected_dt1[9,-1])))
tree9_8_values[,tree9_8_predictions:=predict(tree_fit9_8,tree9_8_values)]
colnames(tree9_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree9_8_values = cbind(time_index_column, tree9_8_values)
data_plot9_8 = melt(tree9_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted9_8 = ggplot(data_plot9_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted9_8




minimize(tree_fit9_1$cptable[length(tree_fit9_1$cptable[, "xerror"]), "xerror"],
         tree_fit9_2$cptable[length(tree_fit9_2$cptable[, "xerror"]), "xerror"],
         tree_fit9_3$cptable[length(tree_fit9_3$cptable[, "xerror"]), "xerror"],
         tree_fit9_4$cptable[length(tree_fit9_4$cptable[, "xerror"]), "xerror"],
         tree_fit9_5$cptable[length(tree_fit9_5$cptable[, "xerror"]), "xerror"],
         tree_fit9_6$cptable[length(tree_fit9_6$cptable[, "xerror"]), "xerror"],
         tree_fit9_7$cptable[length(tree_fit9_7$cptable[, "xerror"]), "xerror"],
         tree_fit9_8$cptable[length(tree_fit9_8$cptable[, "xerror"]), "xerror"])

exact_vs_predicted9_1
#maxdepth = 1 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 1



#######################################

value10_1 = as.numeric(selected_dt1[10,-1])
tree_fit10_1=rpart(value10_1~time_index,selected_dt1,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_1)


tree10_1_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_1_values[,tree10_1_predictions:=predict(tree_fit10_1,tree10_1_values)]
colnames(tree10_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")

tree10_1_values = cbind(time_index_column, tree10_1_values)
data_plot10_1 = melt(tree10_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_1 = ggplot(data_plot10_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_1







value10_2 = as.numeric(selected_dt1[10,-1])
tree_fit10_2=rpart(value10_2~time_index,selected_dt1,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_2)


tree10_2_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_2_values[,tree10_2_predictions:=predict(tree_fit10_2,tree10_2_values)]
colnames(tree10_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_2_values = cbind(time_index_column, tree10_2_values)
data_plot10_2 = melt(tree10_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_2 = ggplot(data_plot10_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_2







value10_3 = as.numeric(selected_dt1[10,-1])
tree_fit10_3=rpart(value10_3~time_index,selected_dt1,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_3)


tree10_3_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_3_values[,tree10_3_predictions:=predict(tree_fit10_3,tree10_3_values)]
colnames(tree10_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_3_values = cbind(time_index_column, tree10_3_values)
data_plot10_3 = melt(tree10_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_3 = ggplot(data_plot10_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_3







value10_4 = as.numeric(selected_dt1[10,-1])
tree_fit10_4=rpart(value10_4~time_index,selected_dt1,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_4)


tree10_4_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_4_values[,tree10_4_predictions:=predict(tree_fit10_4,tree10_4_values)]
colnames(tree10_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_4_values = cbind(time_index_column, tree10_4_values)
data_plot10_4 = melt(tree10_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_4 = ggplot(data_plot10_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_4








value10_5 = as.numeric(selected_dt1[10,-1])
tree_fit10_5=rpart(value10_5~time_index,selected_dt1,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_5)


tree10_5_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_5_values[,tree10_5_predictions:=predict(tree_fit10_5,tree10_5_values)]
colnames(tree10_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_5_values = cbind(time_index_column, tree10_5_values)
data_plot10_5 = melt(tree10_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_5 = ggplot(data_plot10_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_5








value10_6 = as.numeric(selected_dt1[10,-1])
tree_fit10_6=rpart(value10_6~time_index,selected_dt1,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_6)


tree10_6_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_6_values[,tree10_6_predictions:=predict(tree_fit10_6,tree10_6_values)]
colnames(tree10_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_6_values = cbind(time_index_column, tree10_6_values)
data_plot10_6 = melt(tree10_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_6 = ggplot(data_plot10_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_6






value10_7 = as.numeric(selected_dt1[10,-1])
tree_fit10_7=rpart(value10_7~time_index,selected_dt1,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_7)


tree10_7_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_7_values[,tree10_7_predictions:=predict(tree_fit10_7,tree10_7_values)]
colnames(tree10_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_7_values = cbind(time_index_column, tree10_7_values)
data_plot10_7 = melt(tree10_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_7 = ggplot(data_plot10_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_7




value10_8 = as.numeric(selected_dt1[10,-1])
tree_fit10_8=rpart(value10_8~time_index,selected_dt1,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit10_8)


tree10_8_values = as.data.table(t(data.table(selected_dt1[10,-1])))
tree10_8_values[,tree10_8_predictions:=predict(tree_fit10_8,tree10_8_values)]
colnames(tree10_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree10_8_values = cbind(time_index_column, tree10_8_values)
data_plot10_8 = melt(tree10_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted10_8 = ggplot(data_plot10_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted10_8




minimize(tree_fit10_1$cptable[length(tree_fit10_1$cptable[, "xerror"]), "xerror"],
         tree_fit10_2$cptable[length(tree_fit10_2$cptable[, "xerror"]), "xerror"],
         tree_fit10_3$cptable[length(tree_fit10_3$cptable[, "xerror"]), "xerror"],
         tree_fit10_4$cptable[length(tree_fit10_4$cptable[, "xerror"]), "xerror"],
         tree_fit10_5$cptable[length(tree_fit10_5$cptable[, "xerror"]), "xerror"],
         tree_fit10_6$cptable[length(tree_fit10_6$cptable[, "xerror"]), "xerror"],
         tree_fit10_7$cptable[length(tree_fit10_7$cptable[, "xerror"]), "xerror"],
         tree_fit10_8$cptable[length(tree_fit10_8$cptable[, "xerror"]), "xerror"])

exact_vs_predicted10_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5




#class 2 deki her time series in regression tree si

selected_dt2 = train_dataset_usable_with_time[12:23,]

value11_1 = as.numeric(selected_dt2[1,-1])
tree_fit11_1=rpart(value11_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_1)


tree11_1_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_1_values[,tree11_1_predictions:=predict(tree_fit11_1,tree11_1_values)]
colnames(tree11_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_1_values = cbind(time_index_column, tree11_1_values)
data_plot11_1 = melt(tree11_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_1 = ggplot(data_plot11_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_1







value11_2 = as.numeric(selected_dt2[1,-1])
tree_fit11_2=rpart(value11_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_2)


tree11_2_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_2_values[,tree11_2_predictions:=predict(tree_fit11_2,tree11_2_values)]
colnames(tree11_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_2_values = cbind(time_index_column, tree11_2_values)
data_plot11_2 = melt(tree11_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_2 = ggplot(data_plot11_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_2







value11_3 = as.numeric(selected_dt2[1,-1])
tree_fit11_3=rpart(value11_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_3)


tree11_3_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_3_values[,tree11_3_predictions:=predict(tree_fit11_3,tree11_3_values)]
colnames(tree11_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_3_values = cbind(time_index_column, tree11_3_values)
data_plot11_3 = melt(tree11_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_3 = ggplot(data_plot11_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_3







value11_4 = as.numeric(selected_dt2[1,-1])
tree_fit11_4=rpart(value11_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_4)


tree11_4_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_4_values[,tree11_4_predictions:=predict(tree_fit11_4,tree11_4_values)]
colnames(tree11_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_4_values = cbind(time_index_column, tree11_4_values)
data_plot11_4 = melt(tree11_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_4 = ggplot(data_plot11_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_4








value11_5 = as.numeric(selected_dt2[1,-1])
tree_fit11_5=rpart(value11_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_5)


tree11_5_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_5_values[,tree11_5_predictions:=predict(tree_fit11_5,tree11_5_values)]
colnames(tree11_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_5_values = cbind(time_index_column, tree11_5_values)
data_plot11_5 = melt(tree11_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_5 = ggplot(data_plot11_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_5








value11_6 = as.numeric(selected_dt2[1,-1])
tree_fit11_6=rpart(value11_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_6)


tree11_6_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_6_values[,tree11_6_predictions:=predict(tree_fit11_6,tree11_6_values)]
colnames(tree11_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_6_values = cbind(time_index_column, tree11_6_values)
data_plot11_6 = melt(tree11_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_6 = ggplot(data_plot11_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_6






value11_7 = as.numeric(selected_dt2[1,-1])
tree_fit11_7=rpart(value11_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_7)


tree11_7_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_7_values[,tree11_7_predictions:=predict(tree_fit11_7,tree11_7_values)]
colnames(tree11_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_7_values = cbind(time_index_column, tree11_7_values)
data_plot11_7 = melt(tree11_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_7 = ggplot(data_plot11_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_7




value11_8 = as.numeric(selected_dt2[1,-1])
tree_fit11_8=rpart(value11_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit11_8)


tree11_8_values = as.data.table(t(data.table(selected_dt2[1,-1])))
tree11_8_values[,tree11_8_predictions:=predict(tree_fit11_8,tree11_8_values)]
colnames(tree11_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree11_8_values = cbind(time_index_column, tree11_8_values)
data_plot11_8 = melt(tree11_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted11_8 = ggplot(data_plot11_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted11_8




minimize(tree_fit11_1$cptable[length(tree_fit11_1$cptable[, "xerror"]), "xerror"],
         tree_fit11_2$cptable[length(tree_fit11_2$cptable[, "xerror"]), "xerror"],
         tree_fit11_3$cptable[length(tree_fit11_3$cptable[, "xerror"]), "xerror"],
         tree_fit11_4$cptable[length(tree_fit11_4$cptable[, "xerror"]), "xerror"],
         tree_fit11_5$cptable[length(tree_fit11_5$cptable[, "xerror"]), "xerror"],
         tree_fit11_6$cptable[length(tree_fit11_6$cptable[, "xerror"]), "xerror"],
         tree_fit11_7$cptable[length(tree_fit11_7$cptable[, "xerror"]), "xerror"],
         tree_fit11_8$cptable[length(tree_fit11_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted11_6

#maxdepth = 6 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 6

#########################################

value12_1 = as.numeric(selected_dt2[2,-1])
tree_fit12_1=rpart(value12_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_1)


tree12_1_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_1_values[,tree12_1_predictions:=predict(tree_fit12_1,tree12_1_values)]
colnames(tree12_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_1_values = cbind(time_index_column, tree12_1_values)
data_plot12_1 = melt(tree12_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_1 = ggplot(data_plot12_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_1







value12_2 = as.numeric(selected_dt2[2,-1])
tree_fit12_2=rpart(value12_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_2)


tree12_2_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_2_values[,tree12_2_predictions:=predict(tree_fit12_2,tree12_2_values)]
colnames(tree12_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_2_values = cbind(time_index_column, tree12_2_values)
data_plot12_2 = melt(tree12_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_2 = ggplot(data_plot12_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_2







value12_3 = as.numeric(selected_dt2[2,-1])
tree_fit12_3=rpart(value12_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_3)


tree12_3_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_3_values[,tree12_3_predictions:=predict(tree_fit12_3,tree12_3_values)]
colnames(tree12_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_3_values = cbind(time_index_column, tree12_3_values)
data_plot12_3 = melt(tree12_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_3 = ggplot(data_plot12_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_3







value12_4 = as.numeric(selected_dt2[2,-1])
tree_fit12_4=rpart(value12_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_4)


tree12_4_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_4_values[,tree12_4_predictions:=predict(tree_fit12_4,tree12_4_values)]
colnames(tree12_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_4_values = cbind(time_index_column, tree12_4_values)
data_plot12_4 = melt(tree12_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_4 = ggplot(data_plot12_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_4








value12_5 = as.numeric(selected_dt2[2,-1])
tree_fit12_5=rpart(value12_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_5)


tree12_5_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_5_values[,tree12_5_predictions:=predict(tree_fit12_5,tree12_5_values)]
colnames(tree12_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_5_values = cbind(time_index_column, tree12_5_values)
data_plot12_5 = melt(tree12_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_5 = ggplot(data_plot12_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_5








value12_6 = as.numeric(selected_dt2[2,-1])
tree_fit12_6=rpart(value12_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_6)


tree12_6_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_6_values[,tree12_6_predictions:=predict(tree_fit12_6,tree12_6_values)]
colnames(tree12_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_6_values = cbind(time_index_column, tree12_6_values)
data_plot12_6 = melt(tree12_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_6 = ggplot(data_plot12_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_6






value12_7 = as.numeric(selected_dt2[2,-1])
tree_fit12_7=rpart(value12_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_7)


tree12_7_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_7_values[,tree12_7_predictions:=predict(tree_fit12_7,tree12_7_values)]
colnames(tree12_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_7_values = cbind(time_index_column, tree12_7_values)
data_plot12_7 = melt(tree12_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_7 = ggplot(data_plot12_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_7




value12_8 = as.numeric(selected_dt2[2,-1])
tree_fit12_8=rpart(value12_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit12_8)


tree12_8_values = as.data.table(t(data.table(selected_dt2[2,-1])))
tree12_8_values[,tree12_8_predictions:=predict(tree_fit12_8,tree12_8_values)]
colnames(tree12_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree12_8_values = cbind(time_index_column, tree12_8_values)
data_plot12_8 = melt(tree12_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted12_8 = ggplot(data_plot12_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted12_8




minimize(tree_fit12_1$cptable[length(tree_fit12_1$cptable[, "xerror"]), "xerror"],
         tree_fit12_2$cptable[length(tree_fit12_2$cptable[, "xerror"]), "xerror"],
         tree_fit12_3$cptable[length(tree_fit12_3$cptable[, "xerror"]), "xerror"],
         tree_fit12_4$cptable[length(tree_fit12_4$cptable[, "xerror"]), "xerror"],
         tree_fit12_5$cptable[length(tree_fit12_5$cptable[, "xerror"]), "xerror"],
         tree_fit12_6$cptable[length(tree_fit12_6$cptable[, "xerror"]), "xerror"],
         tree_fit12_7$cptable[length(tree_fit12_7$cptable[, "xerror"]), "xerror"],
         tree_fit12_8$cptable[length(tree_fit12_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted12_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5




#############################

value13_1 = as.numeric(selected_dt2[3,-1])
tree_fit13_1=rpart(value13_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_1)


tree13_1_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_1_values[,tree13_1_predictions:=predict(tree_fit13_1,tree13_1_values)]
colnames(tree13_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_1_values = cbind(time_index_column, tree13_1_values)
data_plot13_1 = melt(tree13_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_1 = ggplot(data_plot13_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_1







value13_2 = as.numeric(selected_dt2[3,-1])
tree_fit13_2=rpart(value13_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_2)


tree13_2_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_2_values[,tree13_2_predictions:=predict(tree_fit13_2,tree13_2_values)]
colnames(tree13_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_2_values = cbind(time_index_column, tree13_2_values)
data_plot13_2 = melt(tree13_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_2 = ggplot(data_plot13_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_2







value13_3 = as.numeric(selected_dt2[3,-1])
tree_fit13_3=rpart(value13_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_3)


tree13_3_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_3_values[,tree13_3_predictions:=predict(tree_fit13_3,tree13_3_values)]
colnames(tree13_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_3_values = cbind(time_index_column, tree13_3_values)
data_plot13_3 = melt(tree13_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_3 = ggplot(data_plot13_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_3







value13_4 = as.numeric(selected_dt2[3,-1])
tree_fit13_4=rpart(value13_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_4)


tree13_4_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_4_values[,tree13_4_predictions:=predict(tree_fit13_4,tree13_4_values)]
colnames(tree13_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_4_values = cbind(time_index_column, tree13_4_values)
data_plot13_4 = melt(tree13_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_4 = ggplot(data_plot13_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_4








value13_5 = as.numeric(selected_dt2[3,-1])
tree_fit13_5=rpart(value13_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_5)


tree13_5_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_5_values[,tree13_5_predictions:=predict(tree_fit13_5,tree13_5_values)]
colnames(tree13_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_5_values = cbind(time_index_column, tree13_5_values)
data_plot13_5 = melt(tree13_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_5 = ggplot(data_plot13_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_5








value13_6 = as.numeric(selected_dt2[3,-1])
tree_fit13_6=rpart(value13_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_6)


tree13_6_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_6_values[,tree13_6_predictions:=predict(tree_fit13_6,tree13_6_values)]
colnames(tree13_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_6_values = cbind(time_index_column, tree13_6_values)
data_plot13_6 = melt(tree13_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_6 = ggplot(data_plot13_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_6






value13_7 = as.numeric(selected_dt2[3,-1])
tree_fit13_7=rpart(value13_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_7)


tree13_7_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_7_values[,tree13_7_predictions:=predict(tree_fit13_7,tree13_7_values)]
colnames(tree13_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_7_values = cbind(time_index_column, tree13_7_values)
data_plot13_7 = melt(tree13_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_7 = ggplot(data_plot13_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_7




value13_8 = as.numeric(selected_dt2[3,-1])
tree_fit13_8=rpart(value13_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit13_8)


tree13_8_values = as.data.table(t(data.table(selected_dt2[3,-1])))
tree13_8_values[,tree13_8_predictions:=predict(tree_fit13_8,tree13_8_values)]
colnames(tree13_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree13_8_values = cbind(time_index_column, tree13_8_values)
data_plot13_8 = melt(tree13_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted13_8 = ggplot(data_plot13_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted13_8




minimize(tree_fit13_1$cptable[length(tree_fit13_1$cptable[, "xerror"]), "xerror"],
         tree_fit13_2$cptable[length(tree_fit13_2$cptable[, "xerror"]), "xerror"],
         tree_fit13_3$cptable[length(tree_fit13_3$cptable[, "xerror"]), "xerror"],
         tree_fit13_4$cptable[length(tree_fit13_4$cptable[, "xerror"]), "xerror"],
         tree_fit13_5$cptable[length(tree_fit13_5$cptable[, "xerror"]), "xerror"],
         tree_fit13_6$cptable[length(tree_fit13_6$cptable[, "xerror"]), "xerror"],
         tree_fit13_7$cptable[length(tree_fit13_7$cptable[, "xerror"]), "xerror"],
         tree_fit13_8$cptable[length(tree_fit13_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted13_4
#maxdepth = 4 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 4

############################################



value14_1 = as.numeric(selected_dt2[4,-1])
tree_fit14_1=rpart(value14_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_1)


tree14_1_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_1_values[,tree14_1_predictions:=predict(tree_fit14_1,tree14_1_values)]
colnames(tree14_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_1_values = cbind(time_index_column, tree14_1_values)
data_plot14_1 = melt(tree14_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_1 = ggplot(data_plot14_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_1







value14_2 = as.numeric(selected_dt2[4,-1])
tree_fit14_2=rpart(value14_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_2)


tree14_2_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_2_values[,tree14_2_predictions:=predict(tree_fit14_2,tree14_2_values)]
colnames(tree14_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_2_values = cbind(time_index_column, tree14_2_values)
data_plot14_2 = melt(tree14_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_2 = ggplot(data_plot14_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_2







value14_3 = as.numeric(selected_dt2[4,-1])
tree_fit14_3=rpart(value14_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_3)


tree14_3_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_3_values[,tree14_3_predictions:=predict(tree_fit14_3,tree14_3_values)]
colnames(tree14_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_3_values = cbind(time_index_column, tree14_3_values)
data_plot14_3 = melt(tree14_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_3 = ggplot(data_plot14_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_3







value14_4 = as.numeric(selected_dt2[4,-1])
tree_fit14_4=rpart(value14_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_4)


tree14_4_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_4_values[,tree14_4_predictions:=predict(tree_fit14_4,tree14_4_values)]
colnames(tree14_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_4_values = cbind(time_index_column, tree14_4_values)
data_plot14_4 = melt(tree14_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_4 = ggplot(data_plot14_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_4








value14_5 = as.numeric(selected_dt2[4,-1])
tree_fit14_5=rpart(value14_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_5)


tree14_5_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_5_values[,tree14_5_predictions:=predict(tree_fit14_5,tree14_5_values)]
colnames(tree14_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_5_values = cbind(time_index_column, tree14_5_values)
data_plot14_5 = melt(tree14_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_5 = ggplot(data_plot14_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_5








value14_6 = as.numeric(selected_dt2[4,-1])
tree_fit14_6=rpart(value14_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_6)


tree14_6_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_6_values[,tree14_6_predictions:=predict(tree_fit14_6,tree14_6_values)]
colnames(tree14_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_6_values = cbind(time_index_column, tree14_6_values)
data_plot14_6 = melt(tree14_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_6 = ggplot(data_plot14_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_6






value14_7 = as.numeric(selected_dt2[4,-1])
tree_fit14_7=rpart(value14_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_7)


tree14_7_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_7_values[,tree14_7_predictions:=predict(tree_fit14_7,tree14_7_values)]
colnames(tree14_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_7_values = cbind(time_index_column, tree14_7_values)
data_plot14_7 = melt(tree14_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_7 = ggplot(data_plot14_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_7




value14_8 = as.numeric(selected_dt2[4,-1])
tree_fit14_8=rpart(value14_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit14_8)


tree14_8_values = as.data.table(t(data.table(selected_dt2[4,-1])))
tree14_8_values[,tree14_8_predictions:=predict(tree_fit14_8,tree14_8_values)]
colnames(tree14_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree14_8_values = cbind(time_index_column, tree14_8_values)
data_plot14_8 = melt(tree14_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted14_8 = ggplot(data_plot14_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted14_8




minimize(tree_fit14_1$cptable[length(tree_fit14_1$cptable[, "xerror"]), "xerror"],
         tree_fit14_2$cptable[length(tree_fit14_2$cptable[, "xerror"]), "xerror"],
         tree_fit14_3$cptable[length(tree_fit14_3$cptable[, "xerror"]), "xerror"],
         tree_fit14_4$cptable[length(tree_fit14_4$cptable[, "xerror"]), "xerror"],
         tree_fit14_5$cptable[length(tree_fit14_5$cptable[, "xerror"]), "xerror"],
         tree_fit14_6$cptable[length(tree_fit14_6$cptable[, "xerror"]), "xerror"],
         tree_fit14_7$cptable[length(tree_fit14_7$cptable[, "xerror"]), "xerror"],
         tree_fit14_8$cptable[length(tree_fit14_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted14_3
#maxdepth = 3 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 3

############################################


value15_1 = as.numeric(selected_dt2[5,-1])
tree_fit15_1=rpart(value15_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_1)


tree15_1_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_1_values[,tree15_1_predictions:=predict(tree_fit15_1,tree15_1_values)]
colnames(tree15_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_1_values = cbind(time_index_column, tree15_1_values)
data_plot15_1 = melt(tree15_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_1 = ggplot(data_plot15_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_1







value15_2 = as.numeric(selected_dt2[5,-1])
tree_fit15_2=rpart(value15_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_2)


tree15_2_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_2_values[,tree15_2_predictions:=predict(tree_fit15_2,tree15_2_values)]
colnames(tree15_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_2_values = cbind(time_index_column, tree15_2_values)
data_plot15_2 = melt(tree15_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_2 = ggplot(data_plot15_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_2







value15_3 = as.numeric(selected_dt2[5,-1])
tree_fit15_3=rpart(value15_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_3)


tree15_3_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_3_values[,tree15_3_predictions:=predict(tree_fit15_3,tree15_3_values)]
colnames(tree15_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_3_values = cbind(time_index_column, tree15_3_values)
data_plot15_3 = melt(tree15_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_3 = ggplot(data_plot15_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_3







value15_4 = as.numeric(selected_dt2[5,-1])
tree_fit15_4=rpart(value15_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_4)


tree15_4_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_4_values[,tree15_4_predictions:=predict(tree_fit15_4,tree15_4_values)]
colnames(tree15_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_4_values = cbind(time_index_column, tree15_4_values)
data_plot15_4 = melt(tree15_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_4 = ggplot(data_plot15_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_4








value15_5 = as.numeric(selected_dt2[5,-1])
tree_fit15_5=rpart(value15_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_5)


tree15_5_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_5_values[,tree15_5_predictions:=predict(tree_fit15_5,tree15_5_values)]
colnames(tree15_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_5_values = cbind(time_index_column, tree15_5_values)
data_plot15_5 = melt(tree15_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_5 = ggplot(data_plot15_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_5








value15_6 = as.numeric(selected_dt2[5,-1])
tree_fit15_6=rpart(value15_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_6)


tree15_6_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_6_values[,tree15_6_predictions:=predict(tree_fit15_6,tree15_6_values)]
colnames(tree15_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_6_values = cbind(time_index_column, tree15_6_values)
data_plot15_6 = melt(tree15_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_6 = ggplot(data_plot15_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_6






value15_7 = as.numeric(selected_dt2[5,-1])
tree_fit15_7=rpart(value15_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_7)


tree15_7_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_7_values[,tree15_7_predictions:=predict(tree_fit15_7,tree15_7_values)]
colnames(tree15_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_7_values = cbind(time_index_column, tree15_7_values)
data_plot15_7 = melt(tree15_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_7 = ggplot(data_plot15_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_7




value15_8 = as.numeric(selected_dt2[5,-1])
tree_fit15_8=rpart(value15_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit15_8)


tree15_8_values = as.data.table(t(data.table(selected_dt2[5,-1])))
tree15_8_values[,tree15_8_predictions:=predict(tree_fit15_8,tree15_8_values)]
colnames(tree15_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree15_8_values = cbind(time_index_column, tree15_8_values)
data_plot15_8 = melt(tree15_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted15_8 = ggplot(data_plot15_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted15_8




minimize(tree_fit15_1$cptable[length(tree_fit15_1$cptable[, "xerror"]), "xerror"],
         tree_fit15_2$cptable[length(tree_fit15_2$cptable[, "xerror"]), "xerror"],
         tree_fit15_3$cptable[length(tree_fit15_3$cptable[, "xerror"]), "xerror"],
         tree_fit15_4$cptable[length(tree_fit15_4$cptable[, "xerror"]), "xerror"],
         tree_fit15_5$cptable[length(tree_fit15_5$cptable[, "xerror"]), "xerror"],
         tree_fit15_6$cptable[length(tree_fit15_6$cptable[, "xerror"]), "xerror"],
         tree_fit15_7$cptable[length(tree_fit15_7$cptable[, "xerror"]), "xerror"],
         tree_fit15_8$cptable[length(tree_fit15_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted15_3
#maxdepth = 3 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 3




################################################

value16_1 = as.numeric(selected_dt2[6,-1])
tree_fit16_1=rpart(value16_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_1)


tree16_1_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_1_values[,tree16_1_predictions:=predict(tree_fit16_1,tree16_1_values)]
colnames(tree16_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_1_values = cbind(time_index_column, tree16_1_values)
data_plot16_1 = melt(tree16_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_1 = ggplot(data_plot16_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_1







value16_2 = as.numeric(selected_dt2[6,-1])
tree_fit16_2=rpart(value16_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_2)


tree16_2_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_2_values[,tree16_2_predictions:=predict(tree_fit16_2,tree16_2_values)]
colnames(tree16_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_2_values = cbind(time_index_column, tree16_2_values)
data_plot16_2 = melt(tree16_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_2 = ggplot(data_plot16_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_2







value16_3 = as.numeric(selected_dt2[6,-1])
tree_fit16_3=rpart(value16_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_3)


tree16_3_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_3_values[,tree16_3_predictions:=predict(tree_fit16_3,tree16_3_values)]
colnames(tree16_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_3_values = cbind(time_index_column, tree16_3_values)
data_plot16_3 = melt(tree16_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_3 = ggplot(data_plot16_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_3







value16_4 = as.numeric(selected_dt2[6,-1])
tree_fit16_4=rpart(value16_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_4)


tree16_4_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_4_values[,tree16_4_predictions:=predict(tree_fit16_4,tree16_4_values)]
colnames(tree16_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_4_values = cbind(time_index_column, tree16_4_values)
data_plot16_4 = melt(tree16_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_4 = ggplot(data_plot16_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_4








value16_5 = as.numeric(selected_dt2[6,-1])
tree_fit16_5=rpart(value16_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_5)


tree16_5_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_5_values[,tree16_5_predictions:=predict(tree_fit16_5,tree16_5_values)]
colnames(tree16_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_5_values = cbind(time_index_column, tree16_5_values)
data_plot16_5 = melt(tree16_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_5 = ggplot(data_plot16_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_5








value16_6 = as.numeric(selected_dt2[6,-1])
tree_fit16_6=rpart(value16_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_6)


tree16_6_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_6_values[,tree16_6_predictions:=predict(tree_fit16_6,tree16_6_values)]
colnames(tree16_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_6_values = cbind(time_index_column, tree16_6_values)
data_plot16_6 = melt(tree16_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_6 = ggplot(data_plot16_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_6






value16_7 = as.numeric(selected_dt2[6,-1])
tree_fit16_7=rpart(value16_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_7)


tree16_7_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_7_values[,tree16_7_predictions:=predict(tree_fit16_7,tree16_7_values)]
colnames(tree16_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_7_values = cbind(time_index_column, tree16_7_values)
data_plot16_7 = melt(tree16_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_7 = ggplot(data_plot16_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_7




value16_8 = as.numeric(selected_dt2[6,-1])
tree_fit16_8=rpart(value16_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit16_8)


tree16_8_values = as.data.table(t(data.table(selected_dt2[6,-1])))
tree16_8_values[,tree16_8_predictions:=predict(tree_fit16_8,tree16_8_values)]
colnames(tree16_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree16_8_values = cbind(time_index_column, tree16_8_values)
data_plot16_8 = melt(tree16_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted16_8 = ggplot(data_plot16_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted16_8




minimize(tree_fit16_1$cptable[length(tree_fit16_1$cptable[, "xerror"]), "xerror"],
         tree_fit16_2$cptable[length(tree_fit16_2$cptable[, "xerror"]), "xerror"],
         tree_fit16_3$cptable[length(tree_fit16_3$cptable[, "xerror"]), "xerror"],
         tree_fit16_4$cptable[length(tree_fit16_4$cptable[, "xerror"]), "xerror"],
         tree_fit16_5$cptable[length(tree_fit16_5$cptable[, "xerror"]), "xerror"],
         tree_fit16_6$cptable[length(tree_fit16_6$cptable[, "xerror"]), "xerror"],
         tree_fit16_7$cptable[length(tree_fit16_7$cptable[, "xerror"]), "xerror"],
         tree_fit16_8$cptable[length(tree_fit16_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted16_6
#maxdepth = 6 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 6


##################################


value17_1 = as.numeric(selected_dt2[7,-1])
tree_fit17_1=rpart(value17_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_1)


tree17_1_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_1_values[,tree17_1_predictions:=predict(tree_fit17_1,tree17_1_values)]
colnames(tree17_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_1_values = cbind(time_index_column, tree17_1_values)
data_plot17_1 = melt(tree17_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_1 = ggplot(data_plot17_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_1







value17_2 = as.numeric(selected_dt2[7,-1])
tree_fit17_2=rpart(value17_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_2)


tree17_2_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_2_values[,tree17_2_predictions:=predict(tree_fit17_2,tree17_2_values)]
colnames(tree17_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_2_values = cbind(time_index_column, tree17_2_values)
data_plot17_2 = melt(tree17_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_2 = ggplot(data_plot17_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_2







value17_3 = as.numeric(selected_dt2[7,-1])
tree_fit17_3=rpart(value17_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_3)


tree17_3_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_3_values[,tree17_3_predictions:=predict(tree_fit17_3,tree17_3_values)]
colnames(tree17_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_3_values = cbind(time_index_column, tree17_3_values)
data_plot17_3 = melt(tree17_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_3 = ggplot(data_plot17_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_3







value17_4 = as.numeric(selected_dt2[7,-1])
tree_fit17_4=rpart(value17_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_4)


tree17_4_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_4_values[,tree17_4_predictions:=predict(tree_fit17_4,tree17_4_values)]
colnames(tree17_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_4_values = cbind(time_index_column, tree17_4_values)
data_plot17_4 = melt(tree17_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_4 = ggplot(data_plot17_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_4








value17_5 = as.numeric(selected_dt2[7,-1])
tree_fit17_5=rpart(value17_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_5)


tree17_5_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_5_values[,tree17_5_predictions:=predict(tree_fit17_5,tree17_5_values)]
colnames(tree17_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_5_values = cbind(time_index_column, tree17_5_values)
data_plot17_5 = melt(tree17_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_5 = ggplot(data_plot17_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_5








value17_6 = as.numeric(selected_dt2[7,-1])
tree_fit17_6=rpart(value17_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_6)


tree17_6_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_6_values[,tree17_6_predictions:=predict(tree_fit17_6,tree17_6_values)]
colnames(tree17_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_6_values = cbind(time_index_column, tree17_6_values)
data_plot17_6 = melt(tree17_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_6 = ggplot(data_plot17_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_6






value17_7 = as.numeric(selected_dt2[7,-1])
tree_fit17_7=rpart(value17_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_7)


tree17_7_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_7_values[,tree17_7_predictions:=predict(tree_fit17_7,tree17_7_values)]
colnames(tree17_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_7_values = cbind(time_index_column, tree17_7_values)
data_plot17_7 = melt(tree17_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_7 = ggplot(data_plot17_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_7




value17_8 = as.numeric(selected_dt2[7,-1])
tree_fit17_8=rpart(value17_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit17_8)


tree17_8_values = as.data.table(t(data.table(selected_dt2[7,-1])))
tree17_8_values[,tree17_8_predictions:=predict(tree_fit17_8,tree17_8_values)]
colnames(tree17_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree17_8_values = cbind(time_index_column, tree17_8_values)
data_plot17_8 = melt(tree17_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted17_8 = ggplot(data_plot17_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted17_8




minimize(tree_fit17_1$cptable[length(tree_fit17_1$cptable[, "xerror"]), "xerror"],
         tree_fit17_2$cptable[length(tree_fit17_2$cptable[, "xerror"]), "xerror"],
         tree_fit17_3$cptable[length(tree_fit17_3$cptable[, "xerror"]), "xerror"],
         tree_fit17_4$cptable[length(tree_fit17_4$cptable[, "xerror"]), "xerror"],
         tree_fit17_5$cptable[length(tree_fit17_5$cptable[, "xerror"]), "xerror"],
         tree_fit17_6$cptable[length(tree_fit17_6$cptable[, "xerror"]), "xerror"],
         tree_fit17_7$cptable[length(tree_fit17_7$cptable[, "xerror"]), "xerror"],
         tree_fit17_8$cptable[length(tree_fit17_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted17_8
#maxdepth = 8 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 8





#################################

value18_1 = as.numeric(selected_dt2[8,-1])
tree_fit18_1=rpart(value18_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_1)


tree18_1_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_1_values[,tree18_1_predictions:=predict(tree_fit18_1,tree18_1_values)]
colnames(tree18_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_1_values = cbind(time_index_column, tree18_1_values)
data_plot18_1 = melt(tree18_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_1 = ggplot(data_plot18_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_1







value18_2 = as.numeric(selected_dt2[8,-1])
tree_fit18_2=rpart(value18_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_2)


tree18_2_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_2_values[,tree18_2_predictions:=predict(tree_fit18_2,tree18_2_values)]
colnames(tree18_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_2_values = cbind(time_index_column, tree18_2_values)
data_plot18_2 = melt(tree18_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_2 = ggplot(data_plot18_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_2







value18_3 = as.numeric(selected_dt2[8,-1])
tree_fit18_3=rpart(value18_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_3)


tree18_3_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_3_values[,tree18_3_predictions:=predict(tree_fit18_3,tree18_3_values)]
colnames(tree18_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_3_values = cbind(time_index_column, tree18_3_values)
data_plot18_3 = melt(tree18_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_3 = ggplot(data_plot18_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_3







value18_4 = as.numeric(selected_dt2[8,-1])
tree_fit18_4=rpart(value18_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_4)


tree18_4_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_4_values[,tree18_4_predictions:=predict(tree_fit18_4,tree18_4_values)]
colnames(tree18_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_4_values = cbind(time_index_column, tree18_4_values)
data_plot18_4 = melt(tree18_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_4 = ggplot(data_plot18_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_4








value18_5 = as.numeric(selected_dt2[8,-1])
tree_fit18_5=rpart(value18_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_5)


tree18_5_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_5_values[,tree18_5_predictions:=predict(tree_fit18_5,tree18_5_values)]
colnames(tree18_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_5_values = cbind(time_index_column, tree18_5_values)
data_plot18_5 = melt(tree18_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_5 = ggplot(data_plot18_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_5








value18_6 = as.numeric(selected_dt2[8,-1])
tree_fit18_6=rpart(value18_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_6)


tree18_6_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_6_values[,tree18_6_predictions:=predict(tree_fit18_6,tree18_6_values)]
colnames(tree18_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_6_values = cbind(time_index_column, tree18_6_values)
data_plot18_6 = melt(tree18_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_6 = ggplot(data_plot18_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_6






value18_7 = as.numeric(selected_dt2[8,-1])
tree_fit18_7=rpart(value18_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_7)


tree18_7_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_7_values[,tree18_7_predictions:=predict(tree_fit18_7,tree18_7_values)]
colnames(tree18_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_7_values = cbind(time_index_column, tree18_7_values)
data_plot18_7 = melt(tree18_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_7 = ggplot(data_plot18_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_7




value18_8 = as.numeric(selected_dt2[8,-1])
tree_fit18_8=rpart(value18_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit18_8)


tree18_8_values = as.data.table(t(data.table(selected_dt2[8,-1])))
tree18_8_values[,tree18_8_predictions:=predict(tree_fit18_8,tree18_8_values)]
colnames(tree18_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree18_8_values = cbind(time_index_column, tree18_8_values)
data_plot18_8 = melt(tree18_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted18_8 = ggplot(data_plot18_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted18_8




minimize(tree_fit18_1$cptable[length(tree_fit18_1$cptable[, "xerror"]), "xerror"],
         tree_fit18_2$cptable[length(tree_fit18_2$cptable[, "xerror"]), "xerror"],
         tree_fit18_3$cptable[length(tree_fit18_3$cptable[, "xerror"]), "xerror"],
         tree_fit18_4$cptable[length(tree_fit18_4$cptable[, "xerror"]), "xerror"],
         tree_fit18_5$cptable[length(tree_fit18_5$cptable[, "xerror"]), "xerror"],
         tree_fit18_6$cptable[length(tree_fit18_6$cptable[, "xerror"]), "xerror"],
         tree_fit18_7$cptable[length(tree_fit18_7$cptable[, "xerror"]), "xerror"],
         tree_fit18_8$cptable[length(tree_fit18_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted18_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5


###########################


value19_1 = as.numeric(selected_dt2[9,-1])
tree_fit19_1=rpart(value19_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_1)


tree19_1_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_1_values[,tree19_1_predictions:=predict(tree_fit19_1,tree19_1_values)]
colnames(tree19_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_1_values = cbind(time_index_column, tree19_1_values)
data_plot19_1 = melt(tree19_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_1 = ggplot(data_plot19_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_1







value19_2 = as.numeric(selected_dt2[9,-1])
tree_fit19_2=rpart(value19_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_2)


tree19_2_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_2_values[,tree19_2_predictions:=predict(tree_fit19_2,tree19_2_values)]
colnames(tree19_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_2_values = cbind(time_index_column, tree19_2_values)
data_plot19_2 = melt(tree19_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_2 = ggplot(data_plot19_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_2







value19_3 = as.numeric(selected_dt2[9,-1])
tree_fit19_3=rpart(value19_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_3)


tree19_3_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_3_values[,tree19_3_predictions:=predict(tree_fit19_3,tree19_3_values)]
colnames(tree19_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_3_values = cbind(time_index_column, tree19_3_values)
data_plot19_3 = melt(tree19_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_3 = ggplot(data_plot19_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_3







value19_4 = as.numeric(selected_dt2[9,-1])
tree_fit19_4=rpart(value19_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_4)


tree19_4_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_4_values[,tree19_4_predictions:=predict(tree_fit19_4,tree19_4_values)]
colnames(tree19_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_4_values = cbind(time_index_column, tree19_4_values)
data_plot19_4 = melt(tree19_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_4 = ggplot(data_plot19_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_4








value19_5 = as.numeric(selected_dt2[9,-1])
tree_fit19_5=rpart(value19_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_5)


tree19_5_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_5_values[,tree19_5_predictions:=predict(tree_fit19_5,tree19_5_values)]
colnames(tree19_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_5_values = cbind(time_index_column, tree19_5_values)
data_plot19_5 = melt(tree19_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_5 = ggplot(data_plot19_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_5








value19_6 = as.numeric(selected_dt2[9,-1])
tree_fit19_6=rpart(value19_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_6)


tree19_6_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_6_values[,tree19_6_predictions:=predict(tree_fit19_6,tree19_6_values)]
colnames(tree19_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_6_values = cbind(time_index_column, tree19_6_values)
data_plot19_6 = melt(tree19_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_6 = ggplot(data_plot19_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_6






value19_7 = as.numeric(selected_dt2[9,-1])
tree_fit19_7=rpart(value19_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_7)


tree19_7_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_7_values[,tree19_7_predictions:=predict(tree_fit19_7,tree19_7_values)]
colnames(tree19_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_7_values = cbind(time_index_column, tree19_7_values)
data_plot19_7 = melt(tree19_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_7 = ggplot(data_plot19_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_7




value19_8 = as.numeric(selected_dt2[9,-1])
tree_fit19_8=rpart(value19_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit19_8)


tree19_8_values = as.data.table(t(data.table(selected_dt2[9,-1])))
tree19_8_values[,tree19_8_predictions:=predict(tree_fit19_8,tree19_8_values)]
colnames(tree19_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree19_8_values = cbind(time_index_column, tree19_8_values)
data_plot19_8 = melt(tree19_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted19_8 = ggplot(data_plot19_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted19_8




minimize(tree_fit19_1$cptable[length(tree_fit19_1$cptable[, "xerror"]), "xerror"],
         tree_fit19_2$cptable[length(tree_fit19_2$cptable[, "xerror"]), "xerror"],
         tree_fit19_3$cptable[length(tree_fit19_3$cptable[, "xerror"]), "xerror"],
         tree_fit19_4$cptable[length(tree_fit19_4$cptable[, "xerror"]), "xerror"],
         tree_fit19_5$cptable[length(tree_fit19_5$cptable[, "xerror"]), "xerror"],
         tree_fit19_6$cptable[length(tree_fit19_6$cptable[, "xerror"]), "xerror"],
         tree_fit19_7$cptable[length(tree_fit19_7$cptable[, "xerror"]), "xerror"],
         tree_fit19_8$cptable[length(tree_fit19_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted19_8
#maxdepth = 8 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 8









value20_1 = as.numeric(selected_dt2[10,-1])
tree_fit20_1=rpart(value20_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_1)


tree20_1_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_1_values[,tree20_1_predictions:=predict(tree_fit20_1,tree20_1_values)]
colnames(tree20_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_1_values = cbind(time_index_column, tree20_1_values)
data_plot20_1 = melt(tree20_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_1 = ggplot(data_plot20_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_1







value20_2 = as.numeric(selected_dt2[10,-1])
tree_fit20_2=rpart(value20_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_2)


tree20_2_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_2_values[,tree20_2_predictions:=predict(tree_fit20_2,tree20_2_values)]
colnames(tree20_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_2_values = cbind(time_index_column, tree20_2_values)
data_plot20_2 = melt(tree20_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_2 = ggplot(data_plot20_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_2







value20_3 = as.numeric(selected_dt2[10,-1])
tree_fit20_3=rpart(value20_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_3)


tree20_3_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_3_values[,tree20_3_predictions:=predict(tree_fit20_3,tree20_3_values)]
colnames(tree20_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_3_values = cbind(time_index_column, tree20_3_values)
data_plot20_3 = melt(tree20_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_3 = ggplot(data_plot20_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_3







value20_4 = as.numeric(selected_dt2[10,-1])
tree_fit20_4=rpart(value20_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_4)


tree20_4_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_4_values[,tree20_4_predictions:=predict(tree_fit20_4,tree20_4_values)]
colnames(tree20_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_4_values = cbind(time_index_column, tree20_4_values)
data_plot20_4 = melt(tree20_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_4 = ggplot(data_plot20_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_4








value20_5 = as.numeric(selected_dt2[10,-1])
tree_fit20_5=rpart(value20_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_5)


tree20_5_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_5_values[,tree20_5_predictions:=predict(tree_fit20_5,tree20_5_values)]
colnames(tree20_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_5_values = cbind(time_index_column, tree20_5_values)
data_plot20_5 = melt(tree20_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_5 = ggplot(data_plot20_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_5








value20_6 = as.numeric(selected_dt2[10,-1])
tree_fit20_6=rpart(value20_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_6)


tree20_6_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_6_values[,tree20_6_predictions:=predict(tree_fit20_6,tree20_6_values)]
colnames(tree20_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_6_values = cbind(time_index_column, tree20_6_values)
data_plot20_6 = melt(tree20_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_6 = ggplot(data_plot20_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_6






value20_7 = as.numeric(selected_dt2[10,-1])
tree_fit20_7=rpart(value20_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_7)


tree20_7_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_7_values[,tree20_7_predictions:=predict(tree_fit20_7,tree20_7_values)]
colnames(tree20_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_7_values = cbind(time_index_column, tree20_7_values)
data_plot20_7 = melt(tree20_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_7 = ggplot(data_plot20_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_7




value20_8 = as.numeric(selected_dt2[10,-1])
tree_fit20_8=rpart(value20_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit20_8)


tree20_8_values = as.data.table(t(data.table(selected_dt2[10,-1])))
tree20_8_values[,tree20_8_predictions:=predict(tree_fit20_8,tree20_8_values)]
colnames(tree20_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree20_8_values = cbind(time_index_column, tree20_8_values)
data_plot20_8 = melt(tree20_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted20_8 = ggplot(data_plot20_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted20_8




minimize(tree_fit20_1$cptable[length(tree_fit20_1$cptable[, "xerror"]), "xerror"],
         tree_fit20_2$cptable[length(tree_fit20_2$cptable[, "xerror"]), "xerror"],
         tree_fit20_3$cptable[length(tree_fit20_3$cptable[, "xerror"]), "xerror"],
         tree_fit20_4$cptable[length(tree_fit20_4$cptable[, "xerror"]), "xerror"],
         tree_fit20_5$cptable[length(tree_fit20_5$cptable[, "xerror"]), "xerror"],
         tree_fit20_6$cptable[length(tree_fit20_6$cptable[, "xerror"]), "xerror"],
         tree_fit20_7$cptable[length(tree_fit20_7$cptable[, "xerror"]), "xerror"],
         tree_fit20_8$cptable[length(tree_fit20_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted20_3
#maxdepth = 3 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 3




####################################################


value21_1 = as.numeric(selected_dt2[11,-1])
tree_fit21_1=rpart(value21_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_1)


tree21_1_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_1_values[,tree21_1_predictions:=predict(tree_fit21_1,tree21_1_values)]
colnames(tree21_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_1_values = cbind(time_index_column, tree21_1_values)
data_plot21_1 = melt(tree21_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_1 = ggplot(data_plot21_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_1







value21_2 = as.numeric(selected_dt2[11,-1])
tree_fit21_2=rpart(value21_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_2)


tree21_2_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_2_values[,tree21_2_predictions:=predict(tree_fit21_2,tree21_2_values)]
colnames(tree21_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_2_values = cbind(time_index_column, tree21_2_values)
data_plot21_2 = melt(tree21_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_2 = ggplot(data_plot21_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_2







value21_3 = as.numeric(selected_dt2[11,-1])
tree_fit21_3=rpart(value21_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_3)


tree21_3_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_3_values[,tree21_3_predictions:=predict(tree_fit21_3,tree21_3_values)]
colnames(tree21_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_3_values = cbind(time_index_column, tree21_3_values)
data_plot21_3 = melt(tree21_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_3 = ggplot(data_plot21_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_3







value21_4 = as.numeric(selected_dt2[11,-1])
tree_fit21_4=rpart(value21_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_4)


tree21_4_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_4_values[,tree21_4_predictions:=predict(tree_fit21_4,tree21_4_values)]
colnames(tree21_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_4_values = cbind(time_index_column, tree21_4_values)
data_plot21_4 = melt(tree21_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_4 = ggplot(data_plot21_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_4








value21_5 = as.numeric(selected_dt2[11,-1])
tree_fit21_5=rpart(value21_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_5)


tree21_5_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_5_values[,tree21_5_predictions:=predict(tree_fit21_5,tree21_5_values)]
colnames(tree21_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_5_values = cbind(time_index_column, tree21_5_values)
data_plot21_5 = melt(tree21_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_5 = ggplot(data_plot21_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_5








value21_6 = as.numeric(selected_dt2[11,-1])
tree_fit21_6=rpart(value21_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_6)


tree21_6_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_6_values[,tree21_6_predictions:=predict(tree_fit21_6,tree21_6_values)]
colnames(tree21_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_6_values = cbind(time_index_column, tree21_6_values)
data_plot21_6 = melt(tree21_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_6 = ggplot(data_plot21_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_6






value21_7 = as.numeric(selected_dt2[11,-1])
tree_fit21_7=rpart(value21_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_7)


tree21_7_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_7_values[,tree21_7_predictions:=predict(tree_fit21_7,tree21_7_values)]
colnames(tree21_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_7_values = cbind(time_index_column, tree21_7_values)
data_plot21_7 = melt(tree21_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_7 = ggplot(data_plot21_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_7




value21_8 = as.numeric(selected_dt2[11,-1])
tree_fit21_8=rpart(value21_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit21_8)


tree21_8_values = as.data.table(t(data.table(selected_dt2[11,-1])))
tree21_8_values[,tree21_8_predictions:=predict(tree_fit21_8,tree21_8_values)]
colnames(tree21_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree21_8_values = cbind(time_index_column, tree21_8_values)
data_plot21_8 = melt(tree21_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted21_8 = ggplot(data_plot21_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted21_8




minimize(tree_fit21_1$cptable[length(tree_fit21_1$cptable[, "xerror"]), "xerror"],
         tree_fit21_2$cptable[length(tree_fit21_2$cptable[, "xerror"]), "xerror"],
         tree_fit21_3$cptable[length(tree_fit21_3$cptable[, "xerror"]), "xerror"],
         tree_fit21_4$cptable[length(tree_fit21_4$cptable[, "xerror"]), "xerror"],
         tree_fit21_5$cptable[length(tree_fit21_5$cptable[, "xerror"]), "xerror"],
         tree_fit21_6$cptable[length(tree_fit21_6$cptable[, "xerror"]), "xerror"],
         tree_fit21_7$cptable[length(tree_fit21_7$cptable[, "xerror"]), "xerror"],
         tree_fit21_8$cptable[length(tree_fit21_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted21_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7, 8; 
#therefore maxdepth should be equal to 5







value22_1 = as.numeric(selected_dt2[12,-1])
tree_fit22_1=rpart(value22_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_1)


tree22_1_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_1_values[,tree22_1_predictions:=predict(tree_fit22_1,tree22_1_values)]
colnames(tree22_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_1_values = cbind(time_index_column, tree22_1_values)
data_plot22_1 = melt(tree22_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_1 = ggplot(data_plot22_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_1







value22_2 = as.numeric(selected_dt2[12,-1])
tree_fit22_2=rpart(value22_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_2)


tree22_2_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_2_values[,tree22_2_predictions:=predict(tree_fit22_2,tree22_2_values)]
colnames(tree22_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_2_values = cbind(time_index_column, tree22_2_values)
data_plot22_2 = melt(tree22_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_2 = ggplot(data_plot22_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_2







value22_3 = as.numeric(selected_dt2[12,-1])
tree_fit22_3=rpart(value22_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_3)


tree22_3_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_3_values[,tree22_3_predictions:=predict(tree_fit22_3,tree22_3_values)]
colnames(tree22_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_3_values = cbind(time_index_column, tree22_3_values)
data_plot22_3 = melt(tree22_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_3 = ggplot(data_plot22_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_3







value22_4 = as.numeric(selected_dt2[12,-1])
tree_fit22_4=rpart(value22_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_4)


tree22_4_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_4_values[,tree22_4_predictions:=predict(tree_fit22_4,tree22_4_values)]
colnames(tree22_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_4_values = cbind(time_index_column, tree22_4_values)
data_plot22_4 = melt(tree22_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_4 = ggplot(data_plot22_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_4








value22_5 = as.numeric(selected_dt2[12,-1])
tree_fit22_5=rpart(value22_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_5)


tree22_5_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_5_values[,tree22_5_predictions:=predict(tree_fit22_5,tree22_5_values)]
colnames(tree22_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_5_values = cbind(time_index_column, tree22_5_values)
data_plot22_5 = melt(tree22_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_5 = ggplot(data_plot22_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_5








value22_6 = as.numeric(selected_dt2[12,-1])
tree_fit22_6=rpart(value22_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_6)


tree22_6_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_6_values[,tree22_6_predictions:=predict(tree_fit22_6,tree22_6_values)]
colnames(tree22_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_6_values = cbind(time_index_column, tree22_6_values)
data_plot22_6 = melt(tree22_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_6 = ggplot(data_plot22_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_6






value22_7 = as.numeric(selected_dt2[12,-1])
tree_fit22_7=rpart(value22_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_7)


tree22_7_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_7_values[,tree22_7_predictions:=predict(tree_fit22_7,tree22_7_values)]
colnames(tree22_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_7_values = cbind(time_index_column, tree22_7_values)
data_plot22_7 = melt(tree22_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_7 = ggplot(data_plot22_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_7




value22_8 = as.numeric(selected_dt2[12,-1])
tree_fit22_8=rpart(value22_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit22_8)


tree22_8_values = as.data.table(t(data.table(selected_dt2[12,-1])))
tree22_8_values[,tree22_8_predictions:=predict(tree_fit22_8,tree22_8_values)]
colnames(tree22_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree22_8_values = cbind(time_index_column, tree22_8_values)
data_plot22_8 = melt(tree22_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted22_8 = ggplot(data_plot22_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted22_8




minimize(tree_fit22_1$cptable[length(tree_fit22_1$cptable[, "xerror"]), "xerror"],
         tree_fit22_2$cptable[length(tree_fit22_2$cptable[, "xerror"]), "xerror"],
         tree_fit22_3$cptable[length(tree_fit22_3$cptable[, "xerror"]), "xerror"],
         tree_fit22_4$cptable[length(tree_fit22_4$cptable[, "xerror"]), "xerror"],
         tree_fit22_5$cptable[length(tree_fit22_5$cptable[, "xerror"]), "xerror"],
         tree_fit22_6$cptable[length(tree_fit22_6$cptable[, "xerror"]), "xerror"],
         tree_fit22_7$cptable[length(tree_fit22_7$cptable[, "xerror"]), "xerror"],
         tree_fit22_8$cptable[length(tree_fit22_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted22_7
#maxdepth = 7 gives the best cv error according to cp values among 1,2,3,4,5,6,7, 8; 
#therefore maxdepth should be equal to 7






####################################################

#class 3 teki her time series in regression tree si

selected_dt3 = train_dataset_usable_with_time[24:31,]

value23_1 = as.numeric(selected_dt3[1,-1])
tree_fit23_1=rpart(value23_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_1)


tree23_1_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_1_values[,tree23_1_predictions:=predict(tree_fit23_1,tree23_1_values)]
colnames(tree23_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_1_values = cbind(time_index_column, tree23_1_values)
data_plot23_1 = melt(tree23_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_1 = ggplot(data_plot23_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_1







value23_2 = as.numeric(selected_dt3[1,-1])
tree_fit23_2=rpart(value23_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_2)


tree23_2_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_2_values[,tree23_2_predictions:=predict(tree_fit23_2,tree23_2_values)]
colnames(tree23_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_2_values = cbind(time_index_column, tree23_2_values)
data_plot23_2 = melt(tree23_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_2 = ggplot(data_plot23_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_2







value23_3 = as.numeric(selected_dt3[1,-1])
tree_fit23_3=rpart(value23_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_3)


tree23_3_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_3_values[,tree23_3_predictions:=predict(tree_fit23_3,tree23_3_values)]
colnames(tree23_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_3_values = cbind(time_index_column, tree23_3_values)
data_plot23_3 = melt(tree23_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_3 = ggplot(data_plot23_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_3







value23_4 = as.numeric(selected_dt3[1,-1])
tree_fit23_4=rpart(value23_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_4)


tree23_4_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_4_values[,tree23_4_predictions:=predict(tree_fit23_4,tree23_4_values)]
colnames(tree23_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_4_values = cbind(time_index_column, tree23_4_values)
data_plot23_4 = melt(tree23_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_4 = ggplot(data_plot23_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_4








value23_5 = as.numeric(selected_dt3[1,-1])
tree_fit23_5=rpart(value23_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_5)


tree23_5_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_5_values[,tree23_5_predictions:=predict(tree_fit23_5,tree23_5_values)]
colnames(tree23_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_5_values = cbind(time_index_column, tree23_5_values)
data_plot23_5 = melt(tree23_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_5 = ggplot(data_plot23_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_5








value23_6 = as.numeric(selected_dt3[1,-1])
tree_fit23_6=rpart(value23_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_6)


tree23_6_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_6_values[,tree23_6_predictions:=predict(tree_fit23_6,tree23_6_values)]
colnames(tree23_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_6_values = cbind(time_index_column, tree23_6_values)
data_plot23_6 = melt(tree23_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_6 = ggplot(data_plot23_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_6






value23_7 = as.numeric(selected_dt3[1,-1])
tree_fit23_7=rpart(value23_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_7)


tree23_7_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_7_values[,tree23_7_predictions:=predict(tree_fit23_7,tree23_7_values)]
colnames(tree23_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_7_values = cbind(time_index_column, tree23_7_values)
data_plot23_7 = melt(tree23_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_7 = ggplot(data_plot23_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_7




value23_8 = as.numeric(selected_dt3[1,-1])
tree_fit23_8=rpart(value23_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit23_8)


tree23_8_values = as.data.table(t(data.table(selected_dt3[1,-1])))
tree23_8_values[,tree23_8_predictions:=predict(tree_fit23_8,tree23_8_values)]
colnames(tree23_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree23_8_values = cbind(time_index_column, tree23_8_values)
data_plot23_8 = melt(tree23_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted23_8 = ggplot(data_plot23_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted23_8




minimize(tree_fit23_1$cptable[length(tree_fit23_1$cptable[, "xerror"]), "xerror"],
         tree_fit23_2$cptable[length(tree_fit23_2$cptable[, "xerror"]), "xerror"],
         tree_fit23_3$cptable[length(tree_fit23_3$cptable[, "xerror"]), "xerror"],
         tree_fit23_4$cptable[length(tree_fit23_4$cptable[, "xerror"]), "xerror"],
         tree_fit23_5$cptable[length(tree_fit23_5$cptable[, "xerror"]), "xerror"],
         tree_fit23_6$cptable[length(tree_fit23_6$cptable[, "xerror"]), "xerror"],
         tree_fit23_7$cptable[length(tree_fit23_7$cptable[, "xerror"]), "xerror"],
         tree_fit23_8$cptable[length(tree_fit23_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted23_7
#maxdepth = 7 gives the best cv error according to cp values among 1,2,3,4,5,6,7, 8; 
#therefore maxdepth should be equal to 7


####################################




######################


value24_1 = as.numeric(selected_dt3[2,-1])
tree_fit24_1=rpart(value24_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_1)


tree24_1_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_1_values[,tree24_1_predictions:=predict(tree_fit24_1,tree24_1_values)]
colnames(tree24_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_1_values = cbind(time_index_column, tree24_1_values)
data_plot24_1 = melt(tree24_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_1 = ggplot(data_plot24_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_1







value24_2 = as.numeric(selected_dt3[2,-1])
tree_fit24_2=rpart(value24_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_2)


tree24_2_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_2_values[,tree24_2_predictions:=predict(tree_fit24_2,tree24_2_values)]
colnames(tree24_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_2_values = cbind(time_index_column, tree24_2_values)
data_plot24_2 = melt(tree24_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_2 = ggplot(data_plot24_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_2







value24_3 = as.numeric(selected_dt3[2,-1])
tree_fit24_3=rpart(value24_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_3)


tree24_3_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_3_values[,tree24_3_predictions:=predict(tree_fit24_3,tree24_3_values)]
colnames(tree24_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_3_values = cbind(time_index_column, tree24_3_values)
data_plot24_3 = melt(tree24_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_3 = ggplot(data_plot24_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_3







value24_4 = as.numeric(selected_dt3[2,-1])
tree_fit24_4=rpart(value24_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_4)


tree24_4_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_4_values[,tree24_4_predictions:=predict(tree_fit24_4,tree24_4_values)]
colnames(tree24_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_4_values = cbind(time_index_column, tree24_4_values)
data_plot24_4 = melt(tree24_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_4 = ggplot(data_plot24_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_4








value24_5 = as.numeric(selected_dt3[2,-1])
tree_fit24_5=rpart(value24_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_5)


tree24_5_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_5_values[,tree24_5_predictions:=predict(tree_fit24_5,tree24_5_values)]
colnames(tree24_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_5_values = cbind(time_index_column, tree24_5_values)
data_plot24_5 = melt(tree24_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_5 = ggplot(data_plot24_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_5








value24_6 = as.numeric(selected_dt3[2,-1])
tree_fit24_6=rpart(value24_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_6)


tree24_6_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_6_values[,tree24_6_predictions:=predict(tree_fit24_6,tree24_6_values)]
colnames(tree24_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_6_values = cbind(time_index_column, tree24_6_values)
data_plot24_6 = melt(tree24_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_6 = ggplot(data_plot24_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_6






value24_7 = as.numeric(selected_dt3[2,-1])
tree_fit24_7=rpart(value24_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_7)


tree24_7_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_7_values[,tree24_7_predictions:=predict(tree_fit24_7,tree24_7_values)]
colnames(tree24_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_7_values = cbind(time_index_column, tree24_7_values)
data_plot24_7 = melt(tree24_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_7 = ggplot(data_plot24_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_7




value24_8 = as.numeric(selected_dt3[2,-1])
tree_fit24_8=rpart(value24_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit24_8)


tree24_8_values = as.data.table(t(data.table(selected_dt3[2,-1])))
tree24_8_values[,tree24_8_predictions:=predict(tree_fit24_8,tree24_8_values)]
colnames(tree24_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree24_8_values = cbind(time_index_column, tree24_8_values)
data_plot24_8 = melt(tree24_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted24_8 = ggplot(data_plot24_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted24_8




minimize(tree_fit24_1$cptable[length(tree_fit24_1$cptable[, "xerror"]), "xerror"],
         tree_fit24_2$cptable[length(tree_fit24_2$cptable[, "xerror"]), "xerror"],
         tree_fit24_3$cptable[length(tree_fit24_3$cptable[, "xerror"]), "xerror"],
         tree_fit24_4$cptable[length(tree_fit24_4$cptable[, "xerror"]), "xerror"],
         tree_fit24_5$cptable[length(tree_fit24_5$cptable[, "xerror"]), "xerror"],
         tree_fit24_6$cptable[length(tree_fit24_6$cptable[, "xerror"]), "xerror"],
         tree_fit24_7$cptable[length(tree_fit24_7$cptable[, "xerror"]), "xerror"],
         tree_fit24_8$cptable[length(tree_fit24_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted24_7
#maxdepth = 7 gives the best cv error according to cp values among 1,2,3,4,5,6,7, 8; 
#therefore maxdepth should be equal to 7


####################################






value25_1 = as.numeric(selected_dt3[3,-1])
tree_fit25_1=rpart(value25_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_1)


tree25_1_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_1_values[,tree25_1_predictions:=predict(tree_fit25_1,tree25_1_values)]
colnames(tree25_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_1_values = cbind(time_index_column, tree25_1_values)
data_plot25_1 = melt(tree25_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_1 = ggplot(data_plot25_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_1







value25_2 = as.numeric(selected_dt3[3,-1])
tree_fit25_2=rpart(value25_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_2)


tree25_2_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_2_values[,tree25_2_predictions:=predict(tree_fit25_2,tree25_2_values)]
colnames(tree25_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_2_values = cbind(time_index_column, tree25_2_values)
data_plot25_2 = melt(tree25_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_2 = ggplot(data_plot25_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_2







value25_3 = as.numeric(selected_dt3[3,-1])
tree_fit25_3=rpart(value25_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_3)


tree25_3_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_3_values[,tree25_3_predictions:=predict(tree_fit25_3,tree25_3_values)]
colnames(tree25_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_3_values = cbind(time_index_column, tree25_3_values)
data_plot25_3 = melt(tree25_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_3 = ggplot(data_plot25_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_3







value25_4 = as.numeric(selected_dt3[3,-1])
tree_fit25_4=rpart(value25_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_4)


tree25_4_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_4_values[,tree25_4_predictions:=predict(tree_fit25_4,tree25_4_values)]
colnames(tree25_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_4_values = cbind(time_index_column, tree25_4_values)
data_plot25_4 = melt(tree25_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_4 = ggplot(data_plot25_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_4








value25_5 = as.numeric(selected_dt3[3,-1])
tree_fit25_5=rpart(value25_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_5)


tree25_5_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_5_values[,tree25_5_predictions:=predict(tree_fit25_5,tree25_5_values)]
colnames(tree25_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_5_values = cbind(time_index_column, tree25_5_values)
data_plot25_5 = melt(tree25_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_5 = ggplot(data_plot25_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_5








value25_6 = as.numeric(selected_dt3[3,-1])
tree_fit25_6=rpart(value25_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_6)


tree25_6_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_6_values[,tree25_6_predictions:=predict(tree_fit25_6,tree25_6_values)]
colnames(tree25_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_6_values = cbind(time_index_column, tree25_6_values)
data_plot25_6 = melt(tree25_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_6 = ggplot(data_plot25_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_6






value25_7 = as.numeric(selected_dt3[3,-1])
tree_fit25_7=rpart(value25_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_7)


tree25_7_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_7_values[,tree25_7_predictions:=predict(tree_fit25_7,tree25_7_values)]
colnames(tree25_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_7_values = cbind(time_index_column, tree25_7_values)
data_plot25_7 = melt(tree25_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_7 = ggplot(data_plot25_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_7



value25_8 = as.numeric(selected_dt3[3,-1])
tree_fit25_8=rpart(value25_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit25_8)


tree25_8_values = as.data.table(t(data.table(selected_dt3[3,-1])))
tree25_8_values[,tree25_8_predictions:=predict(tree_fit25_8,tree25_8_values)]
colnames(tree25_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree25_8_values = cbind(time_index_column, tree25_8_values)
data_plot25_8 = melt(tree25_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted25_8 = ggplot(data_plot25_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted25_8

minimize(tree_fit25_1$cptable[length(tree_fit25_1$cptable[, "xerror"]), "xerror"],
         tree_fit25_2$cptable[length(tree_fit25_2$cptable[, "xerror"]), "xerror"],
         tree_fit25_3$cptable[length(tree_fit25_3$cptable[, "xerror"]), "xerror"],
         tree_fit25_4$cptable[length(tree_fit25_4$cptable[, "xerror"]), "xerror"],
         tree_fit25_5$cptable[length(tree_fit25_5$cptable[, "xerror"]), "xerror"],
         tree_fit25_6$cptable[length(tree_fit25_6$cptable[, "xerror"]), "xerror"],
         tree_fit25_7$cptable[length(tree_fit25_7$cptable[, "xerror"]), "xerror"],
         tree_fit25_8$cptable[length(tree_fit25_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted25_7
#maxdepth = 7 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 7



#####################################


value26_1 = as.numeric(selected_dt3[4,-1])
tree_fit26_1=rpart(value26_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_1)


tree26_1_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_1_values[,tree26_1_predictions:=predict(tree_fit26_1,tree26_1_values)]
colnames(tree26_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_1_values = cbind(time_index_column, tree26_1_values)
data_plot26_1 = melt(tree26_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_1 = ggplot(data_plot26_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_1







value26_2 = as.numeric(selected_dt3[4,-1])
tree_fit26_2=rpart(value26_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_2)


tree26_2_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_2_values[,tree26_2_predictions:=predict(tree_fit26_2,tree26_2_values)]
colnames(tree26_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_2_values = cbind(time_index_column, tree26_2_values)
data_plot26_2 = melt(tree26_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_2 = ggplot(data_plot26_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_2







value26_3 = as.numeric(selected_dt3[4,-1])
tree_fit26_3=rpart(value26_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_3)


tree26_3_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_3_values[,tree26_3_predictions:=predict(tree_fit26_3,tree26_3_values)]
colnames(tree26_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_3_values = cbind(time_index_column, tree26_3_values)
data_plot26_3 = melt(tree26_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_3 = ggplot(data_plot26_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_3







value26_4 = as.numeric(selected_dt3[4,-1])
tree_fit26_4=rpart(value26_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_4)


tree26_4_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_4_values[,tree26_4_predictions:=predict(tree_fit26_4,tree26_4_values)]
colnames(tree26_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_4_values = cbind(time_index_column, tree26_4_values)
data_plot26_4 = melt(tree26_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_4 = ggplot(data_plot26_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_4








value26_5 = as.numeric(selected_dt3[4,-1])
tree_fit26_5=rpart(value26_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_5)


tree26_5_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_5_values[,tree26_5_predictions:=predict(tree_fit26_5,tree26_5_values)]
colnames(tree26_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_5_values = cbind(time_index_column, tree26_5_values)
data_plot26_5 = melt(tree26_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_5 = ggplot(data_plot26_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_5








value26_6 = as.numeric(selected_dt3[4,-1])
tree_fit26_6=rpart(value26_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_6)


tree26_6_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_6_values[,tree26_6_predictions:=predict(tree_fit26_6,tree26_6_values)]
colnames(tree26_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_6_values = cbind(time_index_column, tree26_6_values)
data_plot26_6 = melt(tree26_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_6 = ggplot(data_plot26_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_6






value26_7 = as.numeric(selected_dt3[4,-1])
tree_fit26_7=rpart(value26_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_7)


tree26_7_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_7_values[,tree26_7_predictions:=predict(tree_fit26_7,tree26_7_values)]
colnames(tree26_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_7_values = cbind(time_index_column, tree26_7_values)
data_plot26_7 = melt(tree26_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_7 = ggplot(data_plot26_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_7




value26_8 = as.numeric(selected_dt3[4,-1])
tree_fit26_8=rpart(value26_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit26_8)


tree26_8_values = as.data.table(t(data.table(selected_dt3[4,-1])))
tree26_8_values[,tree26_8_predictions:=predict(tree_fit26_8,tree26_8_values)]
colnames(tree26_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree26_8_values = cbind(time_index_column, tree26_8_values)
data_plot26_8 = melt(tree26_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted26_8 = ggplot(data_plot26_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted26_8


minimize(tree_fit26_1$cptable[length(tree_fit26_1$cptable[, "xerror"]), "xerror"],
         tree_fit26_2$cptable[length(tree_fit26_2$cptable[, "xerror"]), "xerror"],
         tree_fit26_3$cptable[length(tree_fit26_3$cptable[, "xerror"]), "xerror"],
         tree_fit26_4$cptable[length(tree_fit26_4$cptable[, "xerror"]), "xerror"],
         tree_fit26_5$cptable[length(tree_fit26_5$cptable[, "xerror"]), "xerror"],
         tree_fit26_6$cptable[length(tree_fit26_6$cptable[, "xerror"]), "xerror"],
         tree_fit26_7$cptable[length(tree_fit26_7$cptable[, "xerror"]), "xerror"],
         tree_fit26_8$cptable[length(tree_fit26_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted26_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5


##############################

value27_1 = as.numeric(selected_dt3[5,-1])
tree_fit27_1=rpart(value27_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_1)


tree27_1_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_1_values[,tree27_1_predictions:=predict(tree_fit27_1,tree27_1_values)]
colnames(tree27_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_1_values = cbind(time_index_column, tree27_1_values)
data_plot27_1 = melt(tree27_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_1 = ggplot(data_plot27_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_1







value27_2 = as.numeric(selected_dt3[5,-1])
tree_fit27_2=rpart(value27_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_2)


tree27_2_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_2_values[,tree27_2_predictions:=predict(tree_fit27_2,tree27_2_values)]
colnames(tree27_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_2_values = cbind(time_index_column, tree27_2_values)
data_plot27_2 = melt(tree27_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_2 = ggplot(data_plot27_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_2







value27_3 = as.numeric(selected_dt3[5,-1])
tree_fit27_3=rpart(value27_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_3)


tree27_3_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_3_values[,tree27_3_predictions:=predict(tree_fit27_3,tree27_3_values)]
colnames(tree27_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_3_values = cbind(time_index_column, tree27_3_values)
data_plot27_3 = melt(tree27_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_3 = ggplot(data_plot27_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_3







value27_4 = as.numeric(selected_dt3[5,-1])
tree_fit27_4=rpart(value27_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_4)


tree27_4_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_4_values[,tree27_4_predictions:=predict(tree_fit27_4,tree27_4_values)]
colnames(tree27_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_4_values = cbind(time_index_column, tree27_4_values)
data_plot27_4 = melt(tree27_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_4 = ggplot(data_plot27_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_4








value27_5 = as.numeric(selected_dt3[5,-1])
tree_fit27_5=rpart(value27_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_5)


tree27_5_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_5_values[,tree27_5_predictions:=predict(tree_fit27_5,tree27_5_values)]
colnames(tree27_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_5_values = cbind(time_index_column, tree27_5_values)
data_plot27_5 = melt(tree27_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_5 = ggplot(data_plot27_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_5






value27_6 = as.numeric(selected_dt3[5,-1])
tree_fit27_6=rpart(value27_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_6)


tree27_6_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_6_values[,tree27_6_predictions:=predict(tree_fit27_6,tree27_6_values)]
colnames(tree27_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_6_values = cbind(time_index_column, tree27_6_values)
data_plot27_6 = melt(tree27_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_6 = ggplot(data_plot27_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_6




value27_7 = as.numeric(selected_dt3[5,-1])
tree_fit27_7=rpart(value27_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_7)


tree27_7_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_7_values[,tree27_7_predictions:=predict(tree_fit27_7,tree27_7_values)]
colnames(tree27_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_7_values = cbind(time_index_column, tree27_7_values)
data_plot27_7 = melt(tree27_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_7 = ggplot(data_plot27_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_7




value27_8 = as.numeric(selected_dt3[5,-1])
tree_fit27_8=rpart(value27_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit27_8)


tree27_8_values = as.data.table(t(data.table(selected_dt3[5,-1])))
tree27_8_values[,tree27_8_predictions:=predict(tree_fit27_8,tree27_8_values)]
colnames(tree27_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree27_8_values = cbind(time_index_column, tree27_8_values)
data_plot27_8 = melt(tree27_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted27_8 = ggplot(data_plot27_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted27_8

minimize(tree_fit27_1$cptable[length(tree_fit27_1$cptable[, "xerror"]), "xerror"],
         tree_fit27_2$cptable[length(tree_fit27_2$cptable[, "xerror"]), "xerror"],
         tree_fit27_3$cptable[length(tree_fit27_3$cptable[, "xerror"]), "xerror"],
         tree_fit27_4$cptable[length(tree_fit27_4$cptable[, "xerror"]), "xerror"],
         tree_fit27_5$cptable[length(tree_fit27_5$cptable[, "xerror"]), "xerror"],
         tree_fit27_6$cptable[length(tree_fit27_6$cptable[, "xerror"]), "xerror"],
         tree_fit27_7$cptable[length(tree_fit27_7$cptable[, "xerror"]), "xerror"],
         tree_fit27_8$cptable[length(tree_fit27_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted27_6
#maxdepth = 6 gives the best cv error according to cp values among 1,2,3,4,5,6; 
#therefore maxdepth should be equal to 6






value28_1 = as.numeric(selected_dt3[6,-1])
tree_fit28_1=rpart(value28_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_1)


tree28_1_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_1_values[,tree28_1_predictions:=predict(tree_fit28_1,tree28_1_values)]
colnames(tree28_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_1_values = cbind(time_index_column, tree28_1_values)
data_plot28_1 = melt(tree28_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_1 = ggplot(data_plot28_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_1







value28_2 = as.numeric(selected_dt3[6,-1])
tree_fit28_2=rpart(value28_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_2)


tree28_2_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_2_values[,tree28_2_predictions:=predict(tree_fit28_2,tree28_2_values)]
colnames(tree28_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_2_values = cbind(time_index_column, tree28_2_values)
data_plot28_2 = melt(tree28_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_2 = ggplot(data_plot28_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_2







value28_3 = as.numeric(selected_dt3[6,-1])
tree_fit28_3=rpart(value28_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_3)


tree28_3_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_3_values[,tree28_3_predictions:=predict(tree_fit28_3,tree28_3_values)]
colnames(tree28_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_3_values = cbind(time_index_column, tree28_3_values)
data_plot28_3 = melt(tree28_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_3 = ggplot(data_plot28_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_3







value28_4 = as.numeric(selected_dt3[6,-1])
tree_fit28_4=rpart(value28_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_4)


tree28_4_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_4_values[,tree28_4_predictions:=predict(tree_fit28_4,tree28_4_values)]
colnames(tree28_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_4_values = cbind(time_index_column, tree28_4_values)
data_plot28_4 = melt(tree28_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_4 = ggplot(data_plot28_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_4








value28_5 = as.numeric(selected_dt3[6,-1])
tree_fit28_5=rpart(value28_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_5)


tree28_5_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_5_values[,tree28_5_predictions:=predict(tree_fit28_5,tree28_5_values)]
colnames(tree28_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_5_values = cbind(time_index_column, tree28_5_values)
data_plot28_5 = melt(tree28_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_5 = ggplot(data_plot28_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_5






value28_6 = as.numeric(selected_dt3[6,-1])
tree_fit28_6=rpart(value28_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_6)


tree28_6_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_6_values[,tree28_6_predictions:=predict(tree_fit28_6,tree28_6_values)]
colnames(tree28_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_6_values = cbind(time_index_column, tree28_6_values)
data_plot28_6 = melt(tree28_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_6 = ggplot(data_plot28_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_6





value28_7 = as.numeric(selected_dt3[6,-1])
tree_fit28_7=rpart(value28_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_7)


tree28_7_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_7_values[,tree28_7_predictions:=predict(tree_fit28_7,tree28_7_values)]
colnames(tree28_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_7_values = cbind(time_index_column, tree28_7_values)
data_plot28_7 = melt(tree28_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_7 = ggplot(data_plot28_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_7





value28_8 = as.numeric(selected_dt3[6,-1])
tree_fit28_8=rpart(value28_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit28_8)


tree28_8_values = as.data.table(t(data.table(selected_dt3[6,-1])))
tree28_8_values[,tree28_8_predictions:=predict(tree_fit28_8,tree28_8_values)]
colnames(tree28_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree28_8_values = cbind(time_index_column, tree28_8_values)
data_plot28_8 = melt(tree28_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted28_8 = ggplot(data_plot28_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted28_8




minimize(tree_fit28_1$cptable[length(tree_fit28_1$cptable[, "xerror"]), "xerror"],
         tree_fit28_2$cptable[length(tree_fit28_2$cptable[, "xerror"]), "xerror"],
         tree_fit28_3$cptable[length(tree_fit28_3$cptable[, "xerror"]), "xerror"],
         tree_fit28_4$cptable[length(tree_fit28_4$cptable[, "xerror"]), "xerror"],
         tree_fit28_5$cptable[length(tree_fit28_5$cptable[, "xerror"]), "xerror"],
         tree_fit28_6$cptable[length(tree_fit28_6$cptable[, "xerror"]), "xerror"],
         tree_fit28_7$cptable[length(tree_fit28_7$cptable[, "xerror"]), "xerror"],
         tree_fit28_8$cptable[length(tree_fit28_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted28_5
#maxdepth = 5 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 5









value29_1 = as.numeric(selected_dt3[7,-1])
tree_fit29_1=rpart(value29_1~time_index,selected_dt2,
                   control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_1)


tree29_1_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_1_values[,tree29_1_predictions:=predict(tree_fit29_1,tree29_1_values)]
colnames(tree29_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_1_values = cbind(time_index_column, tree29_1_values)
data_plot29_1 = melt(tree29_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_1 = ggplot(data_plot29_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_1







value29_2 = as.numeric(selected_dt3[7,-1])
tree_fit29_2=rpart(value29_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_2)


tree29_2_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_2_values[,tree29_2_predictions:=predict(tree_fit29_2,tree29_2_values)]
colnames(tree29_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_2_values = cbind(time_index_column, tree29_2_values)
data_plot29_2 = melt(tree29_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_2 = ggplot(data_plot29_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_2







value29_3 = as.numeric(selected_dt3[7,-1])
tree_fit29_3=rpart(value29_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_3)


tree29_3_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_3_values[,tree29_3_predictions:=predict(tree_fit29_3,tree29_3_values)]
colnames(tree29_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_3_values = cbind(time_index_column, tree29_3_values)
data_plot29_3 = melt(tree29_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_3 = ggplot(data_plot29_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_3







value29_4 = as.numeric(selected_dt3[7,-1])
tree_fit29_4=rpart(value29_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_4)


tree29_4_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_4_values[,tree29_4_predictions:=predict(tree_fit29_4,tree29_4_values)]
colnames(tree29_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_4_values = cbind(time_index_column, tree29_4_values)
data_plot29_4 = melt(tree29_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_4 = ggplot(data_plot29_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_4








value29_5 = as.numeric(selected_dt3[7,-1])
tree_fit29_5=rpart(value29_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_5)


tree29_5_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_5_values[,tree29_5_predictions:=predict(tree_fit29_5,tree29_5_values)]
colnames(tree29_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_5_values = cbind(time_index_column, tree29_5_values)
data_plot29_5 = melt(tree29_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_5 = ggplot(data_plot29_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_5





value29_6 = as.numeric(selected_dt3[7,-1])
tree_fit29_6=rpart(value29_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_6)


tree29_6_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_6_values[,tree29_6_predictions:=predict(tree_fit29_6,tree29_6_values)]
colnames(tree29_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_6_values = cbind(time_index_column, tree29_6_values)
data_plot29_6 = melt(tree29_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_6 = ggplot(data_plot29_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_6





value29_7 = as.numeric(selected_dt3[7,-1])
tree_fit29_7=rpart(value29_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_7)


tree29_7_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_7_values[,tree29_7_predictions:=predict(tree_fit29_7,tree29_7_values)]
colnames(tree29_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_7_values = cbind(time_index_column, tree29_7_values)
data_plot29_7 = melt(tree29_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_7 = ggplot(data_plot29_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_7






value29_8 = as.numeric(selected_dt3[7,-1])
tree_fit29_8=rpart(value29_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit29_8)


tree29_8_values = as.data.table(t(data.table(selected_dt3[7,-1])))
tree29_8_values[,tree29_8_predictions:=predict(tree_fit29_8,tree29_8_values)]
colnames(tree29_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree29_8_values = cbind(time_index_column, tree29_8_values)
data_plot29_8 = melt(tree29_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted29_8 = ggplot(data_plot29_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted29_8



minimize(tree_fit29_1$cptable[length(tree_fit29_1$cptable[, "xerror"]), "xerror"],
         tree_fit29_2$cptable[length(tree_fit29_2$cptable[, "xerror"]), "xerror"],
         tree_fit29_3$cptable[length(tree_fit29_3$cptable[, "xerror"]), "xerror"],
         tree_fit29_4$cptable[length(tree_fit29_4$cptable[, "xerror"]), "xerror"],
         tree_fit29_5$cptable[length(tree_fit29_5$cptable[, "xerror"]), "xerror"],
         tree_fit29_6$cptable[length(tree_fit29_6$cptable[, "xerror"]), "xerror"],
         tree_fit29_7$cptable[length(tree_fit29_7$cptable[, "xerror"]), "xerror"],
         tree_fit29_8$cptable[length(tree_fit29_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted29_6
#maxdepth = 6 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 6



value30_1 = as.numeric(selected_dt3[8,-1])
tree_fit30_1=rpart(value30_1~time_index,selected_dt2,
                 control=rpart.control(maxdepth=1, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_1)


tree30_1_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_1_values[,tree30_1_predictions:=predict(tree_fit30_1,tree30_1_values)]
colnames(tree30_1_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_1_values = cbind(time_index_column, tree30_1_values)
data_plot30_1 = melt(tree30_1_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_1 = ggplot(data_plot30_1,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_1







value30_2 = as.numeric(selected_dt3[8,-1])
tree_fit30_2=rpart(value30_2~time_index,selected_dt2,
                   control=rpart.control(maxdepth=2, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_2)


tree30_2_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_2_values[,tree30_2_predictions:=predict(tree_fit30_2,tree30_2_values)]
colnames(tree30_2_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_2_values = cbind(time_index_column, tree30_2_values)
data_plot30_2 = melt(tree30_2_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_2 = ggplot(data_plot30_2,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_2







value30_3 = as.numeric(selected_dt3[8,-1])
tree_fit30_3=rpart(value30_3~time_index,selected_dt2,
                   control=rpart.control(maxdepth=3, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_3)


tree30_3_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_3_values[,tree30_3_predictions:=predict(tree_fit30_3,tree30_3_values)]
colnames(tree30_3_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_3_values = cbind(time_index_column, tree30_3_values)
data_plot30_3 = melt(tree30_3_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_3 = ggplot(data_plot30_3,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_3







value30_4 = as.numeric(selected_dt3[8,-1])
tree_fit30_4=rpart(value30_4~time_index,selected_dt2,
                   control=rpart.control(maxdepth=4, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_4)


tree30_4_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_4_values[,tree30_4_predictions:=predict(tree_fit30_4,tree30_4_values)]
colnames(tree30_4_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_4_values = cbind(time_index_column, tree30_4_values)
data_plot30_4 = melt(tree30_4_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_4 = ggplot(data_plot30_4,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_4








value30_5 = as.numeric(selected_dt3[8,-1])
tree_fit30_5=rpart(value30_5~time_index,selected_dt2,
                   control=rpart.control(maxdepth=5, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_5)


tree30_5_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_5_values[,tree30_5_predictions:=predict(tree_fit30_5,tree30_5_values)]
colnames(tree30_5_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_5_values = cbind(time_index_column, tree30_5_values)
data_plot30_5 = melt(tree30_5_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_5 = ggplot(data_plot30_5,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_5








value30_6 = as.numeric(selected_dt3[8,-1])
tree_fit30_6=rpart(value30_6~time_index,selected_dt2,
                   control=rpart.control(maxdepth=6, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_6)


tree30_6_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_6_values[,tree30_6_predictions:=predict(tree_fit30_6,tree30_6_values)]
colnames(tree30_6_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_6_values = cbind(time_index_column, tree30_6_values)
data_plot30_6 = melt(tree30_6_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_6 = ggplot(data_plot30_6,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_6







value30_7 = as.numeric(selected_dt3[8,-1])
tree_fit30_7=rpart(value30_7~time_index,selected_dt2,
                   control=rpart.control(maxdepth=7, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_7)


tree30_7_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_7_values[,tree30_7_predictions:=predict(tree_fit30_7,tree30_7_values)]
colnames(tree30_7_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_7_values = cbind(time_index_column, tree30_7_values)
data_plot30_7 = melt(tree30_7_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_7 = ggplot(data_plot30_7,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_7







value30_8 = as.numeric(selected_dt3[8,-1])
tree_fit30_8=rpart(value30_8~time_index,selected_dt2,
                   control=rpart.control(maxdepth=8, cp = 0, minsplit=20, minbucket = 10))
#fancyRpartPlot(tree_fit30_8)


tree30_8_values = as.data.table(t(data.table(selected_dt3[8,-1])))
tree30_8_values[,tree30_8_predictions:=predict(tree_fit30_8,tree30_8_values)]
colnames(tree30_8_values) <- c("exact value", "predicted value")

colnames(time_index_column) <- c("time")
tree30_8_values = cbind(time_index_column, tree30_8_values)
data_plot30_8 = melt(tree30_8_values,id.vars="time",measure.vars=c("exact value", "predicted value"))
exact_vs_predicted30_8 = ggplot(data_plot30_8,aes(x=time,y=value,color=variable))+
  geom_line()
#exact_vs_predicted30_8



minimize(tree_fit30_1$cptable[length(tree_fit30_1$cptable[, "xerror"]), "xerror"],
         tree_fit30_2$cptable[length(tree_fit30_2$cptable[, "xerror"]), "xerror"],
         tree_fit30_3$cptable[length(tree_fit30_3$cptable[, "xerror"]), "xerror"],
         tree_fit30_4$cptable[length(tree_fit30_4$cptable[, "xerror"]), "xerror"],
         tree_fit30_5$cptable[length(tree_fit30_5$cptable[, "xerror"]), "xerror"],
         tree_fit30_6$cptable[length(tree_fit30_6$cptable[, "xerror"]), "xerror"],
         tree_fit30_7$cptable[length(tree_fit30_7$cptable[, "xerror"]), "xerror"],
         tree_fit30_8$cptable[length(tree_fit30_8$cptable[, "xerror"]), "xerror"])
exact_vs_predicted30_8
#maxdepth = 8 gives the best cv error according to cp values among 1,2,3,4,5,6,7,8; 
#therefore maxdepth should be equal to 8


#QUESTION - 3
mse_fused_data1 = cbind(as.numeric(unlist(tree1_4_values[,2])), 
                        fused_lasso_1_predicted, as.numeric(unlist(tree1_4_values[,3])))
mse_fused_value1 = mse(mse_fused_data1[,1], mse_fused_data1[,2])
mse_tree_value1 = mse(mse_fused_data1[,1], mse_fused_data1[,3])

mse_fused_data2 = cbind(as.numeric(unlist(tree2_5_values[,2])), 
                        fused_lasso_2_predicted, as.numeric(unlist(tree2_5_values[,3])))
mse_fused_value2 = mse(mse_fused_data2[,1], mse_fused_data2[,2])
mse_tree_value2 = mse(mse_fused_data2[,1], mse_fused_data2[,3])

mse_fused_data3 = cbind(as.numeric(unlist(tree3_2_values[,2])), 
                        fused_lasso_3_predicted, as.numeric(unlist(tree3_2_values[,3])))
mse_fused_value3 = mse(mse_fused_data3[,1], mse_fused_data3[,2])
mse_tree_value3 = mse(mse_fused_data3[,1], mse_fused_data3[,3])

mse_fused_data4 = cbind(as.numeric(unlist(tree4_2_values[,2])), 
                        fused_lasso_4_predicted, as.numeric(unlist(tree4_2_values[,3])))
mse_fused_value4 = mse(mse_fused_data4[,1], mse_fused_data4[,2])
mse_tree_value4 = mse(mse_fused_data4[,1], mse_fused_data4[,3])

mse_fused_data5 = cbind(as.numeric(unlist(tree5_2_values[,2])), 
                        fused_lasso_5_predicted, as.numeric(unlist(tree5_2_values[,3])))
mse_fused_value5 = mse(mse_fused_data5[,1], mse_fused_data5[,2])
mse_tree_value5 = mse(mse_fused_data5[,1], mse_fused_data5[,3])

mse_fused_data6 = cbind(as.numeric(unlist(tree6_2_values[,2])), 
                        fused_lasso_6_predicted, as.numeric(unlist(tree6_2_values[,3])))
mse_fused_value6 = mse(mse_fused_data6[,1], mse_fused_data6[,2])
mse_tree_value6 = mse(mse_fused_data6[,1], mse_fused_data6[,3])

mse_fused_data7 = cbind(as.numeric(unlist(tree7_8_values[,2])), 
                        fused_lasso_7_predicted, as.numeric(unlist(tree7_8_values[,3])))
mse_fused_value7 = mse(mse_fused_data7[,1], mse_fused_data7[,2])
mse_tree_value7 = mse(mse_fused_data7[,1], mse_fused_data7[,3])

mse_fused_data8 = cbind(as.numeric(unlist(tree8_2_values[,2])), 
                        fused_lasso_8_predicted, as.numeric(unlist(tree8_2_values[,3])))
mse_fused_value8 = mse(mse_fused_data8[,1], mse_fused_data8[,2])
mse_tree_value8 = mse(mse_fused_data8[,1], mse_fused_data8[,3])

mse_fused_data9 = cbind(as.numeric(unlist(tree9_1_values[,2])), 
                        fused_lasso_9_predicted, as.numeric(unlist(tree9_1_values[,3])))
mse_fused_value9 = mse(mse_fused_data9[,1], mse_fused_data9[,2])
mse_tree_value9 = mse(mse_fused_data9[,1], mse_fused_data9[,3])

mse_fused_data10 = cbind(as.numeric(unlist(tree10_5_values[,2])), 
                        fused_lasso_10_predicted, as.numeric(unlist(tree10_5_values[,3])))
mse_fused_value10 = mse(mse_fused_data10[,1], mse_fused_data10[,2])
mse_tree_value10 = mse(mse_fused_data10[,1], mse_fused_data10[,3])

mse_fused_data11 = cbind(as.numeric(unlist(tree11_6_values[,2])), 
                        fused_lasso_11_predicted, as.numeric(unlist(tree11_6_values[,3])))
mse_fused_value11 = mse(mse_fused_data11[,1], mse_fused_data11[,2])
mse_tree_value11 = mse(mse_fused_data11[,1], mse_fused_data11[,3])

mse_fused_data12 = cbind(as.numeric(unlist(tree12_5_values[,2])), 
                        fused_lasso_12_predicted, as.numeric(unlist(tree12_5_values[,3])))
mse_fused_value12 = mse(mse_fused_data12[,1], mse_fused_data12[,2])
mse_tree_value12 = mse(mse_fused_data12[,1], mse_fused_data12[,3])

mse_fused_data13 = cbind(as.numeric(unlist(tree13_4_values[,2])), 
                        fused_lasso_13_predicted, as.numeric(unlist(tree13_4_values[,3])))
mse_fused_value13 = mse(mse_fused_data13[,1], mse_fused_data13[,2])
mse_tree_value13 = mse(mse_fused_data13[,1], mse_fused_data13[,3])

mse_fused_data14 = cbind(as.numeric(unlist(tree14_3_values[,2])), 
                        fused_lasso_14_predicted, as.numeric(unlist(tree14_3_values[,3])))
mse_fused_value14 = mse(mse_fused_data14[,1], mse_fused_data14[,2])
mse_tree_value14 = mse(mse_fused_data14[,1], mse_fused_data14[,3])

mse_fused_data15 = cbind(as.numeric(unlist(tree15_3_values[,2])), 
                        fused_lasso_15_predicted, as.numeric(unlist(tree15_3_values[,3])))
mse_fused_value15 = mse(mse_fused_data15[,1], mse_fused_data15[,2])
mse_tree_value15 = mse(mse_fused_data15[,1], mse_fused_data15[,3])

mse_fused_data16 = cbind(as.numeric(unlist(tree16_6_values[,2])), 
                        fused_lasso_16_predicted, as.numeric(unlist(tree16_6_values[,3])))
mse_fused_value16 = mse(mse_fused_data16[,1], mse_fused_data16[,2])
mse_tree_value16 = mse(mse_fused_data16[,1], mse_fused_data16[,3])

mse_fused_data17 = cbind(as.numeric(unlist(tree17_8_values[,2])), 
                        fused_lasso_17_predicted, as.numeric(unlist(tree17_8_values[,3])))
mse_fused_value17 = mse(mse_fused_data17[,1], mse_fused_data17[,2])
mse_tree_value17 = mse(mse_fused_data17[,1], mse_fused_data17[,3])

mse_fused_data18 = cbind(as.numeric(unlist(tree18_5_values[,2])), 
                        fused_lasso_18_predicted, as.numeric(unlist(tree18_5_values[,3])))
mse_fused_value18 = mse(mse_fused_data18[,1], mse_fused_data18[,2])
mse_tree_value18 = mse(mse_fused_data18[,1], mse_fused_data18[,3])

mse_fused_data19 = cbind(as.numeric(unlist(tree19_8_values[,2])), 
                        fused_lasso_19_predicted, as.numeric(unlist(tree19_8_values[,3])))
mse_fused_value19 = mse(mse_fused_data19[,1], mse_fused_data19[,2])
mse_tree_value19 = mse(mse_fused_data19[,1], mse_fused_data19[,3])

mse_fused_data20 = cbind(as.numeric(unlist(tree20_3_values[,2])), 
                        fused_lasso_20_predicted, as.numeric(unlist(tree20_3_values[,3])))
mse_fused_value20 = mse(mse_fused_data20[,1], mse_fused_data20[,2])
mse_tree_value20 = mse(mse_fused_data20[,1], mse_fused_data20[,3])

mse_fused_data21 = cbind(as.numeric(unlist(tree21_5_values[,2])), 
                        fused_lasso_21_predicted, as.numeric(unlist(tree21_5_values[,3])))
mse_fused_value21 = mse(mse_fused_data21[,1], mse_fused_data21[,2])
mse_tree_value21 = mse(mse_fused_data21[,1], mse_fused_data21[,3])

mse_fused_data22 = cbind(as.numeric(unlist(tree22_7_values[,2])), 
                        fused_lasso_22_predicted, as.numeric(unlist(tree22_7_values[,3])))
mse_fused_value22 = mse(mse_fused_data22[,1], mse_fused_data22[,2])
mse_tree_value22 = mse(mse_fused_data22[,1], mse_fused_data22[,3])

mse_fused_data23 = cbind(as.numeric(unlist(tree23_7_values[,2])), 
                        fused_lasso_23_predicted, as.numeric(unlist(tree23_7_values[,3])))
mse_fused_value23 = mse(mse_fused_data23[,1], mse_fused_data23[,2])
mse_tree_value23 = mse(mse_fused_data23[,1], mse_fused_data23[,3])

mse_fused_data24 = cbind(as.numeric(unlist(tree24_7_values[,2])), 
                        fused_lasso_24_predicted, as.numeric(unlist(tree24_7_values[,3])))
mse_fused_value24 = mse(mse_fused_data24[,1], mse_fused_data24[,2])
mse_tree_value24 = mse(mse_fused_data24[,1], mse_fused_data24[,3])

mse_fused_data25 = cbind(as.numeric(unlist(tree25_7_values[,2])), 
                        fused_lasso_25_predicted, as.numeric(unlist(tree25_7_values[,3])))
mse_fused_value25 = mse(mse_fused_data25[,1], mse_fused_data25[,2])
mse_tree_value25 = mse(mse_fused_data25[,1], mse_fused_data25[,3])

mse_fused_data26 = cbind(as.numeric(unlist(tree26_5_values[,2])), 
                        fused_lasso_26_predicted, as.numeric(unlist(tree26_5_values[,3])))
mse_fused_value26 = mse(mse_fused_data26[,1], mse_fused_data26[,2])
mse_tree_value26 = mse(mse_fused_data26[,1], mse_fused_data26[,3])

mse_fused_data27 = cbind(as.numeric(unlist(tree27_6_values[,2])), 
                        fused_lasso_27_predicted, as.numeric(unlist(tree27_6_values[,3])))
mse_fused_value27 = mse(mse_fused_data27[,1], mse_fused_data27[,2])
mse_tree_value27 = mse(mse_fused_data27[,1], mse_fused_data27[,3])

mse_fused_data28 = cbind(as.numeric(unlist(tree28_5_values[,2])), 
                        fused_lasso_28_predicted, as.numeric(unlist(tree28_5_values[,3])))
mse_fused_value28 = mse(mse_fused_data28[,1], mse_fused_data28[,2])
mse_tree_value28 = mse(mse_fused_data28[,1], mse_fused_data28[,3])

mse_fused_data29 = cbind(as.numeric(unlist(tree29_6_values[,2])), 
                        fused_lasso_29_predicted, as.numeric(unlist(tree29_6_values[,3])))
mse_fused_value29 = mse(mse_fused_data29[,1], mse_fused_data29[,2])
mse_tree_value29 = mse(mse_fused_data29[,1], mse_fused_data29[,3])

mse_fused_data30 = cbind(as.numeric(unlist(tree30_8_values[,2])), 
                        fused_lasso_30_predicted, as.numeric(unlist(tree30_8_values[,3])))
mse_fused_value30 = mse(mse_fused_data30[,1], mse_fused_data30[,2])
mse_tree_value30 = mse(mse_fused_data30[,1], mse_fused_data30[,3])

all_mse_fused = c(mse_fused_value1, mse_fused_value2, mse_fused_value3,
                  mse_fused_value4, mse_fused_value5, mse_fused_value6,
                  mse_fused_value7, mse_fused_value8, mse_fused_value9,
                  mse_fused_value10, mse_fused_value11, mse_fused_value12,
                  mse_fused_value13, mse_fused_value14, mse_fused_value15,
                  mse_fused_value16, mse_fused_value17, mse_fused_value18,
                  mse_fused_value19, mse_fused_value20, mse_fused_value21,
                  mse_fused_value22, mse_fused_value23, mse_fused_value24,
                  mse_fused_value25, mse_fused_value26, mse_fused_value27,
                  mse_fused_value28, mse_fused_value29, mse_fused_value30)

all_mse_tree = c(mse_tree_value1, mse_tree_value2, mse_tree_value3,
                 mse_tree_value4, mse_tree_value5, mse_tree_value6,
                 mse_tree_value7, mse_tree_value8, mse_tree_value9,
                 mse_tree_value10, mse_tree_value11, mse_tree_value12,
                 mse_tree_value13, mse_tree_value14, mse_tree_value15,
                 mse_tree_value16, mse_tree_value17, mse_tree_value18,
                 mse_tree_value19, mse_tree_value20, mse_tree_value21,
                 mse_tree_value22, mse_tree_value23, mse_tree_value24,
                 mse_tree_value25, mse_tree_value26, mse_tree_value27,
                 mse_tree_value28, mse_tree_value29, mse_tree_value30)
par(mfrow=c(1,2))
boxplot(all_mse_fused, main = "1D Fused Lasso MSE Values - Boxplot", staplewex = 1)
boxplot(all_mse_tree, main = "1D Fused Lasso MSE Values - Boxplot", staplewex = 1)



#QUESTION - 4

euclidian = function(x, y)
{
  distance = 0
  for (i in 1:length(x))
  {
    distance = distance + (x[i] - y[i])^2
  }
  return(sqrt(distance))
}



mse_fused_all = rbind(t(mse_fused_data1[,2]),t(mse_fused_data2[,2]),
                           t(mse_fused_data3[,2]),t(mse_fused_data4[,2]),
                           t(mse_fused_data5[,2]),t(mse_fused_data6[,2]),
                           t(mse_fused_data7[,2]),t(mse_fused_data8[,2]),
                           t(mse_fused_data9[,2]),t(mse_fused_data10[,2]),
                           t(mse_fused_data11[,2]),t(mse_fused_data12[,2]),
                           t(mse_fused_data13[,2]),t(mse_fused_data14[,2]),
                           t(mse_fused_data15[,2]),t(mse_fused_data16[,2]),
                           t(mse_fused_data17[,2]),t(mse_fused_data18[,2]),
                           t(mse_fused_data19[,2]),t(mse_fused_data20[,2]),
                           t(mse_fused_data21[,2]),t(mse_fused_data22[,2]),
                           t(mse_fused_data23[,2]),t(mse_fused_data24[,2]),
                           t(mse_fused_data25[,2]),t(mse_fused_data26[,2]),
                           t(mse_fused_data27[,2]),t(mse_fused_data28[,2]),
                           t(mse_fused_data29[,2]),t(mse_fused_data30[,2])
)

mse_tree_all = rbind(t(mse_fused_data1[,3]),t(mse_fused_data2[,3]),
                      t(mse_fused_data3[,3]),t(mse_fused_data4[,3]),
                      t(mse_fused_data5[,3]),t(mse_fused_data6[,3]),
                      t(mse_fused_data7[,3]),t(mse_fused_data8[,3]),
                      t(mse_fused_data9[,3]),t(mse_fused_data10[,3]),
                      t(mse_fused_data11[,3]),t(mse_fused_data12[,3]),
                      t(mse_fused_data13[,3]),t(mse_fused_data14[,3]),
                      t(mse_fused_data15[,3]),t(mse_fused_data16[,3]),
                      t(mse_fused_data17[,3]),t(mse_fused_data18[,3]),
                      t(mse_fused_data19[,3]),t(mse_fused_data20[,3]),
                      t(mse_fused_data21[,3]),t(mse_fused_data22[,3]),
                      t(mse_fused_data23[,3]),t(mse_fused_data24[,3]),
                      t(mse_fused_data25[,3]),t(mse_fused_data26[,3]),
                      t(mse_fused_data27[,3]),t(mse_fused_data28[,3]),
                      t(mse_fused_data29[,3]),t(mse_fused_data30[,3])
)


setnames(train_dataset_raw, "V1", "class")
rawdata_distance_matrix = data.table(c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                     c(rep(0,30)),c(rep(0,30)))
fuse_distance_matrix = data.table(c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)))
tree_distance_matrix = data.table(c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),c(rep(0,30)),
                                  c(rep(0,30)),c(rep(0,30)))




for (i in 1:30)
{
  for (j in 1:30)
  {
    if (i == j)
      next()
    else
    {
      rawdata_distance_matrix[i,j] = euclidian(train_dataset_usable_decreased[i,], train_dataset_usable_decreased[j,])
      fuse_distance_matrix[i,j] = euclidian(mse_fused_all[i,], mse_fused_all[j,])
      tree_distance_matrix[i,j] = euclidian(mse_tree_all[i,], mse_tree_all[j,])
    }
  }

}

#(sort(rawdata_distance_matrix[1,])[[2]]) #class 1 is predicted as class 3
#(sort(rawdata_distance_matrix[2,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[3,])[[2]]) #class 1 is predicted as class 3
#(sort(rawdata_distance_matrix[4,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[5,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[6,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[7,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[8,])[[2]]) #class 1 is predicted as class 1
#(sort(rawdata_distance_matrix[9,])[[2]]) #class 1 is predicted as class 1 
#(sort(rawdata_distance_matrix[10,])[[2]]) #class 1 is predicted as class 1 ==> class 1 accuracy = 8/10
#(sort(rawdata_distance_matrix[11,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[12,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[13,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[14,])[[2]]) #class 2 is predicted as class 1
#(sort(rawdata_distance_matrix[15,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[16,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[17,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[18,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[19,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[20,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[21,])[[2]]) #class 2 is predicted as class 2
#(sort(rawdata_distance_matrix[22,])[[2]]) #class 2 is predicted as class 2 ==> class 2 accuracy = 11/12
#(sort(rawdata_distance_matrix[23,])[[2]]) #class 3 is predicted as class 3
#(sort(rawdata_distance_matrix[24,])[[2]]) #class 3 is predicted as class 3 
#(sort(rawdata_distance_matrix[25,])[[2]]) #class 3 is predicted as class 3
#(sort(rawdata_distance_matrix[26,])[[2]]) #class 3 is predicted as class 3
#(sort(rawdata_distance_matrix[27,])[[2]]) #class 3 is predicted as class 3
#(sort(rawdata_distance_matrix[28,])[[2]]) #class 3 is predicted as class 1
#(sort(rawdata_distance_matrix[29,])[[2]]) #class 3 is predicted as class 1
#(sort(rawdata_distance_matrix[30,])[[2]]) #class 3 is predicted as class 3 ==> class 3 accuracy = 6/8

rawdataexact = c(rep(1,10), rep(2,12), rep(3,8))
rawdatapredicted = c(3,1,3,1,1,1,1,1,1,1,
                     2,2,2,1,2,2,2,2,2,2,2,2,
                     3,3,3,3,3,1,1,3)
rawdata_exact_vs_predicted = cbind(rawdataexact,rawdatapredicted)
colnames(rawdata_exact_vs_predicted) = c("Exact Class Value", "Predicted Class Value - 1-NN")
rawdata_exact_vs_predicted
#(sort(fuse_distance_matrix[1,])[[2]]) #class 1 is predicted as class 3
#(sort(fuse_distance_matrix[2,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[3,])[[2]]) #class 1 is predicted as class 3
#(sort(fuse_distance_matrix[4,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[5,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[6,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[7,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[8,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[9,])[[2]]) #class 1 is predicted as class 1
#(sort(fuse_distance_matrix[10,])[[2]]) #class 1 is predicted as class 3 ==> class 1 accuracy = 7/10
#(sort(fuse_distance_matrix[11,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[12,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[13,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[14,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[15,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[16,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[17,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[18,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[19,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[20,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[21,])[[2]]) #class 2 is predicted as class 2
#(sort(fuse_distance_matrix[22,])[[2]]) #class 2 is predicted as class 2 ==> class 2 accuracy = 12/12
#(sort(fuse_distance_matrix[23,])[[2]]) #class 3 is predicted as class 3
#(sort(fuse_distance_matrix[24,])[[2]]) #class 3 is predicted as class 3
#(sort(fuse_distance_matrix[25,])[[2]]) #class 3 is predicted as class 1
#(sort(fuse_distance_matrix[26,])[[2]]) #class 3 is predicted as class 3
#(sort(fuse_distance_matrix[27,])[[2]]) #class 3 is predicted as class 3 
#(sort(fuse_distance_matrix[28,])[[2]]) #class 3 is predicted as class 3
#(sort(fuse_distance_matrix[29,])[[2]]) #class 3 is predicted as class 3
#(sort(fuse_distance_matrix[30,])[[2]]) #class 3 is predicted as class 3 ==> class 3 accuracy = 7/8

fusedataexact = c(rep(1,10), rep(2,12), rep(3,8))
fusedatapredicted = c(3,1,3,1,1,1,1,1,1,3,
                     2,2,2,2,2,2,2,2,2,2,2,2,
                     3,3,1,3,3,3,3,3)
fusedata_exact_vs_predicted = cbind(fusedataexact,fusedatapredicted)
colnames(fusedata_exact_vs_predicted) = c("Exact Class Value", "Predicted Class Value - 1D Fused Lasso")
fusedata_exact_vs_predicted
#(sort(tree_distance_matrix[1,])[[2]]) #class 1 is predicted as class 3
#(sort(tree_distance_matrix[2,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[3,])[[2]]) #class 1 is predicted as class 3
#(sort(tree_distance_matrix[4,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[5,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[6,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[7,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[8,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[9,])[[2]]) #class 1 is predicted as class 1
#(sort(tree_distance_matrix[10,])[[2]]) #class 1 is predicted as class 1 ==> class 1 accuracy = 8/10
#(sort(tree_distance_matrix[11,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[12,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[13,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[14,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[15,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[16,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[17,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[18,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[19,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[20,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[21,])[[2]]) #class 2 is predicted as class 2
#(sort(tree_distance_matrix[22,])[[2]]) #class 2 is predicted as class 2 ==> class 2 accuracy = 12/12
#(sort(tree_distance_matrix[23,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[24,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[25,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[26,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[27,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[28,])[[2]]) #class 3 is predicted as class 3
#(sort(tree_distance_matrix[29,])[[2]]) #class 3 is predicted as class 1
#(sort(tree_distance_matrix[30,])[[2]]) #class 3 is predicted as class 3 ==> class 3 accuracy = 7/8

treedataexact = c(rep(1,10), rep(2,12), rep(3,8))
treedatapredicted = c(3,1,3,1,1,1,1,1,1,1,
                      2,2,2,2,2,2,2,2,2,2,2,2,
                      3,3,3,3,3,3,1,3)
treedata_exact_vs_predicted = cbind(treedataexact,treedatapredicted)
colnames(treedata_exact_vs_predicted) = c("Exact Class Value", "Predicted Class Value - 1D Fused Lasso")
treedata_exact_vs_predicted
