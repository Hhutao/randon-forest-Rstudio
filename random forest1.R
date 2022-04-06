# 加载包
library(tidyverse)
library(caret)

# 加载数据
data3 <- read.csv("sh_selected_5.csv")
colnames(data3)
for (i in c(1,3:5,7)) {
  data3[,i] <- factor(data3[,i])
}
skimr::skim(data3)

# 变量取值分布图形
for (i in 1:10) {
  if (i %in% c(1,3:5,7)) {
    ggplot(data3, aes_string(x = colnames(data3)[i])) +
      geom_bar(fill = "skyblue", width = 0.8) +
      theme_bw()
    ggsave(paste0("sh_", colnames(data3)[i], ".png"))
  } else {
    ggplot(data3, aes_string(x = colnames(data3)[i])) +
      geom_histogram(aes(y=..density..), fill = "skyblue") +
      geom_density() +
      theme_bw()
    ggsave(paste0("sh_", colnames(data3)[i], ".png"))
  }
}
# 相关性热图
DataExplorer::plot_correlation(data3)
ggsave("sh_all_corplot.tiff", height = 10, width = 10)
DataExplorer::plot_correlation(data3, type = "continuous")
ggsave("sh_cont_corplot.tiff", height = 10, width = 10)
DataExplorer::plot_correlation(data3, type = "discrete")
ggsave("sh_disc_corplot.tiff", height = 10, width = 10)

# 拆分数据集
set.seed(42)
trainindex <- createDataPartition(data3$均价, p = 0.8)
traindata <- data3[trainindex[[1]], ]
traindata_x <- traindata[,-2]
traindata_y <- traindata[,2]
testdata <- data3[-trainindex[[1]], ]
testdata_x <- testdata[,-2]
testdata_y <- testdata[,2]

# 独热编码
dummyVarsfunction <- 
  dummyVars(~., data = traindata_x, fullRank = T, sep = "_")
traindata_x <- predict(dummyVarsfunction, newdata = traindata_x)
testdata_x <- predict(dummyVarsfunction, newdata = testdata_x)

# 归一化
rangefunction <- 
  preProcess(traindata_x, method = "range")
traindata_x <- predict(rangefunction, newdata = traindata_x)
testdata_x <- predict(rangefunction, newdata = testdata_x)


# 设置训练参数
set.seed(43)
cvindex <- createFolds(traindata$均价, k = 5, returnTrain = T)
trainset <- trainControl(method = "cv", index = cvindex)

# 训练随机森林
set.seed(4)
fit_rf <- train(x = traindata_x,
                y = traindata_y,
                method = "rf",
                trControl = trainset,
                tuneLength = 5,
                nodesize = 10)
fit_rf
plot(fit_rf)



# 预测训练集
predtrain <- predict(fit_rf, newdata = traindata_x)
evaltrain <- data.frame(
  set = "train",
  t(defaultSummary(data = data.frame(obs = traindata_y,
                                     pred = predtrain)))
)
# 预测测试集
predtest <- predict(fit_rf, newdata = testdata_x)
evaltest <- data.frame(
  set = "test",
  t(defaultSummary(data = data.frame(obs = testdata_y,
                                     pred = predtest)))
)
rbind(evaltrain, evaltest)
write.csv(rbind(evaltrain, evaltest), "sh_eval.csv", row.names = F)

testdata2 <- testdata %>%
  mutate(预测均价 = predtest) %>%
  select(均价, 预测均价) %>%
  mutate(预测误差 = 均价 - 预测均价,
             相对绝对误差 = abs(预测误差)/均价)
write.csv(testdata2, "sh_predtest.csv", row.names = F)
ggplot(testdata2, aes(x = 均价, y = 预测均价)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_bw()
ggsave("sh_predtest.tiff", height = 10, width = 10)
