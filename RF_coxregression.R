# Load một số gói cho xử lí và phân tích sơ bộ
library(magrittr)
library(tidyverse)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

# Đọc dữ liệu và đánh giá sơ bộ về dữ liệu:
rm(list = ls())
ChurnStudy <- read.csv("C:/Users/lanhu/Downloads/churn_data.csv")
ChurnStudy %>% str()
#check dữ liệu missing
ChurnStudy %>% dim()
sapply(ChurnStudy, function(x) {x %>% is.na() %>% sum()})
#loại hết đám missing đi (do dữ liệu thấp)
#ChurnStudy %<>% na.omit()
#ou thay thế bằng mean:
imp_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}
ChurnStudy <- ChurnStudy %>% mutate_if(is.numeric, imp_mean)
#check lại:
sapply(ChurnStudy, function(x) {sum(is.na(x))})
#summary
ChurnStudy %>% summary()
sum(ChurnStudy$Xmonthly.charges < 0)
#chuyển hết các vụ mua chịu (trả chậm), coi như thành giao dịch dương
convert_to_pos <- function(x) {
  case_when(x < 0 ~ -x,
            
            x > 0 ~ x)}
ChurnStudy %<>% mutate(Xmonthly.charges = convert_to_pos(Xmonthly.charges))
#học vấn of KH
hoc_van <- ChurnStudy$Xeducation %>% unique()
hoc_van
#đặt lại nhãn học vấn cho gọn
convert_hoc_van <- function(x) {case_when(x == hoc_van[1] ~ "Thac si", 
                                          x == hoc_van[2] ~ "Cu nhan", 
                                          x == hoc_van[3] ~ "Tien si")}
ChurnStudy %<>% mutate(Xeducation = convert_hoc_van(Xeducation))

#Exploratory Data analysis (EDA-pt khám phá)
library(hrbrthemes)
theme_set(theme_ipsum())
# Cơ cấu học vấn theo giới tính không có nhiều  khác biệt: 
ChurnStudy %>% 
  group_by(Xgender, Xeducation) %>% 
  count() %>% 
  ggplot(aes(Xgender, n, fill = Xeducation)) +
  geom_col(position = "fill") + 
  coord_flip() + 
  scale_y_percent() + 
  scale_fill_ipsum(name = "Education Level") + 
  theme(legend.position = "top")
# Rời bỏ cũng không khác biệt nhiều giữa các nhóm học vấn khác nhau: 
ChurnStudy %>% 
  mutate(Churn = as.factor(Churn)) %>% 
  group_by(Churn, Xeducation) %>% 
  count() %>% 
  ggplot(aes(Churn, n, fill = Xeducation)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  scale_y_percent() + 
  scale_fill_ipsum(name = "Education Level") + 
  theme(legend.position = "top")

# Cũng không có khác biệt dáng kể về giới tính và học vấn: 
ChurnStudy %>% 
  mutate(Churn = as.factor(Churn)) %>% 
  group_by(Churn, Xeducation, Xgender) %>% 
  count() %>% 
  ggplot(aes(Churn, n, fill = Xeducation)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  scale_y_percent() + 
  scale_fill_ipsum(name = "Education Level") + 
  theme(legend.position = "top") + 
  facet_wrap(~ Xgender)

#Số tiền mua hàng trung bình theo từng nhóm giới tính và học vấn. 
#Nhìn chung nam giới ở tất cả các nhóm học vấn dành nhiều tiền hơn để mua hàng
ChurnStudy %>% 
  group_by(Xgender, Xeducation) %>% 
  summarise_each(funs(mean), Xmonthly.charges) %>% 
  ggplot(aes(Xgender, Xmonthly.charges, fill = Xeducation)) + 
  geom_col(position = "dodge") + 
  scale_fill_ipsum(name = "Education Level")
#số lần mua hàng trung bình mỗi tháng thì nam giới cũng cao hơn:
ChurnStudy %>% 
  group_by(Xgender, Xeducation) %>% 
  summarise_each(funs(mean), Xpurch.last.month) %>% 
  ggplot(aes(Xgender, Xpurch.last.month, fill = Xeducation)) + 
  geom_col(position = "dodge") + 
  scale_fill_ipsum(name = "Education Level")

#Survival Analysis
# Dữ liệu huấn luyện: 
set.seed(1020)
train <- ChurnStudy %>% sample_n(round(0.75*nrow(.))) 
# Dữ liệu kiểm định: 
test <- dplyr::setdiff(ChurnStudy, train)
ChurnStudy_train <- train
ChurnStudy_test <- test
#-------------------------------------------------------------------
#   Thực hiện SA cho toàn bộ không phân biệt giới tính hay  học vấn
#-------------------------------------------------------------------
library(survival)
fit <- survfit(Surv(time, Churn) ~ 1, data = train)
print(fit)

# Kết quả của  phân tích SA: 
u <- summary(fit)
u
#hình ảnh hóa bằng đường cong sống còn (survival curve):
df1 <- data.frame(Rate = u$surv, Tenure = u$time)
df1 %>% 
  ggplot(aes(Tenure, Rate)) + 
  geom_line() + 
  geom_point(color = "red") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  theme_ipsum(grid = "XY")
#cách khác:
library(survminer)
ggsurvplot(fit,
           pval = TRUE, 
           conf.int = TRUE,
           ggtheme = theme_minimal(),
           palette = c("#E7B800"))
#-------------------------------------
#   Thực hiện SA theo nhóm giới tính
#-------------------------------------
fit <- survfit(Surv(time, Churn) ~ Xgender, data = train)
print(fit)
summary(fit)$table
# tốc độ và tỉ lệ rời bỏ cho hai nhóm giới tính
ggsurvplot(fit,
           pval = TRUE, 
           conf.int = TRUE,
           # Bổ sung thêm Risk Table: 
           risk.table = TRUE, 
           # Thay bằng theme_minimal() của gói ggplot2: 
           ggtheme = theme_minimal())
#log-rank test:
survdiff(Surv(time, Churn) ~ Xgender, data = train)

#-------------------------------------
#   Thực hiện SA theo nhóm học vấn
#-------------------------------------
fit_educ <- survfit(Surv(time, Churn) ~ Xeducation +Xeducation , data = train)
fit_educ %>% summary()
#hình ảnh hóa
ggsurvplot(fit_educ,
           pval = TRUE, 
           conf.int = TRUE,
           ggtheme = theme_minimal()) -> p
#Cách 1:
p
#Cách 2:
p$plot + facet_wrap(~ Xeducation)
#log-rank test:
survdiff(Surv(time, Churn) ~ Xeducation, data = train)

#----------------------------------------------
#   Thực hiện SA theo giới tính và học vấn
#----------------------------------------------
fit2 <- survfit(Surv(time, Churn) ~ Xgender + Xeducation, data = train)
p1 <- ggsurvplot(fit2,
                 pval = TRUE, 
                 conf.int = TRUE,
                 risk.table = TRUE, 
                 ggtheme = theme_minimal())
#C1:
p1
#C2:
p1$plot + theme(legend.position = "right")
#C3:
p2 <- ggsurvplot(fit2,
                 pval = TRUE, 
                 conf.int = TRUE, 
                 ggtheme = theme_minimal())
p2
#C4:
p2$plot +
  theme_minimal() + 
  theme(legend.position = "top") +
  facet_grid(Xgender ~ Xeducation)
#log-rank test:
survdiff(Surv(time, Churn) ~ Xgender + Xeducation, data = train)
# Hoặc hình  ảnh hóa tỉ lệ rời bỏ tích lũy theo thời gian:  
p3 <- ggsurvplot(fit2, 
                 fun = "event", 
                 conf.int = TRUE,
                 ggtheme = theme_bw())

p3$plot +
  theme_bw() + 
  theme(legend.position = "right")+
  facet_grid(Xeducation ~ Xgender)
#giữa các nhóm có mức độ thỏa mãn về dịch vụ - chăm sóc khách hàng (Xsatisfaction)
fit3 <- survfit(Surv(time, Churn) ~ Xsatisfaction, data = train)
ggsurvplot(fit3,
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           ggtheme = theme_minimal())
survdiff(Surv(time, Churn) ~ Xsatisfaction, data = train)
#Mức độ không hài lòng của khách hàng thể hiện qua số cuộc gọi phản hồi về dịch vụ và chăm sóc khách hàng (Xservice.calls)
ChurnStudy %>% 
  group_by(Xservice.calls) %>% 
  count() %>% 
  ggplot(aes(Xservice.calls, n)) + 
  geom_col() + 
  geom_text(aes(label = n), vjust = -0.5)
# Dán lại  nhãn:  
train %<>% mutate(Calls = case_when(Xservice.calls == 0 ~ "Yes", 
                                    Xservice.calls != 0 ~ "No"))
# Thực hiện mô  hình KM và hình  ảnh  hóa: 
fit4 <- survfit(Surv(time, Churn) ~ Calls , data = train)
ggsurvplot(fit4,
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           ggtheme = theme_minimal())
#  Có bằng chứng thống kê cho thấy sự khác biệt  về tỉ lệ rời bỏ là có ý  nghĩa:  
survdiff(Surv(time, Churn) ~ Calls, data = train)

#Cox Regression
#hồi quy Cox với hai biến định lượng là Xmonthly.charges và Xpurch.last.month
res.cox1 <- coxph(Surv(time, Churn) ~ Xgender + Xmonthly.charges + 
                    Xeducation + Xsatisfaction + Xpurch.last.month + Xservice.calls, data = train)
res.cox1 %>% summary()
#ước lượng tỷ lệ ở lại/ rời bỏ
res.cox1 %>% 
  survfit() %>% 
  summary() -> u
u
cox_fit <- survfit(res.cox1)
# Và hình ảnh hóa tỉ lệ ở lại: 
df3 <- data.frame(Rate = u$surv, Tenure = u$time)
df3 %>% 
  ggplot(aes(Tenure, Rate)) + 
  geom_line() + 
  geom_point(color = "red") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  theme_ipsum(grid = "XY")
#cách khác:
ggsurvplot(survfit(res.cox1), 
           color = "#2E9FDF",
           ggtheme = theme_minimal())
#hình ảnh hóa tỷ lệ ở lại theo giới tính:
df_new <- data.frame(Xgender = c("M", "F"), 
                     Xmonthly.charges = rep(mean(train$Xmonthly.charges),  2), 
                     Xsatisfaction  = rep(mean(train$Xsatisfaction),  2), 
                     Xpurch.last.month = rep(mean(train$Xpurch.last.month), 2))

fit1 <- survfit(res.cox1, newdata = df_new)

ggsurvplot(fit1, 
           legend.labs = c("Sex = M", "Sex = F"),
           ggtheme = theme_minimal())

#Random Forests Model
r_fit <- ranger::ranger(Surv(time, Churn) ~ Xgender + Xmonthly.charges + 
                          Xeducation + Xsatisfaction + Xpurch.last.month + Xservice.calls, data = train, mtry = 4,
                        importance = "permutation",
                        splitrule = "extratrees",
                        verbose = TRUE)
library(randomForest)
train$Churn <- as.factor(train$Churn)
ChurnStudy$Churn <- as.factor(ChurnStudy$Churn)
set.seed(10)
id <- sample(2,nrow(ChurnStudy), prob = c(0.75,0.25), replace = TRUE)
ChurnStudy_train <- ChurnStudy[id == 1,]
ChurnStudy_test <- ChurnStudy[id == 2,]


ChurnStudy_train$hocvan[ChurnStudy_train$Xeducation == "Thac si"]<- 1
ChurnStudy_train$hocvan[ChurnStudy_train$Xeducation == "Cu nhan"]<- 2
ChurnStudy_train$hocvan[ChurnStudy_train$Xeducation == "Tien si"]<- 3
ChurnStudy_test$hocvan[ChurnStudy_test$Xeducation == "Thac si"]<- 1
ChurnStudy_test$hocvan[ChurnStudy_test$Xeducation == "Cu nhan"]<- 2
ChurnStudy_test$hocvan[ChurnStudy_test$Xeducation == "Tien si"]<- 3
ChurnStudy_train$Xeducation <- NULL
bestmtry <- tuneRF(ChurnStudy_train,ChurnStudy_train$Churn, stepFactor = 1.2, improve = 0.01,trace = TRUE, plot = TRUE)
#-> mtry = 3
Churn_forest <- randomForest(Churn ~ Xgender + hocvan + Xmonthly.charges
                             + Xsatisfaction + Xpurch.last.month + Xservice.calls, 
                             data = ChurnStudy_train, mtry = 3)
Churn_forest
#Biến quan trọng & đồ thị
importance(Churn_forest)
varImpPlot(Churn_forest, sort = T, main = "Variable Importance")
#Predict base on RF:
pred1_Churn <- predict(Churn_forest, newdata = ChurnStudy_test, type = "class")
library(caret)
confusionMatrix(table(pred1_Churn,ChurnStudy_test$Churn))

#Check trên tập test
res.cox2 <- coxph(formula = Surv(time, Churn) ~ Xgender + Xmonthly.charges + Xeducation +
                    Xsatisfaction + Xpurch.last.month, data = test)
ggforest(res.cox2, data=test)
res.cox2 %>% summary()
res.cox2 %>% 
  survfit() %>% 
  summary() -> v
v
#Random Forests Model
r_fit <- ranger::ranger(Surv(time, Churn) ~ Xgender + Xeducation + Xmonthly.charges + 
                          Xsatisfaction + Xpurch.last.month + Xservice.calls, data = train, mtry = 3,
                        importance = "permutation",
                        splitrule = "extratrees",
                        verbose = TRUE)
# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(train)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))
#The next block of code illustrates how ranger() ranks variable importance.
vi <- data.frame(sort(round(r_fit$vsariable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
#computing the area under the ROC curve
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)

#Đồ thị so sánh
coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()


#Set up for ggplot
fit <- survfit(Surv(time, Churn) ~ 1, data = train)
kmi <- rep("KM",length(fit$time))
km_df <- data.frame(fit$time,fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)



