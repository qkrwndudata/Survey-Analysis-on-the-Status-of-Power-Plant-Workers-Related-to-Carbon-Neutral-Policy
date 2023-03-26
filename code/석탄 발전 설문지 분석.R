#install.packages("lavaan")
#install.packages("semTools")
#install.packages("psych")
#install.packages("semPlot")
#install.packages("readr")
#install.packages("stargazer")
#install.packages("nparcomp")

library(FSA)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(dunn.test)
library(lavaan)
library(semTools)
library(psych)
library(semPlot)
library(readr)
library(knitr)
library(stargazer)
library(gridExtra)

data <- read.csv("C:/Users/Park JuYoung/Desktop/survey_data.csv", header = T, fileEncoding = "euc-kr")
summary(data)
table(data$q7_7)
table(data$q7_7_n2)

data %>% 
  filter(q7_7 == 4, q7_7_n2 == 4) %>%
  summarize(count = n(),
            prop = count / 1928)

data %>% 
  filter(q7_7 == 4, q7_7_n2 == 3) %>%
  summarize(count = n(),
            prop = count / 1928)

data %>% 
  filter(q7_7 == 3, q7_7_n2 == 4) %>%
  summarize(count = n(),
            prop = count / 1928)

data %>% 
  filter(q7_7 == 3, q7_7_n2 == 3) %>%
  summarize(count = n(),
            prop = count / 1928)


0.1099585 + 0.07624481 + 0.04875519 + 0.2173237   # 약 34.5% 정도가 잘 모른 상태에서 답변함


View(data)

Summarize(q5_2 ~ 성별, data)
Summarize(q5_2 ~ 연령대, data)
Summarize(q5_2 ~ 학력, data)
Summarize(q5_2 ~ 근속년수, data)
Summarize(q5_2 ~ 소속회사, data)   # 한전산업개발의 값이 다른 회사들에 비해 유독 높게 나타남
Summarize(q5_2 ~ 근무처, data)   # 서부발전의 답이 상대적으로 조금 낮게 나타남 
Summarize(q5_2 ~ 근무지역, data)
Summarize(q5_2 ~ 직종, data)

Summarize(q5_2 ~ 소속회사 + 근무처, data)   # 한 번에 확인 가능

ggbarplot(data, x = "소속회사", y = "q5_2", 
          add = c("mean_ci"), fill = "소속회사", legend = "bottom", ylab = "응답 평균")

a <- ggbarplot(data, x = "직종", y = "q5_2", 
          add = c("mean_ci"), fill = "직종", legend = "bottom", ylab = "응답 평균")

ggbarplot(data, x = "근속년수", y = "q5_2", 
          add = c("mean_ci"), fill = "근속년수", legend = "bottom", ylab = "응답 평균")

ggbarplot(data, x = "학력", y = "q5_2", 
          add = c("mean_ci"), fill = "학력", legend = "bottom", ylab = "응답 평균")

grid.arrange(a,b,c,  nrow=1, ncol=3)

### anova 1: 소속회사 별로 고용불안 정도에 차이가 있는가###
y <- data$q5_2
group <- data$소속회사

# 등분산성 가정 확인
boxplot(y ~ group)
bartlett.test(y ~ group)   # 등분산성을 만족하지 않음

# 정규성 가정 확인
obj <- aov(y ~ group)

e <- resid(obj)

par(mfrow = c(1,2), cex.main = 1)
hist(e, main = "histogram of residual", xlab = "residual")
qqnorm(e); qqline(e)

shapiro.test(e)   # 정규성을 만족하지 않음


## 비모수 anova 진행
kruskal.test(q5_2 ~ 소속회사, data = data)   # 적어도 어느 한 집단의 평균은 다르다.

## 사후분석
pairwise.t.test(data$q5_2, data$소속회사, p.adjust.method="bonferroni")   # 한전산업개발이 다른 기업들과 차이가 있음
nparcomp::nparcomp(q5_2 ~ 소속회사, data = data, type = "Tukey")


### anova 2: 근무처 별로 고용불안 정도에 차이가 있는지###
subdata <- data %>%
  filter(근무처 != '해당없음')


y <- subdata$q5_2
group <- subdata$근무처

# 등분산성 가정 확인
dev.off()
boxplot(y ~ group)
bartlett.test(y ~ group)   # 등분산성을 만족하지 않음

# 정규성 가정 확인
obj <- aov(y ~ group)

e <- resid(obj)

par(mfrow = c(1,2), cex.main = 1)
hist(e, main = "histogram of residual", xlab = "residual")
qqnorm(e); qqline(e)

shapiro.test(e)   # 정규성을 만족하지 않음


## 비모수 anova 진행
kruskal.test(q5_2 ~ 근무처, data = subdata)   # 그룹 간 유의미한 차이가 없음

### anova 3: 직종 별로 고용불안 정도에 차이가 있는가###
y <- data$q5_2
group <- data$직종

# 등분산성 가정 확인
boxplot(y ~ group)
bartlett.test(y ~ group)   # 등분산성을 만족하지 않음

# 정규성 가정 확인
obj <- aov(y ~ group)

e <- resid(obj)

par(mfrow = c(1,2), cex.main = 1)
hist(e, main = "histogram of residual", xlab = "residual")
qqnorm(e); qqline(e)

shapiro.test(e)   # 정규성을 만족하지 않음


## 비모수 anova 진행
kruskal.test(q5_2 ~ 직종, data = data)   # 적어도 어느 한 집단의 평균은 다르다.

## 사후분석
nparcomp::nparcomp(q5_2 ~ 직종, data = data, type = "Tukey")


### anova 4: 근속년수 별로 고용불안 정도에 차이가 있는가###
y <- data$q5_2
group <- data$근속년수

# 등분산성 가정 확인
boxplot(y ~ group)
bartlett.test(y ~ group)   # 등분산성을 만족하지 않음

# 정규성 가정 확인
obj <- aov(y ~ group)

e <- resid(obj)

par(mfrow = c(1,2), cex.main = 1)
hist(e, main = "histogram of residual", xlab = "residual")
qqnorm(e); qqline(e)

shapiro.test(e)   # 정규성을 만족하지 않음


## 비모수 anova 진행
kruskal.test(q5_2 ~ 근속년수, data = data)   # 적어도 어느 한 집단의 평균은 다르다.

## 사후분석
dunn.test(data$q5_2, data$근속년수, method = 'bonferroni')   # 20년이상과 1~5년, 5~1-년과 10~20년에서만 차이가 없음
nparcomp::nparcomp(q5_2 ~ 근속년수, data = data, type = "Tukey")

### anova 5: 학력 별로 고용불안 정도에 차이가 있는가###
y <- data$q5_2
group <- data$학력

# 등분산성 가정 확인
boxplot(y ~ group)
bartlett.test(y ~ group)   # 등분산성을 만족함

# 정규성 가정 확인
obj <- aov(y ~ group)

e <- resid(obj)

par(mfrow = c(1,2), cex.main = 1)
hist(e, main = "histogram of residual", xlab = "residual")
qqnorm(e); qqline(e)

shapiro.test(e)   # 정규성을 만족하지 않음


## 비모수 anova 진행
kruskal.test(q5_2 ~ 학력, data = data)   # 적어도 어느 한 집단의 평균은 다르다.

## 사후분석
dunn.test(data$q5_2, data$학력, method = 'bonferroni')   # 전문대 졸업이 고등학교 졸업, 대학교 졸업과 차이를 보임
nparcomp::nparcomp(q5_2 ~ 학력, data = data, type = "Tukey")



########## SEM ###########################
data$소속회사 <- as.factor(data$소속회사)

# 모르겠습니다를 어떻게 처리할 지 (보통이다로 대체하자): 3, 4 -> 4, 5 / 5 -> 3
data[data$q2_1 == 5, "q2_1"] = 6
data[data$q2_1 == 4, "q2_1"] = 5
data[data$q2_1 == 3, "q2_1"] = 4
data[data$q2_1 == 6, "q2_1"] = 3

table(data$q2_1)

data[data$q2_1_n2 == 5, "q2_1_n2"] = 6
data[data$q2_1_n2 == 4, "q2_1_n2"] = 5
data[data$q2_1_n2 == 3, "q2_1_n2"] = 4
data[data$q2_1_n2 == 6, "q2_1_n2"] = 3

table(data$q2_1_n2)

data[data$q4_1 == 5, "q4_1"] = 6
data[data$q4_1 == 4, "q4_1"] = 5
data[data$q4_1 == 3, "q4_1"] = 4
data[data$q4_1 == 6, "q4_1"] = 3

table(data$q4_1)


data[data$q4_1_n2 == 5, "q4_1_n2"] = 6
data[data$q4_1_n2 == 4, "q4_1_n2"] = 5
data[data$q4_1_n2 == 3, "q4_1_n2"] = 4
data[data$q4_1_n2 == 6, "q4_1_n2"] = 3

table(data$q4_1_n2)

data[data$q6_2 == 5, "q6_2"] = 6
data[data$q6_2 == 4, "q6_2"] = 5
data[data$q6_2 == 3, "q6_2"] = 4
data[data$q6_2 == 6, "q6_2"] = 3

table(data$q6_2)


data[data$q7_1 == 5, "q7_1"] = 6
data[data$q7_1 == 4, "q7_1"] = 5
data[data$q7_1 == 3, "q7_1"] = 4
data[data$q7_1 == 6, "q7_1"] = 3

table(data$q7_1)

data[data$q7_4_n2 == 5, "q7_4_n2"] = 6
data[data$q7_4_n2 == 4, "q7_4_n2"] = 5
data[data$q7_4_n2 == 3, "q7_4_n2"] = 4
data[data$q7_4_n2 == 6, "q7_4_n2"] = 3

table(data$q7_4_n2)

data[data$q7_4_n3 == 5, "q7_4_n3"] = 6
data[data$q7_4_n3 == 4, "q7_4_n3"] = 5
data[data$q7_4_n3 == 3, "q7_4_n3"] = 4
data[data$q7_4_n3 == 6, "q7_4_n3"] = 3

table(data$q7_4_n3)

data[data$q7_2 == 5, "q7_2"] = 6
data[data$q7_2 == 4, "q7_2"] = 5
data[data$q7_2 == 3, "q7_2"] = 4
data[data$q7_2 == 6, "q7_2"] = 3

table(data$q7_2)

data[data$q7_5_n2 == 5, "q7_5_n2"] = 6
data[data$q7_5_n2 == 4, "q7_5_n2"] = 5
data[data$q7_5_n2 == 3, "q7_5_n2"] = 4
data[data$q7_5_n2 == 6, "q7_5_n2"] = 3

table(data$q7_5_n2)

data[data$q7_5_n3 == 5, "q7_5_n3"] = 6
data[data$q7_5_n3 == 4, "q7_5_n3"] = 5
data[data$q7_5_n3 == 3, "q7_5_n3"] = 4
data[data$q7_5_n3 == 6, "q7_5_n3"] = 3

table(data$q7_5_n3)

data[data$q7_3 == 5, "q7_3"] = 6
data[data$q7_3 == 4, "q7_3"] = 5
data[data$q7_3 == 3, "q7_3"] = 4
data[data$q7_3 == 6, "q7_3"] = 3

table(data$q7_3)

data[data$q7_6_n2 == 5, "q7_6_n2"] = 6
data[data$q7_6_n2 == 4, "q7_6_n2"] = 5
data[data$q7_6_n2 == 3, "q7_6_n2"] = 4
data[data$q7_6_n2 == 6, "q7_6_n2"] = 3

table(data$q7_6_n2)

data[data$q7_6_n3 == 5, "q7_6_n3"] = 6
data[data$q7_6_n3 == 4, "q7_6_n3"] = 5
data[data$q7_6_n3 == 3, "q7_6_n3"] = 4
data[data$q7_6_n3 == 6, "q7_6_n3"] = 3

table(data$q7_6_n3)

dev.off()
par("mar")
par(mar=c(1,1,1,1))

par(mfrow = c(1,3))

hist(data$q7_1, col = "blue", freq = FALSE, main = "Q. 7-1", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))
hist(data$q7_4_n2, col = "blue", freq = FALSE, main = "Q. 7-4.2", xlab = "응답분포", ylab = "응답비율", ylim = c(0, 1))
hist(data$q7_4_n3, col = "blue", freq = FALSE, main = "Q. 7-4.3", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))

hist(data$q7_2, col = "blue", freq = FALSE, main = "Q. 7-2", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))
hist(data$q7_5_n2, col = "blue", freq = FALSE, main = "Q. 7-5.2", xlab = "응답분포", ylab = "응답비율", ylim = c(0, 1))
hist(data$q7_5_n3, col = "blue", freq = FALSE, main = "Q. 7-5.3", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))

hist(data$q7_3, col = "blue", freq = FALSE, main = "Q. 7-3", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))
hist(data$q7_6_n2, col = "blue", freq = FALSE, main = "Q. 7-6.2", xlab = "응답분포", ylab = "응답비율", ylim = c(0, 1))
hist(data$q7_6_n3, col = "blue", freq = FALSE, main = "Q. 7-6.3", xlab = "응답분포", ylab = "응답비율", ylim = c(0,1))


overallmodel = '
policy =~ q4_1 + q4_1_n2 + q6_2
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
'

overallmodel.fit <- cfa(overallmodel, data = data, meanstructure = T)
summary(overallmodel.fit, rsquare = T, standardized = T, fit.measures = T)   # 1078.184




## group subset
group1 <- subset(data, 소속회사 =="발전공기업")   
group2 <- subset(data, 소속회사 =="발전자회사")
group3 <- subset(data, 소속회사 =="한전KPS")
group4 <- subset(data, 소속회사 =="한전산업개발")

overallmodel.fit1 <- cfa(overallmodel, data = group1, meanstructure = T)
summary(overallmodel.fit1, rsquare = T, standardized = T, fit.measures = T)   # 535.926

overallmodel.fit2 <- cfa(overallmodel, data = group2, meanstructure = T)
summary(overallmodel.fit2, rsquare = T, standardized = T, fit.measures = T)   # 78.936

overallmodel.fit3 <- cfa(overallmodel, data = group3, meanstructure = T)   
summary(overallmodel.fit3, rsquare = T, standardized = T, fit.measures = T)   # 307.576

overallmodel.fit4 <- cfa(overallmodel, data = group4, meanstructure = T)
summary(overallmodel.fit4, rsquare = T, standardized = T, fit.measures = T)   # 358.215

multisteps <- measurementInvariance(model = overallmodel, data = data, group = "소속회사", strict = T)


data$소속회사 %>% table()

overallmodel = '
government =~ q4_1 + q4_1_n2 + q6_2
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2
'

dev.off()


### 발전공기업 SEM 적합 ###


## 모델 수정

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_1 ~~ q7_2 + q7_4_n3
q7_4_n2	~~q7_4_n3
q7_2	~~ q7_3
q7_5_n2	~~ q7_5_n3
q7_4_n2	~~	q7_6_n3
'
model.fit1 <- sem(overallmodel, data = subset(data, 소속회사 == "발전공기업"))
model.fit1 %>% summary(fit = T, stand = T)
model.fit1 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정도 좋음
model.fit1 %>% semPaths(what = "paths", whatLabels = "stand", rotation = 2)
## 2. 구조모델 적합 ##

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_1 ~~ q7_2 + q7_4_n3
q7_4_n2	~~q7_4_n3
q7_2	~~ q7_3
q7_5_n2	~~ q7_5_n3
q7_4_n2	~~	q7_6_n3

anxiety ~ government + company + union
'

model.fit1 <- sem(overallmodel, data = subset(data, 소속회사 == "발전공기업"))


standardizedsolution(model.fit1) %>%
  filter(op=="~") %>%
  mutate(stars=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std,
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)   # 모든 변수가 통계적으로 유의하게 나타남

lavInspect(model.fit1, what="rsquare")  # 결정계수

semPaths(model.fit1, what="std", layout="tree2", edge.label.cex=1, edge.color="royalblue",
         color=list(lat="lightcoral", man="lavenderblush"), fade=FALSE,
         style="lisrel", curvature=2, rotation = 4)


### 한전KPS SEM 적합 ###

## 1. 확인적 요인분석 진행 ##

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2
'

model.fit2 <- sem(overallmodel, data = subset(data, 소속회사 == "한전KPS"))
model.fit2 %>% summary(fit = T, stand = T)
model.fit2 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정도 좋지 않음

# 모형 개선: 논문 방식을 따라보자(수정지표 활용)
summary(model.fit2, modindices=TRUE)
modindices(model.fit2, sort.=TRUE, minimum.value=3)

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_2 ~~ q7_3
q7_4_n2	~~ q7_4_n3
'

model.fit2 <- sem(overallmodel, data = subset(data, 소속회사 == "한전KPS"))
model.fit2 %>% summary(fit = T, stand = T)
model.fit2 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정 좋음
model.fit2 %>% semPaths(what = "paths", whatLabels = "stand", rotation = 2)

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_2 ~~ q7_3
q7_4_n2	~~ q7_4_n3

anxiety ~ government + company + union
'

model.fit2 <- sem(overallmodel, data = subset(data, 소속회사 == "한전KPS"))

standardizedsolution(model.fit2) %>%
  filter(op=="~") %>%
  mutate(stars=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std,
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)   # 모든 변수가 통계적으로 유의하게 나타남

lavInspect(model.fit2, what="rsquare")  # 결정계수

semPaths(model.fit2, what="std", layout="tree2", edge.label.cex=1, edge.color="royalblue",
         color=list(lat="lightcoral", man="lavenderblush"), fade=FALSE,
         style="lisrel", curvature=2, rotation = 4)


### 한전산업개발 SEM 적합 ###

## 1. 확인적 요인분석 진행 ##

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2
'

model.fit3 <- sem(overallmodel, data = subset(data, 소속회사 == "한전산업개발"))
model.fit3 %>% summary(fit = T, stand = T)
model.fit3 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정도 좋지 않음

# 모형 개선: 논문 방식을 따라보자(수정지표 활용)
summary(model.fit3, modindices=TRUE)
modindices(model.fit3, sort.=TRUE, minimum.value=3)

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_2 ~~ q7_3
'

model.fit3 <- sem(overallmodel, data = subset(data, 소속회사 == "한전산업개발"))
model.fit3 %>% summary(fit = T, stand = T)
model.fit3 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정 좋음
model.fit3 %>% semPaths(what = "paths", whatLabels = "stand", rotation = 2)

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

q7_2 ~~ q7_3

anxiety ~ government + company + union
'

model.fit3 <- sem(overallmodel, data = subset(data, 소속회사 == "한전산업개발"))

standardizedsolution(model.fit3) %>%
  filter(op=="~") %>%
  mutate(stars=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std,
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)   # 모든 변수가 통계적으로 유의하게 나타남

lavInspect(model.fit3, what="rsquare")  # 결정계수

semPaths(model.fit3, what="std", layout="tree2", edge.label.cex=1, edge.color="royalblue",
         color=list(lat="lightcoral", man="lavenderblush"), fade=FALSE,
         style="lisrel", curvature=2, rotation = 4)


### 발전자회사 SEM 적합 ###

## 1. 확인적 요인분석 진행 ##

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2
'

model.fit4 <- sem(overallmodel, data = subset(data, 소속회사 == "발전자회사"))
model.fit4 %>% summary(fit = T, stand = T)
model.fit4 %>% fitMeasures(c("gfi", "rmsea", "cfi"))   # 적합정도 괜찮음
model.fit4 %>% semPaths(what = "paths", whatLabels = "stand", rotation = 2)

overallmodel = '
government =~ q7_1 + q7_4_n2 + q7_4_n3
company =~ q7_2 + q7_5_n2 + q7_5_n3
union =~ q7_3 + q7_6_n2 + q7_6_n3
anxiety =~ q5_2

anxiety ~ government + company + union
'

model.fit4 <- sem(overallmodel, data = subset(data, 소속회사 == "발전자회사"))

standardizedsolution(model.fit4) %>%
  filter(op=="~") %>%
  mutate(stars=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "")))) %>%
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std,
         Z=z, "p-value"=pvalue, Sig.=stars) %>%
  stargazer(type="text", title="Regression Coefficients", summary=FALSE,
            digits=3, digits.extra=0, rownames=FALSE)   # 정부 변수만 통계적으로 유의하게 나타남

lavInspect(model.fit4, what="rsquare")  # 결정계수

semPaths(model.fit4, what="std", layout="tree2", edge.label.cex=1, edge.color="royalblue",
         color=list(lat="lightcoral", man="lavenderblush"), fade=FALSE,
         style="lisrel", curvature=2, rotation = 4)
