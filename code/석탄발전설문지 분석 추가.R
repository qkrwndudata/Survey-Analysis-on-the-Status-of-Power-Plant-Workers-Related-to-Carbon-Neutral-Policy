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
dunn.test(data$q5_2, data$직종, method = 'bonferroni')   # 사무 vs 기타


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


### anova 4: 학력 별로 고용불안 정도에 차이가 있는가###
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
