## ggplot 및 dplyr을 사용해 설문지 문항에 대한 시각화를 진행한 코드입니다.

# 라이브러리 
library(rlang)
library(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)

data <- read_spss("C:/Users/Park JuYoung/Desktop/연구소/석탄발전보고서/(0622)에너지 전환관련 조합원 인식조사_v1.SAV")

names(data)
dim(data)

data$q1_6 <- ifelse(is.na(data$q1_6), "해당없음", data$q1_6)

data <- data %>% 
  select(-("이름":"연락처")) %>%
  rename("성별" = "q1_1",
         "연령대" = "q1_2",
         "학력" = "q1_3",
         "근속년수" = "q1_4",
         "소속회사" = "q1_5",
         "근무처" = "q1_6",
         "근무지역" = "q1_7",
         "직종" = "q1_8")

data$성별 <- as.factor(data$성별)
table(data$성별)
levels(data$성별) <- c("남", "여")

data$연령대 <- as.factor(data$연령대)
table(data$연령대)
levels(data$연령대) <- c("20대", "30대", "40대", "50대", "60대 이상")

data$학력 <- as.factor(data$학력)
table(data$학력)
levels(data$학력) <- c("중학교 졸업 이하", "고등학교 졸업", "전문대 졸업", "대학교 졸업", "대학원 졸업")

data$근속년수 <- as.factor(data$근속년수)
table(data$근속년수)
levels(data$근속년수) <- c("1년 미만", "1~5년", "5~10년", "10~20년", "20년 이상")

data$소속회사 <- as.factor(data$소속회사)
table(data$소속회사)
levels(data$소속회사) <- c("발전공기업", "한전KPS", "한전산업개발", "발전자회사")

data$근무처 <- as.factor(data$근무처)
table(data$근무처)
levels(data$근무처) <- c("남동발전", "남부발전", "서부발전", "중부발전", "동서발전", "해당없음")

data$근무지역 <- as.factor(data$근무지역)
table(data$근무지역)
levels(data$근무지역) <- c("수도권", "서해권", "남해권", "동해권", "부산울산권", "제주도", "기타")

data$직종 <- as.factor(data$직종)
table(data$직종)
levels(data$직종) <- c("사무", "기계", "전기", "화학", "환경미화", "경비", "기타")




table(is.na(data))

data <- na.omit(data)
dim(data)
View(data)
summary(data)



### 기본적인 EDA ###
data %>% 
  group_by(성별) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
    pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 성별)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 성별 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

data %>% 
  group_by(연령대) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 연령대)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.4)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 연령대 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

data %>% 
  group_by(소속회사) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 소속회사)) + 
  geom_col(color = "black") + 
  geom_text(aes(x = 1.1, label = pct),
            position = position_stack(vjust = 0.6)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 소속회사 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.text=element_text(size=10))

data %>% 
  group_by(근무처) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 근무처)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "발전공기업 노동자 근무처 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.text=element_text(size=10))



data %>% 
  group_by(학력) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 학력)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5),
            check_overlap = T) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 학력 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

  data %>% 
  group_by(근속년수) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 근속년수)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 근속년수 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

data %>% 
  group_by(근무지역) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 근무지역)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.6)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 근무지역 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

data %>% 
  group_by(직종) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = 직종)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "발전소 노동자 직종 분포") + 
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))


### Q2_1 ###

data %>%
  ggplot(aes(x = q2_1)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 기후위기는 심각한 사회문제로 정부 차원의 적극 대응이 필요하다") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q2_1) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q2_1, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.5) +
  ggtitle("Q. 기후위기는 심각한 사회문제로 정부 차원의 적극 대응이 필요하다") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


# 폰트 수정하기!
  

### Q2_2 ###
data %>%
  ggplot(aes(x = q2_1_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 기후위기는 작업환경 악화, 실업 문제 등 노동자에게 큰 영향을 미치고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q2_1_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q2_1_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.7) +
  ggtitle("Q. 기후위기는 작업환경 악화, 실업 문제 등 노동자에게 큰 영향을 미치고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


### Q3_1 ###
data$q3_1 <- as.factor(data$q3_1)
data$q3_1 <- ifelse(data$q3_1 == 1, "네", "아니오")

a <- data %>% 
  group_by(q3_1) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = q3_1)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Q. 정부가 발표한 \n ‘2050 탄소중립’ 정책 선언에 대해 들어본 적이 있다.") + 
  theme_void() +
  theme(plot.title = element_text(size = 13, hjust = 0.5))+
  guides(fill = guide_legend(title = "응답"))


### Q3_1_n2 ###
data$q3_1_n2 <- as.factor(data$q3_1_n2)
data$q3_1_n2 <- ifelse(data$q3_1_n2 == 1, "네", "아니오")

b <- data %>% 
  group_by(q3_1_n2) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = q3_1_n2)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Q. 2022년 3월 25일 시행된 기후 위기 대응을 위한 \n ‘탄소중립·녹색성장기본법’에 대해 알고 있다.") + 
  theme_void() +
  theme(plot.title = element_text(size = 13, hjust = 0.5))+
  guides(fill = guide_legend(title = "응답"))

grid.arrange(a,b, nrow=1, ncol=2)

### Q3_2 ###
data %>%
  ggplot(aes(x = q3_2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 위의 정보를 알고 있다면 어떻게 알게 되었습니까?") +
  scale_x_continuous(breaks = c(1:7), label = c("신문, 인터넷 등 메스컴", "직장 동료", "회사 관리자", "노동조합", "지역 주민", "위의 정보를 모른다", "기타")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q3_2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q3_2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 위의 정보를 알고 있다면 어떻게 알게 되었습니까?") +
  scale_x_continuous(breaks = c(1:7), label = c("신문, 인터넷 등 메스컴", "직장 동료", "회사 관리자", "노동조합", "지역 주민", "위의 정보를 모른다", "기타")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


# 3-3, 3-4 없음!

### Q4_1 ###
data %>%
  ggplot(aes(x = q4_1)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. ‘2050 탄소중립’ 정책과 탄소중립·녹색성장기본법의 취지 및 필요성에 공감한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q4_1) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q4_1, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. ‘2050 탄소중립’ 정책과 탄소중립·녹색성장기본법의 취지 및 필요성에 공감한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


### Q4_1_2 ###
data %>%
  ggplot(aes(x = q4_1_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. ‘2050 탄소중립 정책’은 현실적으로 실현 가능한 정책이라고 생각한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q4_1_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q4_1_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. ‘2050 탄소중립 정책’은 현실적으로 실현 가능한 정책이라고 생각한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

# 4-2 없음!



### Q5_1 
data1 <- data %>% 
  group_by(소속회사, q5_1) %>%
  summarise(total = n())

data2 <- data %>% 
  group_by(소속회사, q5_1_m2) %>%
  summarise(total = n()) %>%
  rename("q5_1" = "q5_1_m2")

data3 <- data %>% 
  group_by(소속회사, q5_1_m3) %>%
  summarise(total = n()) %>%
  rename("q5_1" = "q5_1_m3")

data5_1 <- rbind(data1, data2)
data5_1 <- rbind(data5_1, data3)


data5_1_sum <- aggregate(total ~ q5_1, data5_1, sum)

sum(data5_1_sum$total)

data5_1_sum <- data5_1_sum %>%
  mutate(pct = total / 5784)

data5_1_sum %>%
  ggplot(aes(x = q5_1, y = pct)) +
  geom_bar(stat = "identity", fill = "blue", colour = "black") + 
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.25) +
  ggtitle("Q. 2030 NDC 정책에 따라 석탄화력발전소가 단계적으로 폐쇄될 예정입니다. \n 이에 따라 나타날 문제 중 가장 걱정되는 문제 세 개를 선택해 주세요. ") +
  scale_x_continuous(breaks = c(1:6), labels = c("재생 에너지 개발에 따른 환경훼손", "경제성장 둔화", "전기 요금 상승", "전기 공급의 불안정성", "투명한 정보공개 및 소통", "일자리 문제(실업 등)")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

data5_1 %>% 
  group_by(소속회사, q5_1) %>%
  summarise(total = sum(total))%>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total))%>%
  ggplot(aes(x = q5_1, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 2030 NDC 정책에 따라 석탄화력발전소가 단계적으로 폐쇄될 예정입니다. \n 이에 따라 나타날 문제 중 가장 걱정되는 문제 세 개를 선택해 주세요. ") +
  scale_x_continuous(breaks = c(1:6), labels = c("재생 에너지 개발에 따른 환경훼손", "경제성장 둔화", "전기 요금 상승", "전기 공급의 불안정성", "투명한 정보공개 및 소통", "일자리 문제(실업 등)")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))




### Q5_2
data %>%
  ggplot(aes(x = q5_2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 정부 정책에 따라 2034년까지 30기의 석탄화력발전소가 폐쇄됩니다. \n 이에 대해 귀하가 느끼는 고용 위기 및 불안감은 어떠하십니까.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 심각하지 않다", "심각하지 않다", "보통이다", "심각하다", "매우 심각하다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q5_2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q5_2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정부 정책에 따라 2034년까지 30기의 석탄화력발전소가 폐쇄됩니다. \n 이에 대해 귀하가 느끼는 고용 위기 및 불안감은 어떠하십니까.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 심각하지 않다", "심각하지 않다", "보통이다", "심각하다", "매우 심각하다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

### Q5_3
data %>%
  ggplot(aes(x = q5_3)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 귀하가 속한 발전소에서 탈석탄 정책으로 언제부터 고용 문제가 발생할 것으로 예상하십니까.") +
  scale_x_continuous(breaks = c(1:5), label = c("1년 이내", "2년 ~ 3년", "3년 ~ 5년", "5년 ~ 10년", "10년 이상")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q5_3) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q5_3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.5) +
  ggtitle("Q. 귀하가 속한 발전소에서 탈석탄 정책으로 언제부터 고용 문제가 발생할 것으로 예상하십니까.") +
  scale_x_continuous(breaks = c(1:5), label = c("1년 이내", "2년 ~ 3년", "3년 ~ 5년", "5년 ~ 10년", "10년 이상")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


### Q6_1
data$q6_1 <- as.factor(data$q6_1)
data$q6_1 <- ifelse(data$q6_1 == 1, "네", "아니오")

a <- data %>% 
  group_by(q6_1) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = q6_1)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Q. ‘정의로운 전환’의 개념을 들어본 적이 있다.") + 
  theme_void() +
  theme(plot.title = element_text(size = 13, hjust = 0.5))+
  guides(fill = guide_legend(title = "응답"))

data$q6_1_n2 <- as.factor(data$q6_1_n2)
data$q6_1_n2 <- ifelse(data$q6_1_n2 == 1, "네", "아니오")

b <- data %>% 
  group_by(q6_1_n2) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = paste(round(n/total*100,1), "%")) %>%
  ggplot(aes(x = "", y = n, fill = q6_1_n2)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Q. ‘정부가 ‘정의로운 전환’을 위해 \n 충분한 소통을 하고 있다고 생각한다.") + 
  theme_void() +
  theme(plot.title = element_text(size = 13, hjust = 0.5))+
  guides(fill = guide_legend(title = "응답"))

grid.arrange(a,b, nrow=1, ncol=2)


### Q6_2
data %>%
  ggplot(aes(x = q6_2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 탄소중립 기본법에 따라 정부는 사업전환 및 구조적 실업에 따른 피해를 최소화하기 위해 \n 재교육, 재취업 및 전직 등을 지원방안을 마련해야 합니다. \n 이러한 정부의 지원방안이 귀하의 고용불안 해소에 도움이 되리라 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q6_2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q6_2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 탄소중립 기본법에 따라 정부는 사업전환 및 구조적 실업에 따른 피해를 최소화하기 위해 \n 재교육, 재취업 및 전직 등을 지원방안을 마련해야 합니다. \n 이러한 정부의 지원방안이 귀하의 고용불안 해소에 도움이 되리라 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


### Q6_3
data1 <- data %>% 
  group_by(소속회사, q6_3) %>%
  summarise(total = n())
  
data2 <- data %>% 
  group_by(소속회사, q6_3_m2) %>%
  summarise(total = n()) %>%
  rename("q6_3" = "q6_3_m2")

data3 <- data %>% 
  group_by(소속회사, q6_3_m3) %>%
  summarise(total = n()) %>%
  rename("q6_3" = "q6_3_m3")

data6_3 <- rbind(data1, data2)
data6_3 <- rbind(data6_3, data3)


data6_3_sum <- aggregate(total ~ q6_3, data6_3, sum)

sum(data6_3_sum$total)

data6_3_sum <- data6_3_sum %>%
  mutate(pct = total / 5784)

data6_3_sum %>%
  ggplot(aes(x = q6_3, y = pct)) +
  geom_bar(stat = "identity", fill = "blue", colour = "black") + 
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.25) +
  ggtitle("Q. 정의로운 전환 및 구조조정 대비 정책 중 \n 가장 우선적이라고 생각하는 정책 세 개를 선택해 주세요.") +
  scale_x_continuous(breaks = c(1:9), labels = c("고용 유지", "발전소 정보 공유", "직업훈련 지원", "전환수당 지급", "실업급여 확대", "생활비 지원", "사회적 대화 기구", "처우 보장", "정규직 전환")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

data6_3 %>% 
  group_by(소속회사, q6_3) %>%
  summarise(total = sum(total))%>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total))%>%
  ggplot(aes(x = q6_3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정의로운 전환 및 구조조정 대비 정책 중 \n 가장 우선적이라고 생각하는 정책 세 개를 선택해 주세요.") +
  scale_x_continuous(breaks = c(1:9), labels = c("고용 유지", "발전소 정보 공유", "직업훈련 지원", "전환수당 지급", "실업급여 확대", "생활비 지원", "사회적 대화 기구", "처우 보장", "정규직 전환")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))



### 정부
data %>%
  ggplot(aes(x = q7_1)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 정부는 석탄화력발전소 폐쇄에 따른 노동자 고용불안 해소에 \n 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_1) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_1, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정부는 석탄화력발전소 폐쇄에 따른 노동자 고용불안 해소에 \n 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_4)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 정부의 민간기업 중심의 에너지 전환정책으로 향후 전기 민영화가 우려된다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_4) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_4, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정부의 민간기업 중심의 에너지 전환정책으로 향후 전기 민영화가 우려된다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_4_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 정부의 ‘2030 NDC’ 목표 상향에 따른 고용불안 대책 등 일자리 전환정책 마련이 부족하다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_4_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_4_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정부의 ‘2030 NDC’ 목표 상향에 따른 고용불안 대책 등 일자리 전환정책 마련이 부족하다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_4_n3)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 정부의 정책 결정에 있어 이해당사자인 노조의 참여가 보장되지 않는다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_4_n3) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_4_n3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 정부의 정책 결정에 있어 이해당사자인 노조의 참여가 보장되지 않는다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_7)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 새 정부가 추진할 신한울 3·4호기 건설 재개 등 \n 탈원전 백지화는 탄소중립 정책에 이바지할 것이다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_7) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_7, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 새 정부가 추진할 신한울 3·4호기 건설 재개 등 \n 탈원전 백지화는 탄소중립 정책에 이바지할 것이다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_7_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 새 정부하에서는 석탄화력발전소 폐쇄 시기를 조절할 것으로 예상한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_7_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_7_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 새 정부하에서는 석탄화력발전소 폐쇄 시기를 조절할 것으로 예상한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

### 회사
data %>%
  ggplot(aes(x = q7_2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 귀하가 소속된 회사는 석탄화력발전소 폐쇄에 따른 \n 노동자 고용불안 해소에 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 귀하가 소속된 회사는 석탄화력발전소 폐쇄에 따른 \n 노동자 고용불안 해소에 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_5)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 사측은 발전소 폐쇄에 대비해 노동자 보호를 위해 노력하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_5) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_5, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 사측은 발전소 폐쇄에 대비해 노동자 보호를 위해 노력하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_5_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 사측은 재생에너지 분야 등 새로운 일자리 창출을 위해 노력하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_5_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_5_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 사측은 재생에너지 분야 등 새로운 일자리 창출을 위해 노력하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))



data %>%
  ggplot(aes(x = q7_5_n3)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 향후 예상되는 발전소 폐쇄와 관련 노동조합과의 협의가 원만하게 진행되고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_5_n3) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_5_n3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 향후 예상되는 발전소 폐쇄와 관련 노동조합과의 협의가 원만하게 진행되고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))




### Q7_3
data %>%
  ggplot(aes(x = q7_3)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 귀하가 소속된 노동조합은 석탄화력발전소 폐쇄에 따른 \n 노동자 고용불안 해소에 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_3) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 귀하가 소속된 노동조합은 석탄화력발전소 폐쇄에 따른 \n 노동자 고용불안 해소에 어느 정도 노력하고 있다고 생각합니까?") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_6)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 노조는 발전소 폐쇄에 대비하여 노동자 보호를 위해 사측과 긴밀히 협의하고 있다") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_6) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_6, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 노조는 발전소 폐쇄에 대비하여 노동자 보호를 위해 사측과 긴밀히 협의하고 있다") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


data %>%
  ggplot(aes(x = q7_6_n2)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 노조는 발전소 폐쇄에 따른 대응계획에 관한 정보를 공유하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_6_n2) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_6_n2, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 노조는 발전소 폐쇄에 따른 대응계획에 관한 정보를 공유하고 있다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

data %>%
  ggplot(aes(x = q7_6_n3)) + 
  geom_bar(width = 0.5, fill = "blue", colour = "black", aes(y = ..prop.., group = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  ggtitle("Q. 발전소 폐쇄에 따른 대응을 향후 노조의 핵심 사업으로 배치할 필요성이 있다고 생각한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

data %>% 
  group_by(소속회사, q7_6_n3) %>%
  summarise(total = n()) %>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total)) %>%
  ggplot(aes(x = q7_6_n3, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 발전소 폐쇄에 따른 대응을 향후 노조의 핵심 사업으로 배치할 필요성이 있다고 생각한다.") +
  scale_x_continuous(breaks = c(1:5), label = c("전혀 그렇지 않다", "그렇지 않다", "그렇다", "매우 그렇다", "모르겠다")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))


### Q7_8
data1 <- data %>% 
  group_by(소속회사, q7_8) %>%
  summarise(total = n())

data2 <- data %>% 
  group_by(소속회사, q7_8_m2) %>%
  summarise(total = n()) %>%
  rename("q7_8" = "q7_8_m2")

data3 <- data %>% 
  group_by(소속회사, q7_8_m3) %>%
  summarise(total = n()) %>%
  rename("q7_8" = "q7_8_m3")

data7_8 <- rbind(data1, data2)
data7_8 <- rbind(data7_8, data3)


data7_8_sum <- aggregate(total ~ q7_8, data7_8, sum)

sum(data7_8_sum$total)

data7_8_sum <- data7_8_sum %>%
  mutate(pct = total / 5784)

data7_8_sum %>%
  ggplot(aes(x = q7_8, y = pct)) +
  geom_bar(stat = "identity", fill = "blue", colour = "black") + 
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.25) +
  ggtitle("Q. 기후 위기 대응정책과 관련하여 귀하가 속한 노동조합이 주력해야 할 정책 중 \n 가장 우선적이라고 생각하는 정책 세 개를 선택해 주세요.") +
  scale_x_continuous(breaks = c(1:8), labels = c("시민단체와의 연대", "전환배치 조건 마련", "고용보장협의 체결", "기후위기 정책 개발", "정부와 사회적 대화", "언론 홍보활동", "발전공기업 통합 추진", "노동조합 연대")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))

data7_8 %>% 
  group_by(소속회사, q7_8) %>%
  summarise(total = sum(total))%>%
  group_by(소속회사) %>%
  mutate(pct = total / sum(total))%>%
  ggplot(aes(x = q7_8, y = pct, color = 소속회사)) +
  geom_line() + geom_point() +
  geom_text(aes(y = pct, label = scales::percent(pct)), vjust = -0.3) +
  ggtitle("Q. 기후 위기 대응정책과 관련하여 귀하가 속한 노동조합이 주력해야 할 정책 중 \n 가장 우선적이라고 생각하는 정책 세 개를 선택해 주세요.") +
  scale_x_continuous(breaks = c(1:8), labels = c("시민단체와의 연대", "전환배치 조건 마련", "고용보장협의 체결", "기후위기 정책 개발", "정부와 사회적 대화", "언론 홍보활동", "발전공기업 통합 추진", "노동조합 연대")) +
  labs(x = "", y = " 비율") + 
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5 ,face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        legend.position = "bottom") +
  guides(color = guide_legend(label.position = "bottom"))

