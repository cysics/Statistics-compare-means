#### 1. 데이터 불러오기 ####
library(readxl)
data <- read_excel("data/sample.xlsx", sheet=2)



#### 2. 데이터 확인과 전처리 ####
summary(data)
str(data)

#### 가. 결측치 처리 ####
colSums(is.na(data))
str(data <- na.omit(data))

#### 나. 범주형 처리 ####
data$성별 <- as.factor(data$성별)
str(data)
data$나이 <- as.factor(data$나이)
data$재방문여부 <- as.factor(data$재방문여부)

data$성별 <- gsub("0", "남자", data$성별)
data$성별 <- gsub("1", "여자", data$성별)

data$나이 <- gsub("1", "10대", data$나이)
data$나이 <- gsub("2", "20대", data$나이)
data$나이 <- gsub("3", "30대", data$나이)
data$나이 <- gsub("4", "40대", data$나이)
data$나이 <- gsub("5", "50대", data$나이)
data$나이 <- gsub("6", "60대이상", data$나이)

data$재방문여부 <- gsub("1", "처음", data$재방문여부)
data$재방문여부 <- gsub("2", "재방문", data$재방문여부)



#### 3. 구체적으로 비교하기 ####
#### 가. 기술통계 ####
library(FSA)
Summarize(만족도~성별, data)
Summarize(만족도~나이, data)
Summarize(만족도~재방문여부, data)
Summarize(만족도~성별+재방문여부, data)
Summarize(만족도~재방문여부+성별, data)

#### 나. 통계적인 비교 ####
wilcox.test(만족도~성별, data)        # 두 집단 비교

kruskal.test(만족도~나이, data)       # 세 집단 이상 비교
dunnTest(만족도~나이, data)           # 세 집단 이상 사후 비교

wilcox.test(만족도~재방문여부, data)  # 두 집단 비교



#### 3. 그래프로 표현하기 ####
library(ggpubr)
ggbarplot(data, x="성별", y="만족도", add=c("mean_ci"), fill="성별", legend="right")+
  stat_compare_means(method="wilcox", label.x=1.2)

ggbarplot(data, x="나이", y="만족도", add=c("mean_ci"), fill="나이", legend="right")+
  stat_compare_means(method="kruskal", label.x=2.5, label.y=5.3)

ggbarplot(data, x="재방문여부", y="만족도", add=c("mean_ci"), fill="재방문여부", 
          legend="right")+stat_compare_means(method = "wilcox", label.x=1.2)
