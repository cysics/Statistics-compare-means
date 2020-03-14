#### 1. 데이터 불러오기 ####
data <- readxl::read_excel("data/sample.xlsx", sheet=3)



#### 2. 데이터 확인과 전처리 ####
summary(data)
data <- data[2:11]
colnames(data) <- c("성별", "나이", "재방문여부", "주차", "음식맛", "친절", 
                    "가격", "청결", "인테리어", "만족도")

#### 가. 결측치 처리 ####
colSums(is.na(data))
summary(data <- data[!is.na(data$재방문여부),])

#### 나. 범주형 처리 ####
data$성별 <- as.factor(data$성별)                                    # 하나의 칼럼만
data[, c(1:3)] <- lapply(data[,c(1:3)], function(x) as.factor(x))    # 여러 칼럼을 동시에 

data <- data.frame(lapply(data, function(x) gsub("매우 그렇다.", 5, x)))
data <- data.frame(lapply(data, function(x) gsub("그렇다.", 4, x)))
data <- data.frame(lapply(data, function(x) gsub("보통이다.", 3, x)))
data <- data.frame(lapply(data, function(x) gsub("전혀 그렇지 않다.", 1, x)))
data <- data.frame(lapply(data, function(x) gsub("그렇지 않다.", 2, x)))

data[, c(4:10)] <- lapply(data[,c(4:10)], function(x) as.numeric(as.character(x)))



#### 3. 구체적으로 비교하기 ####
#### 가. 기술통계 ####
library(FSA)
Summarize(만족도~성별, data)
Summarize(만족도~나이, data)
Summarize(만족도~재방문여부, data)
Summarize(만족도~성별+재방문여부, data)
Summarize(만족도~재방문여부+성별, data)

print(result <- Summarize(만족도~나이+재방문여부, data))
write.csv(result, "result.csv")
write.csv(Summarize(만족도~나이, data), "result.csv")

#### 나. 통계적인 비교 ####
wilcox.test(만족도~성별, data)        # 두 집단 비교

kruskal.test(만족도~나이, data)       # 세 집단 이상 비교
dunnTest(만족도~나이, data)           # 세 집단 이상 사후 비교

wilcox.test(만족도~재방문여부, data)  # 두 집단 비교



#### 3. 그래프로 표현하기 ####
library(ggpubr)
ggbarplot(data, x="성별", y="만족도", add=c("mean_ci"), fill="성별", legend="right",
          label=T, lab.nb.digits=2, lab.hjust=1.5)+
  stat_compare_means(method = "wilcox", label.x=1.2, label.y=5.3)

ggbarplot(data, x="나이", y="만족도", add=c("mean_ci"), fill="나이", legend="right",
          label=T, lab.nb.digits=2, lab.hjust=1.2)+
  stat_compare_means(method = "kruskal", label.x=2.5, label.y=5.5)

data$나이 <- factor(data$나이, 
                  levels=c("60대이상", "50대", "40대", "30대", "20대", "10대"))

ggbarplot(data, x="나이", y="만족도", add=c("mean_ci"), fill="나이", legend="right",
          label=T, lab.nb.digits=2, lab.hjust=1.2, sort.val="desc", sort.by.groups=F)+
  scale_x_discrete(guide=guide_axis(n.dodge=2))

ggbarplot(data, x="재방문여부", y="만족도", add=c("mean_ci"), fill="재방문여부", 
          legend="right", label=T, lab.nb.digits=2, lab.hjust=1.5)+
  stat_compare_means(method = "wilcox", label.x=1.2)

