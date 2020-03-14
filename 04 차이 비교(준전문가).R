#### 1. 데이터 불러오기 ####
data <- readxl::read_excel("data/sample.xlsx", sheet=3)



#### 2. 데이터 확인과 전처리 ####
summary(data)
data <- data[2:11]
colnames(data) <- c("성별", "나이", "재방문여부", "주차", "음식맛", "친절", 
                    "가격", "청결", "인테리어", "만족도")

#### 가. 결측치 처리 ####
colSums(is.na(data))
summary(data <- na.omit(data))
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

#### 나. 통계적인 비교 ####
#### _1) 독립적인 두 집단 비교 ####
t.test(만족도~성별, data)                        # 정규성을 만족할 때
wilcox.test(만족도~성별, data)                   # 비정규성일 때

shapiro.test(resid(lm(만족도~성별, data)))       # 정규성 검정 => 비정규성

shapiro.test(resid(lm(만족도~재방문여부, data))) # 정규성 검정 => 비정규성
wilcox.test(만족도~재방문여부, data)             # 두 집단 비교

#### _2) 독립적인 세집단 이상 비교 ####
shapiro.test(resid(lm(만족도~나이, data)))       # 정규성 검정 => 비정규성
oneway.test(만족도~나이, data)                   # 정규성을 만족할 때
kruskal.test(만족도~나이, data)                  # 비정규성, 분산 가정 없음

#### _3) 독립적인 세집단 이상 사후 분석 ####
# install.packages("PMCMRplus")
library(PMCMRplus)
gamesHowellTest(만족도~나이, data)               # 정규성을 만족할 때

library(FSA)
dunnTest(만족도~나이, data)                      # kruskal.test 사후검정



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

# 재방문 여부에 의한 만족도는 성별에 따라 다를까?
ggbarplot(data, x="재방문여부", y="만족도", add=c("mean_ci"), fill="성별",
          add.params=list(group="성별"), position=position_dodge(0.8), legend="right",
          label=T, lab.vjust=-2, label.pos="out", lab.col="black", lab.nb.digits=2)+
  stat_compare_means(method = "wilcox", 
                     aes(group=성별, label=paste0("p=",round(..p.adj.., 3))),label.y=5.5)+ 
  scale_fill_brewer(palette="Paired")

# 성별에 의한 만족도는 재방문 여부에 따라 다를까?
ggbarplot(data, x="성별", y="만족도", fill="재방문여부", add=c("mean_ci"),
          add.params=list(group="재방문여부"), position=position_dodge(0.8), legend="right",
          label=T, lab.vjust=3, label.pos="out", lab.col="white", lab.nb.digits=2)+
  stat_compare_means(method = "wilcox", 
                     aes(group=재방문여부, label=paste0("p=",round(..p.adj.., 3))),label.y=5)+ 
  scale_fill_brewer(palette="Paired")
