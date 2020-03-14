#### 1. 데이터 불러오기 ####
data <- read.csv("data/sample.csv") 


### 2. 데이터 확인하기 ####
summary(data)


#### 3. 구체적으로 비교하기 ####
# install.packages("FSA")
library(FSA)
Summarize(만족도~성별, data)
Summarize(만족도~나이, data)
Summarize(만족도~재방문여부, data)
Summarize(만족도~성별+재방문여부, data)
Summarize(만족도~재방문여부+성별, data)


#### 3. 그래프로 표현하기 ####
# install.packages("ggpubr")
library(ggpubr)
ggbarplot(data, x="성별", y="만족도", add=c("mean"))
ggbarplot(data, x="성별", y="만족도", add=c("mean_sd"))
ggbarplot(data, x="성별", y="만족도", add=c("mean_ci"))
ggbarplot(data, x="성별", y="만족도", add=c("mean_ci"), fill="성별")
ggbarplot(data, x="성별", y="만족도", add=c("mean_ci"), fill="성별", legend="right")

ggbarplot(data, x="나이", y="만족도", add=c("mean_ci"), fill="나이", legend="right")

ggbarplot(data, x="재방문여부", y="만족도", add=c("mean_ci"), fill="재방문여부", legend="right")
