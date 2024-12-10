library(tidyverse)
library(gt)
library(webshot2)
library(gtsummary)

data <- read.csv(file = 'demographic_data_AD_DiagnosisName.csv')

data <- data %>%
  rename(`Number of Diagnosis`="X.diagnosis",
         `Death Status` = "Death_Status")

data <- data %>%
  mutate(Cluster = case_when(
    Cluster == 0 ~ "Cluster0",
    Cluster == 1 ~ "Cluster1",
    Cluster == 2 ~ "Cluster2",
    Cluster == 3 ~ "Cluster3",
    Cluster == 4 ~ "Cluster4",
    TRUE ~ as.character(Cluster)
  ))

data <- data %>%
  mutate(isAD = case_when(
    isAD == 1 ~ "AD",
    isAD == 0 ~ "Control",
    TRUE ~ as.character(isAD)
  ))

data <- data %>%
  mutate(Race = case_when(
    Race == "Other" ~ "Other/Unknown",
    Race == "0" ~ "Other/Unknown",
    Race == "Unknown/Declined" ~ "Other/Unknown",
    Race == "Unknown" ~ "Other/Unknown",
    Race == "Declined" ~ "Other/Unknown",
    Race == "American Indian or Alaska Native" ~ "Other/Unknown",
    TRUE ~ as.character(Race)
  ))%>%
  mutate(Sex = case_when(
    Sex == "*Unspecified"~"Unknown",
    TRUE ~ as.character(Sex)
  ))


demographics <- data %>%
  select("Sex","Race",
         "Death Status","Cluster")%>%
  tbl_summary(by=  "Cluster",
              sort = list(everything() ~ "frequency"))%>%
  add_overall()

demographics

continuous <- data %>%
  select( "Age", "Number of Diagnosis","Cluster")%>%
  tbl_summary(by= "Cluster",
              missing = "no",
              digits = list(everything() ~ c(1, 1)),
              statistic = list(all_continuous() ~ "{median} ({sd}) / ({p25}-{p75})"))%>%
  add_overall()

continuous

# Stratified by Cluster
demographics <- data %>%
  select("Sex","Race",
         "Death Status","Cluster")%>%
  tbl_summary(by= "Cluster",
              sort = list(everything() ~ "frequency"))%>%
  add_overall()%>%
  modify_footnote(everything() ~ NA)

demographics

continuous <- data %>%
  select( "Age", "Number of Diagnosis","Cluster")%>%
  tbl_summary(by= "Cluster",
              missing = "no",
              digits = list(everything() ~ c(1, 1)),
              statistic = list(all_continuous() ~ "{median} ({p25}-{p75})"))%>%
  add_overall()%>%
  modify_footnote(everything() ~ NA)

continuous

# demographicsテーブルとcontinuousテーブルをマージ
merged_table <- tbl_merge(
  tbls = list(demographics, continuous),  # 2つのテーブルをリストとして渡す
  tab_spanner = c("**Demographics**", "**Continuous Variables**")  # 各テーブルにラベルをつける
)

# 結合したテーブルを表示
merged_table

# demographicsテーブルとcontinuousテーブルを縦にスタック
stacked_table <- tbl_stack(
  tbls = list(demographics, continuous)
)

# 結合したテーブルを表示
stacked_table



# Stratified by Cluster and isAD
# Sexとクラスターの集計をカスタム順序で並べ替える
data <- data %>%
  select("Sex","Race","Age","Number of Diagnosis",
         "Death Status","Cluster","isAD") %>%
  mutate(Group = case_when(
    Cluster == 'Cluster0' & isAD == "AD" ~ "Cluster0.AD",
    Cluster == 'Cluster0' & isAD == "Control" ~ "Cluster0.Control",
    Cluster == 'Cluster1' & isAD == "AD" ~ "Cluster1.AD",
    Cluster == 'Cluster1' & isAD == "Control" ~ "Cluster1.Control",
    Cluster == 'Cluster2' & isAD == "AD" ~ "Cluster2.AD",
    Cluster == 'Cluster2' & isAD == "Control" ~ "Cluster2.Control",
    Cluster == 'Cluster3' & isAD == "AD" ~ "Cluster3.AD",
    Cluster == 'Cluster3' & isAD == "Control" ~ "Cluster3.Control",
    Cluster == 'Cluster4' & isAD == "AD" ~ "Cluster4.AD",
    Cluster == 'Cluster4' & isAD == "Control" ~ "Cluster4.Control"
  ))


demographics <- data %>%
  select("Sex","Race",
         "Death Status","Group")%>%
  tbl_summary(by=  "Group",
              sort = list(everything() ~ "frequency"))%>%
  add_overall()

demographics


continuous <- data %>%
  select( "Age", "Number of Diagnosis","Group")%>%
  tbl_summary(by= "Group",
              missing = "no",
              digits = list(everything() ~ c(1, 1)),
              statistic = list(all_continuous() ~ "{median} \n\ ({sd}) / ({p25}-{p75})"))%>%
  add_overall()

continuous



# tbl_summary()による集計
continuous <- data %>%
  select("Age", "Number of Diagnosis", "Group") %>%
  tbl_summary(by = "Group",
              missing = "no",
              digits = list(everything() ~ c(1, 1)),
              statistic = list(
                all_continuous() ~ "{median} \n({p25}-{p75})"  # 改行を挿入
              )) %>%
  add_overall()

# as_gt()を使ってHTML用に変換
html_table <- continuous %>%
  as_gt()  # gtオブジェクトに変換

# gtsave()を使ってHTMLファイルとして保存
gtsave(html_table, "continuous_summary.html")


# Statistical test for Age
boxplot(Age ~ Cluster, data=data, main='Age distribution by cluster', xlab="Cluster", ylab = "Age")
# Kruskal-wallis test due to the violation of the normal distribution
kruskal.test(Age ~ Cluster, data=data)

# Check which pair has the significant difference
library(dunn.test)
dunn.test(data$Age, data$Cluster, method = "bonferroni")
?dunn.test

# Statistical test for Number of comorbidites
boxplot(`Number of Diagnosis` ~ Cluster, data=data, main='Number of Diagnosis distribution by cluster', xlab="Cluster", ylab = "Number of diagnosis")

# Kruskal-wallis test due to the violation of the normal distribution
kruskal.test(`Number of Diagnosis` ~ Cluster, data=data)

# Check which pair has the significant difference
library(dunn.test)
dunn.test(data$`Number of Diagnosis`, data$Cluster, method = "bonferroni")


# 性別とクラスターのクロス集計表を作成
table_gender_cluster <- table(data$Cluster, data$Sex)
table_gender_cluster <- table_gender_cluster[,1:2]
# カイ二乗検定の実行
chisq.test(table_gender_cluster)
table_gender_cluster
# Death Statusとクラスターのクロス集計表を作成
table_death_cluster <- table(data$Cluster, data$`Death Status`)
table_death_cluster <- table_death_cluster[,1:2]
# カイ二乗検定の実行
chisq.test(table_death_cluster)
table_death_cluster

# Death Statusとクラスターのクロス集計表を作成
table_race_cluster <- table(data$Cluster, data$Race)
table_race_cluster <- table_race_cluster[,1:5]
# カイ二乗検定の実行
chisq.test(table_race_cluster)
table_race_cluster
unique(data$Race)

data1 = data[data$Sex != 'Unknown',]
# クラスターごとの性別のクロス集計表を作成
table_gender_cluster <- table(data1$Cluster, data1$Sex)

# クラスターごとの性別の割合をペアワイズで比較
pairwise_test_result <- pairwise.prop.test(table_gender_cluster, p.adjust.method = "bonferroni")

pairwise_test_result
