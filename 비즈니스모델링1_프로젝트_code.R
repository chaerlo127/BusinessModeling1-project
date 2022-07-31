install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 한글이 깨지므로 reencode를 사용해준다.
# 가구용 조사설계서
data_file <- read.spss(file = "Koweps_h16_2021_beta1.sav", reencode='utf-8',to.data.frame = T)
View(data_file)

# 여성과 남성의 교육 수준(대입 등의 입결)----
# 데이터 이름 변경
data_file <- rename(data_file,
                  sex = h1601_4, #성별
                  birth = h1601_5, # 태어난 년도
                  graduated = h1601_6) # 교육 수준

# 가구원 1 성별 결측치, 이상치 처리-> 결측치 이상치 없음
table(data_file$sex)
data_file$sex <- ifelse(data_file$sex == 9, NA, data_file$sex)
table(is.na(data_file$sex))

# 가구원 1  성별 가시적으로 쉽게 표현 (male = 1, female = 2)
data_file$sex <- ifelse(data_file$sex == 1, "male", "female")

# 가구원 1  태어난 년도 결측치, 이상치 처리-> 결측치 이상치 없음
table(data_file$birth)
data_file$birth <- ifelse(data_file$birth == 9999, NA, data_file$birth)
table(is.na(data_file$birth))

# 가구원 1 나이 변수 추가
data_file$age <- 2021 - data_file$birth + 1
summary(data_file$age)
qplot(welfare$age)

# 가구원 1  교육 수준 확인 및 결측치 이상치 처리-> 결측치 이상치 없음
table(data_file$graduated)
data_file$graduated <- ifelse(data_file$graduated == 99, NA, data_file$graduated)
table(is.na(data_file$graduated))
levels(data_file$graduated) # null
# categorical 로 변경
data_file$graduated <- as.factor(data_file$graduated)

# 가구원 1 연령 층 변수 추가
data_file <- data_file %>% 
  mutate(ageg = ifelse(age<30, "young", ifelse(age<=59, "middle", "old")))
table(data_file$ageg)
count(data_file$sex)

# [노년층 제외] 남성과 여성의 명수 차이
 data_file %>% 
    filter(ageg != "old") %>% 
    select(sex) %>% 
    group_by(sex) %>% 
    summarise(n = n())
 
# 가구원 1 데이터 분석
# 성별에 따른 교육 수준 차이
sex_graduated<- data_file %>% 
  filter(ageg != "old") %>% 
  select(sex, graduated) %>% 
  group_by(sex, graduated) %>% 
  summarise(n = n()) %>% 
  mutate(totalSex = sum(n)) %>% 
  mutate(pct = round(n/totalSex*100, 1))

# 성별에 따른 교육 수준 그래프 도출
ggplot(sex_graduated, aes(x = graduated, y = pct, fill = sex)) + geom_col(position = "dodge") + 
  xlab("성별 교육 수준") + ylab("퍼센트")



# 나이에 따른 근로 능력 정도----
data_file <- rename(data_file,
                    ability_to_work = h1603_2)  # 근로 능력
table(data_file$ability_to_work)

# 근로능력, 결측치, 이상치 처리 -> 결측치 이상치 없음
data_file$ability_to_work <- ifelse(data_file$ability_to_work == 9, NA, data_file$ability_to_work)
table(is.na(data_file$ability_to_work))

# 나이에따른 근로 능력 정도 결과 도출
ability_data <- data_file %>% 
  select(ageg, ability_to_work) %>% 
  group_by(ageg, ability_to_work) %>% 
  summarise(n = n()) %>% 
  mutate(totalability = sum(n)) %>% 
  mutate(abilitypct = round(n/totalability*100, 1))

# 나이에따른 근로 능력 정도 그래프 도출
ggplot(ability_data, aes(x = ability_to_work, y = abilitypct, fill = ageg)) + geom_col(position = "dodge")


# 4번(근로 능력 정도)에 분포한 노년층 나이
data_file %>% 
  filter(ageg == "old" & ability_to_work == 4) %>% 
  select(ageg, ability_to_work, age) %>% 
  group_by(ageg, ability_to_work) %>% 
  summarise(mean_age = mean(age))

# 직업에 따른 이혼율 ----
# 새로운 데이터 파일의 3번째 시트에 있는 데이터 저장
list_job = read_excel("(2021년 16차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx", col_names = T, sheet = 4)
View(list_job)
# 4 번째 열이 ..4 로 저장되어 있어 code_job으로 변경 완료
list_job <- rename(list_job,
                   code_job = 소분류,
                   job_name = ...4)  # 직종 

# 필요한 데이터만 남기고 버리기(분석에 사용할 데이터는 code_job, job_name 두개임)
list_job <- list_job %>% 
  select(code_job, job_name)

data_file <- rename(data_file,
                    code_job = h1603_8) 
# 서로의 데이터 type이 다르므로 data_file의 타입으로 변경해준다.
class(data_file$code_job) # numeric
class(list_job$code_job) # character
list_job$code_job <- as.numeric(list_job$code_job)

data_file <- left_join(data_file, list_job, id = "code_job") # code job 기점으로 join을 해준다. 

# marriage 변수 이름 변경
data_file <- rename(data_file,
                    marriage = h1601_11)

# 필요한 결혼 상태 유무 제외하고 모두 NA로 처리
data_file$group_marriage <- ifelse(data_file$marriage == 1, "marriage", ifelse(data_file$marriage == 3, "divorce", NA))

# 직업에 따른 결혼, 이혼율
job_marriage <- data_file %>% 
  filter(!is.na(group_marriage) & !is.na(job_name)) %>% 
  group_by(group_marriage, job_name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

# 직업에 따른 이혼률 top 5
job_divorce_top5 <- job_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  arrange(desc(group_marriage)) %>% 
  head(5)

# 직업에 따른 이혼율 그래프 도출
ggplot(job_divorce_top5, aes(x = reorder(job_name, -pct), y = pct)) + geom_col() 


# 건강검진 횟수에 따른 건강 상태 및 만성 질환 6개월 이상 투병중인 병명 중 높은 순위 top 5----
# 변수 이름 변경
data_file <- rename(data_file, 
                    health_state = h1602_2,
                    disease = h1602_aq1,
                    disease_name = h1602_9,
                    health_check = h1602_8)

# 건강 상태 결측치 이상치 처리 -> 결측치 이상치 없음
data_file$health_state <- ifelse(data_file$health_state == 9, NA, data_file$health_state)
table(data_file$health_state )
table(is.na(data_file$health_state))

# 만성 질환 해당 결측치 이상치처리 -> 결측치 이상치 없음
data_file$disease <- ifelse(data_file$disease == 9, NA, data_file$disease )
table(data_file$disease)

# 병명 결측치 이상치 처리 -> 결측치 이상치 없음
data_file$disease_name <- ifelse(data_file$disease_name == 99, NA, data_file$disease_name)
table(is.na(data_file$disease_name))
table(data_file$disease_name)

# 건강검진 횟수 결측치 아싱치 처리 -> 결측치 없음
data_file$health_check <- ifelse(data_file$health_check == 99, NA, data_file$health_check)
table(data_file$health_check)
table(is.na(data_file$health_check))

# 일년간 건강검진 횟수가 20인 경우가 있음. 이상치로 확인되어 결측치 처리를 하고, 측정에서 배제하여 분석하고자 함.
data_file$health_check <- ifelse(data_file$health_check == 20, NA, data_file$health_check)
table(is.na(data_file$health_check))
# 건강검진 횟수에 따른 건강 상태 데이터 도출
health_care <- data_file %>% 
  filter(!is.na(health_check)) %>% 
  select(health_state, health_check) %>% 
  group_by(health_check, health_state) %>% 
  summarise(n = n()) 
# 건강검진 횟수에 따른 건강 상태 그래프 도출
ggplot(health_care, aes(x = health_state, y = n, fill = health_check)) + geom_col(position = "dodge")


# 만성 질환 6개월 이상 투병 중인 병명 중 높은 순위 top 5
# 병명은 numeric이 아닌 categorical variable로 타입 변경을 해준다.
data_file$disease_name <- as.factor(data_file$disease_name)
disease_6month<- data_file %>% 
  filter(disease == 3) %>% 
  group_by(disease_name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(5)
ggplot(disease_6month, aes(x = reorder(disease_name, -n), y = n)) + geom_col()

# 노년층에서 만성질환을 6개월 이상 투병 중인 병명 ㅈ우 높은 순위 top 5
disease_6month_old<- data_file %>% 
  filter(disease == 3 & ageg == "old") %>% 
  group_by(disease_name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(5)
ggplot(disease_6month_old, aes(x = reorder(disease_name, -n), y = n)) + geom_col()
