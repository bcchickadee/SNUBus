# ===== Importing Dependencies =====

library(tidyverse); library(magrittr); library(showtext); library(ggrepel); library(modelr); library(gt)
font_add_google("Nanum Gothic", "nanum")
showtext_auto()

# ===== Importing & Tidying Data =====

bus_col_spec <- cols(사용일자 = "c",
                     노선번호 = "c",
                     노선명 = "c",
                     버스정류장ARS번호 = "c",
                     표준버스정류장ID = "_",
                     역명 = "c",
                     승차총승객수 = "i",
                     하차총승객수 = "i",
                     등록일자 = "c")

bus <- rbind(
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202201.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202202.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202203.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202204.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202205.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202206.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202207.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202208.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202209.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202210.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202211.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202212.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202301.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202302.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202303.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202304.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202305.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202306.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202307.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202308.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202309.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202310.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202311.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr")),
  read_csv("raw_data/BUS_STATION_BOARDING_MONTH_202312.csv", col_types = bus_col_spec, locale = locale(encoding = "euc-kr"))
)

bus %<>% mutate(사용일자 = ymd(사용일자),
                등록일자 = ymd(등록일자))

bus %<>% filter((
  (
    (사용일자 >= ymd(20220901)) & (사용일자 <= ymd(20221215))
    ) |
  (
    (사용일자 >= ymd(20230302)) & (사용일자 <= ymd(20230615))
  ) |
  (
    (사용일자 >= ymd(20230901) & (사용일자 <= ymd(20231215)))
  )
  )
  )

bus %<>%
  mutate(weekday = wday(사용일자, label = T)) %>% 
  filter(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

write_csv(bus, file = "raw_data/bus_organized.csv")
bus <- read_csv("raw_data/bus_organized.csv")

# ===== Ranking bus lines =====

buses_query <- c("5511", "5513", "5516", "관악02",
                 "5515", "5528", "6515")

# 1st observation: Rank of SNU buses (during weekdays, in semester) - average
bus_rank_sum<- bus %>% 
  group_by(노선번호) %>% 
  summarize(pass = sum(승차총승객수)) %>% 
  arrange(desc(pass))

bus_rank_sum$rank <- 1:nrow(bus_rank_sum)

bus_rank_sum %>% 
  head(10) %>% 
  gt()

bus_rank_sum %>%
  filter(노선번호 %in% buses_query) %>% 
  gt()

# 5511 101st / 5513 245th / 5516 154th / 관악02 190th out of total 654


# Divide by number of stops? - average stop in average day

bus_rank_rate <- bus %>% 
  group_by(노선번호) %>% 
  summarize(pass = mean(승차총승객수)) %>% 
  arrange(desc(pass))

bus_rank_rate$rank <- 1:nrow(bus_rank_rate)

bus_rank_rate %>% 
  head(10) %>%
  gt()

bus_rank_rate %>% 
  filter(노선번호 %in% buses_query) %>% 
  gt()

# Rate: 5511 121th / 5513 132th / 5516 182nd / 관악02 35th / 5515 10th!

# by total passengers by stop?
bus_rank_stop <- bus %>% 
  group_by(노선번호, 역명) %>% 
  summarize(pass = mean(승차총승객수)) %>% 
  arrange(desc(pass))

bus_rank_stop$rank <- 1:nrow(bus_rank_stop)

bus_rank_stop %>% 
  head(10) %>% 
  gt(groupname_col = F)

bus_rank_stop %>% 
  filter(노선번호 %in% buses_query) %>% 
  head(10) %>% 
  gt(groupname_col = F)

# 5515 서울대입구역 9th / 관악02 낙성대역 15th / 5511 서울대입구역 41st / 5513 서울대입구역 68th


# ===== Examining Buses inside SNU =====

# Station information: Filtering stops inside SNU

stop_snu <- read_csv("raw_data/stops_filtered.csv", locale = locale(encoding = "euc-kr"))
stop_snu_wdorm <- read_csv("raw_data/stops_filtered_w_dorm.csv", locale = locale(encoding = "euc-kr"))
college_mapping <- read_csv("raw_data/stops_filtered_w_colleges.csv", locale = locale(encoding = "euc-kr"))

# Filtering lines & stops

bus_snu <- bus %>% 
  filter(노선번호 %in% c("5511", "5513", "5516", "관악02"),
         버스정류장ARS번호 %in% stop_snu_wdorm$버스정류장ARS번호)

# Finding out busiest stops inside SNU

# By lines
bus_snu %>% 
  group_by(버스정류장ARS번호, 역명, 노선번호) %>% 
  summarize(pass = mean(하차총승객수)) %>% 
  arrange(desc(pass)) %>% 
  ungroup() %>% 
  select(노선번호, 역명, pass) %>% 
  head(10) %>% 
  gt(groupname_col = F)

# Combined lines (but not for 관악02)
bus_snu %>% 
  group_by(버스정류장ARS번호, 역명, 노선번호) %>% 
  summarize(pass = mean(하차총승객수)) %>% 
  group_by(버스정류장ARS번호) %>% 
  summarize(pass = sum(pass)) %>% 
  arrange(desc(pass)) %>% 
  mutate(버스정류장ARS번호 = as.numeric(버스정류장ARS번호)) %>% 
  left_join(stop_snu_wdorm, by = "버스정류장ARS번호") %>% 
  select(역명, pass) %>% 
  head(10) %>% 
  gt(groupname_col = F)

# Busiest stops: 제2공학관, 기숙사삼거리, 노천강당, etc.

# ====== Bus lines by weekday: viewing 금공강 trend ======

# Average passenger deboarding by line and weekday

bus_snu_line_weekday <- bus_snu %>% 
  filter(버스정류장ARS번호 %in% stop_snu$버스정류장ARS번호) %>% 
  group_by(노선번호, weekday) %>% 
  summarize(pass = mean(하차총승객수)) %>% 
  arrange(desc(pass))
  
bus_snu_line_weekday %>% 
  ggplot(mapping = aes(x = weekday, y = pass, group = 노선번호, color = 노선번호)) +
  geom_line() +
  labs(title = "노선별 요일별 정류장 평균 하차인원 수",
       x = "요일", y = "일일 평균 하차인원 수") +
  theme_gray(base_family = "nanum")

# Examine weekday by bus line

bus_snu_line_weekday_relat <- bus_snu_line_weekday %>% 
  group_by(노선번호) %>% 
  mutate(avg = mean(pass),
         relat = pass/avg)

bus_snu_line_weekday_relat %>%  
  ggplot(mapping = aes(x = weekday, y = relat, group = 노선번호, color = 노선번호)) +
  geom_line() +
  geom_ref_line(h = 1, size = 1, colour = "black") +
  geom_text_repel(data = filter(bus_snu_line_weekday_relat, weekday == "Fri"),
                  label = paste(
                    filter(bus_snu_line_weekday_relat, weekday == "Fri")$노선번호,
                    ": ",
                    round(filter(bus_snu_line_weekday_relat, weekday == "Fri")$relat, 3)
                  ),
                  size = 3, color = "black", family = "nanum"
  ) +
  labs(title = "노선별 요일별 상대적 이용 비율",
       x = "요일", y = "하차 비율(평균 = 1)") +
  theme_gray(base_family = "nanum")

#  === Grouping by college ===

# Making bus stops by college
# Making list of colleges, each colleges contain set of bus ARS number
colleges <- c(
  college_mapping$단과대1,
  college_mapping$단과대2,
  college_mapping$단과대3,
  college_mapping$단과대4
) %>% 
  unique()

colleges <- colleges[!is.na(colleges)]

college_stop = list()

for (i in colleges) {
  college_stop[[i]] <- vector()
}

for (j in 1:nrow(college_mapping)) {
  college_temp <- as.vector(college_mapping[j, 3:6], mode = "character")
  college_temp <- college_temp[which(!is.na(college_temp))]
  for (x in college_temp) {
    l <- length(college_stop[[x]])
    college_stop[[x]][l+1] <- as.numeric(college_mapping[j, 1])
  }
}

bus_college <- tibble(
  college = rep(names(college_stop), each = 5),
  weekday = rep(
    wday(ymd(20240401:20240405), label = T),
      length(names(college_stop))
      ),
  pass = 0
)

# Function to search college based on bus stop ARS number
findcollege <- function(ars) {
  ans <- c()
  for (i in seq_along(college_stop)) {
    if (ars %in% college_stop[[i]]) {
      ans[length(ans)+1] <- names(college_stop[i])
    }
  }
  return(ans)
}

# Temporary df to organize data
bus_college_temp <- bus_snu %>% 
  filter(버스정류장ARS번호 %in% stop_snu$버스정류장ARS번호) %>% 
  group_by(버스정류장ARS번호, weekday) %>% 
  summarize(pass = mean(승차총승객수))

for (i in 1:nrow(bus_college_temp)) {
  stop_college <- findcollege(bus_college_temp[[i,1]])
  index <- which(
    ((bus_college$college %in% stop_college) & (bus_college$weekday == bus_college_temp[[i,2]]))
      )
  bus_college$pass[index] <- bus_college$pass[index] + bus_college_temp[[i,3]]
}

# bus_college: average number of people getting off at college, by day of week

bus_college %>% 
  ggplot(mapping = aes(x = weekday, y = pass, group = college, color = college)) +
  geom_line() +
  geom_text_repel(data = filter(bus_college, weekday == "Wed"),
                  label = paste(filter(bus_college, weekday == "Fri")$college),
                  size = 3, color = "black", family = "nanum"
  ) +
  labs(title = "요일별 단과대별 버스 하차 평균 인원",
       x = "요일", y = "일일 평균 하차 인원", color = "단과대") +
  theme_gray(base_family = "nanum")

# Discerning differences between colleges

bus_college_relat <- bus_college %>% 
  group_by(college) %>% 
  mutate(avg = mean(pass)) %>% 
  mutate(relat = pass / avg)

ggplot(data = bus_college_relat,
       mapping = aes(x = weekday, y = relat, group = college, color = college)) +
  geom_line() + 
  geom_ref_line(h = 1, size = 1, colour = "black") +
  geom_text_repel(data = filter(bus_college_relat, weekday == "Fri"),
                  label = paste(
                    filter(bus_college_relat, weekday == "Fri")$college,
                    ": ",
                    round(filter(bus_college_relat, weekday == "Fri")$relat, 3)
                    ),
                  size = 3, color = "black", family = "nanum"
                  ) +
  labs(title = "요일별 단과대별 버스 하차 인원의 상대적 정도",
       x = "요일", y = "일일 상대적 인원 (단과대별 평균 = 1)", color = "단과대") +
  theme_gray(base_family = "nanum")

# Clear 금공강 trend
# Differences between colleges: CSS(0.945) - CNS(0.866): may be attributed to class schedules