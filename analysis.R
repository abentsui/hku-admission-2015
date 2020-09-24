#Library
library(tidyverse)
library(pdftools)
library(ggplot2)
library(tidyr)

#Set directory to this project directory

#Step 1: Read the table from HKU report
path1 <- "data/HKU_2015_2016_UgReport.pdf"
txt1 <- pdf_text(path1)

txt1 <- txt1[[28]]
tab1 <- str_split(txt1, "\r\n")
tab1 <- tab1[[1]]

#As the header names spread over two rows, I need to manually input the entries
header_names1 <- c("HKD", "Arch", "Arts", "Bus & Econ", "Dent", "Ed", "Eng", "Law", "LKS Med", "Sci", "Soc Sci", "hku_percent")

hku_local_stu <- 3236

uni_house_income <- tab1[28:46] %>%
  str_trim() %>%
  str_replace_all("(\\d)\\s(\\d)", "\\1  \\2") %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  as.data.frame(stringAsFactor = FALSE) %>%
  setNames(header_names1) %>%
  mutate_at(-1, parse_number) %>%
  mutate(hku_num = round(hku_local_stu * hku_percent / 100)) %>%
  select(HKD, hku_percent, hku_num)


#Combine row 2 and row 3 to for < 4,000
uni_house_income[3,2] <- uni_house_income[3,2] + uni_house_income[2,2]
uni_house_income[3,3] <- uni_house_income[3,3] + uni_house_income[2,3]

#Combine row 14 and row 15 to for 60,000 - 79,999
uni_house_income[15,2] <- uni_house_income[15,2] + uni_house_income[14,2]
uni_house_income[15,3] <- uni_house_income[15,3] + uni_house_income[14,3]

#Combine row 16 and row 17 to for 80,000 - 99,999
uni_house_income[17,2] <- uni_house_income[17,2] + uni_house_income[16,2]
uni_house_income[17,3] <- uni_house_income[17,3] + uni_house_income[16,3]

#The need of defining hkd range is because the "-" symbol is different
#between the two pdf
hkd_range <- c("Not applicable", "Under 2,000", "2,000 - 3,999",
               "4,000 - 5,999", "6,000 - 7,999",
               "8,000 - 9,999", "10,000 - 14,999",
               "15,000 - 19,999", "20,000 - 24,999",
               "25,000 - 29,999", "30,000 - 39,999",
               "40,000 - 49,999", "50,000 - 59,999",
               "60,000 - 69,999", "70,000 - 79,999",
               "80,000 - 89,999", "90,000 - 99,999",
               ">= 100,000", "Total")

uni_house_income_2 <- uni_house_income %>%
  mutate(HKD = hkd_range) %>%
  filter(!HKD %in% c("Under 2,000", "60,000 - 69,999", "80,000 - 89,999")) %>%
  mutate(HKD = recode(HKD, 
                      '2,000 - 3,999' = "< 4,000",
                      '70,000 - 79,999' = "60,000 - 79,999",
                      '90,000 - 99,999' = "80,000 - 99,999"))

uni_house_income_2


#Step 2: Read table from HKGov Census and Statistics Dept Report
path2 <- "data/HKGOV_2016Q1_Report.pdf"
txt2 <- pdf_text(path2)

txt2 <- txt2[[142]]
tab2 <- str_split(txt2, "\r\n")
tab2 <- tab2[[1]]

header_names2 <- c("HKD", "2015", "2016")
ind <- str_which(tab2, "\\d+\\s*\\(\\d+.\\d\\)")

hk_house_income <- tab2[ind] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE)


#Select the 1-3 columns and replace row entries
hk_house_income <- hk_house_income[,1:3]
hk_house_income[16,1] <- ">= 100,000"
hk_house_income[17,1] <- "Total"


hk_house_income <- hk_house_income %>%
  as.data.frame(stringAsFactor= FALSE) %>%
  setNames(header_names2) %>%
  extract(`2015`, c("2015_num", "2015_percent"), "(\\d+.\\d)\\s*\\((\\d+.\\d)\\)") %>%
  extract(`2016`, c("2016_num", "2016_percent"), "(\\d+.\\d)\\s*\\((\\d+.\\d)\\)") %>%
  mutate_at(-1, parse_number) %>%
  select(`HKD`, `2015_percent`, `2015_num`)


#Combine row 9 and 10; Combine row 11 and 12.
hk_house_income[10,2] <- hk_house_income[10,2] + hk_house_income[9,2]
hk_house_income[10,3] <- hk_house_income[10,3] + hk_house_income[9,3]

hk_house_income[12,2] <- hk_house_income[12,2] + hk_house_income[11,2]
hk_house_income[12,3] <- hk_house_income[12,3] + hk_house_income[11,3]


hk_house_income_2 <- hk_house_income %>%
  filter(!HKD %in% c("30,000 - 34,999", "40,000 - 44,999")) %>%
  mutate(HKD = recode(HKD, 
                      '35,000 - 39,999' = "30,000 - 39,999",
                      '45,000 - 49,999' = "40,000 - 49,999"))

hk_house_income_2


#Step 3: From hk_house_income to deduce income for 2015 DSE candidates
uni_house_income_2
hk_house_income_2

#Use hk_house_income to approximate the income distribution of 2015 DSE candidates
total_student <- 74131
stu_house_income <- hk_house_income_2 %>%
  mutate(`2015_student_num` = round(total_student * `2015_percent` / 100))

jt_income <- uni_house_income_2 %>% left_join(stu_house_income, by = 'HKD')


#Step 4: Plot of the percent of income distribution
order_hkd <- c("Not applicable", "< 4,000", "4,000 - 5,999", "6,000 - 7,999",
               "8,000 - 9,999", "10,000 - 14,999", "15,000 - 19,999",
               "20,000 - 24,999", "25,000 - 29,999",
               "30,000 - 39,999", "40,000 - 49,999",
               "50,000 - 59,999", "60,000 - 79,999", 
               "80,000 - 99,999", ">= 100,000", "Total")
jt_income %>%
  mutate(HKD = factor(HKD, levels = order_hkd)) %>%
  filter(!HKD %in% c("Not applicable", "Total")) %>%
  select(HKD, `hku_percent`, `2015_percent`) %>%
  rename(`population_percent` = `2015_percent`) %>%
  gather(key = "Type", value = "Percent", c(`hku_percent`, `population_percent`)) %>%
  ggplot(aes(HKD, Percent, color = Type)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Numeric form of HKD
jt_income_num <- jt_income %>%
  filter(!HKD %in% c("Not applicable", "Total")) %>%
  mutate(HKD = recode(HKD, 
                      '< 4,000' = '4000',
                      '4,000 - 5,999' = '5000',
                      '6,000 - 7,999' = '8000',
                      '8,000 - 9,999' = '9000',
                      '10,000 - 14,999' = '12500',
                      '15,000 - 19,999' = '17500',
                      '20,000 - 24,999' = '22500',
                      '25,000 - 29,999' = '27500',
                      '30,000 - 39,999' = '35000',
                      '40,000 - 49,999' = '45000',
                      '50,000 - 59,999' = '55000',
                      '60,000 - 79,999' = '70000',
                      '80,000 - 99,999' = '90000',
                      '>= 100,000' = '100,000')) %>%
  mutate_at(1, parse_number)

jt_income_num %>%
  select(HKD, `hku_percent`, `2015_percent`) %>%
  rename(`HKU Student` = `hku_percent`) %>%
  rename(`Population` = `2015_percent`) %>%
  gather(key = "Type", value = "Percent", c(`HKU Student`, `Population`)) %>%
  ggplot(aes(HKD, Percent, color = Type)) +
  geom_line() +
  ggtitle("Monthly Household Income Distribution of HKU Students and Population in 2015") +
  xlab("Monthly Household Income (HKD)") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("figs/linePlot.png")


#Step 5: Calculate odds ratio
median_income <- 25000
below_median_group <- jt_income_num %>%
  filter(HKD < median_income) %>%
  mutate(not_admit = `2015_student_num` - hku_num) %>%
  summarize(ttl_hku_num = sum(hku_num), ttl_not_admit = sum(not_admit), ttl_stu_num = sum(`2015_student_num`))
or1 <- below_median_group$ttl_hku_num/below_median_group$ttl_not_admit

above_median_group <- jt_income_num %>%
  filter(HKD > median_income) %>%
  mutate(not_admit = `2015_student_num` - hku_num) %>%
  summarize(ttl_hku_num = sum(hku_num), ttl_not_admit = sum(not_admit), ttl_stu_num = sum(`2015_student_num`))
or2 <- above_median_group$ttl_hku_num/above_median_group$ttl_not_admit

or1/or2
