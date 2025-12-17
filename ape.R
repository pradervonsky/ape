library(dplyr)
library(lubridate)
library(emmeans)

# > load the data ----
employee <- read.csv("./employee_data.csv")
str(employee)

# unique and summary of relevant columns
summary(employee$EmpID)
summary(employee$ADEmail)
summary(employee$DOB)
unique(employee$EmployeeStatus)
unique(employee$EmployeeType)
unique(employee$DepartmentType)
unique(employee$Performance.Score)

# select only EmpID, ADEmail, EmployeeStatus, EmployeeType, DepartmentType, DOB, and Performance.Score columns
# and filter only active employee
employee <- employee %>%
  select(
    EmpID,
    ADEmail,
    EmployeeStatus,
    EmployeeType,
    DepartmentType,
    DOB,
    Performance.Score
  ) %>%
  mutate(DepartmentType = trimws(DepartmentType) # clean string
  ) %>%
  filter(EmployeeStatus == "Active") %>%
  select(-EmployeeStatus)
head(employee, 5)

# create age classification from DOB
employee <- employee %>%
  mutate(
    DOB = dmy(DOB),
    BirthYear = year(DOB),
    Generation = case_when(
      BirthYear >= 1928 & BirthYear <= 1945 ~ "PostWar",
      BirthYear >= 1946 & BirthYear <= 1964 ~ "Boomer",
      BirthYear >= 1965 & BirthYear <= 1980 ~ "GenX",
      BirthYear >= 1981 & BirthYear <= 1996 ~ "GenY",
      BirthYear >= 1997                     ~ "GenZ",
      TRUE                                  ~ NA_character_
    )
  ) %>%
  select(-DOB, -BirthYear)
sum(is.na(employee$Generation))  # check for any NA in Generation
unique(employee$Generation)
head(employee, 5)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# > simulate the phishing experiment results ----
## >> generate characteristics variables ----
phishing <- expand.grid(
  Topic = c("administrative", "income", "career"),
  Tone = c("friendly", "threatening"),
  Structure = c("short", "detailed")
)
phishing

# randomize and assign phishing variables to each of EmpID on the employee data, but keep each variance balanced
set.seed(123)
employee_phishing <- employee %>%
  slice_sample(n = nrow(employee), replace = FALSE) %>%
  mutate(
    Tone = rep(phishing$Tone, length.out = n()),
    Structure = rep(phishing$Structure, length.out = n()),
    Topic = rep(phishing$Topic, length.out = n())
  )
employee_phishing

# join between Tone, Structure, and Topic to see the class balance between 12 combinations
employee_phishing <- employee_phishing %>%
  mutate(
    Group = interaction(Topic, Tone, Structure, sep = "_")
  )
table(employee_phishing$Group)  # balanced groups

## >> create effect sizes for each characteristic ----
### >>> email features effect ----
topic_effect <- c(
  administrative = 0.00,
  income = 0.08,
  career = 0.12
)

tone_effect <- c(
  friendly = 0.00,
  threatening = 0.15
)

structure_effect <- c(
  short = 0.00,
  detailed = 0.10
)

### >>> employee characteristics effect ----
dept_effect <- function(dept) {
  if (dept %in% c("IT/IS", "Software Engineering")) {
    -0.12
  } else if (dept %in% c("Executive Office")) {
    -0.05
  } else {
    0.00
  }
}

gen_effect <- c(
  PostWar = 0.15,
  Boomer  = 0.12,
  GenX    = 0.08,
  GenY    = 0.03,
  GenZ    = 0.00
)

employee_type_effect <- c(
  "Full-Time" = 0.00,
  "Part-Time" = 0.04,
  "Contract"  = 0.08
)

performance_effect <- c(
  "Exceeds"            = -0.08,
  "Fully Meets"        =  0.00,
  "Needs Improvement"  =  0.10,
  "PIP"                =  0.18
)

## >> calculate the probability of clicking for each employee ----
p_base <- 0.12  # friendly_short_income baseline

employee_phishing <- employee_phishing %>%
  rowwise() %>%
  mutate(
    ClickProb =
      p_base +
      tone_effect[Tone] +
      structure_effect[Structure] +
      topic_effect[Topic] +
      dept_effect(DepartmentType) +
      gen_effect[Generation] +
      employee_type_effect[EmployeeType] +
      performance_effect[Performance.Score],
    
    ClickProb = pmin(pmax(ClickProb, 0.01), 0.90)
  ) %>%
  ungroup()
employee_phishing

# simulate clicks based on the calculated probabilities
set.seed(456)
employee_phishing <- employee_phishing %>%
  mutate(
    Clicked = rbinom(n(), 1, ClickProb)
  )
table(employee_phishing$Clicked)
head(employee_phishing, 5)

# > descriptive statistics of CTR per group
group_summary <- employee_phishing %>%
  group_by(Group) %>%
  summarise(
    Conversions = sum(Clicked),
    EmailSent = n(),
    CTR = Conversions / EmailSent
  ) %>%
  arrange(CTR)
group_summary

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# > pairwise proportion testing against control group ----
control <- group_summary %>% filter(Group == "administrative_friendly_short")
ctr_C = control$CTR
sent_C = control$EmailSent

delta0 = 0.1  # desired minimum uplift (10 pp)

results <- group_summary %>%
  filter(Group != "administrative_friendly_short") %>%
  rowwise() %>%
  mutate(
    ctr_T = CTR,
    sent_T = EmailSent,
    diff_hat = ctr_T - ctr_C, # observed difference in proportions
    uplift = (ctr_T - ctr_C) / ctr_C * 100,
    SE = sqrt(ctr_T * (1 - ctr_T) / sent_T +
                ctr_C * (1 - ctr_C) / sent_C), # unpooled standard error
    z = (diff_hat - delta0) / SE, # test statistics for T - C > delta0
    p_value = 1 - pnorm(z), # one-sided p-value
    winner = p_value < 0.05 & diff_hat > delta0
  ) %>%
  select(-ctr_T, -sent_T)
control
results

## >> ex-post power calculation ----
sent_T <- sent_C
alpha <- 0.05 # significance level
mde <- ctr_C + delta0  # minimum detectable effect

# pooled SE
SE <- sqrt(
  ctr_C * (1 - ctr_C) / sent_C +
    mde   * (1 - mde)   / sent_T
)

# z value for alpha
z_alpha <- qnorm(1 - alpha)

# calculate power
power <- 1 - pnorm(z_alpha - (delta0 / SE))
power

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# > GLM fit and emmeans analysis ----

## >> check full model with 3-way interaction ----
model_glm <- glm(
  Clicked ~ Topic * Tone * Structure,
  family = binomial(link = "logit"),
  data   = employee_phishing
)
summary(model_glm)

## >> check reduced model with only 2-way interactions ----
model_glm2 <- glm(
  Clicked ~ Topic * Tone + Topic * Structure + Tone * Structure,
  family = binomial,
  data   = employee_phishing
)
summary(model_glm2)

## >> check main effects only model ----
model_glm_main <- glm(
  Clicked ~ Topic + Tone + Structure,
  family = binomial,
  data   = employee_phishing
)
summary(model_glm_main)

# exponentiate all coefficients to get odds ratios
exp(coef(model_glm_main))

## >> main-effect emmeans on CTR ----
emm_topic <- emmeans(model_glm_main, ~ Topic, type = "response")
emm_tone <- emmeans(model_glm_main, ~ Tone, type = "response")
emm_structure <- emmeans(model_glm_main, ~ Structure, type = "response")
emm_topic
emm_tone
emm_structure

## >> pairwise comparisons with Bonferroni adjustment ----
pairs_topic <- pairs(emm_topic, adjust = "bonferroni")
pairs_tone <- pairs(emm_tone, adjust = "bonferroni")
pairs_structure <- pairs(emm_structure, adjust = "bonferroni")
pairs_topic
pairs_tone
pairs_structure

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
