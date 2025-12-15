# APE: AI-assisted Phishing Experiment
The experiment is designed as a simulated cybersecurity training exercise and focuses on understanding human susceptibility to phishing emails generated using modern AI tools.

The project combines experimental design, data simulation, and statistical analysis to evaluate how email **topic**, **tone**, and **structure** affect employee click-through rates (CTR).

Data source: [Kaggle Employee/HR Dataset](https://www.kaggle.com/datasets/ravindrasinghrana/employeedataset?select=employee_data.csv)

---

# Statistical Analysis and Results

Created using R, knitted in markdown format.

# \> Data Processing

``` r
employee <- read.csv("./employee_data.csv")
str(employee)
```

    ## 'data.frame':    3000 obs. of  26 variables:
    ##  $ EmpID                     : int  3427 3428 3429 3430 3431 3432 3433 3434 3435 3436 ...
    ##  $ FirstName                 : chr  "Uriah" "Paula" "Edward" "Michael" ...
    ##  $ LastName                  : chr  "Bridges" "Small" "Buck" "Riordan" ...
    ##  $ StartDate                 : chr  "20-Sep-19" "11-Feb-23" "10-Dec-18" "21-Jun-21" ...
    ##  $ ExitDate                  : chr  "" "" "" "" ...
    ##  $ Title                     : chr  "Production Technician I" "Production Technician I" "Area Sales Manager" "Area Sales Manager" ...
    ##  $ Supervisor                : chr  "Peter Oneill" "Renee Mccormick" "Crystal Walker" "Rebekah Wright" ...
    ##  $ ADEmail                   : chr  "uriah.bridges@bilearner.com" "paula.small@bilearner.com" "edward.buck@bilearner.com" "michael.riordan@bilearner.com" ...
    ##  $ BusinessUnit              : chr  "CCDR" "EW" "PL" "CCDR" ...
    ##  $ EmployeeStatus            : chr  "Active" "Active" "Active" "Active" ...
    ##  $ EmployeeType              : chr  "Contract" "Contract" "Full-Time" "Contract" ...
    ##  $ PayZone                   : chr  "Zone C" "Zone A" "Zone B" "Zone A" ...
    ##  $ EmployeeClassificationType: chr  "Temporary" "Part-Time" "Part-Time" "Full-Time" ...
    ##  $ TerminationType           : chr  "Unk" "Unk" "Unk" "Unk" ...
    ##  $ TerminationDescription    : chr  "" "" "" "" ...
    ##  $ DepartmentType            : chr  "Production       " "Production       " "Sales" "Sales" ...
    ##  $ Division                  : chr  "Finance & Accounting" "Aerial" "General - Sga" "Finance & Accounting" ...
    ##  $ DOB                       : chr  "07-10-1969" "30-08-1965" "06-10-1991" "04-04-1998" ...
    ##  $ State                     : chr  "MA" "MA" "MA" "ND" ...
    ##  $ JobFunctionDescription    : chr  "Accounting" "Labor" "Assistant" "Clerk" ...
    ##  $ GenderCode                : chr  "Female" "Male" "Male" "Male" ...
    ##  $ LocationCode              : int  34904 6593 2330 58782 33174 6050 90007 97756 78789 78207 ...
    ##  $ RaceDesc                  : chr  "White" "Hispanic" "Hispanic" "Other" ...
    ##  $ MaritalDesc               : chr  "Widowed" "Widowed" "Widowed" "Single" ...
    ##  $ Performance.Score         : chr  "Fully Meets" "Fully Meets" "Fully Meets" "Fully Meets" ...
    ##  $ Current.Employee.Rating   : int  4 3 4 2 3 3 4 2 3 5 ...

Unique and summary of relevant columns

``` r
# unique and summary of relevant columns
summary(employee$EmpID)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1001    1751    2500    2500    3250    4000

``` r
summary(employee$ADEmail)
```

    ##    Length     Class      Mode 
    ##      3000 character character

``` r
summary(employee$DOB)
```

    ##    Length     Class      Mode 
    ##      3000 character character

``` r
unique(employee$EmployeeStatus)
```

    ## [1] "Active"                 "Future Start"           "Voluntarily Terminated"
    ## [4] "Leave of Absence"       "Terminated for Cause"

``` r
unique(employee$EmployeeType)
```

    ## [1] "Contract"  "Full-Time" "Part-Time"

``` r
unique(employee$DepartmentType)
```

    ## [1] "Production       "    "Sales"                "IT/IS"               
    ## [4] "Executive Office"     "Software Engineering" "Admin Offices"

``` r
unique(employee$Performance.Score)
```

    ## [1] "Fully Meets"       "Exceeds"           "Needs Improvement"
    ## [4] "PIP"

Select only EmpID, ADEmail, EmployeeStatus, EmployeeType,
DepartmentType, DOB, and Performance.Score columns and filter only
active employee

``` r
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
```

    ##   EmpID                       ADEmail EmployeeType DepartmentType        DOB
    ## 1  3427   uriah.bridges@bilearner.com     Contract     Production 07-10-1969
    ## 2  3428     paula.small@bilearner.com     Contract     Production 30-08-1965
    ## 3  3429     edward.buck@bilearner.com    Full-Time          Sales 06-10-1991
    ## 4  3430 michael.riordan@bilearner.com     Contract          Sales 04-04-1998
    ## 5  3431   jasmine.onque@bilearner.com     Contract          Sales 29-08-1969
    ##   Performance.Score
    ## 1       Fully Meets
    ## 2       Fully Meets
    ## 3       Fully Meets
    ## 4       Fully Meets
    ## 5       Fully Meets

Create age classification from DOB

``` r
employee <- employee %>%
  mutate(
    DOB = dmy(DOB),
    BirthYear = year(DOB),
    Generation = case_when(
      BirthYear >= 1928 & BirthYear <= 1945 ~ "PostWar",
      BirthYear >= 1946 & BirthYear <= 1964 ~ "Boomer",
      BirthYear >= 1965 & BirthYear <= 1980 ~ "GenX",
      BirthYear >= 1981 & BirthYear <= 1996 ~ "GenY",
      BirthYear >= 1997                    ~ "GenZ",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  select(-DOB, -BirthYear)
sum(is.na(employee$Generation))  # check for any NA in Generation
```

    ## [1] 0

``` r
unique(employee$Generation)
```

    ## [1] "GenX"    "GenY"    "GenZ"    "Boomer"  "PostWar"

``` r
head(employee, 5)
```

    ##   EmpID                       ADEmail EmployeeType DepartmentType
    ## 1  3427   uriah.bridges@bilearner.com     Contract     Production
    ## 2  3428     paula.small@bilearner.com     Contract     Production
    ## 3  3429     edward.buck@bilearner.com    Full-Time          Sales
    ## 4  3430 michael.riordan@bilearner.com     Contract          Sales
    ## 5  3431   jasmine.onque@bilearner.com     Contract          Sales
    ##   Performance.Score Generation
    ## 1       Fully Meets       GenX
    ## 2       Fully Meets       GenX
    ## 3       Fully Meets       GenY
    ## 4       Fully Meets       GenZ
    ## 5       Fully Meets       GenX

# \> Simulate the phishing experiment results

## \>\> Generate characteristics variables

``` r
phishing <- expand.grid(
  Topic = c("administrative", "income", "career"),
  Tone = c("friendly", "threatening"),
  Structure = c("short", "detailed")
)
print(phishing)
```

    ##             Topic        Tone Structure
    ## 1  administrative    friendly     short
    ## 2          income    friendly     short
    ## 3          career    friendly     short
    ## 4  administrative threatening     short
    ## 5          income threatening     short
    ## 6          career threatening     short
    ## 7  administrative    friendly  detailed
    ## 8          income    friendly  detailed
    ## 9          career    friendly  detailed
    ## 10 administrative threatening  detailed
    ## 11         income threatening  detailed
    ## 12         career threatening  detailed

Randomize and assign characteristic variables to individual employee and
keep each variant balanced

``` r
set.seed(123)
employee_phishing <- employee %>%
  slice_sample(n = nrow(employee), replace = FALSE) %>%
  mutate(
    Tone = rep(phishing$Tone, length.out = n()),
    Structure = rep(phishing$Structure, length.out = n()),
    Topic = rep(phishing$Topic, length.out = n())
  )
head(employee_phishing, 5)
```

    ##   EmpID                        ADEmail EmployeeType DepartmentType
    ## 1  3094 jordin.alexander@bilearner.com     Contract     Production
    ## 2  1095    karen.mancuso@bilearner.com    Full-Time     Production
    ## 3  3670  sandra.atkinson@bilearner.com    Full-Time     Production
    ## 4  2647        rylan.key@bilearner.com     Contract          Sales
    ## 5  1856     matteo.oneal@bilearner.com    Full-Time     Production
    ##   Performance.Score Generation        Tone Structure          Topic
    ## 1       Fully Meets     Boomer    friendly     short administrative
    ## 2       Fully Meets    PostWar    friendly     short         income
    ## 3 Needs Improvement       GenX    friendly     short         career
    ## 4       Fully Meets     Boomer threatening     short administrative
    ## 5           Exceeds       GenX threatening     short         income

Check the joined data and balanced groups

``` r
employee_phishing <- employee_phishing %>%
  mutate(
    Group = interaction(Topic, Tone, Structure, sep = "_")
  )
table(employee_phishing$Group)
```

    ## 
    ##       administrative_friendly_short               income_friendly_short 
    ##                                 205                                 205 
    ##               career_friendly_short    administrative_threatening_short 
    ##                                 205                                 205 
    ##            income_threatening_short            career_threatening_short 
    ##                                 205                                 205 
    ##    administrative_friendly_detailed            income_friendly_detailed 
    ##                                 205                                 205 
    ##            career_friendly_detailed administrative_threatening_detailed 
    ##                                 205                                 205 
    ##         income_threatening_detailed         career_threatening_detailed 
    ##                                 204                                 204

## \>\> Create effect sizes for each characteristics and interaction

Email features effect

``` r
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
```

Employee characteristics effect

``` r
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
```

## \>\> Calculate the probability of clicking for each employee

``` r
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
# showing head without EmployeeStatus, Topic, Structure, and Tone, without removing them from the original dataframe
head(as.data.frame(employee_phishing), 5)
```

    ##   EmpID                        ADEmail EmployeeType DepartmentType
    ## 1  3094 jordin.alexander@bilearner.com     Contract     Production
    ## 2  1095    karen.mancuso@bilearner.com    Full-Time     Production
    ## 3  3670  sandra.atkinson@bilearner.com    Full-Time     Production
    ## 4  2647        rylan.key@bilearner.com     Contract          Sales
    ## 5  1856     matteo.oneal@bilearner.com    Full-Time     Production
    ##   Performance.Score Generation        Tone Structure          Topic
    ## 1       Fully Meets     Boomer    friendly     short administrative
    ## 2       Fully Meets    PostWar    friendly     short         income
    ## 3 Needs Improvement       GenX    friendly     short         career
    ## 4       Fully Meets     Boomer threatening     short administrative
    ## 5           Exceeds       GenX threatening     short         income
    ##                              Group ClickProb
    ## 1    administrative_friendly_short      0.32
    ## 2            income_friendly_short      0.35
    ## 3            career_friendly_short      0.42
    ## 4 administrative_threatening_short      0.47
    ## 5         income_threatening_short      0.35

Simulate clicks

``` r
# simulate clicks based on the calculated probabilities
set.seed(456)
employee_phishing <- employee_phishing %>%
  mutate(
    Clicked = rbinom(n(), 1, ClickProb)
  )
table(employee_phishing$Clicked)
```

    ## 
    ##    0    1 
    ## 1480  978

``` r
head(as.data.frame(employee_phishing %>% select(-EmpID)), 5)
```

    ##                          ADEmail EmployeeType DepartmentType Performance.Score
    ## 1 jordin.alexander@bilearner.com     Contract     Production       Fully Meets
    ## 2    karen.mancuso@bilearner.com    Full-Time     Production       Fully Meets
    ## 3  sandra.atkinson@bilearner.com    Full-Time     Production Needs Improvement
    ## 4        rylan.key@bilearner.com     Contract          Sales       Fully Meets
    ## 5     matteo.oneal@bilearner.com    Full-Time     Production           Exceeds
    ##   Generation        Tone Structure          Topic
    ## 1     Boomer    friendly     short administrative
    ## 2    PostWar    friendly     short         income
    ## 3       GenX    friendly     short         career
    ## 4     Boomer threatening     short administrative
    ## 5       GenX threatening     short         income
    ##                              Group ClickProb Clicked
    ## 1    administrative_friendly_short      0.32       0
    ## 2            income_friendly_short      0.35       0
    ## 3            career_friendly_short      0.42       1
    ## 4 administrative_threatening_short      0.47       1
    ## 5         income_threatening_short      0.35       1

Analyze the CTR per group

``` r
# > descriptive statistics of CTR per group
group_summary <- employee_phishing %>%
  group_by(Group) %>%
  summarise(
    Conversions = sum(Clicked),
    EmailSent = n(),
    CTR = Conversions / EmailSent
  ) %>%
  arrange(CTR)
print(group_summary, n = Inf, width = Inf)
```

    ## # A tibble: 12 × 4
    ##    Group                               Conversions EmailSent   CTR
    ##    <fct>                                     <int>     <int> <dbl>
    ##  1 administrative_friendly_short                50       205 0.244
    ##  2 income_friendly_short                        55       205 0.268
    ##  3 administrative_friendly_detailed             64       205 0.312
    ##  4 career_friendly_short                        68       205 0.332
    ##  5 administrative_threatening_short             72       205 0.351
    ##  6 income_friendly_detailed                     75       205 0.366
    ##  7 income_threatening_short                     88       205 0.429
    ##  8 career_friendly_detailed                     88       205 0.429
    ##  9 administrative_threatening_detailed          96       205 0.468
    ## 10 income_threatening_detailed                 106       204 0.520
    ## 11 career_threatening_short                    108       205 0.527
    ## 12 career_threatening_detailed                 108       204 0.529

# \> Pairwise uplift testing against control group

``` r
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
print(results, n = Inf, width = Inf)
```

    ## # A tibble: 11 × 10
    ## # Rowwise: 
    ##    Group                               Conversions EmailSent   CTR diff_hat
    ##    <fct>                                     <int>     <int> <dbl>    <dbl>
    ##  1 income_friendly_short                        55       205 0.268   0.0244
    ##  2 administrative_friendly_detailed             64       205 0.312   0.0683
    ##  3 career_friendly_short                        68       205 0.332   0.0878
    ##  4 administrative_threatening_short             72       205 0.351   0.107 
    ##  5 income_friendly_detailed                     75       205 0.366   0.122 
    ##  6 income_threatening_short                     88       205 0.429   0.185 
    ##  7 career_friendly_detailed                     88       205 0.429   0.185 
    ##  8 administrative_threatening_detailed          96       205 0.468   0.224 
    ##  9 income_threatening_detailed                 106       204 0.520   0.276 
    ## 10 career_threatening_short                    108       205 0.527   0.283 
    ## 11 career_threatening_detailed                 108       204 0.529   0.286 
    ##    uplift     SE      z   p_value winner
    ##     <dbl>  <dbl>  <dbl>     <dbl> <lgl> 
    ##  1   10.0 0.0431 -1.75  0.960     FALSE 
    ##  2   28   0.0441 -0.719 0.764     FALSE 
    ##  3   36   0.0445 -0.274 0.608     FALSE 
    ##  4   44   0.0448  0.163 0.435     FALSE 
    ##  5   50   0.0451  0.487 0.313     FALSE 
    ##  6   76   0.0458  1.87  0.0311    TRUE  
    ##  7   76   0.0458  1.87  0.0311    TRUE  
    ##  8   92   0.0460  2.71  0.00341   TRUE  
    ##  9  113.  0.0461  3.81  0.0000686 TRUE  
    ## 10  116   0.0460  3.98  0.0000349 TRUE  
    ## 11  117.  0.0461  4.03  0.0000281 TRUE

# \> GLM fit and emmeans analysis

## \>\> Check full model with 3-way interaction

``` r
model_glm <- glm(
  Clicked ~ Topic * Tone * Structure,
  family = binomial(link = "logit"),
  data   = employee_phishing
)
summary(model_glm)
```

    ## 
    ## Call:
    ## glm(formula = Clicked ~ Topic * Tone * Structure, family = binomial(link = "logit"), 
    ##     data = employee_phishing)
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error z value
    ## (Intercept)                                   -1.13140    0.16264  -6.957
    ## Topicincome                                    0.12810    0.22650   0.566
    ## Topiccareer                                    0.43093    0.22013   1.958
    ## Tonethreatening                                0.51772    0.21877   2.367
    ## Structuredetailed                              0.34153    0.22174   1.540
    ## Topicincome:Tonethreatening                    0.20075    0.30433   0.660
    ## Topiccareer:Tonethreatening                    0.29017    0.29905   0.970
    ## Topicincome:Structuredetailed                  0.11173    0.30829   0.362
    ## Topiccareer:Structuredetailed                  0.07411    0.30180   0.246
    ## Tonethreatening:Structuredetailed              0.14516    0.30028   0.483
    ## Topicincome:Tonethreatening:Structuredetailed -0.23511    0.41903  -0.561
    ## Topiccareer:Tonethreatening:Structuredetailed -0.55043    0.41392  -1.330
    ##                                               Pr(>|z|)    
    ## (Intercept)                                   3.49e-12 ***
    ## Topicincome                                     0.5717    
    ## Topiccareer                                     0.0503 .  
    ## Tonethreatening                                 0.0180 *  
    ## Structuredetailed                               0.1235    
    ## Topicincome:Tonethreatening                     0.5095    
    ## Topiccareer:Tonethreatening                     0.3319    
    ## Topicincome:Structuredetailed                   0.7170    
    ## Topiccareer:Structuredetailed                   0.8060    
    ## Tonethreatening:Structuredetailed               0.6288    
    ## Topicincome:Tonethreatening:Structuredetailed   0.5748    
    ## Topiccareer:Tonethreatening:Structuredetailed   0.1836    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3304.3  on 2457  degrees of freedom
    ## Residual deviance: 3208.0  on 2446  degrees of freedom
    ## AIC: 3232
    ## 
    ## Number of Fisher Scoring iterations: 4

## \>\> Check reduced model with only 2-way interactions

``` r
model_glm2 <- glm(
  Clicked ~ Topic * Tone + Topic * Structure + Tone * Structure,
  family = binomial,
  data   = employee_phishing
)
summary(model_glm2)
```

    ## 
    ## Call:
    ## glm(formula = Clicked ~ Topic * Tone + Topic * Structure + Tone * 
    ##     Structure, family = binomial, data = employee_phishing)
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                       -1.212720   0.147773  -8.207 2.27e-16 ***
    ## Topicincome                        0.198723   0.192005   1.035 0.300674    
    ## Topiccareer                        0.587012   0.187109   3.137 0.001705 ** 
    ## Tonethreatening                    0.662882   0.175277   3.782 0.000156 ***
    ## Structuredetailed                  0.490379   0.176107   2.785 0.005360 ** 
    ## Topicincome:Tonethreatening        0.074823   0.209315   0.357 0.720744    
    ## Topiccareer:Tonethreatening        0.003042   0.206749   0.015 0.988261    
    ## Topicincome:Structuredetailed     -0.017410   0.208912  -0.083 0.933583    
    ## Topiccareer:Structuredetailed     -0.218345   0.206499  -1.057 0.290344    
    ## Tonethreatening:Structuredetailed -0.126634   0.168763  -0.750 0.453034    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3304.3  on 2457  degrees of freedom
    ## Residual deviance: 3209.8  on 2448  degrees of freedom
    ## AIC: 3229.8
    ## 
    ## Number of Fisher Scoring iterations: 4

## \>\> Check main effects only model

``` r
model_glm_main <- glm(
  Clicked ~ Topic + Tone + Structure,
  family = binomial,
  data   = employee_phishing
)
summary(model_glm_main)
```

    ## 
    ## Call:
    ## glm(formula = Clicked ~ Topic + Tone + Structure, family = binomial, 
    ##     data = employee_phishing)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -1.14729    0.09873 -11.620  < 2e-16 ***
    ## Topicincome        0.22922    0.10402   2.204   0.0275 *  
    ## Topiccareer        0.47671    0.10318   4.620 3.84e-06 ***
    ## Tonethreatening    0.62272    0.08421   7.394 1.42e-13 ***
    ## Structuredetailed  0.34083    0.08407   4.054 5.04e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3304.3  on 2457  degrees of freedom
    ## Residual deviance: 3211.9  on 2453  degrees of freedom
    ## AIC: 3221.9
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# exponentiate all coefficients to get odds ratios
exp(coef(model_glm_main))
```

    ##       (Intercept)       Topicincome       Topiccareer   Tonethreatening 
    ##         0.3174971         1.2576164         1.6107611         1.8639873 
    ## Structuredetailed 
    ##         1.4061141

## \>\> emmeans analysis on main effects model

``` r
emm_topic <- emmeans(model_glm_main, ~ Topic, type = "response")
emm_tone <- emmeans(model_glm_main, ~ Tone, type = "response")
emm_structure <- emmeans(model_glm_main, ~ Structure, type = "response")

emm_topic
```

    ##  Topic           prob     SE  df asymp.LCL asymp.UCL
    ##  administrative 0.340 0.0167 Inf     0.307     0.373
    ##  income         0.393 0.0173 Inf     0.359     0.427
    ##  career         0.453 0.0177 Inf     0.419     0.488
    ## 
    ## Results are averaged over the levels of: Tone, Structure 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale

``` r
emm_tone
```

    ##  Tone         prob     SE  df asymp.LCL asymp.UCL
    ##  friendly    0.323 0.0134 Inf     0.297     0.350
    ##  threatening 0.470 0.0144 Inf     0.442     0.499
    ## 
    ## Results are averaged over the levels of: Topic, Structure 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale

``` r
emm_structure
```

    ##  Structure  prob     SE  df asymp.LCL asymp.UCL
    ##  short     0.354 0.0138 Inf     0.328     0.382
    ##  detailed  0.435 0.0144 Inf     0.407     0.464
    ## 
    ## Results are averaged over the levels of: Topic, Tone 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale

## \>\> Pairwise comparisons with Bonferroni adjustment

``` r
pairs_topic <- pairs(emm_topic, adjust = "bonferroni")
pairs_tone <- pairs(emm_tone, adjust = "bonferroni")
pairs_structure <- pairs(emm_structure, adjust = "bonferroni")

pairs_topic
```

    ##  contrast                odds.ratio     SE  df null z.ratio p.value
    ##  administrative / income      0.795 0.0827 Inf    1  -2.204  0.0826
    ##  administrative / career      0.621 0.0641 Inf    1  -4.620  <.0001
    ##  income / career              0.781 0.0794 Inf    1  -2.434  0.0448
    ## 
    ## Results are averaged over the levels of: Tone, Structure 
    ## P value adjustment: bonferroni method for 3 tests 
    ## Tests are performed on the log odds ratio scale

``` r
pairs_tone
```

    ##  contrast               odds.ratio     SE  df null z.ratio p.value
    ##  friendly / threatening      0.536 0.0452 Inf    1  -7.394  <.0001
    ## 
    ## Results are averaged over the levels of: Topic, Structure 
    ## Tests are performed on the log odds ratio scale

``` r
pairs_structure
```

    ##  contrast         odds.ratio     SE  df null z.ratio p.value
    ##  short / detailed      0.711 0.0598 Inf    1  -4.054  0.0001
    ## 
    ## Results are averaged over the levels of: Topic, Tone 
    ## Tests are performed on the log odds ratio scale
