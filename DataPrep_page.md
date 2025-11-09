## Data Mining in Business

**Project description:** Tasked with using historical repayment data on 300,000+ loans to construct a model to identify the probability of default for new loan applicants.  As with any banking institution, loan application requires a credit pull and you have information on the credit history of the borrow as well as any additional information provided at the time of application. The goal in this part of the project is prepare this data for modeling.

### Markdown of Data Cleaning Process

```{r}
pacman::p_load(tidyverse, caret, DataExplorer, skimr, fastDummies, forcats, lubridate)
options(scipen=999)
```

```{r}
df <-readr::read_csv("C:/Users/Public/Desktop/loan_default_project.csv", name_repair = make.names)
```

```{r}
#Recode Target and character variables as factors

df$loan_status<-as.factor(df$loan_status)
df <- df%>%dplyr::mutate(across(where(is.character), factor))
```

```{r}
#Remove unnecessary/redundant variables and ones with >50% missing

df<-dplyr::select(df, -ID, -application_type, -debt_settlement_flag, -hardship_flag, -mths_since_last_delinq, -pub_rec, -last_credit_pull_d, -title)
```

```{r}
#Highly correlated variable handling
df$fico_avg <-(df$fico_range_high+df$fico_range_low)/2
df<-dplyr::select(df, -fico_range_high, -fico_range_low) 
```

```{r}
#Collapsing purpose into significant categories
df$purpose <- forcats::fct_recode(df$purpose,
                                "debt_consolidation" = "debt_consolidation",
                                "credit_card" = "credit_card",
                                'other' = "car",
                                'other' = "home_improvement",
                                'other' = 'moving',
                                'other' = "vacation",
                                'other' = 'house',
                                'other' = 'other',
                                'other' = 'wedding',
                                'other' = 'major_purchase',
                                'other' = 'renewable_energy',
                                'other' = 'educational',
                                'other' = 'medical',
                                'other' = 'small_business')
```

```{r}
#Collapsing emp_length
df$emp_length <- forcats::fct_recode(df$emp_length,
                                "Less_than_5_years" = "< 1 year",
                                "Less_than_5_years" = "1 year",
                                'Less_than_5_years' = "2 years",
                                'Less_than_5_years' = "3 years",
                                'Less_than_5_years' = '4 years',
                                '5to9_years' = "5 years",
                                '5to9_years' = '6 years',
                                '5to9_years' = '7 years',
                                '5to9_years' = '8 years',
                                '5to9_years' = '9 years',
                                '10_plus_years' = '10+ years')
```

```{r}
#Collapsing home ownership
df$home_ownership <- forcats::fct_recode(df$home_ownership,
                                "other" = "NONE",
                                "other" = "ANY",
                                'other' = "OTHER",
                                'rent' = "RENT",
                                'own' = 'OWN',
                                'mortgage' = "MORTGAGE")
```

```{r}
#Collapsing grade 
df$grade <- forcats::fct_recode(df$grade,
                                "A" = "A",
                                "B" = "B",
                                'C' = "C",
                                'D' = "D",
                                'Lower_than_D' = 'E',
                                'Lower_than_D' = "F",
                                'Lower_than_D' = "G")
```


```{r}
#sub grade collapsing into good and poor (1+2 are good, 3-5 are poor)
df$sub_grade <- forcats::fct_recode(df$sub_grade,
"Good" = "A1", "Good" = "B1", "Good" = "C1", "Good" = "D1", "Good" = "E1", "Good" = "F1", "Good" = "G1", "Good" = "A2", "Good" = "B2", "Good" = "C2", "Good" = "D2", "Good" = "E2", "Good" = "F2", "Good" = "G2", "Poor" = "A3", "Poor" = "B3", "Poor" = "C3", "Poor" = "D3", "Poor" = "E3", "Poor" = "F3", "Poor" = "G3", "Poor" = "A4", "Poor" = "B4", "Poor" = "C4", "Poor" = "D4", "Poor" = "E4", "Poor" = "F4", "Poor" = "G4", "Poor" = "A5", "Poor" = "B5", "Poor" = "C5", "Poor" = "D5", "Poor" = "E5", "Poor" = "F5", "Poor" = "G5")

```

```{r}
#Recoding target
df$default <- forcats::fct_recode(df$loan_status,
                                "Yes" = "Fully Paid",
                                "No" = "Charged Off")
df<-dplyr::select(df, -loan_status)
```


```{r}
#Getting month and year out of issue_d
df$issue_d <- my(as.character(df$issue_d))
df$mo_issue_d<-month(df$issue_d)
df$yr_issue_d<-year(df$issue_d)
```

```{r}
#Getting year out of earliest_cr_line
df$earliest_cr_line<- my(as.character(df$earliest_cr_line))
df$yr_earliest_cr_line<-year(df$earliest_cr_line)
```

```{r}
#Removing originals
df<-dplyr::select(df, -issue_d, -earliest_cr_line)
```

```{r}
#Missing and imputation

## Indicator column: chose to not make M_ columns for ones with <10% missing (mort_acc is 9.6%)
df$M_mort_acc<-as.factor(ifelse(is.na(df$mort_acc), 1, 0))

## changing temporarily to character
df$emp_title<-as.character(df$emp_title)
df$emp_length<-as.character(df$emp_length) 
df$revol_util<-as.numeric(df$revol_util)
df$mort_acc<-as.numeric(df$mort_acc)
df$pub_rec_bankruptcies<-as.numeric(df$pub_rec_bankruptcies)

## Imputation for NA's
df$emp_title[is.na(df$emp_title)]<-'NA'
df$emp_length[is.na(df$emp_length)]<-'NA' 
df$revol_util[is.na(df$revol_util)]<-median(df$revol_util, na.rm=TRUE)
df$mort_acc[is.na(df$mort_acc)]<-median(df$mort_acc, na.rm=TRUE)
df$pub_rec_bankruptcies[is.na(df$pub_rec_bankruptcies)]<-median(df$pub_rec_bankruptcies, na.rm=TRUE)

#generated using AI to handle 6 outliers
df <- df %>%
  mutate(dti = ifelse(dti > 100, mean(dti[dti <= 100], na.rm = TRUE), dti))
```



```{r}
#Back to factors
df$emp_length<-as.factor(df$emp_length)
df$mo_issue_d<-as.factor(df$mo_issue_d)
df$yr_issue_d<-as.factor(df$yr_issue_d)
df$yr_earliest_cr_line<-as.factor(df$yr_earliest_cr_line)
```


```{r}
#Generated by AI to handle yr_earliest_cr_line
library(readr)
library(dplyr)

df <- df %>%
  mutate(
    # convert to numeric year (e.g., "2004-05-01" → 2004)
    year_ec = parse_number(as.character(yr_earliest_cr_line)),

    # assign buckets based on the actual year instead of fixed age gaps
    yr_earliest_cr_line = case_when(
      year_ec < 1990 ~ "Before_1990",
      year_ec < 2000 ~ "1990s",
      year_ec < 2010 ~ "2000s",
      year_ec < 2020 ~ "2010s",
      TRUE ~ "2020s"
    )
  )
df$yr_earliest_cr_line = as.factor(df$yr_earliest_cr_line)
```

```{r}
df <- df %>%
  mutate(
    # convert to numeric year (e.g., "2004-05-01" → 2004)
    issue_year = parse_number(as.character(yr_issue_d)),

    # assign buckets based on the actual year instead of fixed age gaps
    yr_issue_d = case_when(
      issue_year < 1990 ~ "Before_1990",
      issue_year < 2000 ~ "1990s",
      issue_year < 2010 ~ "2000s",
      issue_year < 2020 ~ "2010s",
      TRUE ~ "2020s"
    )
  )
df$yr_issue_d = as.factor(df$yr_issue_d)
df<-dplyr::select(df, -year_ec, -issue_year)
```


```{r}
#Feature Engineering - applications in the last 6 months 
df$inq_last_6mths<-as.factor(df$inq_last_6mths)
df$inq_risk_category <- forcats::fct_recode(df$inq_last_6mths,
  "Low_Risk" = "0",
  "Low_Risk" = "1",
  "Possible_Risk" = "2",
  "Possible_Risk" = "3",
  "Possible_Risk" = "4",
  "Possible_Risk" = "5",
  "Possible_Risk" = "6",
  "Possible_Risk" = "7",
  "Possible_Risk" = "8") 
df$inq_last_6mths<-as.numeric(df$inq_last_6mths)
```

```{r}
#Feature Engineering - monthly installment/monthly income
df$Installment_to_MoInc_Ratio <-(df$installment/(df$annual_inc/12))
df$Installment_to_MoInc_Ratio <- ifelse(df$Installment_to_MoInc_Ratio > 0.1, "Possible_Risk", "Low_Risk")
df$Installment_to_MoInc_Ratio<-as.factor(df$Installment_to_MoInc_Ratio)
```


```{r}
#Generated by AI to extract from emp_title
library(dplyr)
library(stringr)
library(forcats)

# normalize text
normalize_title <- function(x) {
  x <- tolower(as.character(x))
  x <- str_replace_all(x, "&", " and ")
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  x <- str_squish(x)
  x
}

# bucket into 2 levels, manager/supervisor only significant (>10%) category
bucket_mgr_other <- function(x) {
  case_when(
    is.na(x) | x == "" ~ "Other",
    str_detect(x, "\\b(manager|supervisor)\\b|general manager|office manager|operations manager") ~ "Manager_or_Supervisor",
    TRUE ~ "Other"
  )
}

# apply
df$emp_title <- bucket_mgr_other(normalize_title(df$emp_title))
df$emp_title <- fct_relevel(df$emp_title, "Other", "Manager_or_Supervisor")
```


```{r}

#Spliting Address into regions 

df$state <- sub(".*\\b([A-Z]{2})\\s\\d{5}$", "\\1", df$address) #Generated by AI
df$region <- forcats::fct_recode(df$state,
  "Northeast" = "CT", "Northeast" = "ME", "Northeast" = "MA",
  "Northeast" = "NH", "Northeast" = "RI", "Northeast" = "VT",
  "Northeast" = "NJ", "Northeast" = "NY", "Northeast" = "PA",
  "Midwest" = "IL", "Midwest" = "IN", "Midwest" = "MI",
  "Midwest" = "OH", "Midwest" = "WI", "Midwest" = "IA",
  "Midwest" = "KS", "Midwest" = "MN", "Midwest" = "MO",
  "Midwest" = "NE", "Midwest" = "ND", "Midwest" = "SD",
  "South" = "DE", "South" = "FL", "South" = "GA",
  "South" = "MD", "South" = "NC", "South" = "SC",
  "South" = "VA", "South" = "DC", "South" = "WV",
  "South" = "AL", "South" = "KY", "South" = "MS",
  "South" = "TN", "South" = "AR", "South" = "LA",
  "South" = "OK", "South" = "TX",
  "West" = "AZ", "West" = "CO", "West" = "ID",
  "West" = "MT", "West" = "NV", "West" = "NM",
  "West" = "UT", "West" = "WY", "West" = "AK",
  "West" = "CA", "West" = "HI", "West" = "OR",
  "West" = "WA", 
  "Armed Forces" = "AA", "Armed Forces" = "AE","Armed Forces"="AP"
)
```


```{r}
#Removing originals
df<-dplyr::select(df, -address, -state)
```

```{r}
#Fixing right Skew
library(tidyverse)

df<-mutate(df, annual_inc=log(annual_inc+0.1),
           delinq_2yrs=log(delinq_2yrs+0.1),
           mort_acc=log(mort_acc+0.1), 
           revol_util=log(revol_util+0.1),
           revol_bal=log(revol_bal+0.1),
           open_acc=log(open_acc+0.1),
           total_acc=log(total_acc+0.1))

```



```{r}
#Creating dummies
df<-dummy_columns(df, select_columns = c("term", "grade", "sub_grade", "emp_title", "emp_length", "home_ownership", "verification_status", "initial_list_status", "purpose", "M_mort_acc", "inq_risk_category", "Installment_to_MoInc_Ratio", "region" ,"yr_earliest_cr_line", "mo_issue_d", "yr_issue_d"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
```

```{r}
saveRDS(df, "Final_Loan.data.clean.RDS")
```






