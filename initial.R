# reading csv files
appl_test <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\application_test.csv")
appl_train <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\application_train.csv")
bureau <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\bureau.csv")
bureau_balance <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\bureau_balance.csv")
cr_card_balance <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\credit_card_balance.csv")
installments_payments <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\installments_payments.csv")
PC_balance <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\POS_CASH_balance.csv")
previous_application <- read.csv(
  "F:\\Kaggle\\Home credit default risk\\csv\\previous_application.csv")

# Custom function for creating table 
# <varname><class><unique><min><1st><med><3rd><max><sd><NA>
create_table <- function(df){
  tbl<-data.frame(matrix(nrow = ncol(df),
                         ncol = 11))
  colnames(tbl) <- c("varname",
                     "class",
                     "unique",
                     "min",
                     "1st",
                     "med",
                     "3rd",
                     "max",
                     "sd",
                     "normality(p)",
                     "NA")
  norm_test_p <- ifelse(nrow(df) > 5000,function(x){
                                ks.test(unique(x),"pnorm")$p.value},
                                     function(x){
                                       shapiro.test(unique(x))$p.value
                                     })
  for(i in 1:ncol(df)){
    tbl[i,"varname"] <- colnames(df)[i]
    tbl[i,"class"] <- class(df[[i]])
    tbl[i,"unique"] <- length(unique(df[[i]]))
    tbl[i,"NA"] <- sum(is.na(df[[i]]))
    if(!(class(df[[i]]) %in% c("factor","character"))){
      qnt <- quantile(df[[i]],probs = c(.25,.5,.75), na.rm = T)
      tbl[i,"min"] <- min(df[[i]], na.rm = T)
      tbl[i,"1st"] <- qnt[1] 
      tbl[i,"med"] <- qnt[2]
      tbl[i,"3rd"] <- qnt[3]
      tbl[i,"max"] <- max(df[[i]], na.rm = T)
      tbl[i,"sd"] <- sd(df[[i]], na.rm = T)
      tbl[i,"normality(p)"] <- ifelse(norm_test_p(df[[i]]) < 0.1,"Not Normal","Normal")
    }else{
      tbl[i,"min"] <- "-"
      tbl[i,"1st"] <- "-" 
      tbl[i,"med"] <- "-"
      tbl[i,"3rd"] <- "-"
      tbl[i,"max"] <- "-"
      tbl[i,"sd"] <- "-"
      tbl[i,"normality(p)"] <- "-"
    }
  }
  write.csv(tbl,paste("F:\\Kaggle\\Home credit default risk\\csv_gen\\",
                      deparse(substitute(df)),
                      ".csv"))
}

# Creating table for application_test.csv
create_table(appl_test)

# Creating table for application_train.csv
create_table(appl_train)

# Creating table for bureau.csv
create_table(bureau)

# Creating table for bureau_balance.csv
create_table(bureau_balance)

# Creating table for credit_card_balance.csv
create_table(cr_card_balance)

# Creating table for installments_payments.csv
create_table(installments_payments)

# Creating table for POS_CASH_balance.csv
create_table(PC_balance)

# Creating table for previous_application.csv
create_table(previous_application)
