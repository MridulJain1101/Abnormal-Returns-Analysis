install.packages("expss")
install.packages("tidyverse")
install.packages("xtable")
install.packages("readxl")
install.packages("stargazer")
install.packages("maditr")
install.packages("dplyr")


library(expss)
library(tidyverse)
library(xtable)
library(readxl)
library(stargazer)
library(dplyr)
library(readr)
library(ggplot2)


######2) Data

#Import Event Dates
event_dates <- read_excel("C:/Users/forti/Downloads/R data/EVENT_DATES.xlsx")

#Import Stock Prices of 10 Firms over one year
ADM <- read_excel("C:/Users/forti/Downloads/R data/ADM.xlsx")
ALGN <- read_excel("C:/Users/forti/Downloads/R data/ALGN.xlsx")
DGX <- read_excel("C:/Users/forti/Downloads/R data/DGX.xlsx")
EG <- read_excel("C:/Users/forti/Downloads/R data/EG.xlsx")
FRT <- read_excel("C:/Users/forti/Downloads/R data/FRT.xlsx")
GILD <- read_excel("C:/Users/forti/Downloads/R data/GILD.xlsx")
HUM <- read_excel("C:/Users/forti/Downloads/R data/HUM.xlsx")
ROK <- read_excel("C:/Users/forti/Downloads/R data/ROK.xlsx")
TTWO <- read_excel("C:/Users/forti/Downloads/R data/TTWO.xlsx")
UNP <- read_excel("C:/Users/forti/Downloads/R data/UNP.xlsx")
FACTORS <- read_csv("C:/Users/forti/Downloads/R data/FACTORS.csv")
colnames(FACTORS)[1] <- "Date"
colnames(FACTORS)[2] <- "MKTRF"

# Remove NAs
ADM <- na.omit(ADM)
UNP <- na.omit(UNP)
DGX <- na.omit(DGX)
FRT <- na.omit(FRT)
TTWO <- na.omit(TTWO)
EG <- na.omit(EG)
ROK <- na.omit(ROK)
GILD <- na.omit(GILD)
HUM <- na.omit(HUM)
ALGN <- na.omit(ALGN)


# compute returns of the 10 companies
n <- nrow(UNP)
UNP[2:n,3] <- UNP[2:n,2]/UNP[1:(n-1),2] - 1
colnames(UNP) <- c("Date","UNP_Price","UNP_Return")

n <- nrow(DGX)
DGX[2:n,3] <- DGX[2:n,2]/DGX[1:(n-1),2] - 1
colnames(DGX) <- c("Date","DGX_Price","DGX_Return")

n <- nrow(FRT)
FRT[2:n,3] <- FRT[2:n,2]/FRT[1:(n-1),2] - 1
colnames(FRT) <- c("Date","FRT_Price","FRT_Return")

n <- nrow(TTWO)
TTWO[2:n,3] <- TTWO[2:n,2]/TTWO[1:(n-1),2] - 1
colnames(TTWO) <- c("Date","TTWO_Price","TTWO_Return")

n <- nrow(EG)
EG[2:n,3] <- EG[2:n,2]/EG[1:(n-1),2] - 1
colnames(EG) <- c("Date","EG_Price","EG_Return")

n <- nrow(ROK)
ROK[2:n,3] <- ROK[2:n,2]/ROK[1:(n-1),2] - 1
colnames(ROK) <- c("Date","ROK_Price","ROK_Return")

n <- nrow(GILD)
GILD[2:n,3] <- GILD[2:n,2]/GILD[1:(n-1),2] - 1
colnames(GILD) <- c("Date","GILD_Price","GILD_Return")

n <- nrow(HUM)
HUM[2:n,3] <- HUM[2:n,2]/HUM[1:(n-1),2] - 1
colnames(HUM) <- c("Date","HUM_Price","HUM_Return")

n <- nrow(ADM)
ADM[2:n,3] <- ADM[2:n,2]/ADM[1:(n-1),2] - 1
colnames(ADM) <- c("Date","ADM_Price","ADM_Return")

n <- nrow(ALGN)
ALGN[2:n,3] <- ALGN[2:n,2]/ALGN[1:(n-1),2] - 1
colnames(ALGN) <- c("Date","ALGN_Price","ALGN_Return")



n <- nrow(FACTORS)
FACTORS[ ,2:5] <- FACTORS[ ,2:5]/100

rm(n)


#Standardize Dates
#SP_500_DATA$Date <- as.Date(SP_500_DATA$Date, format = "%d.%m.%Y")
ADM$Date <- as.Date(ADM$Date, format = "%d.%m.%Y")
ALGN$Date <- as.Date(ALGN$Date, format = "%d.%m.%Y")
DGX$Date <- as.Date(DGX$Date, format = "%d.%m.%Y")
EG$Date <- as.Date(EG$Date, format = "%d.%m.%Y")
FRT$Date <- as.Date(FRT$Date, format = "%d.%m.%Y")
GILD$Date <- as.Date(GILD$Date, format = "%d.%m.%Y")
HUM$Date <- as.Date(HUM$Date, format = "%d.%m.%Y")
ROK$Date <- as.Date(ROK$Date, format = "%d.%m.%Y")
TTWO$Date <- as.Date(TTWO$Date, format = "%d.%m.%Y")
UNP$Date <- as.Date(UNP$Date, format = "%d.%m.%Y")
FACTORS$Date <- as.character(FACTORS$Date)
FACTORS$Date <- as.Date(paste0(substr(FACTORS$Date, 1, 4), "-", 
                                 substr(FACTORS$Date, 5, 6), "-", 
                                 substr(FACTORS$Date, 7, 8)))

event_dates$Date <- as.Date(event_dates$Date, format = "%d.%m.%Y") 


#merge stock price datasets with factors
DATA <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), 
               list(ADM, ALGN, DGX, EG, FRT, GILD, HUM, ROK, TTWO, UNP))
DATA <- merge(DATA, FACTORS, by="Date")
DATA <- na.omit(DATA)


#Remove prices so we only have Returns Columns

DATA$ADM_Price <- NULL
DATA$ALGN_Price <- NULL
DATA$DGX_Price <- NULL
DATA$EG_Price <- NULL
DATA$FRT_Price <- NULL
DATA$GILD_Price <- NULL
DATA$HUM_Price <- NULL
DATA$ROK_Price <- NULL
DATA$TTWO_Price <- NULL
DATA$UNP_Price <- NULL


#Find event dates in "DATA" -> find matching Dates in "event_dates" and "DATA"
matching_indices <- which(DATA$Date %in% event_dates$Date)

results <- list()  # Create an empty list to store results

for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]  # Extract event date
  indices <- which(DATA$Date == event_date)  # Find matching rows
  
  results[[i]] <- DATA[indices, ]  # Store the corresponding observations
}


#Create the Estimation and Event Windows 

estimation_start <- -250  # 250 days before event
estimation_end <- -50     # 50 days before event
event_start <- -20         # 20 days before event
event_end <- 20         # 20 days after event


estimation_windows <- list()
event_windows <- list()


# Loop through event dates to extract the event-specific windows
for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]
  firm <- event_dates$Name[i]  # Get firm name
  
  stock_col <- paste0(firm, "_Return")
  
  if (stock_col %in% colnames(DATA)) {  
    # Extract estimation window (-250 to -50 days)
    estimation_window <- DATA %>%
      filter(Date >= (event_date + estimation_start) & Date <= (event_date + estimation_end)) %>%
      select(Date, MKTRF, all_of(stock_col)) %>%
      mutate(Event = i, Firm = firm, Day = as.numeric(Date - event_date))  # Add relative day
    
    # Extract event window (-20 to +20 days)
    event_window <- DATA %>%
      filter(Date >= (event_date + event_start) & Date <= (event_date + event_end)) %>%
      select(Date, MKTRF, all_of(stock_col)) %>%
      mutate(Event = i, Firm = firm, Day = as.numeric(Date - event_date))  
    
    # List
    estimation_windows[[i]] <- estimation_window
    event_windows[[i]] <- event_window
  }
}

# Lists are now converted to data frames 
estimation_windows <- bind_rows(estimation_windows, .id = "Event")
event_windows <- bind_rows(event_windows, .id = "Event")



#########5) Cumulative Abnormal Returns

#5.1) Abnormal returns with constant mean

# List to store abnormal returns (Constant Mean Model)
abnormal_returns_mean <- list()

for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]
  firm <- event_dates$Name[i]  
  stock_col <- paste0(firm, "_Return") 
  
  if (stock_col %in% colnames(DATA)) {
    # Get estimation window (-250 to -50 days)
    estimation_window <- DATA %>%
      filter(Date >= (event_date - 250) & Date <= (event_date - 50)) %>%
      select(Date, all_of(stock_col))
    
    # Compute the mean return
    mean_return <- mean(estimation_window[[stock_col]], na.rm = TRUE)
    
    # Get event window (-20 to +20 days)
    event_window <- DATA %>%
      filter(Date >= (event_date - 20) & Date <= (event_date + 20)) %>%
      select(Date, all_of(stock_col)) %>%
      mutate(Day = as.numeric(Date - event_date))  
    
    # Compute abnormal returns
    event_window$expected_return <- mean_return
    event_window$ar_mean <- event_window[[stock_col]] - event_window$expected_return
    
    # Store abnormal returns with firm ticker and event number
    event_window$Firm <- firm
    event_window$Event <- i  # Mark which event this belongs to
    
    abnormal_returns_mean[[i]] <- event_window
  }
}
# Convert list to data frame
abnormal_returns_mean <- bind_rows(abnormal_returns_mean)


# Calculate CARs of constant mean method

CAR_constant_mean <- abnormal_returns_mean %>%
  group_by(Firm, Event) %>%
  arrange(Date) %>%
  mutate(ar = as.numeric(ar_mean),  # Ensure AR is numeric
         CAR = cumsum(ar)) %>%  # Compute cumulative sum
  ungroup()


#Plot Cumulative Returns of the 10 companies with constant mean method
ggplot(CAR_constant_mean, aes(x = Day, y = CAR, color = Firm, group = interaction(Firm, Event))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Firm-individual Cumulative Abnormal Returns (CAR) - Constant Mean",
       x = "Days Relative to Event",
       y = "CAR (constant mean)") +
  theme_minimal() +
  theme(legend.title = element_blank())


#5.2) Abnormal returns with market model

# List to store abnormal returns
abnormal_returns_market <- list()


#Run for loop to recieve the event-specific abnormal returns
for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]
  firm <- event_dates$Name[i]  
  
  stock_col <- paste0(firm, "_Return")  
  
  if (stock_col %in% colnames(DATA)) {
    # Get estimation window (-250 to -50 days)
    estimation_window <- DATA %>%
      filter(Date >= (event_date - 250) & Date <= (event_date - 50)) %>%
      select(Date, MKTRF, all_of(stock_col))
    
    # Get event window (-20 to +20 days)
    event_window <- DATA %>%
      filter(Date >= (event_date - 20) & Date <= (event_date + 20)) %>%
      select(Date, MKTRF, all_of(stock_col)) %>%
      mutate(Day = as.numeric(Date - event_date))  
    
    # Run regression (Market Model) in the estimation window
    reg1 <- lm(as.formula(paste(stock_col, "~ MKTRF")), data = estimation_window)
    
    # Get alpha and beta
    alpha <- coef(reg1)[1]
    beta <- coef(reg1)[2]
    
    # Predict normal returns in the event window
    event_window$expected_return <- alpha + beta * event_window$MKTRF
    
    # Compute abnormal return
    event_window$ar_market <- event_window[[stock_col]] - event_window$expected_return
    
    # Store abnormal returns with firm ticker and event number
    event_window$Firm <- firm
    event_window$Event <- i  
    
    abnormal_returns_market[[i]] <- event_window
  }
}
# Convert list to data frame
abnormal_returns_market <- bind_rows(abnormal_returns_market)


# Calculate Cumulative Abnormal Return for Market Model Method
CAR_market <- abnormal_returns_market %>%
  group_by(Firm, Event) %>%
  arrange(Date) %>%
  mutate(ar = as.numeric(ar_market),  
         CAR = cumsum(ar)) %>%  
  ungroup()

#Plot Cumulative Returns of the 10 companies with market method
ggplot(CAR_market, aes(x = Day, y = CAR, color = Firm, group = interaction(Firm, Event))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Event date marker
  labs(title = "Firm-individual Cumulative Abnormal Returns (CAR) - Market Model",
       x = "Days Relative to Event",
       y = "CAR (market model)") +
  theme_minimal() +
  theme(legend.title = element_blank())


#5.3) Abnormal returns with Fama&French model
# List to store abnormal returns (Fama-French Model)
abnormal_returns_ff <- list()

for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]
  firm <- event_dates$Name[i]  
  stock_col <- paste0(firm, "_Return")  
  
  if (stock_col %in% colnames(DATA)) {
    # Get estimation window (-250 to -50 days)
    estimation_window <- DATA %>%
      filter(Date >= (event_date - 250) & Date <= (event_date - 50)) %>%
      select(Date, MKTRF, SMB, HML, all_of(stock_col))
    
    # Get event window (-20 to +20 days)
    event_window <- DATA %>%
      filter(Date >= (event_date - 20) & Date <= (event_date + 20)) %>%
      select(Date, MKTRF, SMB, HML, all_of(stock_col)) %>%
      mutate(Day = as.numeric(Date - event_date))  
    
    # Run regression (Fama-French Model)
    reg_ff <- lm(as.formula(paste(stock_col, "~ MKTRF + SMB + HML")), data = estimation_window)
    
    # Get coefficients
    alpha_ff <- coef(reg_ff)[1]
    beta_mktrf <- coef(reg_ff)["MKTRF"]
    beta_smb <- coef(reg_ff)["SMB"]
    beta_hml <- coef(reg_ff)["HML"]
    
    # Predict normal returns in the event window
    event_window$expected_return <- alpha_ff + beta_mktrf * event_window$MKTRF +
      beta_smb * event_window$SMB + beta_hml * event_window$HML
    
    # Compute abnormal returns
    event_window$ar_ff <- event_window[[stock_col]] - event_window$expected_return
    
    # Store abnormal returns with firm ticker and event number
    event_window$Firm <- firm
    event_window$Event <- i  
    
    abnormal_returns_ff[[i]] <- event_window
  }
}

# Convert list to data frame
abnormal_returns_ff <- bind_rows(abnormal_returns_ff)

# Calculate Cumulative Abnormal Return for Fama&French Model 
CAR_ff <- abnormal_returns_ff %>%
  group_by(Firm, Event) %>%
  arrange(Date) %>%
  mutate(ar = as.numeric(ar_ff),  
         CAR = cumsum(ar)) %>%  
  ungroup()

#Plot Cumulative Returns of the 10 companies with Fama&French Model
ggplot(CAR_ff, aes(x = Day, y = CAR, color = Firm, group = interaction(Firm, Event))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Firm-individual Cumulative Abnormal Returns (CAR) - Fama&French Model",
       x = "Days Relative to Event",
       y = "CAR (Fama&French model)") +
  theme_minimal() +
  theme(legend.title = element_blank())


##### Statistical significance


# List for Residuals
residual_sd_list <- list()

# Loop through each firm and event date
for (i in 1:nrow(event_dates)) {
  event_date <- event_dates$Date[i]
  firm <- event_dates$Name[i]  
  stock_col <- paste0(firm, "_Return")  # Stock return column
  
  if (stock_col %in% colnames(DATA)) {
    
    # Define estimation window (-250 to -50 days)
    estimation_window <- DATA %>%
      filter(Date >= (event_date - 250) & Date <= (event_date - 50)) %>%
      select(Date, MKTRF, SMB, HML, all_of(stock_col))  # Include Fama-French factors
    
    # 1. Constant Mean Model (AR = Stock Return - Mean Return)
    constant_mean <- mean(estimation_window[[stock_col]], na.rm = TRUE)
    residual_sd_list[[firm]]$CM <- sd(estimation_window[[stock_col]] - mean_return, na.rm = TRUE)
    
    # 2. Market Model (AR = Stock Return - Market Return Beta)
    market_model <- lm(as.formula(paste(stock_col, "~ MKTRF")), data = estimation_window)
    residual_sd_list[[firm]]$MM <- sd(market_model$residuals, na.rm = TRUE)
    
    # 3. Fama-French Model (AR = Stock Return - Predicted Return using MKTRF, SMB, HML)
    fama_french_model <- lm(as.formula(paste(stock_col, "~ MKTRF + SMB + HML")), data = estimation_window)
    residual_sd_list[[firm]]$FF <- sd(fama_french_model$residuals, na.rm = TRUE)
  }
}
# p-values for all three models
abnormal_returns_mean <- abnormal_returns_mean %>%
  rowwise() %>%
  mutate(
    AR_tstat_mean = ar_mean / residual_sd_list[[Firm]]$CM,  
    AR_pval_mean = 2 * pt(q = abs(AR_tstat_mean), df = 198, lower.tail = FALSE)
  )

abnormal_returns_market <- abnormal_returns_market %>%
  rowwise() %>%
  mutate(
    AR_tstat_market = ar_market / residual_sd_list[[Firm]]$MM,  # Market Model
    AR_pval_market = 2 * pt(q = abs(AR_tstat_market), df = 198, lower.tail = FALSE)
  )

abnormal_returns_ff <- abnormal_returns_ff %>%
  rowwise() %>%
  mutate(
    AR_tstat_ff = ar_ff / residual_sd_list[[Firm]]$FF,  # Fama-French Model
    AR_pval_ff = 2 * pt(q = abs(AR_tstat_ff), df = 198, lower.tail = FALSE)
  )

# Print out pvalues of Day 0 (Event Day) for each model
pvalues_day0_mean <- abnormal_returns_mean %>%
  filter(Day == 0) %>%
  summarise(avg_pval_mean = mean(AR_pval_mean, na.rm = TRUE))

pvalues_day0_market <- abnormal_returns_market %>%
  filter(Day == 0) %>%
  summarise(avg_pval_market = mean(AR_pval_market, na.rm = TRUE))

pvalues_day0_ff <- abnormal_returns_ff %>%
  filter(Day == 0) %>%
  summarise(avg_pval_ff = mean(AR_pval_ff, na.rm = TRUE)) 

# Print out pvalues of Day 1 (Event Day +1 ) for each model
pvalues_day1_mean <- abnormal_returns_mean %>%
  filter(Day == 1) %>%
  summarise(avg_pval_mean = mean(AR_pval_mean, na.rm = TRUE))

pvalues_day1_market <- abnormal_returns_market %>%
  filter(Day == 1) %>%
  summarise(avg_pval_market = mean(AR_pval_market, na.rm = TRUE))

pvalues_day1_ff <- abnormal_returns_ff %>%
  filter(Day == 1) %>%
  summarise(avg_pval_ff = mean(AR_pval_ff, na.rm = TRUE)) 


### CAR statistical significance

residual_sd_CM_vec <- sapply(residual_sd_list, function(x) x$CM)  # Constant Mean Model
residual_sd_MM_vec <- sapply(residual_sd_list, function(x) x$MM)  # Market Model
residual_sd_FF_vec <- sapply(residual_sd_list, function(x) x$FF)  # Fama-French Model

# Compute CAR significance for Constant Mean Model
CAR_constant_mean <- CAR_constant_mean %>%
  group_by(Firm, Event) %>%
  mutate(
    CAR_tstat = CAR / (sqrt(n()) * residual_sd_CM_vec[Firm]),   # Extract using firm name
    CAR_pval = 2 * pt(q = abs(CAR_tstat), df = 198, lower.tail = FALSE)
  ) %>%
  ungroup()

# Compute CAR significance for Market Model
CAR_market <- CAR_market %>%
  group_by(Firm, Event) %>%
  mutate(
    CAR_tstat = CAR / (sqrt(n()) * residual_sd_MM_vec[Firm]),
    CAR_pval = 2 * pt(q = abs(CAR_tstat), df = 198, lower.tail = FALSE)
  ) %>%
  ungroup()

# Compute CAR significance for Fama-French Model
CAR_ff <- CAR_ff %>%
  group_by(Firm, Event) %>%
  mutate(
    CAR_tstat = CAR / (sqrt(n()) * residual_sd_FF_vec[Firm]),
    CAR_pval = 2 * pt(q = abs(CAR_tstat), df = 198, lower.tail = FALSE)
  ) %>%
  ungroup()

# Compute last-day CAR significance for Constant Mean
CAR_last_day_mean <- CAR_constant_mean %>%
  group_by(Firm, Event) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Compute last-day CAR significance for Market Model
CAR_last_day_market <- CAR_market %>%
  group_by(Firm, Event) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Compute last-day CAR significance for Fama-French Model
CAR_last_day_ff <- CAR_ff %>%
  group_by(Firm, Event) %>%
  slice_tail(n = 1) %>%
  ungroup()


###6) Average Cumulative Abnormal Returns for the market model


# Split firms with positive and negative earnings surprise (from looking at the CAR plots)
positive_firms <- c("TTWO", "GILD", "DGX", "ADM", "ALGN", "HUM")
negative_firms <- c("UNP", "EG", "FRT", "ROK")

#Firstly, as weekends fall differently for the different companies, to ensure that there are 10 observations per day, we fill the CARs on the weekends with the last known CAR

#Create a function that adds the missing CARs
fill_missing_CAR <- function(data, firms) {
  data %>%
    filter(Firm %in% firms) %>%
    arrange(Firm, Day) %>%  # Ensure data is ordered by firm and day
    group_by(Firm) %>%
    complete(Day = full_seq(Day, 1)) %>%  # Ensure all days are represented
    fill(CAR, .direction = "down") %>%  # Fill missing CAR with the last available value
    ungroup()
}

# We apply this function to handle missing days
CAR_market_filled <- fill_missing_CAR(CAR_market, c(positive_firms, negative_firms))

# Compute ACAR for positive firms
ACAR_positive <- CAR_market_filled %>%
  filter(Firm %in% positive_firms) %>%
  group_by(Day) %>%
  summarize(ACAR = mean(CAR, na.rm = TRUE)) %>%
  mutate(Group = "Positive Surprise")

# Compute ACAR for negative firms
ACAR_negative <- CAR_market_filled %>%
  filter(Firm %in% negative_firms) %>%
  group_by(Day) %>%
  summarize(ACAR = mean(CAR, na.rm = TRUE)) %>%
  mutate(Group = "Negative Surprise")

# Combine both datasets
ACAR_df <- bind_rows(ACAR_positive, ACAR_negative)

# Plot ACAR for both groups
ggplot(ACAR_df, aes(x = Day, y = ACAR, color = Group, group = Group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Average Cumulative Abnormal Return (ACAR) - Market Model",
       x = "Days Relative to Event",
       y = "ACAR") +
  scale_color_manual(values = c("Positive Surprise" = "blue", "Negative Surprise" = "orange")) +  
  theme_minimal() +
  theme(legend.title = element_blank())

print(xtable(ACAR_df, type='latex', digits = 3)) 


