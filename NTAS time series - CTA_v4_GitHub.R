library(ggplot2)
library(car)
library(lubridate)
library(forecast)
library(urca)
library(dplyr)
library(scales)

rm(list = ls())
setwd("") # change working directory as needed

# ====================== functions ======================
# function: arima grid search
arima_grid_search <- function(pre_int) {
  results_summary <- matrix(nrow = 6^3, ncol = 4)
  
  for (p in 0:5) {
    for (d in 0:5) {
      for (q in 0:5) {
        tryCatch({
          fit <- arima(pre_int, order=c(p,d,q))
          MAPE <- mean(abs(fit$residuals/pre_int))
          print(c(p, d, q, MAPE))
          
          results_summary[(p*36+d*6+q),1] <- p
          results_summary[(p*36+d*6+q),2] <- d
          results_summary[(p*36+d*6+q),3] <- q
          results_summary[(p*36+d*6+q),4] <- MAPE
          
        }, warning = function(w) {
          message(paste("Warning at index", c(p, d, q), ":", w$message))
          NA
          
          
        }, error = function(e) {
          message(paste("Error at index", c(p, d, q), ":", e$message))
          NA
        })
      }
    }
  }
  
  results_summary <- as.data.frame(results_summary)
  names(results_summary) <- c('p', 'd', 'q', 'MAPE')
  
  return(results_summary)
  
}

# function: formatting arima results
arima_print <- function(arima_fit) {
  coef <- arima_fit$coef
  var <- arima_fit$var.coef
  se <- sqrt(diag(var))
  model_summary <- rbind(coef, se, coef/se, dt(coef/se, df=Inf))
  model_summary <- t(model_summary)
  model_summary <- as.data.frame(model_summary)
  names(model_summary) <- c('coef', 'se', 'z', 'p')
  model_summary$sig <- car::recode(model_summary$p, "lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*';  0.05:0.1='.'; 0.1:hi=' '")
  model_summary
}

# ====================== CTA raw ======================
cta_ridership <- read.csv("Data/CTA/CTA_-_Ridership_-_Daily_Boarding_Totals_20240201.csv") # change working directory as needed

# date recoding
cta_ridership$day_type <- car::recode(cta_ridership$day_type, "'W'='Weekday'; 'A'='Saturday'; 'U'='Sunday/Holiday'")
cta_ridership$day_type <- factor(cta_ridership$day_type, levels = c('Weekday', 'Saturday', 'Sunday/Holiday'))
cta_ridership$service_date <- as.Date.character(cta_ridership$service_date, tryFormats = '%m/%d/%Y')
cta_ridership$day_of_week <- weekdays(cta_ridership$service_date)
cta_ridership$day_of_week <- factor(cta_ridership$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# only focus on April 01, 2020 onward data
cond_date <- cta_ridership$service_date > "2020-04-01"
cta_ridership <- cta_ridership[cond_date, ]
startdate <- as.Date.character("05/28/2021", tryFormats = '%m/%d/%Y')

# ====================== CTA raw plotting ======================
cond_date <- cta_ridership$service_date < "2021-08-20"

# total ridership, Mon. - Sun.
ggplot(cta_ridership[cond_date,], aes(x = service_date , y = total_rides, group=day_of_week)) +
  geom_line(aes(color=day_of_week), size = 1.2) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "CTA Total Ridership",x = "Time", y = "Ridership") +
  scale_color_brewer(palette = "Set2", name = "Day type") + 
  geom_vline(xintercept = startdate, linetype="dotted", size = 1.2) +  
  annotate("text", x = startdate, y = 200000, label = "Pass Discount Program", size = 4) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        plot.title = element_text(face="bold", hjust = 0.5, size=14),
        plot.subtitle = element_text(face="bold", hjust = 0.5, size=12),
        legend.position = 'bottom',
        legend.box = "vertical",
        legend.box.spacing = unit(0.7, 'cm'),
        legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm"), 
        legend.key.size = unit(2,"line"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=12),
        axis.title.y = element_text(face="bold", size=12),
        axis.title.x = element_text(face="bold", size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"))
ggsave("Figure/CTA_ridership_total_detailed.png", width = 6, height = 6) # change working directory as needed


# ====================== CTA weekly ridership ======================
cta_ridership$year <- year(cta_ridership$service_date)
cta_ridership$week <- isoweek(cta_ridership$service_date)

week(cta_ridership$service_date) == isoweek(cta_ridership$service_date)

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
cta_ridership$day_type_cta <- cta_ridership$day_type
cta_ridership$day_type <- ifelse(cta_ridership$day_of_week %in% weekday, "Weekday", as.character(cta_ridership$day_of_week))
cta_ridership$day_type <- factor(cta_ridership$day_type, levels = c('Weekday', 'Saturday', 'Sunday'))

# need adjust the 12/31 year and week
cta_ridership$year_adj <- cta_ridership$year
cta_ridership$year_adj <- ifelse(cta_ridership$week >= 52 & month(cta_ridership$service_date)==1 & 
                                   day(cta_ridership$service_date)<=7,  cta_ridership$year-1, cta_ridership$year)

cta_ridership_weekly_NO_HLD <- 
  cta_ridership %>% 
  dplyr::filter(! (day_type_cta=="Sunday/Holiday" & day_type=="Weekday") ) %>% 
  dplyr::group_by(year_adj, week, day_type)%>% 
  dplyr::summarize(mean(bus), mean(rail_boardings), mean(total_rides))
cta_ridership_weekly_NO_HLD <- as.data.frame(cta_ridership_weekly_NO_HLD)

cta_ridership_weekly_NO_HLD$date <- ISOweek::ISOweek2date(paste(cta_ridership_weekly_NO_HLD$year, 
                                                                paste('W', ifelse(cta_ridership_weekly_NO_HLD$week<10, paste(0,cta_ridership_weekly_NO_HLD$week, sep=''), 
                                                                                  cta_ridership_weekly_NO_HLD$week), sep = ''), 
                                                                1, sep = '-'))

names(cta_ridership_weekly_NO_HLD) <- c('year', 'week', 'day_type', 'bus_mean', 'rail_boardings_mean', 'total_rides_mean', 'date')


# ====================== CTA weekly plots, holiday excluded ======================
cond_date <- cta_ridership_weekly_NO_HLD$date < "2021-08-20"

ggplot(cta_ridership_weekly_NO_HLD[cond_date,], aes(x = date , y = total_rides_mean, group=day_type)) +
  geom_line(aes(color=day_type), size = 1.2) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "CTA Total Ridership (Weekly)", x = "Time", y = "Ridership") +
  scale_color_brewer(palette = "Set2", name = "Day type") + 
  geom_vline(xintercept = startdate, linetype="dotted", size = 1.2) +  
  annotate("text", x = startdate, y = 200000, label = "Pass Discount Program", size = 4) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        plot.title = element_text(face="bold", hjust = 0.5, size=14),
        plot.subtitle = element_text(face="bold", hjust = 0.5, size=12),
        legend.position = 'bottom',
        legend.box = "vertical",
        legend.box.spacing = unit(0.7, 'cm'),
        legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm"), 
        legend.key.size = unit(2,"line"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=12),
        axis.title.y = element_text(face="bold", size=12),
        axis.title.x = element_text(face="bold", size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"))
ggsave("Figure/CTA_ridership_total_weekly_excl_holidays.png", width = 6, height = 6) # change working directory as needed



# ====================== CTA ARIMA weekday ======================
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Weekday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-05-28"]

acf(pre_int)
acf(diff(pre_int, 1))
acf(diff(pre_int, 2))
acf(diff(pre_int, 3))

pacf(pre_int)
pacf(diff(pre_int, 1))
pacf(diff(pre_int, 2))
pacf(diff(pre_int, 3))

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search for the best ARIMA specification
results_summary <- arima_grid_search(pre_int)
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top

# best model
(fit <- arima(pre_int, order=c(5,1,3)))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Weekday_ARIMA513.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check: ACF and Ljung-Box test on the residuals
png("Figure/CTA_ridership_total_weekday_ARIMA513_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()

# forecast 12 weeks after the intervention
post_int_prd <- forecast(fit, h=12)
autoplot(post_int_prd)

summary(post_int_prd)
post_int_prd$mean[1:12]
post_int_obs <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Weekday' & 
                                                               cta_ridership_weekly_NO_HLD$date >= "2021-05-28"]
pct_change <- (post_int_obs[1:12] - post_int_prd$mean[1:12])/post_int_prd$mean[1:12]
(post_int_summary <- cbind.data.frame(post_int_obs[1:12], post_int_prd$mean[1:12], pct_change))
write.csv(post_int_summary, "Table/CTA_Weekday_prediction.csv") # change working directory as needed

# paired t-test
t.test(post_int_obs[1:12], post_int_prd$mean[1:12], paired = TRUE, alternative = c('greater'))

# visualization
pred <- c(fitted(fit), post_int_prd$mean[1:12])
length(pred)
obs <- cta_ridership_weekly_NO_HLD[cta_ridership_weekly_NO_HLD$day_type=='Weekday',]
obs <- obs[1:length(pred),]

obs <- obs[,c('date', 'total_rides_mean')]
names(obs)[2] <- 'count'
obs$Ridership <- 'Observed'

pred <- cbind.data.frame(obs$date, pred)
names(pred) <- c('date', 'count')
pred$Ridership <- 'Predicted'
pred$Ridership[1:(nrow(pred)-12)] <- 'Fitted'

df_comb <- rbind.data.frame(obs, pred)
df_comb$Ridership <- factor(df_comb$Ridership , levels = c('Observed', 'Fitted', 'Predicted'))

ggplot(df_comb, aes(x=date, y= count, color = Ridership)) + 
  geom_line(size = 1.2) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_color_manual(values = c("Fitted" = "#e76f51", "Observed" = "#d6ccc2", "Predicted" = "#2a9d8f")) + 
  geom_vline(xintercept = startdate, linetype="dotted", size = 1.2) +  
  annotate("text", x = startdate, y = 300000, label = "Pass Discount Program", size = 4) + 
  labs(title = "CTA Total Ridership (Weekday)", x = "Time", y = "Ridership") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        plot.title = element_text(face="bold", hjust = 0.5, size=14),
        plot.subtitle = element_text(face="bold", hjust = 0.5, size=12),
        legend.position = 'bottom',
        legend.box = "vertical",
        legend.box.spacing = unit(0.7, 'cm'),
        legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm"), 
        legend.key.size = unit(2,"line"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=12),
        axis.title.y = element_text(face="bold", size=12),
        axis.title.x = element_text(face="bold", size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"))
ggsave("Figure/CTA_ridership_total_weekday_ARIMA513.png", width = 6, height = 6) # change working directory as needed

## additional step: include a dummy indicator
pre_int_length <- length(cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Weekday' & 
                                                                        cta_ridership_weekly_NO_HLD$date < "2021-05-28"])
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Weekday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-08-20"]
int_indicator <- c(rep(0, pre_int_length), rep(1, (length(pre_int) - pre_int_length)))

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search, indicator included
results_summary <- matrix(nrow = 6^3, ncol = 4)
for (p in 0:5) {
  for (d in 0:5) {
    for (q in 0:5) {
      tryCatch({
        fit <- arima(pre_int, order=c(p,d,q), xreg = int_indicator)
        MAPE <- mean(abs(fit$residuals/pre_int))
        print(c(p, d, q, MAPE))
        
        results_summary[(p*36+d*6+q),1] <- p
        results_summary[(p*36+d*6+q),2] <- d
        results_summary[(p*36+d*6+q),3] <- q
        results_summary[(p*36+d*6+q),4] <- MAPE
        
      }, warning = function(w) {
        message(paste("Warning at index", c(p, d, q), ":", w$message))
        NA
        
      }, error = function(e) {
        message(paste("Error at index", c(p, d, q), ":", e$message))
        NA
      })
    }
  }
}


results_summary <- as.data.frame(results_summary)
names(results_summary) <- c('p', 'd', 'q', 'MAPE')
# results_summary <- results_summary[results_summary$d>0 & results_summary$d<3,]
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top

# best model
(fit <- arima(pre_int, order=c(5,1,3), xreg = int_indicator))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Weekday_ARIMA513.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check
png("Figure/CTA_ridership_total_weekday_ARIMA513_indicator_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()



# ====================== CTA ARIMA Saturday ======================
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Saturday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-05-28"]

acf(pre_int)
acf(diff(pre_int, 1))
acf(diff(pre_int, 2))
acf(diff(pre_int, 3))

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search for the best ARIMA specification
results_summary <- arima_grid_search(pre_int)
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top

# best model
(fit <- arima(pre_int, order=c(4,1,3)))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Saturday_ARIMA413.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check: ACF and Ljung-Box test on the residuals
png("Figure/CTA_ridership_total_Saturday_ARIMA413_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()

# forecast 12 weeks after the intervention
post_int_prd <- forecast(fit, h=12)
autoplot(post_int_prd)

summary(post_int_prd)
post_int_prd$mean[1:12]
post_int_obs <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Saturday' & 
                                                               cta_ridership_weekly_NO_HLD$date >= "2021-05-28"]
pct_change <- (post_int_obs[1:12] - post_int_prd$mean[1:12])/post_int_prd$mean[1:12]
(post_int_summary <- cbind.data.frame(post_int_obs[1:12], post_int_prd$mean[1:12], pct_change))
write.csv(post_int_summary, "Table/CTA_Saturday_prediction.csv") # change working directory as needed

# paired t-test
t.test(post_int_obs[1:12], post_int_prd$mean[1:12], paired = TRUE, alternative = c('greater'))

# visualization
pred <- c(fitted(fit), post_int_prd$mean[1:12])
length(pred)
obs <- cta_ridership_weekly_NO_HLD[cta_ridership_weekly_NO_HLD$day_type=='Saturday',]
obs <- obs[1:length(pred),]

obs <- obs[,c('date', 'total_rides_mean')]
names(obs)[2] <- 'count'
obs$Ridership <- 'Observed'

pred <- cbind.data.frame(obs$date, pred)
names(pred) <- c('date', 'count')
pred$Ridership <- 'Predicted'
pred$Ridership[1:(nrow(pred)-12)] <- 'Fitted'

df_comb <- rbind.data.frame(obs, pred)
df_comb$Ridership <- factor(df_comb$Ridership , levels = c('Observed', 'Fitted', 'Predicted'))

ggplot(df_comb, aes(x=date, y= count, color = Ridership)) + 
  geom_line(size = 1.2) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_color_manual(values = c("Fitted" = "#e76f51", "Observed" = "#d6ccc2", "Predicted" = "#2a9d8f")) +  
  geom_vline(xintercept = startdate, linetype="dotted", size = 1.2) +  
  annotate("text", x = startdate, y = 300000, label = "Pass Discount Program", size = 4) + 
  labs(title = "CTA Total Ridership (Saturday)", x = "Time", y = "Ridership") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        plot.title = element_text(face="bold", hjust = 0.5, size=14),
        plot.subtitle = element_text(face="bold", hjust = 0.5, size=12),
        legend.position = 'bottom',
        legend.box = "vertical",
        legend.box.spacing = unit(0.7, 'cm'),
        legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm"), 
        legend.key.size = unit(2,"line"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=12),
        axis.title.y = element_text(face="bold", size=12),
        axis.title.x = element_text(face="bold", size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"))
ggsave("Figure/CTA_ridership_total_Saturday_ARIMA413.png", width = 6, height = 6)


## additional step: include a dummy indicator
pre_int_length <- length(cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Saturday' & 
                                                                        cta_ridership_weekly_NO_HLD$date < "2021-05-28"])
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Saturday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-08-20"]
int_indicator <- c(rep(0, pre_int_length), rep(1, (length(pre_int) - pre_int_length)))

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search, indicator included
results_summary <- matrix(nrow = 6^3, ncol = 4)
for (p in 0:5) {
  for (d in 0:5) {
    for (q in 0:5) {
      tryCatch({
        fit <- arima(pre_int, order=c(p,d,q), xreg = int_indicator)
        MAPE <- mean(abs(fit$residuals/pre_int))
        print(c(p, d, q, MAPE))
        
        results_summary[(p*36+d*6+q),1] <- p
        results_summary[(p*36+d*6+q),2] <- d
        results_summary[(p*36+d*6+q),3] <- q
        results_summary[(p*36+d*6+q),4] <- MAPE
        
      }, warning = function(w) {
        message(paste("Warning at index", c(p, d, q), ":", w$message))
        NA
        
      }, error = function(e) {
        message(paste("Error at index", c(p, d, q), ":", e$message))
        NA
      })
    }
  }
}

results_summary <- as.data.frame(results_summary)
names(results_summary) <- c('p', 'd', 'q', 'MAPE')
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top

# best model
auto.arima(pre_int, xreg = int_indicator)
(fit <- auto.arima(pre_int, xreg = int_indicator))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Saturday_ARIMA110.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check
png("Figure/CTA_ridership_total_Saturday_ARIMA110_indicator_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()


# ====================== CTA ARIMA Sunday ======================
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Sunday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-05-28"]

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search for the best ARIMA specification
results_summary <- arima_grid_search(pre_int)
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top

# best model
(fit <- arima(pre_int, order=c(2,1,1)))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Sunday_ARIMA211.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check: ACF and Ljung-Box test on the residuals
png("Figure/CTA_ridership_total_Sunday_ARIMA211_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()

# forecast 12 weeks after the intervention
post_int_prd <- forecast(fit, h=12)
autoplot(post_int_prd)

summary(post_int_prd)
post_int_prd$mean[1:12]
post_int_obs <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Sunday' & 
                                                               cta_ridership_weekly_NO_HLD$date >= "2021-05-28"]
pct_change <- (post_int_obs[1:12] - post_int_prd$mean[1:12])/post_int_prd$mean[1:12]
(post_int_summary <- cbind.data.frame(post_int_obs[1:12], post_int_prd$mean[1:12], pct_change))
write.csv(post_int_summary, "Table/CTA_Sunday_prediction.csv") # change working directory as needed

# paired t-test
t.test(post_int_obs[1:12], post_int_prd$mean[1:12], paired = TRUE, alternative = c('greater'))

# visualization
pred <- c(fitted(fit), post_int_prd$mean[1:12])
length(pred)
obs <- cta_ridership_weekly_NO_HLD[cta_ridership_weekly_NO_HLD$day_type=='Sunday',]
obs <- obs[1:length(pred),]

obs <- obs[,c('date', 'total_rides_mean')]
names(obs)[2] <- 'count'
obs$Ridership <- 'Observed'

pred <- cbind.data.frame(obs$date, pred)
names(pred) <- c('date', 'count')
pred$Ridership <- 'Predicted'
pred$Ridership[1:(nrow(pred)-12)] <- 'Fitted'

df_comb <- rbind.data.frame(obs, pred)
df_comb$Ridership <- factor(df_comb$Ridership , levels = c('Observed', 'Fitted', 'Predicted'))

ggplot(df_comb, aes(x=date, y= count, color = Ridership)) + 
  geom_line(size = 1.2) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_color_manual(values = c("Fitted" = "#e76f51", "Observed" = "#d6ccc2", "Predicted" = "#2a9d8f")) + 
  geom_vline(xintercept = startdate, linetype="dotted", size = 1.2) +  
  annotate("text", x = startdate, y = 200000, label = "Pass Discount Program", size = 4) + 
  labs(title = "CTA Total Ridership (Sunday)", x = "Time", y = "Ridership") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        plot.title = element_text(face="bold", hjust = 0.5, size=14),
        plot.subtitle = element_text(face="bold", hjust = 0.5, size=12),
        legend.position = 'bottom',
        legend.box = "vertical",
        legend.box.spacing = unit(0.7, 'cm'),
        legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm"), 
        legend.key.size = unit(2,"line"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=12),
        axis.title.y = element_text(face="bold", size=12),
        axis.title.x = element_text(face="bold", size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"))
ggsave("Figure/CTA_ridership_total_Sunday_ARIMA211.png", width = 6, height = 6) # change working directory as needed


## additional step: include a dummy indicator
pre_int_length <- length(cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Sunday' & 
                                                                        cta_ridership_weekly_NO_HLD$date < "2021-05-28"])
pre_int <- cta_ridership_weekly_NO_HLD$total_rides_mean[cta_ridership_weekly_NO_HLD$day_type=='Sunday' & 
                                                          cta_ridership_weekly_NO_HLD$date < "2021-08-20"]
int_indicator <- c(rep(0, pre_int_length), rep(1, (length(pre_int) - pre_int_length)))

# null hypothesis: data are stationary, test stats are larger than the critical value, reject
pre_int %>% ur.kpss() %>% summary() 
pre_int %>% diff(1) %>% ur.kpss() %>% summary()
pre_int %>% diff(2) %>% ur.kpss() %>% summary()

# grid search, indicator included
results_summary <- matrix(nrow = 6^3, ncol = 4)
for (p in 0:5) {
  for (d in 0:5) {
    for (q in 0:5) {
      tryCatch({
        fit <- arima(pre_int, order=c(p,d,q), xreg = int_indicator)
        MAPE <- mean(abs(fit$residuals/pre_int))
        print(c(p, d, q, MAPE))
        
        results_summary[(p*36+d*6+q),1] <- p
        results_summary[(p*36+d*6+q),2] <- d
        results_summary[(p*36+d*6+q),3] <- q
        results_summary[(p*36+d*6+q),4] <- MAPE
        
      }, warning = function(w) {
        message(paste("Warning at index", c(p, d, q), ":", w$message))
        NA
        
      }, error = function(e) {
        message(paste("Error at index", c(p, d, q), ":", e$message))
        NA
      })
    }
  }
}

results_summary <- as.data.frame(results_summary)
names(results_summary) <- c('p', 'd', 'q', 'MAPE')
results_summary <- results_summary[results_summary$d>0,]
results_summary[order(results_summary$MAPE),] # testing models from the top


# best model
auto.arima(pre_int, xreg = int_indicator)
(fit <- auto.arima(pre_int, xreg = int_indicator))
arima_print(fit)
sum(fit$coef[1:fit$arma[1]]) # AR
sum(fit$coef[(fit$arma[1]+1):(fit$arma[1]+fit$arma[2])]) #MA
print(checkresiduals(fit))
fit$nobs
write.csv(arima_print(fit), "Table/CTA_Sunday_ARIMA110_indicator.csv") # change working directory as needed

# model fit
mean(abs(fit$residuals/pre_int))

# post-modeling quality check
png("Figure/CTA_ridership_total_Sunday_ARIMA110_indicator_check.png",  width = 1200, height = 900) # change working directory as needed
print(checkresiduals(fit))
dev.off()



##

