
structure_date <-function(dafra='flowdata',S_Day=1,S_Month=4,S_Year=7){
	if(!is.element(dafra,objects(pos=1))){stop("No river flow data named 'flowdata' was found in the workspace. Use data(flowdata) to create an example dataset or provide one with the correct format and name. See help(flowdata) for more info. Date format must be dd/mm/yyyy")} 
  dataframe<-get(dafra)
	if(names(dataframe)[1]!='Date' | names(dataframe)[2]!='Flow'){stop("Input data does not meet the required format. See help(flowdata) as an example.")}
  Ye <- substr(dataframe$Date, start = S_Year, stop = (S_Year+3))
  Mo <- substr(dataframe$Date, start = S_Month, stop = (S_Month+1))
  Da <- substr(dataframe$Date, start = S_Day, stop = (S_Day+1))
  Date_adjusted <- paste(Da,"/",Mo,"/",Ye, sep="")
  dataframe$Date <-Date_adjusted
  Date_real0 <- seq(as.Date(dataframe$Date[1],"%d/%m/%Y"), as.Date(dataframe$Date[length(dataframe$Date)],"%d/%m/%Y"), by="days")
  Ye0 <- substr(Date_real0, start = 1, stop = 4)
  Mo0 <- substr(Date_real0, start = 6, stop = 7)
  Da0 <- substr(Date_real0, start = 9, stop = 10)
  Date_real <- paste(Da0,"/",Mo0,"/",Ye0, sep="")
  dataframe_adj <- data.frame(Date=Date_real, Flow=NA)
  dataframe_adj$Flow[which(Date_real %in% dataframe$Date)] <- dataframe$Flow[which(dataframe$Date %in% Date_real)]
  fd <- dataframe_adj
  return(fd)
  #print('Original data frame successfully formated and saved as fd')
  #print(head(fd))
}

col_per_year <- function(First_year,Last_year){
  # "First_year=1964,Last_year=2011" serían valores para el ejemplo. Creo que no deberían ser valores por defecto @Silvestre
  fd<-structure_date()#get(dafra)
  #attach(fd) # is that necessary? @javier
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Date_col <- seq(as.Date("2011-10-01"), as.Date("2012-09-30"), by="days")
  Date <- seq(as.Date(First_day), as.Date(Last_day), by="days")
  Flow_adj <- subset(fd, (Date>=First_day)&(Date<=Last_day), select="Flow")
  data1 <- data.frame(Date=Date, Flow = Flow_adj)
  ##Analysis of flows
  Years0 <- seq(as.numeric(substr(First_day, start = 1, stop = 4))+1, as.numeric(substr(Last_day, start = 1, stop = 4)))
  Years1 <- paste("Y",Years0,sep="")
  my_mx <- matrix(ncol=length(Years1), nrow=length(Date_col))
  colnames(my_mx) <- Years1
  for(i in 1:length(Years1)){
    if(length(subset(data1, (Date>paste(Years0[i]-1,"-09-30",sep="")) & (Date<= paste(Years0[i],"-09-30",sep="")),select="Flow")[,1])==365){
      my_mx[1:151,i] <- subset(data1, (Date>paste(Years0[i]-1,"-09-30",sep="")) & (Date<= paste(Years0[i],"-09-30",sep="")),select="Flow")[1:151,1]
      my_mx[152,i] <- NA
      my_mx[153:366,i] <- subset(data1, (Date>paste(Years0[i]-1,"-09-30",sep="")) & (Date<= paste(Years0[i],"-09-30",sep="")),select="Flow")[152:365,1]
    }else{
      my_mx[,i] <-subset(data1, (Date>paste(Years0[i]-1,"-09-30",sep="")) & (Date<= paste(Years0[i],"-09-30",sep="")),select="Flow")[,1]
    }
  }
  my_mx1 <- replace(my_mx, my_mx<0, NA)
  my_df <- data.frame(Date=substr(Date_col, start = 6, stop = 11), my_mx1)
  return(my_df)
  #print('Original data frame rearranged and saved as my_df')
  print(head(my_df))
}


#' Provides a summary of flow data during the pre-impact period
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Provides a dataframe on a daily basis of mean, min, p10, p25, median, p75, p90 and max values during the pre-impact period.
#' @examples
#' data(flowdata)
#' summary_flow(First_year=1964, Last_year=2011, Year_impact=1988)
#' @export
summary_flow <- function(First_year, Last_year, Year_impact){
  #fd<-get(dafra)
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Years0 <- seq(as.numeric(substr(First_day, start = 1, stop = 4))+1, as.numeric(substr(Last_day, start = 1, stop = 4)))
  Date_col <- seq(as.Date("2011-10-01"), as.Date("2012-09-30"), by="days")
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  ##summary of daily data of flows
  means<- rowMeans(my_mx1, na.rm = TRUE)
  medians <- apply (my_mx1, 1, median, na.rm = TRUE)
  percentiles <- apply(my_mx1, 1, quantile, na.rm = TRUE)
  m <- rbind(means, percentiles)
  daily_summary <- t(m)
  ##summary of daily data of flows BEFORE impact (1965-1987)
  ref_years_data <- my_mx1[,1:(match(Year_impact, Years0)-1)] ##only years BEFORE the impact
  means<- rowMeans(ref_years_data, na.rm = TRUE)
  medians <- apply (ref_years_data, 1, median, na.rm = TRUE)
  percentiles <- apply(ref_years_data, 1, quantile, probs=c(0,0.1,0.25,0.5,0.75,0.9,1), na.rm = TRUE, name=F)
  m <- rbind(means, percentiles)
  daily_summary_ref_years <- t(m)
  cnames <- c("mean", "min", "p10", "p25", "median", "p75", "p90", "max")
  colnames(daily_summary_ref_years) <- cnames
  daily_summary_ref_years_final <- data.frame(Day =substr(Date_col, start = 6, stop = 11),daily_summary_ref_years)
  return(daily_summary_ref_years_final)
  #print('Results saved as daily_summary_ref_years_final')
}



#' Calculates the admissible range of flow variability
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Calculates the admissible range of flow variability based on the flow data during the pre-impact period.
#' @examples
#' data(flowdata)
#' adm_range(First_year=1964, Last_year=2011, Year_impact=1988)
#' @export
adm_range <- function(First_year, Last_year, Year_impact){
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Years0 <- seq(as.numeric(substr(First_day, start = 1, stop = 4))+1, as.numeric(substr(Last_day, start = 1, stop = 4)))
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year) # get(dafra)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  daily_summary_ref_years_0 <- summary_flow(First_year=First_year,Last_year=Last_year,Year_impact=Year_impact) # get('daily_summary_ref_years_final')
  daily_summary_ref_years <- daily_summary_ref_years_0[,2:ncol(daily_summary_ref_years_0)]
  Date <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by="days")
  daily_summary_ref <- cbind(Date, daily_summary_ref_years)
  all_years <- cbind(Date, my_mx1)
  ## Layouts
  t_secs_1_day <- 1 # 8=86400(sec/dia)
  t_days_1_year <- 366 # 3=366(dias)
  layouts <- t_days_1_year * t_secs_1_day
  #t_one_day_ar   <- array(1:layouts, dim=c(86400,1,366))  # time points of 1day at which the model calculates
  t_one_day <- array(1:layouts, c(t_secs_1_day,1,t_days_1_year)) #8=86400(sec/dia), 1=1(filas), 3=366(dias)
  ## caudal de referencia LOW
  q_ref_cubic_feet_low  <- as.data.frame(daily_summary_ref$p10)#subset(daily_summary_ref, select=p10)
  q_ref_sin_interp_low <- q_ref_cubic_feet_low[1:t_days_1_year,] # caudal de referencia (m3/s) para 1st of October (1913-1952)
  q_ref_sin_interp22_low <- array(0, c(t_secs_1_day,1,t_days_1_year))
  q_ref_sin_interp22_low[, , t_days_1_year] <- seq(
    q_ref_sin_interp_low[t_days_1_year], q_ref_sin_interp_low[1],
    length.out=t_secs_1_day)
  t_days_1_year2 <- t_days_1_year- 1
  f_b <- function(m){
    seq(q_ref_sin_interp_low[m], q_ref_sin_interp_low[m+1], length.out=t_secs_1_day)
  }
  for(i in 1:t_days_1_year2){
    q_ref_sin_interp22_low[, , i] <- f_b(i)
  }
  q_ref_low <- c(q_ref_sin_interp22_low)
  #Smoothing Low flow of reference
  #require(zoo)
  Flow_p10_30days <- c(q_ref_low[352:366], q_ref_low,q_ref_low[1:14])
  Flow_p10_30day <- zoo::rollmean(Flow_p10_30days, 30)
  ## caudal de referencia HIGH
  q_ref_cubic_feet_high  <- as.data.frame(daily_summary_ref$p90)#subset(daily_summary_ref, select=p90)
  q_ref_sin_interp_high <- q_ref_cubic_feet_high[1:t_days_1_year,] # caudal de referencia (m3/s) para 1st of October (1913-1952)
  q_ref_sin_interp22_high <- array(0, c(t_secs_1_day,1,t_days_1_year))
  q_ref_sin_interp22_high[, , t_days_1_year] <- seq(
    q_ref_sin_interp_high[t_days_1_year], q_ref_sin_interp_high[1],
    length.out=t_secs_1_day)
  t_days_1_year2 <- t_days_1_year- 1
  f_b <- function(m){
    seq(q_ref_sin_interp_high[m], q_ref_sin_interp_high[m+1], length.out=t_secs_1_day)
  }
  for(i in 1:t_days_1_year2){
    q_ref_sin_interp22_high[, , i] <- f_b(i)
  }
  q_ref_high <- c(q_ref_sin_interp22_high)
  #Smoothing High flow of reference
  Flow_p90_30days <- c(q_ref_high[352:366], q_ref_high,q_ref_high[1:14])
  Flow_p90_30day <- zoo::rollmean(Flow_p90_30days, 30)

  tab_adm_range <- data.frame(Date=substr(Date, start = 6, stop = 11), Low_ref_flow = q_ref_low, High_ref_flow = q_ref_high,
                              Smoothed_ref_Low =Flow_p10_30day, Smoothed_ref_High = Flow_p90_30day)
  return(tab_adm_range)
}


#' Plots the admissible range of flow variability
#' @param  River_name Name of the river as character (e.g.: River_name = "Esla")
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Plots the admissible range of flow variability based on the flow data during the pre-impact period.
#' @examples
#' data(flowdata)
#' adm_range_plot(River_name = "Esla", First_year=1964, Last_year=2011, Year_impact=1988)
#' @export
#' @import "zoo"
#' @importFrom "graphics" "axis" "legend" "lines" "mtext" "par" "plot" "polygon"
#' @importFrom "stats" "median" "quantile"
#' @importFrom "utils" "head"
adm_range_plot <- function(River_name, First_year, Last_year, Year_impact){

#requireNamespace('zoo', quietly = TRUE)

  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Years0 <- seq(as.numeric(substr(First_day, start = 1, stop = 4))+1, as.numeric(substr(Last_day, start = 1, stop = 4)))
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year) # get(dafra)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  daily_summary_ref_years_0 <- summary_flow(First_year=First_year,Last_year=Last_year,Year_impact=Year_impact)
  daily_summary_ref_years <- daily_summary_ref_years_0[,2:ncol(daily_summary_ref_years_0)]
  Date <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by="days")
  daily_summary_ref <- cbind(Date, daily_summary_ref_years)
  all_years <- cbind(Date, my_mx1)
  ## Layouts
  t_secs_1_day <- 1 # 8=86400(sec/dia)
  t_days_1_year <- 366 # 3=366(dias)
  layouts <- t_days_1_year * t_secs_1_day
  #t_one_day_ar   <- array(1:layouts, dim=c(86400,1,366))  # time points of 1day at which the model calculates
  t_one_day <- array(1:layouts, c(t_secs_1_day,1,t_days_1_year)) #8=86400(sec/dia), 1=1(filas), 3=366(dias)
  ## caudal de referencia LOW
  q_ref_cubic_feet_low  <- as.data.frame(daily_summary_ref$p10)#subset(daily_summary_ref, select=p10)
  q_ref_sin_interp_low <- q_ref_cubic_feet_low[1:t_days_1_year,] # caudal de referencia (m3/s) para 1st of October (1913-1952)
  q_ref_sin_interp22_low <- array(0, c(t_secs_1_day,1,t_days_1_year))
  q_ref_sin_interp22_low[, , t_days_1_year] <- seq(
    q_ref_sin_interp_low[t_days_1_year], q_ref_sin_interp_low[1],
    length.out=t_secs_1_day)
  t_days_1_year2 <- t_days_1_year- 1
  f_b <- function(m){
    seq(q_ref_sin_interp_low[m], q_ref_sin_interp_low[m+1], length.out=t_secs_1_day)
  }
  for(i in 1:t_days_1_year2){
    q_ref_sin_interp22_low[, , i] <- f_b(i)
  }
  q_ref_low <- c(q_ref_sin_interp22_low)
  #Smoothing Low flow of reference
  #require(zoo)
  Flow_p10_30days <- c(q_ref_low[352:366], q_ref_low,q_ref_low[1:14])
  Flow_p10_30day <- zoo::rollmean(Flow_p10_30days, 30)
  ## caudal de referencia HIGH
  q_ref_cubic_feet_high  <- as.data.frame(daily_summary_ref$p90)#subset(daily_summary_ref, select=p90)
  q_ref_sin_interp_high <- q_ref_cubic_feet_high[1:t_days_1_year,] # caudal de referencia (m3/s) para 1st of October (1913-1952)
  q_ref_sin_interp22_high <- array(0, c(t_secs_1_day,1,t_days_1_year))
  q_ref_sin_interp22_high[, , t_days_1_year] <- seq(
    q_ref_sin_interp_high[t_days_1_year], q_ref_sin_interp_high[1],
    length.out=t_secs_1_day)
  t_days_1_year2 <- t_days_1_year- 1
  f_b <- function(m){
    seq(q_ref_sin_interp_high[m], q_ref_sin_interp_high[m+1], length.out=t_secs_1_day)
  }
  for(i in 1:t_days_1_year2){
    q_ref_sin_interp22_high[, , i] <- f_b(i)
  }
  q_ref_high <- c(q_ref_sin_interp22_high)
  #Smoothing High flow of reference
  Flow_p90_30days <- c(q_ref_high[352:366], q_ref_high,q_ref_high[1:14])
  Flow_p90_30day <- zoo::rollmean(Flow_p90_30days, 30)
  plot(q_ref_high, type="l",xaxt='n', ylab="", xlab="", ylim=c(0,max(q_ref_high)*1.5),
       xlim=c(0,390), main=paste("Admissible range in",River_name, "River"))
  mtext(side=2,line=2.5,expression('Flow (m '^3*'/s)'))
  legend("topleft", col=c(1,2,152), cex=1, lty=c(1,1,1), lwd=c(1,1,10),bty="n",
         legend = c(paste("Reference flow from non-regulated period (", Years0[1],"-",Year_impact-1,")" ,sep=""),
                    expression('Smoothed reference flow (Q'[10]*' & Q'[90]*')', 'Admissible range of regulated flow variability' )))
  x <- c(1:366,366:1)
  poly_Flow_p90_p10_30day <- c(Flow_p90_30day,rev(Flow_p10_30day))
  polygon(x, poly_Flow_p90_p10_30day, col = "gray", border = "gray")
  lines(q_ref_high, col=1)
  lines(q_ref_low, col=1)
  lines(Flow_p90_30day, col=2)
  lines(Flow_p10_30day, col=2)
  axis(1, lwd=1, at=0+15, labels="OC")
  axis(1, lwd=1, at=31+15, labels="NO")
  axis(1, lwd=1, at=61+15, labels="DE")
  axis(1, lwd=1, at=92+15, labels="JA")
  axis(1, lwd=1, at=123+15, labels="FE")
  axis(1, lwd=1, at=152+15, labels="MR")
  axis(1, lwd=1, at=183+15, labels="AP")
  axis(1, lwd=1, at=213+15, labels="MY")
  axis(1, lwd=1, at=244+15, labels="JN")
  axis(1, lwd=1, at=274+15, labels="JL")
  axis(1, lwd=1, at=305+15, labels="AU")
  axis(1, lwd=1, at=336+15, labels="SE")
  axis(2, lwd=1, at=0, labels="0")
}


#' Calculates the daily environmental impact of flow regulation (high- and low-flow impact)
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_evaluated Year when the environmental impact is evaluated (e.g.: Year_evaluated = 2010)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Calculates the daily environmental impact of flow regulation (high- and low-flow impact).
#' @examples
#' data(flowdata)
#' impact_reg(First_year=1964, Last_year=2011,Year_evaluated=2010,Year_impact=1988)
#' @export
impact_reg <- function(First_year, Last_year,Year_evaluated,Year_impact){
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  Date <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by="days")
  all_years <- cbind(Date, my_mx1)
  ## Layouts
  t_secs_1_day <- 1 # 8=86400(sec/dia)
  t_days_1_year <- 366 # 3=366(dias)
  layouts <- t_days_1_year * t_secs_1_day
  #Flow (m3/s) AFTER impact (eg. 2010)
  q_year_evaluated <- c(all_years[,paste("Y",Year_evaluated, sep="")])
  #require(zoo)
  ## 3 days
  q_year_evaluated_3days <- c(q_year_evaluated[366], q_year_evaluated,q_year_evaluated[1])
  q_year_evaluated_3day <- rollapply(q_year_evaluated_3days, 3,median,na.rm=TRUE)
  ## 7 days
  q_year_evaluated_7days <- c(q_year_evaluated[364:366], q_year_evaluated,q_year_evaluated[1:3])
  q_year_evaluated_7day <- rollapply(q_year_evaluated_7days, 7,median,na.rm=TRUE)
  ## 30 days
  q_year_evaluated_30days <- c(q_year_evaluated[352:366], q_year_evaluated,q_year_evaluated[1:14])
  q_year_evaluated_30day <- rollapply(q_year_evaluated_30days, 30,median,na.rm=TRUE)

  adm_range <- adm_range (First_year=First_year, Last_year=Last_year,Year_impact=Year_impact)
  Flow_p10_30day <- adm_range$Smoothed_ref_Low
  Flow_p90_30day <- adm_range$Smoothed_ref_High
  ###### Environmental Impact
  #Impact low flows
  ## 1 day low flow
  Imp_year_evaluated_low_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_low_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_1d[i] <- (Flow_p10_30day[i]- q_year_evaluated[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_1d[i] <- 0
      }
    }
  }
  ## 3 days low flow
  Imp_year_evaluated_low_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_low_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_3d[i] <- (Flow_p10_30day[i]- q_year_evaluated_3day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_3d[i] <- 0
      }
    }
  }
  ## 7 days low flow
  Imp_year_evaluated_low_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_low_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_7d[i] <- (Flow_p10_30day[i]- q_year_evaluated_7day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_7d[i] <- 0
      }
    }
  }
  ## 30 days low flow
  Imp_year_evaluated_low_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_low_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_30d[i] <- (Flow_p10_30day[i]- q_year_evaluated_30day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_low <- cbind(Imp_year_evaluated_low_1d,Imp_year_evaluated_low_3d,
                                  Imp_year_evaluated_low_7d,Imp_year_evaluated_low_30d)
  ## 1 day high flow
  Imp_year_evaluated_high_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_high_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_1d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated[i])/(q_year_evaluated[i])
      } else{
        Imp_year_evaluated_high_1d[i] <- 0
      }
    }
  }
  ## 3 days high flow
  Imp_year_evaluated_high_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_high_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_3d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_3day[i])/(q_year_evaluated_3day[i])
      } else{
        Imp_year_evaluated_high_3d[i] <- 0
      }
    }
  }
  ## 7 days high flow
  Imp_year_evaluated_high_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_high_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_7d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_7day[i])/(q_year_evaluated_7day[i])
      } else{
        Imp_year_evaluated_high_7d[i] <- 0
      }
    }
  }
  ## 30 days high flow
  Imp_year_evaluated_high_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_high_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_30d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_30day[i])/(q_year_evaluated_30day[i])
      } else{
        Imp_year_evaluated_high_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_high <- cbind(Imp_year_evaluated_high_1d,Imp_year_evaluated_high_3d,
                                   Imp_year_evaluated_high_7d,Imp_year_evaluated_high_30d)
  impact_total <- Imp_year_evaluated_low + Imp_year_evaluated_high
  Impact_High <- rowMeans(Imp_year_evaluated_high)
  Impact_Low <- rowMeans(Imp_year_evaluated_low)
  Impact_Total <- rowMeans(impact_total)

  Impacts <- data.frame(Date=substr(Date, start = 6, stop = 11), Impact_Low=Impact_Low, Impact_High=Impact_High, Impact_Total=Impact_Total)
  return(Impacts)
}



#' Plots the daily environmental impact of flow regulation (high- and low-flow impact)
#' @param  River_name Name of the river written as character (e.g.: River_name = "Esla")
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_evaluated Year when the environmental impact is evaluated (e.g.: Year_evaluated = 2010)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Plots the daily environmental impact of flow regulation (high- and low-flow impact).
#' @examples
#' data(flowdata)
#' impact_reg_plot(River_name = "Esla", First_year=1964,
#' Last_year=2011, Year_evaluated=2010, Year_impact=1988)
#' @export
impact_reg_plot <- function(River_name, First_year, Last_year,Year_evaluated,Year_impact){
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  Date <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by="days")
  all_years <- cbind(Date, my_mx1)
  adm_range <- adm_range (First_year=First_year, Last_year=Last_year,Year_impact=Year_impact)
  Flow_p10_30day <- adm_range$Smoothed_ref_Low
  Flow_p90_30day <- adm_range$Smoothed_ref_High
  q_ref_high <- adm_range$High_ref_flow
  q_ref_low <- adm_range$Low_ref_flow
  ## Layouts
  t_secs_1_day <- 1 # 8=86400(sec/dia)
  t_days_1_year <- 366 # 3=366(dias)
  layouts <- t_days_1_year * t_secs_1_day
  #Flow (m3/s) AFTER impact (eg. 2010)
  q_year_evaluated <- c(all_years[,paste("Y",Year_evaluated, sep="")])
  ## 3 days
  q_year_evaluated_3days <- c(q_year_evaluated[366], q_year_evaluated,q_year_evaluated[1])
  q_year_evaluated_3day <- rollapply(q_year_evaluated_3days, 3,median,na.rm=TRUE)
  ## 7 days
  q_year_evaluated_7days <- c(q_year_evaluated[364:366], q_year_evaluated,q_year_evaluated[1:3])
  q_year_evaluated_7day <- rollapply(q_year_evaluated_7days, 7,median,na.rm=TRUE)
  ## 30 days
  q_year_evaluated_30days <- c(q_year_evaluated[352:366], q_year_evaluated,q_year_evaluated[1:14])
  q_year_evaluated_30day <- rollapply(q_year_evaluated_30days, 30,median,na.rm=TRUE)
  ###### Environmental Impact
  #Impact low flows
  ## 1 day low flow
  Imp_year_evaluated_low_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_low_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_1d[i] <- (Flow_p10_30day[i]- q_year_evaluated[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_1d[i] <- 0
      }
    }
  }
  ## 3 days low flow
  Imp_year_evaluated_low_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_low_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_3d[i] <- (Flow_p10_30day[i]- q_year_evaluated_3day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_3d[i] <- 0
      }
    }
  }
  ## 7 days low flow
  Imp_year_evaluated_low_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_low_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_7d[i] <- (Flow_p10_30day[i]- q_year_evaluated_7day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_7d[i] <- 0
      }
    }
  }
  ## 30 days low flow
  Imp_year_evaluated_low_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_low_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_30d[i] <- (Flow_p10_30day[i]- q_year_evaluated_30day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_low <- cbind(Imp_year_evaluated_low_1d,Imp_year_evaluated_low_3d,
                                  Imp_year_evaluated_low_7d,Imp_year_evaluated_low_30d)
  ## 1 day high flow
  Imp_year_evaluated_high_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_high_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_1d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated[i])/(q_year_evaluated[i])
      } else{
        Imp_year_evaluated_high_1d[i] <- 0
      }
    }
  }
  ## 3 days high flow
  Imp_year_evaluated_high_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_high_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_3d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_3day[i])/(q_year_evaluated_3day[i])
      } else{
        Imp_year_evaluated_high_3d[i] <- 0
      }
    }
  }
  ## 7 days high flow
  Imp_year_evaluated_high_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_high_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_7d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_7day[i])/(q_year_evaluated_7day[i])
      } else{
        Imp_year_evaluated_high_7d[i] <- 0
      }
    }
  }
  ## 30 days high flow
  Imp_year_evaluated_high_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_high_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_30d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_30day[i])/(q_year_evaluated_30day[i])
      } else{
        Imp_year_evaluated_high_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_high <- cbind(Imp_year_evaluated_high_1d,Imp_year_evaluated_high_3d,
                                   Imp_year_evaluated_high_7d,Imp_year_evaluated_high_30d)
  #require(plotrix)
  eje_x <- 1:366
  par(mar=c(4,4,4,4))
  plot(eje_x,q_year_evaluated, col=1, type="l",xaxt='n', ylab="", xlab="", ylim=c(0,max(q_year_evaluated,q_ref_high ,na.rm=T)*1.5),
       xlim=c(0,366))#, yaxt="n")
  mtext(side=2,line=2.5,expression('Flow (m '^3*'/s)'))
  x <- c(1:366,366:1)
  poly_Flow_p90_p10_30day <- c(Flow_p90_30day,rev(Flow_p10_30day))
  polygon(x, poly_Flow_p90_p10_30day, col = "gray", border = "gray")
  lines(q_year_evaluated, col=1)
  par(new=TRUE)
  plot(eje_x, rowMeans(Imp_year_evaluated_low) ,,type="l", bty="n",xlab="",ylab="",col=2,
       ylim=c(-7,2),xlim=c(0,366),axes = FALSE, yaxt="n", main=paste(River_name,"River - Impact in", Year_evaluated))
  lines(rowMeans(Imp_year_evaluated_high), col=4, lty=2)
  mtext(side=4,line=2.3,expression('                                                     Impact on river flow'))
  axis(1, lwd=1, at=0+15, labels="Oct")
  axis(1, lwd=1, at=31+15, labels="Nov")
  axis(1, lwd=1, at=61+15, labels="Dic")
  axis(1, lwd=1, at=92+15, labels="Jan")
  axis(1, lwd=1, at=123+15, labels="Feb")
  axis(1, lwd=1, at=152+15, labels="Mar")
  axis(1, lwd=1, at=183+15, labels="Apr")
  axis(1, lwd=1, at=213+15, labels="May")
  axis(1, lwd=1, at=244+15, labels="Jun")
  axis(1, lwd=1, at=274+15, labels="Jul")
  axis(1, lwd=1, at=305+15, labels="Aug")
  axis(1, lwd=1, at=336+15, labels="Sep")
  axis(4, lwd=1, at=0, labels="0")
  axis(4, lwd=1, at=1, labels="1")
}


#' Calculates the daily environmental costs of flow regulation
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_evaluated Year when the environmental impact is evaluated (e.g.: Year_evaluated = 2010)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @param  a_low Coefficient a of Low-flow impact of function ku (e.g.: a_low = 0.05)
#' @param  a_high Coefficient a of High-flow impact of function ku (e.g.: a_high = 0.01)
#' @param  b_low Coefficient b of Low-flow impact of function ku (e.g.: b_low = 2)
#' @param  b_high Coefficient b of High-flow impact of function ku (e.g.: b_high = 2)
#' @return Calculates the daily environmental costs of flow regulation for a specific year evaluated.
#' @examples
#' data(flowdata)
#' daily_cost(First_year=1964, Last_year=2011,Year_evaluated=2010,
#' Year_impact=1988, a_low = 0.05, a_high = 0.01, b_low = 2, b_high = 2)
#' @export
daily_cost <- function(First_year, Last_year, Year_evaluated, Year_impact, a_low, a_high,b_low, b_high){
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Impacts <- impact_reg(First_year=First_year, Last_year=Last_year,Year_evaluated=Year_evaluated, Year_impact=Year_impact)
  Impact_Low <- Impacts$Impact_Low
  Impact_High <- Impacts$Impact_High
  Impact_Total <- Impacts$Impact_Total
  ku_low <-  (c(rep(a_low,366))*(Impact_Low>0))*(exp(b_low*Impact_Low))
  ku_high <-  (c(rep(a_high,366))*(Impact_High>0))*(exp(b_high*Impact_High))
  ku <- ku_low+ku_high
  Costs_Total <- Impact_Total*ku
  Costs_Low <- Impact_Low*ku_low
  Costs_High <- Impact_High*ku_high
  Daily_Costs_results <- data.frame(Date=substr(seq(as.Date("2011-10-01"), as.Date("2012-09-30"), by="days"), start = 6, stop = 11),
                                    Impact_Low=Impact_Low, Impact_High=Impact_High, ku_low=ku_low, ku_high=ku_high,
                                    Costs_Low=Costs_Low,Costs_High=Costs_High,Costs_Total=Costs_Total)
  return(Daily_Costs_results)
}



#' Plots the daily environmental costs of flow regulation
#' @param  River_name Name of the river written as character (e.g.: River_name = "Esla")
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_evaluated Year when the environmental impact is evaluated (e.g.: Year_evaluated = 2010)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @param  a_low Coefficient a of Low-flow impact of function ku (e.g.: a_low = 0.05)
#' @param  a_high Coefficient a of High-flow impact of function ku (e.g.: a_high = 0.01)
#' @param  b_low Coefficient b of Low-flow impact of function ku (e.g.: b_low = 2)
#' @param  b_high Coefficient b of High-flow impact of function ku (e.g.: b_high = 2)
#' @return Plots the daily environmental costs of flow regulation for a specific year evaluated.
#' @examples
#' data(flowdata)
#' daily_cost_plot(River_name = "Esla", First_year=1964, Last_year=2011,
#' Year_evaluated=2010, Year_impact=1988, a_low = 0.05, a_high = 0.01,
#' b_low = 2, b_high = 2)
#' @export
daily_cost_plot <- function(River_name, First_year, Last_year,Year_evaluated,Year_impact,a_low, a_high, b_low, b_high){
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  Impacts <- impact_reg(First_year=First_year, Last_year=Last_year,Year_evaluated=Year_evaluated, Year_impact=Year_impact)
  Impact_Low <- Impacts$Impact_Low
  Impact_High <- Impacts$Impact_High
  Impact_Total <- Impacts$Impact_Total
  ku_low <-  (c(rep(a_low,366))*(Impact_Low>0))*(exp(b_low*Impact_Low))
  ku_high <-  (c(rep(a_high,366))*(Impact_High>0))*(exp(b_high*Impact_High))
  ku <- ku_low+ku_high
  Costs_Total <- Impact_Total*ku
  Costs_Low <- Impact_Low*ku_low
  Costs_High <- Impact_High*ku_high
  plot(Costs_Total, type="l", ylab="", col=c(1),
       xlab="", xaxt='n', main=paste(River_name,"River -", Year_evaluated),
       ylim=c(0,max(Impact_Total*ku, na.rm=T)*1.5))
  lines(Costs_Low, col=2)
  lines(Costs_High, col=4)
  lines(c(rep(0,366)), col=1)
  legend("topleft", legend = c(paste("Low-flow env. costs", " (a = ", a_low,  ", b = ",b_low, ")", sep=""),
                               paste("High-flow env. costs", " (a = ", a_high, ", b = ",b_high, ")", sep="")),
         col=c(2,4), pch=1,cex=0.8)
  mtext(expression('Environmental Costs ( Eur / m '^3*')') , side=2, line=2.5)
  axis(1, lwd=1, at=0+15, labels="Oct")
  axis(1, lwd=1, at=31+15, labels="Nov")
  axis(1, lwd=1, at=61+15, labels="Dic")
  axis(1, lwd=1, at=92+15, labels="Jan")
  axis(1, lwd=1, at=123+15, labels="Feb")
  axis(1, lwd=1, at=152+15, labels="Mar")
  axis(1, lwd=1, at=183+15, labels="Apr")
  axis(1, lwd=1, at=213+15, labels="May")
  axis(1, lwd=1, at=244+15, labels="Jun")
  axis(1, lwd=1, at=274+15, labels="Jul")
  axis(1, lwd=1, at=305+15, labels="Aug")
  axis(1, lwd=1, at=336+15, labels="Sep")
}



impact_reg_multi0 <- function(River_name, First_year, Last_year, Year_evaluated, Year_impact, x_coef){
  # He quitado @export porque creo que esta función no debería ser compartida @Silvestre
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  my_mx1_0 <- col_per_year(First_year=First_year,Last_year=Last_year)
  my_mx1 <- my_mx1_0[,2:ncol(my_mx1_0)]
  Date <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by="days")
  all_years <- cbind(Date, my_mx1)
  adm_range <- adm_range (First_year=First_year, Last_year=Last_year,Year_impact=Year_impact)
  Flow_p10_30day <- adm_range$Smoothed_ref_Low
  Flow_p90_30day <- adm_range$Smoothed_ref_High
  q_ref_high <- adm_range$High_ref_flow
  q_ref_low <- adm_range$Low_ref_flow
  ## Layouts
  t_secs_1_day <- 1 # 8=86400(sec/dia)
  t_days_1_year <- 366 # 3=366(dias)
  layouts <- t_days_1_year * t_secs_1_day
  #Flow (m3/s) AFTER impact (eg. 2010)
  q_year_evaluated <- c(all_years[,paste("Y",Year_evaluated, sep="")])
  ## 3 days
  q_year_evaluated_3days <- c(q_year_evaluated[366], q_year_evaluated,q_year_evaluated[1])
  q_year_evaluated_3day <- rollapply(q_year_evaluated_3days, 3,median,na.rm=TRUE)
  ## 7 days
  q_year_evaluated_7days <- c(q_year_evaluated[364:366], q_year_evaluated,q_year_evaluated[1:3])
  q_year_evaluated_7day <- rollapply(q_year_evaluated_7days, 7,median,na.rm=TRUE)
  ## 30 days
  q_year_evaluated_30days <- c(q_year_evaluated[352:366], q_year_evaluated,q_year_evaluated[1:14])
  q_year_evaluated_30day <- rollapply(q_year_evaluated_30days, 30,median,na.rm=TRUE)
  ###### Environmental Impact
  #Impact low flows
  ## 1 day low flow
  Imp_year_evaluated_low_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_low_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_1d[i] <- (Flow_p10_30day[i]- q_year_evaluated[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_1d[i] <- 0
      }
    }
  }
  ## 3 days low flow
  Imp_year_evaluated_low_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_low_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_3d[i] <- (Flow_p10_30day[i]- q_year_evaluated_3day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_3d[i] <- 0
      }
    }
  }
  ## 7 days low flow
  Imp_year_evaluated_low_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_low_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_7d[i] <- (Flow_p10_30day[i]- q_year_evaluated_7day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_7d[i] <- 0
      }
    }
  }
  ## 30 days low flow
  Imp_year_evaluated_low_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_low_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_low_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]< Flow_p10_30day[i]){
        Imp_year_evaluated_low_30d[i] <- (Flow_p10_30day[i]- q_year_evaluated_30day[i])/(Flow_p10_30day[i])
      } else{
        Imp_year_evaluated_low_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_low <- cbind(Imp_year_evaluated_low_1d,Imp_year_evaluated_low_3d,
                                  Imp_year_evaluated_low_7d,Imp_year_evaluated_low_30d)
  ## 1 day high flow
  Imp_year_evaluated_high_1d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_1d)){
    if(is.na(q_year_evaluated[i])){
      Imp_year_evaluated_high_1d[i] <- NA
    } else {
      if (q_year_evaluated[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_1d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated[i])/(q_year_evaluated[i])
      } else{
        Imp_year_evaluated_high_1d[i] <- 0
      }
    }
  }
  ## 3 days high flow
  Imp_year_evaluated_high_3d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_3d)){
    if(is.na(q_year_evaluated_3day[i])){
      Imp_year_evaluated_high_3d[i] <- NA
    } else {
      if (q_year_evaluated_3day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_3d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_3day[i])/(q_year_evaluated_3day[i])
      } else{
        Imp_year_evaluated_high_3d[i] <- 0
      }
    }
  }
  ## 7 days high flow
  Imp_year_evaluated_high_7d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_7d)){
    if(is.na(q_year_evaluated_7day[i])){
      Imp_year_evaluated_high_7d[i] <- NA
    } else {
      if (q_year_evaluated_7day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_7d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_7day[i])/(q_year_evaluated_7day[i])
      } else{
        Imp_year_evaluated_high_7d[i] <- 0
      }
    }
  }
  ## 30 days high flow
  Imp_year_evaluated_high_30d <- rep(0, layouts)
  for(i in 1:length(Imp_year_evaluated_high_30d)){
    if(is.na(q_year_evaluated_30day[i])){
      Imp_year_evaluated_high_30d[i] <- NA
    } else {
      if (q_year_evaluated_30day[i]> Flow_p90_30day[i]){
        Imp_year_evaluated_high_30d[i] <- (-Flow_p90_30day[i]+ q_year_evaluated_30day[i])/(q_year_evaluated_30day[i])
      } else{
        Imp_year_evaluated_high_30d[i] <- 0
      }
    }
  }
  Imp_year_evaluated_high <- cbind(Imp_year_evaluated_high_1d,Imp_year_evaluated_high_3d,
                                   Imp_year_evaluated_high_7d,Imp_year_evaluated_high_30d)
  #require(plotrix)
  eje_x <- 1:366
  par(mar=c(4,4,4,4))
  plot(eje_x,q_year_evaluated, col=1, type="l",xaxt='n', ylab="", xlab="", ylim=c(0,max(q_year_evaluated,q_ref_high ,na.rm=T)*1.5),
       xlim=c(0,366), cex.axis=(1*x_coef))#, yaxt="n")
  mtext(side=2,line=2,expression('Flow (m '^3*'/s)'),cex=(1*x_coef*0.8))
  x <- c(1:366,366:1)
  poly_Flow_p90_p10_30day <- c(Flow_p90_30day,rev(Flow_p10_30day))
  polygon(x, poly_Flow_p90_p10_30day, col = "gray", border = "gray")
  lines(q_year_evaluated, col=1)
  par(new=TRUE)
  plot(eje_x, rowMeans(Imp_year_evaluated_low) ,,type="l", bty="n",xlab="",ylab="",col=2,
       ylim=c(-7,2),xlim=c(0,366),axes = FALSE, yaxt="n") #,main=paste(River_name,"River - Impact in", Year_evaluated), cex.main=(1*x_coef))
  lines(rowMeans(Imp_year_evaluated_high), col=4, lty=2)
  mtext(side=3,line=1,paste(River_name,"River - Impact in", Year_evaluated), cex=(1*x_coef*0.8), font =2)
  mtext(side=4,line=2,"Impact", cex=1*x_coef*0.8)
  axis(1, lwd=1, at=0+15, labels="O", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=31+15, labels="N", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=61+15, labels="D", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=92+15, labels="J", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=123+15, labels="F", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=152+15, labels="M", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=183+15, labels="A", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=213+15, labels="M", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=244+15, labels="J", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=274+15, labels="J", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=305+15, labels="A", cex.axis=(1*x_coef))
  axis(1, lwd=1, at=336+15, labels="S", cex.axis=(1*x_coef))
  axis(4, lwd=1, at=0, labels="0", cex.axis=(1*x_coef))
  axis(4, lwd=1, at=1, labels="1", cex.axis=(1*x_coef))
}



#' Plots the daily environmental impact of flow regulation for multiple years
#' @param  Row Number of rows in the figure to compare multiple years in separated graphs (e.g.:  Row = 2)
#' @param  Column Number of columns in the figure to compare multiple years in separated graphs (e.g.:  Column = 5)
#' @param  sp_years A vector specifying the years to be plotted (e.g.:  sp_years = c(1965,1966,1967,1968,1969,2006,2007,2008,2009,2010))
#' @param  River_name Name of the river written as character (e.g.: River_name = "Esla")
#' @param  First_year First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)
#' @param  Last_year First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)
#' @param  Year_impact Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)
#' @return Plots the daily environmental impact of flow regulation for multiple years.
#' @examples
#' data(flowdata)
#' impact_reg_multi_plot(Row = 1,Column = 2,
#' sp_years = c(1965,2010),
#' River_name = "Esla", First_year=1964, Last_year=2011,
#' Year_impact=1988)
#' @export
impact_reg_multi_plot <- function(Row, Column, sp_years, River_name, First_year, Last_year,Year_impact) {
  First_day <- paste(First_year,"-10-01", sep="")
  Last_day <- paste(Last_year,"-09-30", sep="")
  n_years <- length(sp_years)
  par(mfrow=c(Row, Column))
  x_coef <- -0.144* log(n_years) + 0.971
  for(i in 1:length(sp_years)){
    impact_reg_multi0(River_name = River_name,First_year=First_year, Last_year=Last_year,Year_evaluated=sp_years[i], Year_impact=Year_impact, x_coef=x_coef)
  }
}
