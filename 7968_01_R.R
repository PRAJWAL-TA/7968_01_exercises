
# Create the required data frames by reading in the files
library(readxl)
library(dplyr)
sale <- read_excel('SaleData.xlsx',sheet='Sales Data')


# Q1 Find least sales amount for each item
least_sales <-function(df){
  ls=df %>% group_by(Item)%>% summarise(least_amount_sale=min(Sale_amt,na.rm=TRUE)) 
  return(ls)
}


# Q2 compute total sales at each year X region
sales_year_region <- function(df){
  syr=df%>% group_by(year=format(as.Date(OrderDate,format="%y-%m-%d"),"%Y"),Region,Item)%>%summarize(Total_Sales=sum(Sale_amt,na.rm=TRUE))
  return (syr)
}


# Q3 append column with no of days difference from present date to each order date
days_diff <- function(df,given.date){
  ref.date<-as.Date(given.date,"%Y-%m-%d")
  df <-cbind(df,'days_diff'=as.Date(ref.date,"%Y-%m-%d") - as.Date(df$OrderDate,"%Y-%m-%d"))
  return(df)
}


# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
mgr_slsmn <- function(df){
  ms=aggregate(df['SalesMan'], list(df$Manager),unique)
  colnames(ms)<-c('manager','list_of_salesman')
  return(ms)
}

  
# Q5 For all regions find number of salesman and number of units
slsmn_units<-function(df){
  su1=df%>%group_by(Region)%>%summarise(list_of_salesman=length(unique(SalesMan)))
  su2=(df%>%group_by(Region)%>%summarise(total_sales=sum(Sale_amt,na.rm=TRUE)))['total_sales']
  su=data.frame(su1,su2)
  return(su)
}

  
# Q6 Find total sales as percentage for each manager
sales_pct<-function(df){
  k=sum(df$Sale_amt)
  sp=df %>% group_by(Manager) %>% summarise(percent_sales=sum(Sale_amt,na.rm=TRUE)*100/k)
  return(sp)
}

 #print(least_sales(sale))
 #print(sales_year_region(sale))
 #print(days_diff(sale,'2019-11-29'))
 #print(mgr_slsmn(sale))
 #print(slsmn_units(sale))
 #print(sales_pct(sale))
 #print(sale)
  



imdbdf <- read_delim('imdb.csv',delim=',', escape_double=FALSE, escape_backslash=TRUE)
# Q7 get imdb rating for fifth movie of dataframe
fifth_movie<-function(df){
  v<-as.character(df$imdbRating)
  cat('imdb Rating of 5th movie is:',v[5],'\n')
}

  
# Q8 return titles of movies with shortest and longest run time
movies<-function(df){
  df$duration<-as.numeric(df$duration)
  v<-df$duration
  v<-v[!is.na(v)]
  df<-df[!is.na(df$duration),]
  shortest.movie<-df[df$duration==min(v),'title']
  longest.movie<-df[df$duration==max(v),'title']
  cat('Shortest movie is:',as.character(shortest.movie),'\n')
  cat('Longest movie is:',as.character(longest.movie),'\n')
}

  
# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
sort_df<-function(df){
  df$imdbRating<-as.numeric(df$imdbRating)
  sorted.df <- order(df$year,-df$imdbRating)
  df<-df[sorted.df,]
  return(df[,c('title','year','imdbRating')])
}

  
# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
subset_df<-function(df){
  df$duration<-as.numeric(df$duration)
  return(df[((df$duration/60)>30) & ((df$duration/60)<180),c('title','duration')])
}

 #fifth_movie(imdbdf)
 #movies(imdbdf)
 #print(sort_df(imdbdf))
 #print(subset_df(imdbdf))

 
  
diamondsdf <- read.csv('diamonds.csv')


# Q11 count the duplicate rows of diamonds DataFrame.
dupl_rows<-function(df){
  dupe<-sum(as.numeric(duplicated(df)))
  cat('Count of duplicate elements is:',dupe,'\n')
}
  
   
# Q12 droping those rows where any value in a row is missing in carat and cut columns
drop_row<-function(df){
  missingcarat.val<- is.na(df$carat)
  missingcut.val <- is.na(df$cut)
  miss.val<- missingcarat.val | missingcut.val
  df<-diamondsdf[!miss.val,]
  return(df)
}

# Q13 subset only numeric columns
sub_numeric<-function(df){
  sn<-select_if(diamondsdf, is.numeric)
  print(sn)
}

# Q14 compute volume as (x*y*z) when depth > 60 else 8
volume<-function(df){
  depth.filter<-df$depth>60
  v<-c()
  for(i in 1:length(depth.filter)){
    if(depth.filter[i]){
      vol<-(as.numeric(as.character(df$x[i]))*
              as.numeric(as.character(df$y[i]))*
              as.numeric(as.character(df$z[i])))
    }
    else{
      vol<-8
    }
    v<-c(v,vol)
  }
  df$volume<-v
  return(df)
}
  
  
# Q15 impute missing price values with mean
impute<-function(df){
  missing.price<-!is.na(df$price)
  prices<-df$price[missing.price]
  df$price[is.na(df$price)] <- mean(prices)
  return(df)
}


#dupl_rows(diamondsdf)
#print(drop_row(diamondsdf))
#print(sub_numeric(diamondsdf))
#print(volume(diamondsdf))
#print(impute(diamondsdf))
  
#BONUS QUESTIONS
library(descr)
library(mltools)
library(readr)
library(tidyr)
library(imputeTS)
#Q2 Is there a realation between the length of a movie title and the ratings ? Generate a report that captures
#the trend of the number of letters in movies titles over years. We expect a cross tab between the year of
#the video release and the quantile that length fall under. The results should contain year, min_length,
#max_length, num_videos_less_than25Percentile, num_videos_25_50Percentile ,
#num_videos_50_75Percentile, num_videos_greaterthan75Precentile

#Dataset is imdbdf
titlelength_year<-function(df){
  df$length1<-nchar(gsub(" ","",df$wordsInTitle,fixed=TRUE))
  df$quantile<-as.numeric(ntile(df$length1,4))
  df3<-as.data.frame.matrix(table(df$year,df$quantile))
  df4<-df%>%group_by(year)%>%summarise(minimum=min(length1,na.rm=TRUE),maximum=max(length1,na.rm=TRUE))
  df4=drop_na(df4)
  df5<-cbind(df4,df3)
  row.names(df5)<-NULL
  return(df5)
}

#print(titlelength_year(imdbdf))

#Q3 In diamonds data set Using the volumne calculated above, create bins that have equal population within
#them. Generate a report that contains cross tab between bins and cut. Represent the number under
#each cell as a percentage of total.

#Dataset is diamondsdf

cut_vol<-function(df){
  depth.filter<-df$depth>60
  v<-c()
  for(i in 1:length(depth.filter)){
    if(depth.filter[i]){
      vol<-(as.numeric(as.character(df$x[i]))*
              as.numeric(as.character(df$y[i]))*
              as.numeric(as.character(df$z[i])))
    }
    else{
      vol<-8
    }
    v<-c(v,vol)
  }
  df$volume<-v
  df$Bins<-as.numeric(ntile(df$volume,5))
  tvol=sum(df$volume)
  return(crosstab(df$Bins,df$cut,plot=FALSE,prop.t=TRUE))
}

#print(cut_vol(diamondsdf))

#Q5 Bucket the movies into deciles using the duration. Generate the report that tracks various features like
#nomiations, wins, count, top 3 geners in each decile.

#Dataset is imdbdf
decile<-function(df){
  df<-na_mean(df)
  df$deciles<-as.numeric(ntile(df$duration,10))
  df3<-df%>%group_by(deciles)%>%summarise(nr_Of_Nominations=sum(nrOfNominations,na.rm=TRUE),nr_Of_Wins=sum(nrOfWins))
  df3$count<-as.data.frame(table(df$deciles))['Freq']
  df4<-df[c(17:45)]
  df5<-df4%>%group_by(deciles)%>%summarise_all(sum)
  df5<-as.data.frame.matrix(t(df5))
  names<-row.names(df5)
  high<-function(x)
  {
    return(names[order(x,decreasing = TRUE)[1:3]])
  }
  df6<-as.data.frame.matrix(t(sapply(df5,high)))
  colnames(df6)<-c('first','second','third')
  df3['top genres']<-paste(df6$first,',',df6$second,',',df6$third)
  return(df3)
}
#print(decile(imdbdf))