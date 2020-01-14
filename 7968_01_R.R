
# Create the required data frames by reading in the files
library(readxl)
library(dplyr)
sale <- read_excel('SaleData.xlsx',sheet='Sales Data')


# Q1 Find least sales amount for each item
# has been solved as an example
least_sales <-function(sale){
  ls=sale %>% group_by(Item)%>% summarise(min_sale=min(Sale_amt)) 
  return(ls)
}

least_sales(sale)
  # Q2 compute total sales at each year X region
sales_year_region <- function(sale){
  ls=sale%>% group_by(format(as.Date(OrderDate,format="%y-%m-%d"),"%Y"),Region,Item)%>%summarize(sum(Sale_amt))
  return (ls)
}

  
  # Q3 append column with no of days difference from present date to each order date
days_diff <- function(sale){
  ref.date<-as.Date('2018-12-29',"%Y-%m-%d")
  sale <-cbind(sale,'days_diff'=as.Date(ref.date,"%Y-%m-%d") - as.Date(sale$OrderDate,"%Y-%m-%d"))
  return(sale)
}

  
  
  # Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
mgr_slsmn <- function(sale){
  ls=lapply(split(sale$SalesMan,sale$Manager),unique)
  return(ls)
}

  
  
  # Q5 For all regions find number of salesman and number of units
slsmn_units<-function(sale){
  
  ls=sale%>%group_by(Region)%>%summarise(list_of_salesman=length(unique(SalesMan)))
  ls$total_sales=(sale%>%group_by(Region)%>%summarise(total_sales=sum(Sale_amt)))['total_sales']
  return(ls)
}

  
  
  
  # Q6 Find total sales as percentage for each manager
sales_pct<-function(sale){
  k=sum(sale$Sale_amt)
  ls=sale %>% group_by(Manager) %>% summarise(percent_sales=sum(Sale_amt)*100/k)
  return(ls)
}

  
  
imdbdf <- read.csv('imdb.csv')
  # Q7 get imdb rating for fifth movie of dataframe
  fifth_movie<-function(imdbdf){
  v<-as.character(imdbdf$imdbRating)
  cat('imdb Rating of 5th movie is:',v[5])
}

  
  
  
  # Q8 return titles of movies with shortest and longest run time
movies<-function(imdbdf){
  imdbdf$duration<-as.numeric(imdbdf$duration)
  v<-imdbdf$duration
  v<-v[!is.na(v)]
  imdbdf<-imdbdf[!is.na(imdbdf$duration),]
  shortest.movie<-imdbdf[imdbdf$duration==min(v),'title']
  longest.movie<-imdbdf[imdbdf$duration==max(v),'title']
  cat('Shortest movie is:',as.character(shortest.movie),'\n')
  cat('Longest movie is:',as.character(longest.movie))
}

  
  
  # Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
sort_df<-function(imdbdf){
  imdbdf$imdbRating<-as.numeric(imdbdf$imdbRating)
  sorted.imdbdf <- order(imdbdf['year'],-imdbdf['imdbRating'])
  imdbdf<-imdbdf[sorted.imdbdf,]
  return(imdbdf[,c('title','year','imdbRating')])
}

  
  # Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
subset_df<-function(imdbdf){
  imdbdf$duration<-as.numeric(imdbdf$duration)
  return(imdbdf[((imdbdf$duration/60)>30) & ((imdbdf$duration/60)<180),])
}

  
  
diamondsdf <- read.csv('diamonds.csv')
  # Q11 count the duplicate rows of diamonds DataFrame.
  dupl_rows<-function(diamondsdf){
    dupe<-sum(as.numeric(duplicated(diamondsdf)))
    cat('Count of duplicate elements is:',dupe)
  }
  
  
  # Q12 droping those rows where any value in a row is missing in carat and cut columns
  drop_row<-function(diamondsdf){
    missingcarat.val<- is.na(diamondsdf$carat)
    missingcut.val <- is.na(diamondsdf$cut)
    miss.val<- missingcarat.val | missingcut.val
    df<-diamondsdf[!miss.val,]
    return(df)
  }
  
  
  # Q13 subset only numeric columns
  sub_numeric<-function(diamondsdf){
    v<-c()
    for(i in 1:length(diamondsdf)){
      if(class(diamondsdf[,i])=="numeric"){
        v<-c(v,i)
      }
    }
    #print(v)
    df <- diamondsdf[,v]
    return(df)
  }
  
  
  # Q14 compute volume as (x*y*z) when depth > 60 else 8
  volume<-function(diamondsdf){
    depth.filter<-diamondsdf$depth>60
    v<-c()
    for(i in 1:length(depth.filter)){
      if(depth.filter[i]){
        vol<-(as.numeric(as.character(diamondsdf$x[i]))*
                as.numeric(as.character(diamondsdf$y[i]))*
                as.numeric(as.character(diamondsdf$z[i])))
      }
      else{
        vol<-8
      }
      v<-c(v,vol)
    }
    return(v)
  }
  
  
  # Q15 impute missing price values with mean
  impute<-function(diamondsdf){
    missing.price<-!is.na(diamondsdf$price)
    prices<-diamondsdf$price[missing.price]
    diamondsdf$price[is.na(diamondsdf$price)] <- mean(prices)
    return(diamondsdf)
  }
  
  
  