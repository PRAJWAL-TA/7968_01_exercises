import pandas, numpy
# Create the required data frames by reading in the files
df1=pd.read_excel('SaleData.xlsx',sheet_name='Sales Data')

#Dataset for questions 1-6 is df1(SaleData)
# Q1 Find least sales amount for each item
def least_sales(df):
    ls = df.groupby(["Item"])["Sale_amt"].min().reset_index()
    return ls

# Q2 compute total sales at each year X region
def sales_year_region(df):
    return df.groupby([pd.DatetimeIndex(df['OrderDate']).year,"Region","Item"])["Sale_amt"].sum()


# Q3 append column with no of days difference from present date to each order date
def days_diff(df,givenDate):
    from datetime import datetime
    days=[]
    order_dates = df["OrderDate"]
    for x in order_dates:
        days.append(datetime.strptime(givenDate, "%Y-%m-%d")-datetime.strptime(str(x)[0:10], "%Y-%m-%d"))
    df['days_diff']=days
    return df


# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
def mgr_slsmn(df):
    ms=df.groupby('Manager')['SalesMan'].unique().reset_index(name='list_of_salesman')
    return ms


# Q5 For all regions find number of salesman and number of units
def slsmn_units(df):
    su=pd.DataFrame()
    su['salesmen_count']=df.groupby('Region')['SalesMan'].nunique()
    su['total_sales']=df.groupby('Region')['Sale_amt'].sum()
    return su


# Q6 Find total sales as percentage for each manager
def sales_pct(df):
    k=df['Sale_amt'].sum()
    q10=(df.groupby('Manager')['Sale_amt'].sum()*100/k).reset_index(name="percent_sales")
    return q10


df2=pd.read_csv('imdb.csv',escapechar="\\")

#Dataset for questions 7-10 is df2(imdb)

# Q7 get imdb rating for fifth movie of dataframe
def fifth_movie(df):
    return 'imdb Rating of 5th movie is:'+str(df['imdbRating'][4])

    
# Q8 return titles of movies with shortest and longest run time
def movies(df):
    v=df['duration']
    m1=df[df['duration']==v.min()]['title']
    print('Movies with minimum duration are:')
    print(m1,'\n')
    m2=df[df['duration']==v.max()]['title']
    print('Movies with maximum duration are:')
    print(m2,'\n')


# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
def sort_df(df):
    return df.sort_values(by=['year','imdbRating'],ascending=[True,False])


# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
def subset_df(df):
    a=df['duration']>60*30
    b=df['duration']<60*180
    return df[a & b]


df3=pd.read_csv('diamonds.csv')

#Dataset for questions 11-15 is df3(diamonds)

# Q11 count the duplicate rows of diamonds DataFrame.
def dupl_rows(df):
    return len(df[df.duplicated()])

# Q12 droping those rows where any value in a row is missing in carat and cut columns
def drop_row(df):
    c1=df['carat'].isnull()
    c2=df['cut'].isnull()
    return df[~(c1 | c2)]


# Q13 subset only numeric columns
def sub_numeric(df):
    return df._get_numeric_data()


# Q14 compute volume as (x*y*z) when depth > 60 else 8
def volume(df):
    ls=df['depth']>60
    vol=[]
    for i in range(len(df)):
        if(ls[i]):
            if(df['z'][i]=='None'):
                vol.append(np.nan)  
            else:
                x=float(df['x'][i])*float(df['y'][i])*float((df['z'][i]))
                vol.append(x)
        else:
            vol.append(8)
    df['volume']=vol
    return df


# Q15 impute missing price values with mean
def impute(df):
    df['price']=df['price'].fillna(df['price'].mean())
    return df



#BONUS QUESTIONS
#Q1 Generate a report that tracks the various Genere combinations for each type year on year. The result
#data frame should contain type, Genere_combo, year, avg_rating, min_rating, max_rating,
#total_run_time_mins

#Dataset is imdb(df2)
def genre(df):
    df['GenreCombo']=df.loc[:,'Action':'Western'].T.apply(lambda g: '|'.join(g.index[g==1]),axis=0)
    gen=df.groupby(["type","year","GenreCombo"]).agg({"imdbRating":[min,max,np.mean],'duration':np.sum})
    return gen


#Q2 Is there a realation between the length of a movie title and the ratings ? Generate a report that captures
#the trend of the number of letters in movies titles over years. We expect a cross tab between the year of
#the video release and the quantile that length fall under. The results should contain year, min_length,
#max_length, num_videos_less_than25Percentile, num_videos_25_50Percentile ,
#num_videos_50_75Percentile, num_videos_greaterthan75Precentile

#Dataset is imdb(df2)
def titlelength_rating(df):
    return df['Length_of_title'].corr(df['imdbRating'])

def titlelength_year(df):
    df['Length_of_title']=df['title'].apply(lambda x:len(x.split('(')[0].replace(" ","").rstrip()))
    df['Quantile']=pd.qcut(df['Length_of_title'], 4, labels=False)
    ty = pd.crosstab(df.year,df.Quantile,margins=False)
    ty['min_len'] = df.groupby(["year"])["Length_of_title"].min()
    ty['max_len'] = df.groupby(["year"])["Length_of_title"].max()
    return ty
    

#Q3 In diamonds data set Using the volumne calculated above, create bins that have equal population within
#them. Generate a report that contains cross tab between bins and cut. Represent the number under
#each cell as a percentage of total.

#Dataset is diamonds(df3)
def cut_vol(df):
    ls=df['depth']>60
    vol=[]
    for i in range(len(df)):
        if(ls[i]):
            if(df['z'][i]=='None'):
                vol.append(np.nan)  
            else:
                x=float(df['x'][i])*float(df['y'][i])*float((df['z'][i]))
                vol.append(x)
        else:
            vol.append(8)
    df['volume']=vol
    df['Bins']=pd.qcut(df['volume'], 5, labels=False)
    cv= pd.crosstab(df.Bins,df.cut,normalize=True)*100
    return cv


#Q4 Generate a report that tracks the Avg. imdb rating year on year, for movies
#that are top performing. You can take the top 10% grossing movies every quarter. Add the number of top
#performing movies under each genere in the report as well.

mmd=pd.read_csv('movie_metadata.csv',escapechar="\\")

#Uses both imdb(df2) and movie_metadat(mmd) datasets

def avg_imdb_year_genres(mmd,df2):
    import math
    mmd["url"]=mmd['movie_imdb_link'].apply(lambda x:(x.split('?')[0]))
    t10g=pd.DataFrame()
    years=mmd['title_year'].unique()
    for x in years:
        eachyear=mmd[mmd['title_year']==x]
        sorted_ey=eachyear.sort_values(by=['gross'],ascending=False)
        t10g=t10g.append(sorted_ey.head(math.ceil(len(sorted_ey)*0.1)))
    #df2=pd.read_csv('imdb.csv',escapechar="\\")
    datfr=pd.merge(t10g,df2,on="url",how="left")
    genlist=datfr.loc[:,'Action':'Western'].columns.tolist()
    res=datfr.groupby('title_year')[genlist].sum()
    res['avg_imdb']=datfr.groupby('title_year')['imdb_score'].mean()
    return res


#Q5 Bucket the movies into deciles using the duration. Generate the report that tracks various features like
#nomiations, wins, count, top 3 geners in each decile.

#Dataset is imdb(df2)
def deciles(df):
    df['Deciles']=pd.qcut(df['duration'], 10, labels=False)
    ls=df.groupby(["Deciles"]).agg({"nrOfNominations":np.sum,'nrOfWins':np.sum})
    ls["TotalCount"]=df.groupby(["Deciles"])["year"].count()
    ls.columns=["TotalNrOfNominations","TotalNrOfWins","TotalCount"]
    g=(df.groupby("Deciles")[df.loc[:,'Action':'Western'].columns.tolist()].sum()).transpose()
    c=pd.DataFrame(g.apply(lambda x: x.nlargest(3).index,axis=0).transpose(),)
    c.columns=["1","2","3"]
    ls["TopGenres"]=c["1"]+","+c["2"]+","+c["3"]
    return ls


