#'Hypothesis testing
#'
#'Takes dataframe having binary target variable
#'@params x= dataframe
#'@params y= Binary target variable
#'@export

hypothesis<-function(x,y)
{
  if (is.data.frlame(x)&&length(unique(y))==2)
  {
    m=c()
    n=c()
    c=c()
    for (i in 1:ncol(x))
    {
      if (class(i)=="factor"|((length(unique(x[,i]))/nrow(x)*100)<10&&(length(unique(x[,i])))>1))
      {
        test<-chisq.test(x[,i],y)
        m[i]=test$p.value
        n[i]=test$statistic
      }
      else if(is.numeric(x[,i]) && (length(unique(x[,i])))>1)
      {
        a<-x[,i][y==unique(y)[1]]
        b<-x[,i][y==unique(y)[2]]
        test=t.test(a,b)
        m[i]=test$p.value
        c[i]=test$statistic
      }
    }
    # t=matrix(c(m,c,n),ncol=3)
    #
    # names(t)=names(x)
    # data=data.frame(t)
    # print(t)
    Table = cbind(m,n,c)
    rownames(Table) = names(x)
    colnames(Table)=c("p-value","t-Statistic","X-Statistic")
    return(Table)




    if (class(y)=="numeric")
    {
      print("Anova Test")
      print(aov(y~as.matrix(x)))
    }

  }
  else
  {
   print("Target variable is not binary")
  }
}
