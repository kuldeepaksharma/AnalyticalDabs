choosefun <- function(df,n=3){
  i <- 1
  while(i >=1){
    sample <- sample_n(df,n)
    if(mean(sample$OBP,na.rm = T)>=OBP_constraint & sum(sample$AB,na.rm = T) >= AB_constraint & sample$AB > trunc(AB_constraint/3) & sum(sample$salary)<=sal_constaint) {
      print(paste('Mean OBP is',mean(sample$OBP,na.rm = T),'greater than', OBP_constraint))
      print(paste('Sum of AB is', sum(sample$AB,na.rm = T),'greater than',AB_constraint))
      print(paste('Sum of Salaries is', sum(sample$salary),'less than',sal_constaint))
      return(sample)
      break
    }
    else { i <- i+1}
    }
}
players_chosen <- choosefun(player_pool)

library(ggplot2)
library(plotly)
pl <- ggplot(filter(player_pool,player_pool$AB > trunc(AB_constraint/3)),aes(OBP,AB))
pl2 <- pl + geom_point(aes(color=salary),alpha=0.5,size=2) +
  geom_text(aes(label = playerID),color = 'red',data = players_chosen,check_overlap = T) + scale_colour_gradient(low='blue',high = 'red')
pl3 <- ggplotly(pl2)
print(pl3)

