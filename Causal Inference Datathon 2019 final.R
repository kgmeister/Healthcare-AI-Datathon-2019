#install.packages("CausalImpact")

library(CausalImpact)
setwd("C:/Programs and stuff/R Projects/Datathon 2019")

df_1 = read.csv("metric_final_updated.csv")
# df_1 = read.csv("C:/Users/adria/OneDrive/Desktop/NUS datathon/cov data/param_y_v2.csv")
df_2 = read.csv("pat_adm_no.csv")
df_3 = read.csv("age.csv")
df_4 = read.csv("emerg_prop.csv")
df_5 = read.csv("cci_score_weekly.csv")
#df_6 = read.csv("cov data/week_count.csv")
df_7 = read.csv("dis_married.csv")
df_8 = read.csv("adm_married.csv")
df9 = read.csv("death_rate2016.csv")

namer <- function(df)
{
  num <- ncol(df)
  name_list = c('y')
  for (i in seq(1,num-1))
  {
    name_list = append(name_list,paste0('x',i))
  }
  return(name_list)
}


new_df = cbind(df_1$count, 
               df_2$adm_pat_no, 
               #df_4$Prop_of_em, 
               df_5$cci_score, 
               df_7$count, 
               df_8$count,
               df9$patient_no # Mortality rate
               )

new_df = new_df[10:40,]

print(nrow(new_df))

pre.period <- c(1, 16)
post.period <- c(17,40)

new_df = data.frame(new_df)


names(new_df) = namer(new_df)



impact <- CausalImpact(new_df, pre.period, post.period, alpha=0.1,
                       model.args = list(niter=10000, prior.level.sd=0.3))
plot(impact)

impact.pred = impact$series$point.pred
actual = new_df$y

library(MLmetrics)

MAPE(impact.pred[13:17], actual[13:17])

plot(impact$model)

y_actual = new_df$y
y_pred = impact$series$point.pred
lower = impact$series$point.pred.lower
upper = impact$series$point.pred.upper

library(ggplot2)
library(Hmisc)

dat <- cbind(y_actual, y_pred, lower, upper) # make data

mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE){
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)}
  
a = as.data.frame(dat)
ggplot(a, aes(x=seq(1,31), y =y_pred)) + geom_vline(xintercept = 16) + 
  geom_line(aes(y = y_actual), color = "black") + 
  geom_line(aes(y = y_pred), color="blue", linetype="twodash") +
  geom_ribbon(aes(ymin=a$lower, ymax=a$upper), linetype=2, alpha=0.1)+
    theme_light()+
  ggtitle(" Bayesian structural time-series model") +
  xlab("Time") + ylab("Patient Outcome") + theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  ) 
