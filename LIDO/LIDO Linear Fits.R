library(plotly)

getwd()
setwd("/Users/camilaburne/UT/Data Mining/Data-Mining-ECE-280/LIDO")

df <-  read.csv("df_linear_regression.csv")
names(df)
# Model 1) 

fig <- plot_ly(df, x = ~s1, y = ~s2, z = ~reference_mass_ratio, 
               color = ~remnant_mass)
fig <- fig %>% add_markers(size = 3)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'S1'),
                                   yaxis = list(title = 'S2'),
                                   zaxis = list(title = 'Mass Ratio')))
fig


### LM1  #### 

lm1 <- lm(remnant_mass ~ 
            reference_dimensionless_spin1_1 + 
            reference_dimensionless_spin1_2 + 
            reference_dimensionless_spin1_3 +
            reference_dimensionless_spin2_1 + 
            reference_dimensionless_spin2_2 + 
            reference_dimensionless_spin2_3 +
            reference_mass_ratio, df)

summary(lm1)


### LM2  #### 


lm2 <- lm(remnant_mass ~ 
            reference_dimensionless_spin1_1 + 
            reference_dimensionless_spin1_2 + 
            reference_dimensionless_spin1_3 +
            reference_dimensionless_spin2_1 + 
            reference_dimensionless_spin2_2 + 
            reference_dimensionless_spin2_3 +
            eta, df)

summary(lm2)
plot(lm2)


### LM3  #### 
df$remnant_mass_log = log(df$remnant_mass)
df$remnant_mass_sqrt = sqrt(df$remnant_mass)

names(df)
lm3 <- lm(remnant_mass ~ precessing + 
                  reference_dimensionless_spin1_3 +
                  reference_dimensionless_spin2_3 +
                  notspinning + 
                  s1 + s2 + 
                  eta , df)


summary(lm3)

plot(lm3)

### Manual Residual Plot  #### 


resi_plot = data.frame(cbind(df$remnant_mass, lm3$residuals, df$notspinning, df$precessing))

names(resi_plot) = c('y','residuals','notspinning','precessing')

colors <- c("grey","orangered", "navyblue")
plot(resi_plot$y,resi_plot$residuals, col = colors[factor(resi_plot$notspinning)])
colors <- c("orangered","grey", "navyblue")
plot(resi_plot$y,resi_plot$residuals, col = colors[factor(resi_plot$precessing)])

