#phistogram
hist(dif$pendapatan)

ggplot(data=dif)+
  geom_histogram(aes(x=pendapatan, y=..density..), col="green")+
  labs(x="variable X", y="variable Y")

#plothistogram dengan kurva


hist.pendapatan <- ggplot(data = dif) +
  geom_histogram(aes(x = pendapatan, y = ..density..), 
                 col = "blue", fill = "yellow", bins = 30) +
  geom_density(aes(x = pendapatan), col = "red", size = 1) + # Density curve
  labs(x = "Data Pendapatan", y = "Density", 
       title = "Histogram Pendapatan Dengan Kurva")

hist.pendapatan

hist.kebahagiaan <- ggplot(data=dif) +
  geom_histogram(aes(x=kebahagiaan, y=..density..), col="blue", fill='green') +
  labs(x="data kebahagiaan", y="Density") + #Density Curve
  geom_density(aes(x = kebahagiaan), col = "red", size = 1) + # Density curve
  labs(x = "Data Kebahagiaan", y = "Density", 
       title = "Histogram Kebagiaan Dengan Kurva")


hist.kebahagiaan

#scatter plot (Q-Q plot)
ggplot(data = dif, mapping = aes(sample=pendapatan))+
  stat_qq() + stat_qq_line()+labs(title = "Q-Q Plot Pendapatan")

ggplot(data = dif, mapping = aes(sample=kebahagiaan))+
  stat_qq() + stat_qq_line()+labs(title = "Q-Q Plot Kebahagiaan")


#Regresi Linear Sederhana
simpleregresi <- lm(kebahagiaan ~ pendapatan, dif)
summary(simpleregresi) #run pertama untuk di console

plot(simpleregresi) #run kedua munculin plot default plot() akan muncul 4

# 1. Plot data points pada grafik (scatter plot)
plot(dif$pendapatan, dif$kebahagiaan, 
     pch = 19, col = "green",
     main = "Hubungan Pendapatan dengan Kebahagiaan", #5 judul dan sumbu y dan x
     xlab = "Pendapatan", ylab = "Kebahagiaan")



# 2. Tambahkan garis regresi linear
abline(simpleregresi, col = "red")

# 3. Tambahkan persamaan untuk garis linear
equation <- paste("y = ", round(coef(simpleregresi)[1], 2), 
                  " + ", round(coef(simpleregresi)[2], 2), " * x", sep = "")

# Dynamically add the equation to the plot at a reasonable position
text(x = max(dif$pendapatan) * 0.7, y = max(dif$kebahagiaan) * 0.9, 
     labels = equation, col = "black", cex = 1.2)

