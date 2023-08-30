setwd('C:/Users/mario/OneDrive/Área de Trabalho/mestrado/propabilidade estatistica/t1')
# Lendo os dados
lixo <- read.table("lixo.csv",head=T,sep = ';',dec = ',')


attach(lixo)

tabela_size <- table(Size)

# Gráfico qualitativa
png('grafico_size.png',family = 'Arial')
barplot(tabela_size,
        col = c('red','green','blue','yellow'),
        main = 'Tamanho da residência',
        xlab = 'Tamanho da residência',
        ylab = 'Frequência',
        legend = c('até 60 m²','de 60 a 100 m²','mais de 100 m²','7'))
dev.off()

# medidas de tendência
media <- colMeans(lixo)
mediana <- sapply(lixo, median)
variancia <- sapply(lixo, var)
desvio <- sapply(lixo, sd)
percentil_metal <- quantile(Metal,probs = 0.8)
percentil_paper <- quantile(Paper,probs = 0.8)
percentil_plastic <- quantile(Plastic,probs = 0.8)
tudo <- sapply(lixo, summary)

# boxplot
png('metal_boxplot.png')
boxplot(Metal,col='yellow',main='Metal')
dev.off()

png('metal_size_boxplot.png')
boxplot(Metal~Size,col='yellow',main='Metal')
dev.off()

png('paper_boxplot.png')
boxplot(Paper,col='blue',main='Paper')
dev.off()

png('paper_size_boxplot.png')
boxplot(Paper~Size,col='blue',main='Paper')
dev.off()

png('plastic_boxplot.png')
boxplot(Plastic,col='red',main='Plastic')
dev.off()

png('plastic_size_boxplot.png')
boxplot(Plastic~Size,col='red',main='Plastic')
dev.off()

png('all_boxplot.png')
par(mfrow=c(1,3))
boxplot(Metal,ylim=c(0,25),col='yellow',main='Metal')
boxplot(Paper,ylim=c(0,25),col='blue',main='Paper')
boxplot(Plastic,ylim=c(0,25),col='red',main='Plastic')
dev.off()

# Gráficos de dispersão
png('metalxpaper.png')
colors <- c("1" = "red", "2" = "blue", "3" = "green")
plot(Metal,Paper,pch=19,col=colors[Size],main="Metal x Paper (em libras)")
legend("topleft", legend = c('1','2','3'), col = unique(colors), pch = 19)
dev.off()

png('metalxplastic.png')
colors <- c("1" = "red", "2" = "blue", "3" = "green")
plot(Metal,Plastic,pch=19,col=colors[Size],main="Metal x Plastic (em libras)")
legend("topleft", legend = c('1','2','3'), col = unique(colors), pch = 19)
dev.off()

png('paperxplastic.png')
colors <- c("1" = "red", "2" = "blue", "3" = "green")
plot(Paper,Plastic,pch=19,col=colors[Size],main="Paper X Plastic (em libras)")
legend("topleft", legend = c('1','2','3'), col = unique(colors), pch = 19)
dev.off()


