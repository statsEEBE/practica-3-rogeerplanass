#solucio problema 1
x<- c(0,1)
fx<- c(0.68,0.32)

#taula probabilitat
cbind(x,fx)

plot(x,fx,pch=16,col='red',ylim=c(0,1))
lines(x,fx,type='h',col='red')

mu<- sum(x*fx)
mu

sigmasq<-sum((x-mu)^2*fx) #resultat apartat A
sigmasq
n<-43
Y<-function(i)sum(sample(x,n, prob=fx,replace=TRUE))
#bucle

m<-100
muestra<-sapply(1:m,Y)
muestra
fi<-table(muestra)/m
fi

data.frame(fi)
barplot(fi)
#frequencies relatives
data.frame(fi,fi=cumsum(fi))

#taula prob
data.frame(Y=0:43)
prob=dbinom(0:43,43,0.32)
############
y<-0:43
fy<-dbinom(y,43,0.32)

plot(y,fy,pch=19,col='red') #binomial
lines(y,fy,col='red',type='h')

#taula prob
y<-0:44
Pi<-dbinom(y,44,0.32)
df<-data.frame(Y=0:44,prob=dbinom(0:44,44,0.32))
Fi<-cumsum(df$prob)
cbind(y,fi,Fi)
pbinom(16,44,0.32)
plot(y,Fi,type='s',col='red')
h