#Script da aula 

#teste binomial no R
binom.test(8,14,0.75)

# Gráficos e observações
curve(dnorm(x,3,1),xlim = c(0,6))
curve(dchisq(x,3),xlim = c(0,6))
curve(dchisq(x,2),xlim = c(0,6))
curve(dchisq(x,5),xlim = c(0,6))
curve(dchisq(x,5),xlim = c(0,15))
curve(dchisq(x,50),xlim = c(0,15))
curve(dchisq(x,50),xlim = c(0,150))
quantis=seq(0.05,0.95,0.05)
normal=qnorm(quantis,3,1)
chi=qchisq(quantis,3)
df=data.frame(quantis,normal,chi)
df

#Gerando amostras
set.seed(123);x1=rnorm(30,3,1)
set.seed(45);x2=rchisq(30,3)
quantile(x1,0.05)
quantile(x2,0.05)

#Testando hipoteses

#Quantil 0.05

y=x1<=1.36
sum(y)
binom.test(2,30,0.05)

y=x2<=1.36
sum(y)
binom.test(10,30,0.05)

#Quantil 0.6
y=x1<=3.25
sum(y)
binom.test(19,30,0.6)

y=x2<=3.25
sum(y)
binom.test(22,30,0.6)
