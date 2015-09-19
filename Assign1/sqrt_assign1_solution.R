# entering the number of which we want to find the root say "a"
a <- 2
# entering the precetion
prec <- 5
# defining requared variables for the loop
x1 <-a/2
x2<-0
i<-0
# while loop
while(i==0)
  {
  x2 <- x1-(((x1^2)-a)/(2*x1))
  if (abs(x2-x1) <= 10^(-prec))
  {
    break
  }
  x1<-x2
  print(x2)
}
#options(digits = 20)
sprintf("%.20f",x2)
y <- sqrt(a)
sprintf("%.20f", y)
