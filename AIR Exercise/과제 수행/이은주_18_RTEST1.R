#####AIR RTEST1#####

#1-1)
sort_LEJ <- function(x){
	n <- length(x)
	for(i in n:1){
		k <- 1	
		while(k < i){
			if(x[k]>x[k+1]){
				p <- x[k+1]
				x[k+1] <- x[k]
				x[k] <- p
			}
			k <- k+1
		}
	}
	x
}

#1-2)
set.seed(1234)
x <- sample(1:100, 50, replace=T)
a <- sort_LEJ(x)
a
a[1] #�ּڰ�
a[50]	#�ִ�
(a[25]+a[26])/2 #�߾Ӱ�
sum(a)/50 #��հ�


