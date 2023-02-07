#Silicon_Valley Chap.6

library(dplyr)

#����ð� ����
summary(sleep)
y = sleep$extra[sleep$group==1]
#y = filter(sleep,group==1) #extra,group,ID ��� �����
summary(y)
sd(y)

par(mfrow=c(2,2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y) #���Ժ���
hist(y,prob=TRUE) #��뵵�� ������׷�
lines(density(y),lty=2) #�е��Լ�

#�Ϻ���t����
t.test(y) #df(������), p��(����Ȯ��),�ŷڱ���,��� ����
          #��������
t.test(y,alternative="greater") #��������


