#######2019년도 AIR 동아리 과제1#######

#######R의 시작과 종료
#사칙연산
3+5
2*8

#q함수에 대한 설명
q

#R프로그래밍 종료
q()

#저장하지 않고 R프로그래밍 종료
#q(save="no")



#######R을 이용한 계산
#Exercise1
exp(2)*sin(5)/factorial(6)

#Exercise2
exp(tan(pi/5))*log10(5)/(exp(1)+tan(exp(1))+exp(1))

#Exercise3
k <- c(0,1,2,3,4)
(2^k)*(exp(-2))/factorial(k)

#Exercise4
x <- c(-3,-2,-1,0)
exp(-x^2/2)/sqrt(2*pi)



#######객체 할당
c(1,2,3,6)
vector1 <- c(1,2,3,6)
vector1

#자료의 평균 구하기
mean(vector1)

#자료의 정렬
sort(vector1)#오름차순
sort(vector1, decreasing=TRUE) #내림차순



#######스크립트 편집기
#독립된 문장 작성
x <- c(0,5,7,9,1,2,8)
mean(x)

#두 개의 문장을 한 라인에 표현할 때는 세미콜론(;) 명시
mean(x); sum(x)



#######내장함수와 온라인 도움말
#함수명을 알고 있을 때
?q
help(q)

#함수명을 모를 때_회귀와 관련된 함수
??"regression"
help.search("regression")



#######작업 디렉터리
#현재의 작업 디렉터리
getwd()

#작업디렉터리 위치 변경
setwd("c:/temp")
getwd()



#######작업공간 관리하기
#현재 작업공간 내에 저장되어 있는 객체 확인
ls()
objects()

#현재 로드되어 있는 라이브러리와 데이터 프레임 확인
search()

#현재 설치되어 있는 패키지 확인
library()

#객체 삭제
rm(k)
ls()

#현재 작업공간의 모든 객체들 삭제
rm(list=ls())
ls()

