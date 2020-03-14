###AIR동아리 과제2-4###

#1.리스트>>객체를 생성하는 함수>>각 요소들이 이름을 가질 수 있고 서로 다른 종류의 객체를 구성요소로 가질 수 있다.
list(name="아무개", age=23, address="부산")
x <- list(num=c(21,20,25), "Nick", identity=diag(2))
x

x[[2]]     #리스트의 두번째 객체의 값
x[["num"]] #이름을 이용한 첫번째 객체의 값
x$identity #$를 이용한 세번째 객체의 값
x[[3]][1,] #세번째 객체의 값 중 1행 추출
x[1]       #첫번째 객체 추출
x[1:2]     #첫번째와 두번째 객체 추출



#2.recursive vector
pepper1 <- list(후추팩1=c("알갱이1_1","알갱이1_2","알갱이1_3"), 후추팩2=c("알갱이2_1","알갱이 2_2","알갱이2_3"))
pepper1

pepper2 <- list(후추팩1=list(후추팩2=c("알갱이2_1","알갱이2_2","알갱이2_3")))
pepper2

pepper3 <- list(작은후추통1=list(후추팩1=c("알갱이1_1","알갱이1_2","알갱이1_3"), 후추팩2=c("알갱이2_1","알갱이2_2","알갱이2_3")), 작은후추통2=c("아무거나","넣어보자"))
pepper3



#3.데이터 프레임>>리스트의 특수한 경우>>각 요소가 같은 길이를 가진다.
colors <- c("red","yellow","blue")
numbers <- c(1:3)
col.num <- data.frame(colors, numbers, more.numbers=c(4:6))
col.num

col.num[[2]]   #두번째 객체의 값
col.num$colors #$를 이용한 첫번째 객체의 값

col.num.copy <- col.num
attach(col.num.copy) #열이름만으로 열의 내용 확인 가능 시작
numbers
detach(col.num.copy) #열이름만으로 열의 내용 확인 가능 끝

str(col.num) #객체에 대한 전체적인 정보 확인