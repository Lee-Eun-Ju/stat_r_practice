

# install.packages("shiny")
# install.packages("DT")
# install.packages("shinythemes")
# install.packages("tidyverse")
# install.packages("ggmap")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("lubridate")
# install.packages("psych")
# install.packages("gplots")
# install.packages("devtools")
# install.packages("NbClust")

require("shiny")
require("DT")
require("shinythemes")
require("tidyverse")
require("ggmap")
require("ggplot2")
require("gridExtra")
require("raster")
require("rgeos")
require("maptools")
require("rgdal")
require("lubridate")
require("psych")
require("gplots")
require("devtools")
require("NbClust")

credit = read.csv("credit_card_data.csv", encoding = "UTF-8")
jeju = read.csv("jeju_financial_life_data.csv")

# Data 준비 ------------------------------------------------------------------------------------------------
# Error of Data
credit = credit %>%
    filter(!(year==2016 & month==1 & ages=="10대")) 

# By sex data
sex_credit = credit %>%
    filter( str_sub(pop_cd,1,1) == "Y" ) %>% 
    dplyr::select( -city)
sex_credit = sex_credit[,-1]
sex_credit$sex = factor(sex_credit$sex, levels=c("여자","남자"))

# area_credit 및 map data
area_credit = read.csv("area_credit.csv")
area_credit$id = as.factor(area_credit$id)
korea = shapefile("TL_SCCO_CTPRVN.shp")
korea = spTransform(korea, CRS("+proj=longlat"))
korea_map = fortify(korea)
korea_map$id = as.factor(korea_map$id)

# By time data
time_credit = credit %>% 
    filter( str_sub(pop_cd,1,1) == "L" ) %>% 
    dplyr::select( -sex)
time_credit$date = str_c(time_credit$year, "-", time_credit$month)
time_credit$date = ym(time_credit$date)
time_credit = time_credit %>% 
    filter(date != ym("2016-1"))

# jeju
jeju2 = jeju %>% 
    mutate(ages= ifelse(age<=29, "20대 이하", 
                        ifelse(age<=59, "30-50대", 
                               ifelse(age<=79, "60-70대", "80대 이상")))) %>% 
    dplyr::select(-zip_cd, -year_month, -age)

# jeju map
googleAPIkey = "AIzaSyDi-iGt22_890X9gzsk4oc7pYWALasBtls"
register_google(googleAPIkey)
jeju_map = get_map("jeju", zoom=10, maptype="roadmap")


# Clustering ---------------------------------------------------------------------------------------------
clustering_credit = credit %>%
    filter( str_sub(pop_cd,1,1) == "L" ) %>%  
    dplyr::select(-pop_cd, -year, -month, -avg_rat, - population, -sex)   
clustering_credit$group = str_c(clustering_credit$city,"-",clustering_credit$ages)
clustering_credit = clustering_credit %>% 
    group_by(group) %>% 
    summarise(avg_score=mean(avg_score),
              num_opencard=mean(num_opencard),
              num_usecard=mean(num_usecard),
              monthly_card_spend=mean(monthly_card_spend),
              monthly_lc=mean(monthly_lc),
              monthly_loan=mean(monthly_loan),
              monthly_bk_loan=mean(monthly_bk_loan),
              monthly_cd_loan=mean(monthly_cd_loan),
              monthly_installments_loan=mean(monthly_installments_loan),
              monthly_insurance_loan=mean(monthly_insurance_loan),
              monthly_sbk_loan=mean(monthly_sbk_loan),
              loan_commitment=mean(loan_commitment),
              inst_rep_loanb=mean(inst_rep_loanb),
              ls_rep_loanb=mean(ls_rep_loanb),
              credit_loan=mean(credit_loan),
              mortgage_loan=mean(mortgage_loan),
              credit_card_payment=mean(credit_card_payment),
              credit_card_installments_payment=mean(credit_card_installments_payment)      
    )

clustering_credit = as.data.frame(clustering_credit )
rownames(clustering_credit) = clustering_credit$group
clustering_credit = clustering_credit[,-1]

p=dim(clustering_credit)[2]
Z=scale(clustering_credit, center=TRUE, scale=TRUE)
pcfa<-psych::principal(Z, nfactors=3, rotate="varimax")
L=pcfa$loading[,1:3]
fpc=pcfa$scores
kmeans <- kmeans(fpc,8) 
cluster=data.frame(group=rownames(fpc), cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C7=cluster[(cluster[,2]==7),]
C8=cluster[(cluster[,2]==8),]






### Ui -----------------------------------------------------------------------------------------------------------

ui = fluidPage(
    theme = shinytheme("simplex"),
    navbarPage(
        "Project - Finantial Style",
        tabPanel("Credit Card",
                 sidebarPanel(
                     selectInput('Age', 'Select Age', unique(area_credit$ages)),
                     width=2
                 ),
                 mainPanel(
                     width=10,
                     tabsetPanel(
                     tabPanel("Data",
                              h5("pop_cd - L시리즈 : 연령대별 지역코드, Y시리즈 : 연령대별 성별구분"),
                              h5("year : 데이터 수집 시점 년도(2016, 2017), month : 데이터 수집 시점(월)"),
                              h5("city : 도시, sex : 성별(남자, 여자), ages : 나이(10대~90대)"),
                              h5("population : pop_cd로 구분된 그룹에 포함된 고객인원 수(경제활동 인구수)"),
                              h5("avg_rat : 신용 등급, avg_score : 신용 점수"),
                              h5("num_opencard 카드 개설 개수, num_usecard 카드 사용 개수"),
                              h5("monthly_card_spend 월별 카드 소비액"),
                              h5("monthly_loan : 월별 총 대출금액 / monthly_lc 월별 대출 약정금액 / monthly_bk_loan : 월별 은행 대출금액 "),
                              h5("monthly_cd_loan : 월별 카드 대출금액 / monthly_installments_loan : 월별 할부 대출금액 / monthly_insurance_loan : 월별 보험 대출금액"),
                              h5("monthly_sbk_loan : 월별 저축은행 대출금액"),
                              h5("loan_commitment : 한도 대출 금액(마이너스 통장) / inst_rep_loanb : 분할 상환 대출 금액"),
                              h5("ls_rep_loanb 일시 상환 대출 금액 / credit_loan 카드 대출 금액 / mortage_loan 담보 대출 금액"),
                              h5("credit_card_payment 신용카드 사용금액 / credit_card_installments_payment 신용카드 할부 사용 금액"),
                              br(),
                              br(),
                              DT::dataTableOutput("credit_data")
                              ),
                     tabPanel("By SEX", plotOutput("sexplot", height=600)),
                     tabPanel("By AREA", plotOutput("areaplot", height=600)),
                     tabPanel("By TIME", plotOutput("timeplot", height=600)),
                     tabPanel("Others", plotOutput("others", height=600))
                 ))),
        tabPanel("Clustering Analysis",
                 navlistPanel(
                     widths=c(2,10),
                     tabPanel("PCA",
                              fluidRow( 
                                  column(6, plotOutput("gof_plot")),
                                  column(4, tableOutput("pca_data")))),
                     tabPanel("Clustering",
                              fluidRow(column(12, verbatimTextOutput("clustering"))),
                              fluidRow(column(3, tableOutput("clust_data1")),
                                       column(3, tableOutput("clust_data2")),
                                       column(3, tableOutput("clust_data3")),
                                       column(3, tableOutput("clust_data4"))),
                              fluidRow(column(3, tableOutput("clust_data5")),
                                       column(3, tableOutput("clust_data6")),
                                       column(3, tableOutput("clust_data7")),
                                       column(3, tableOutput("clust_data8")))),
                     tabPanel("Result", 
                              fluidRow(column(12, plotOutput("clust_plot", height=300))),
                              br(),
                              br(),
                              fluidRow(column(7, plotOutput("heatmap")),
                                       column(3, h5("content"))))
                              )),
        tabPanel("Jeju Finantial Life",
                 sidebarPanel(
                   selectInput('Age2', 'Select Age', unique(jeju2$ages)),
                   width=2
                 ),
                 mainPanel(
                   width=10,
                   tabsetPanel(
                     tabPanel("Data",
                               h5("zip_cd : 제주특별자치도 신우편번호 / x_axis : 해당 우편번호의 경도 / y_axis : 해당 우편번호의 위도"),
                               h5("year_month : 데이터 수집 시점(201902) / sex : 성별 1.남성 2.여성 / age : 나이 24세이하 ~ 80세이상(5세 단위 구분)"),
                               h5("job_majorc : 대기업 고객 비율 / job_smallc : 중소기업 고객 비율 / job_public : 공기업 고객 비율 "),
                               h5("job_profession : 전문직 고객 비율 / job_none : 무직 고객 비율 / jop_self : 자영업 고객 비율 / job_other : 기타 고객 비율"),
                               h5("avg_income : 평균 연소득 / med_income : 연소득 중위수 /  avg_spend : 3개월간 평균 소비액 / avg_foreign_spend : 3개월간 해외 소비액"),
                               h5("avg_debt : 평균 총대출잔액 / avg_debt_credit : 평균 신용대출잔액"),
                               h5("avg_debt_noneb : 평균 비은행권대출잔액 / avg_debt_mortage : 평균 주택담보대출잔액"),
                               h5("avg_debt_deposit : 평균 예적금 또는 유가증권 담보대출 잔액 /  avg_debt_collateral : 평균 물건 대출 잔액"),
                               h5("avg_credit_rat : 평균 신용도 / vehicle_own_rat : 중대형 차량 소유 비율"),
                               h5("medium_resid_rat : 부동산 중대형 거주비율 / large_resid_rat : 부동산 대형 거주비율"),
                               DT::dataTableOutput("jeju_data")),
                     tabPanel("Job",plotOutput("jeju_job")),
                     tabPanel("Income", fluidRow(column(7,plotOutput("jeju_income", height=450)),
                                                 column(4,h5("content")))),
                     tabPanel("Asset", plotOutput("jeju_asset")),
                     tabPanel("Spend", fluidRow(column(7,plotOutput("jeju_spend", height=450)),
                                                column(4,h5("content")))),
                     tabPanel("Loan", fluidRow(column(6,plotOutput("jeju_loan", height=400)),
                                               column(6,plotOutput("jeju_loan2", height=400))))
                   )))
    )
)





### Server ---------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
    
    # Creditcard - Data ------------------------------------------------------------
    output$credit_data <-  DT::renderDataTable({DT::datatable( credit )})
    

    # Creditcard - By sex -----------------------------------------------------------
    output$sexplot = renderPlot({
        p1 = sex_credit %>% 
            mutate(reorder_ages = factor(sex_credit$ages, levels=levels(reorder(sex_credit$ages, sex_credit$avg_score, median, na.rim=TRUE)))) %>% 
            ggplot(aes(x=reorder_ages, y=avg_score)) +
            geom_boxplot(aes(color=sex), position="identity")  + ggtitle("나이별 및 성별 신용 점수")
        p2 = sex_credit %>% 
            mutate(reorder_ages = factor(sex_credit$ages, levels=levels(reorder(sex_credit$ages, sex_credit$num_usecard, median, na.rim=TRUE)))) %>% 
            ggplot(aes(x=reorder_ages, y=num_usecard)) +
            geom_boxplot(aes(color=sex), position="identity") + ggtitle("나이별 및 성별 카드 사용개수")
        p3 = sex_credit %>% 
            mutate(reorder_ages = factor(sex_credit$ages, levels=levels(reorder(sex_credit$ages, sex_credit$monthly_card_spend, median, na.rim=TRUE)))) %>% 
            ggplot(aes(x=reorder_ages, y=monthly_card_spend)) + 
            geom_boxplot(aes(color=sex), position="identity") + ggtitle("나이별 및 성별 카드 소비액")
        p4 = sex_credit %>% 
            group_by(sex) %>% 
            summarise( monthly_loan = mean(monthly_loan),
                       monthly_bk_loan = mean(monthly_bk_loan),
                       monthly_cd_loan = mean(monthly_cd_loan),
                       monthly_installments_loan = mean(monthly_installments_loan),
                       monthly_insurance_loan = mean(monthly_insurance_loan),
                       monthly_sbk_loan = mean(monthly_sbk_loan)) %>% 
            pivot_longer("monthly_loan":"monthly_sbk_loan", 
                         values_to = "value", names_to = "variable") %>% 
            mutate(variable = factor(variable, levels=levels(reorder(variable, value)))) %>% 
            ggplot() +
            geom_bar(aes(x=sex, y=value, fill=variable), position="dodge", stat="identity") +
            ggtitle("성별에 따른 월별 대출금액") + theme(legend.title = element_blank(), 
                                              legend.position = c(0.2, 0.8),
                                              legend.text = element_text(size = 6))
        p5 = sex_credit %>% 
            mutate(all_loanb = inst_rep_loanb + ls_rep_loanb) %>%  
            group_by(sex) %>% 
            summarise(all_loanb_mean = mean(all_loanb),
                      loan_commitment_mean = mean(loan_commitment),
                      inst_rep_loanb_mean = mean(inst_rep_loanb), 
                      ls_rep_loanb_mean = mean(ls_rep_loanb),
                      credit_loan_mean = mean(credit_loan),
                      mortgage_loan_mean = mean(mortgage_loan)
            ) %>% 
            pivot_longer("loan_commitment_mean":"mortgage_loan_mean", 
                         values_to = "value", names_to = "variable") %>% 
            ggplot() +
            geom_bar(aes(x=sex, y=value, fill=variable), position="dodge", stat="identity")+
            ggtitle("성별에 따른 대출잔액") + theme(legend.title = element_blank(), 
                                           legend.position = c(0.2, 0.8),
                                           legend.text = element_text(size = 6))
        
        grid.arrange(p1,p2,p3,p4,p5, ncol=3)
    })

    
    
    # Creditcard - By Area -----------------------------------------------------  
    output$areaplot <- renderPlot({
        p1 = area_credit %>% 
            filter(ages==input$Age) %>% 
            right_join(korea_map, by="id") %>% 
            ggplot(aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=num_usecard), colour="grey40") +
            scale_fill_gradient(low="grey",high="darkslategray") + 
            ggtitle("사용 중인 카드 개수")
        p2 = area_credit %>% 
            filter(ages==input$Age) %>% 
            right_join(korea_map, by="id") %>% 
            ggplot(aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=avg_score), colour="grey40") +
            scale_fill_gradient(low="grey",high="darkslategray") +
            ggtitle("신용 점수")
        p3 = area_credit %>% 
            filter(ages==input$Age) %>% 
            right_join(korea_map, by="id") %>% 
            ggplot(aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=monthly_card_spend), colour="grey40") +
            scale_fill_gradient(low="grey",high="darkslategray") +
            ggtitle("월별 카드 소비액")
        p4 = area_credit %>% 
            filter(ages==input$Age) %>% 
            right_join(korea_map, by="id") %>% 
            ggplot(aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=monthly_loan), colour="grey40") +
            scale_fill_gradient(low="grey",high="darkslategray") +
            ggtitle("월별 대출금액")
        
        grid.arrange(p1,p2,p3,p4, ncol=2)
    })
    
    
    # Creditcard - time ---------------------------------------------------------
    output$timeplot <- renderPlot({

        p1 = time_credit %>% 
            group_by(date, city) %>% 
            mutate(Credit_score = mean(avg_score)) %>% 
            ggplot(aes(x=date, y=Credit_score,
                   colour=fct_reorder2(city,date,Credit_score))) +
            geom_line() + geom_point() + labs(colour="city") + ggtitle("지역별 신용점수 Trend")
        p2 = time_credit %>% 
            group_by(date, city) %>% 
            mutate(Using_card = mean(num_usecard)) %>% 
            ggplot(aes(x=date, y=Using_card,
                       colour=fct_reorder2(city,date,Using_card))) +
            geom_line() + geom_point() + labs(colour="city") + ggtitle("지역별 카드사용개수 Trend")
        p3= time_credit %>% 
            group_by(date, city) %>% 
            mutate(Card_spend = mean(monthly_card_spend)) %>% 
            ggplot(aes(x=date, y=Card_spend,
                       colour=fct_reorder2(city,date,Card_spend))) +
            geom_line() + geom_point() + labs(colour="city") + ggtitle("지역별 카드소비액 Trend")
        p4 = time_credit %>% 
            group_by(date, city) %>% 
            mutate(Loan = mean(monthly_loan)) %>% 
            ggplot(aes(x=date, y=Loan,
                       colour=fct_reorder2(city,date,Loan))) +
            geom_line() + geom_point() + labs(colour="city") + ggtitle("지역별 월별 대출금액 Trend")
        
        grid.arrange(p1,p2,p3,p4, ncol=2)
        
    })
    
    # Creditcard - others -------------------------------------------------------
    output$others = renderPlot({
        p1 = credit %>% 
            ggplot(aes(x=num_usecard, y=monthly_card_spend)) +
            geom_point(aes(colour=ages)) + ggtitle("카드 사용 개수에 따른 카드 소비액")
        p2 = credit %>% 
            ggplot(aes(x=num_usecard, y=monthly_loan)) +
            geom_point(aes(colour=ages)) + ggtitle("카드 사용 개수에 따른 총 대출금액")
        p3 = credit %>% 
            ggplot(aes(x=monthly_loan, y=monthly_card_spend)) +
            geom_point(aes(colour=ages)) + ggtitle("카드 소비액과 대출 금액")
        p4 = credit %>% 
            ggplot(aes(x=avg_score, y=monthly_card_spend)) +
            geom_point(aes(colour=ages)) + ggtitle("신용 점수에 따른 카드 소비액")
        
        grid.arrange(p1,p2,p3,p4, ncol=2)
    })
    
    
    # Clustering ---------------------------------------------------------------------------------
    output$gof_plot = renderPlot({
        plot(pcfa$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")
    })
    
    output$pca_data = renderTable({
        round(L, 3)
    },rownames=TRUE)
    
    output$clustering = renderPrint({
        all<-NbClust(fpc, distance="euclidean", min.nc = 2, max.nc = 10,
                     method = "kmeans", index = "all")[[1]]
    })
    
    output$clust_data1 = renderTable({C1%>% head(5)}) 
    output$clust_data2 = renderTable({C2%>% head(5)})
    output$clust_data3 = renderTable({C3%>% head(5)})
    output$clust_data4 = renderTable({C4%>% head(5)})
    output$clust_data5 = renderTable({C5%>% head(5)})
    output$clust_data6 = renderTable({C6%>% head(5)})
    output$clust_data7 = renderTable({C7%>% head(5)})
    output$clust_data8 = renderTable({C8%>% head(5)})
    
    output$clust_plot = renderPlot({
        fpc = data.frame(group=rownames(fpc), fpc)
        data = fpc %>%
            inner_join(cluster, by=c("group"))
        p1 = data %>% ggplot(aes(x=RC1, y=RC2)) + geom_point(aes(colour=factor(cluster)))
        p2 = data %>% ggplot(aes(x=RC1, y=RC3)) + geom_point(aes(colour=factor(cluster)))
        p3 = data %>% ggplot(aes(x=RC2, y=RC3)) + geom_point(aes(colour=factor(cluster)))
        grid.arrange(p1,p2,p3, ncol=3)
    })
    
    
    output$heatmap = renderPlot({
        credit_group2 = data.frame(group=rownames(clustering_credit ), clustering_credit )
        data = credit_group2 %>%
            inner_join(cluster, by=c("group"))
        data_cluster = data %>% 
            group_by(cluster) %>% 
            summarise(avg_score=mean(avg_score),
                      num_opencard=mean(num_opencard),
                      num_usecard=mean(num_usecard),
                      monthly_card_spend=mean(monthly_card_spend),
                      monthly_lc=mean(monthly_lc),
                      monthly_loan=mean(monthly_loan),
                      monthly_bk_loan=mean(monthly_bk_loan),
                      monthly_cd_loan=mean(monthly_cd_loan),
                      monthly_installments_loan=mean(monthly_installments_loan),
                      monthly_insurance_loan=mean(monthly_insurance_loan),
                      monthly_sbk_loan=mean(monthly_sbk_loan),
                      loan_commitment=mean(loan_commitment),
                      inst_rep_loanb=mean(inst_rep_loanb),
                      ls_rep_loanb=mean(ls_rep_loanb),
                      credit_loan=mean(credit_loan),
                      mortgage_loan=mean(mortgage_loan),
                      credit_card_payment=mean(credit_card_payment),
                      credit_card_installments_payment=mean(credit_card_installments_payment))
        data_cluster = as.data.frame(data_cluster )
        rownames(data_cluster) = data_cluster$cluster
        data_cluster  = data_cluster[,-1]
        Z = scale(as.matrix(data_cluster), scale=TRUE, center=TRUE)
        heatmap.2(Z, col=bluered(100),trace="none",density.info="none", margins=c(10,10))
    })
    
    # Jeju - Data ------------------------------------------------------------
    output$jeju_data <-  DT::renderDataTable({DT::datatable( jeju )})
    
    output$jeju_job = renderPlot({
      ggmap(jeju_map) +
        jeju2 %>% 
        pivot_longer(job_majorc:job_other,
                     names_to = "job", values_to = "prop") %>% 
        filter(prop>0, ages==input$Age2) %>% 
        geom_point(mapping=aes(x=x_axis, y=y_axis, colour=job, alpha=prop)) +
        facet_wrap(~job, ncol=4)
    }, height=500, width=800)
    
    output$jeju_income = renderPlot({
      ggmap(jeju_map) +
        jeju2 %>% 
        mutate(income = ifelse(avg_income <=quantile(avg_income)[2], "연소득 하위 25%",
                               ifelse(avg_income <=quantile(avg_income)[4], "연소득 중산층", "연소득 상위 25%"))) %>% 
        geom_point(mapping=aes(x=x_axis, y=y_axis, colour=income)) +
        facet_wrap(~ages, ncol=2)
    })
    
    output$jeju_asset = renderPlot({
      ggmap(jeju_map) +
        jeju2 %>% 
        filter(medium_resid_rat>0, large_resid_rat>0, vehicle_own_rat>0) %>% 
        pivot_longer(medium_resid_rat:vehicle_own_rat,
                     names_to = "asset", values_to = "prop") %>% 
        geom_point(mapping = aes(x=x_axis, y=y_axis, colour=asset, size=prop)) +
        facet_wrap(~asset, ncol=3)
    })
    
    output$jeju_spend = renderPlot({
      ggmap(jeju_map) +
        jeju2 %>% 
        mutate(spend = ifelse(avg_spend <= quantile(avg_spend)[2], "0%-25%",
                              ifelse(avg_spend <= quantile(avg_spend)[3], "26%-50%",
                                     ifelse(avg_spend <= quantile(avg_spend)[4], "51%-75%","76%-100%")))) %>% 
        geom_point(mapping = aes(x=x_axis, y=y_axis, colour=spend)) +
        facet_wrap(~ages, ncol=2)
    })
    
    output$jeju_loan = renderPlot({
      jeju_loan = jeju2 %>% 
        pivot_longer(job_majorc:job_other,
                     names_to = "job", values_to = "prop") %>% 
        filter(prop>0)
      
      jeju_loan %>% 
        pivot_longer(avg_debt_credit:avg_debt_collateral,
                     names_to="debt", values_to="value") %>% 
        ggplot(aes(x=job, y=value, fill=debt)) +
        geom_bar(stat="identity")  +
        theme(axis.text.x=element_text(angle=45, hjust=1))
    })
    
    output$jeju_loan2 = renderPlot({
      ggmap(jeju_map) +
      jeju2 %>% 
        geom_point(mapping=aes(x=x_axis, y=y_axis, size=avg_debt, alpha=avg_debt), colour="darkslategray")
    })
}


### ShinyApp
shinyApp(ui = ui, server = server)

