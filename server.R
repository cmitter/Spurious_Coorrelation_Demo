library(shiny)
library(MASS)

rt2 <- function(n=500,dft=15){ rt(n=n,df=dft) }
formals(rgamma)[1:2] <- c(500,1)
rchisq2 <- function(n=500,dfx=1){ rchisq(n=n,df=dfx) }
formals(rf)[1:3] <- c(500,1,15)
rexp2 <- function(n=500,rate2=1){ rexp(n=n,rate=rate2) }
formals(rbeta)[1:3] <- c(500,2,2)
shinyServer(function(input,output){
     dat <- reactive({
          dist <- switch(input$dist,
                         norm=rnorm,    unif=runif,    t=rt2, F=rf, gam=rgamma, exp=rexp2,    chisq=rchisq2, lnorm=rlnorm, beta=rbeta)
          
          def.args <- switch(input$dist,
                             norm=c(input$mean,input$sd), unif=c(input$min,input$max), t=c(input$dft), F=c(input$df1,input$df2),
                             gam=c(input$shape,input$rate), exp=c(input$rate2), chisq=c(input$dfx), lnorm=c(input$meanlog,input$sdlog), beta=c(input$shape1,input$shape2))
          
          f <- formals(dist);    f <- f[names(f)!="n"]; len <- min(length(f),3-1); f <- f[1:len]
          argList <- list(n=input$n*input$var)
          for(i in 1:len) argList[[names(f)[i]]] <- def.args[i]
          return(list(do.call(dist,argList),names(f)))
     })
     
     output$dist1 <- renderUI({
          input$dist
          isolate({
               lab <- switch(input$dist,
                             norm="Mean:", unif="Minimum:", t="Degrees of freedom:", F="Numerator degrees of freedom:", gam="Shape:", exp="Rate:",
                             chisq="Degrees of freedom:", lnorm="Mean(log):", beta="Alpha:")
               ini <- switch(input$dist,
                             norm=0, unif=0, t=15, F=1, gam=1, exp=1, chisq=1, lnorm=0, beta=2)
               numericInput(dat()[[2]][1],lab,ini)
          })
     })
     
     output$dist2 <- renderUI({
          input$dist
          isolate({
               lab <- switch(input$dist,
                             norm="Standard deviation:", unif="Maximum:", F="Denominator degrees of freedom:", gam="Rate:", lnorm="Standard deviation(log)", beta="Beta:")
               ini <- switch(input$dist,
                             norm=1, unif=1, F=15, gam=1, lnorm=1, beta=2)
               if(any(input$dist==c("norm","unif","F","gam","lnorm","beta"))) numericInput(dat()[[2]][2],lab,ini)
          })
     })
     
     output$dldat <- downloadHandler(
          filename = function() { paste(input$dist, '.csv', sep='') },
          content = function(file) {
               write.csv(data.frame(x=dat()[[1]]), file)
          }
     )

     output$plot <- renderPlot({
          hist(dat()[[1]],main="",xlab="Observations",col="orange",cex.axis=2,cex.lab=1.2,prob=T)
          if(input$density) lines(density(dat()[[1]],adjust=input$bw),lwd=2)
     })
     

     output$plot1 <- renderPlot({
          # transform random values into matrix
          mat<-matrix(as.numeric(dat()[[1]]),input$n) 
          cormat<-cor(mat)
          # take only values of the upper triangular matrix
          ut <- upper.tri(cormat)
          # vectorized correlations coefficients
          corr_m  =cormat[ut]
          hist(corr_m,main="",xlab="Correlation Coefficients",col="orange",cex.axis=2,cex.lab=1.2,prob=T)
          })

          
     
     output$plot2 <- renderPlot({
          
          X<-matrix(as.numeric(dat()[[1]]),input$n)
          nr_class1 <- floor(input$n/2)
          nr_class2 <- input$n - nr_class1
          Y<- c(rep(0,nr_class1),rep(1,nr_class1))
          df <- data.frame(X,Y)
          Acc <- rep(0,input$var)
          for (i in 1:input$var){
            fit<-lda(Y~X[,i],data=df) 
            t<-table(df$Y, predict(fit,df)$class)
            Acc[i] <- (t[1,1]+t[2,2])/input$n
          }
          hist(Acc,main="",xlab="Accuracy estimates",col="orange",cex.axis=2,cex.lab=1.2,prob=T)
     })
     
     output$summary <- renderPrint({
          #print("table to big")
     })
     output$usage <- renderPrint({
          txt1<-'1. Choose distribution type <BR> 2. Depending on the distribition choose parameters<BR>'
          txt2<-'3. Choose Sample size and Number of variables<P><P>'
          txt3<-'The distributions are calculated instantaneously<P>'
          txt4<-'Distr. of random variables shows the distribution of the raw data.<br> 
               Distr. of Correlations shows the distribution of the correlations coefficients of each variables with all others.<br>
               Distr. of Accuracy values shows the accuracy values after an prediction with linar discriminant analysis with an random 2-class asssignment'
  
          txt5<-'Source: Input navigation was build on <A href="https://github.com/ua-snap/shiny-apps/tree/master/RV_distributions">RV_distributions by leonawicz'
          
          HTML(c(txt1,txt2,txt3,txt4,txt5))
     })
     
     output$pageviews <-    renderText({
          if (!file.exists("pageviews.Rdata")) pageviews <- 0 else load(file="pageviews.Rdata")
          pageviews <- pageviews + 1
          save(pageviews,file="pageviews.Rdata")
          paste("Visits:",pageviews)
     })
     
})
