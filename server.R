#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
rm(list=ls())
library(googlesheets)
library(ggplot2)
gs_auth()
key <- gs_url("https://docs.google.com/spreadsheets/d/1Hepp_Vw-INH3nDzrCbp8Dz9G-wTFTlyNae8FYcGNMR4/edit?ts=5d0b11b4#gid=0")
rnm <- gs_read(key, ws = "raw",range = "A1:L5250",col_names =  TRUE)


###preparing data#######

#aggregating number of ids for each status
rnm$creation_date = as.Date(strptime(rnm$creation_date, "%Y-%m-%d" ))
df <- aggregate(rnm$t_id,by=list(rnm$creation_date,rnm$delta_days,rnm$Region,rnm$CH,rnm$SH,rnm$GM,rnm$ESM_Name,
                                 rnm$oyo_id,rnm$status,rnm$status1),length)
colnames(df)<- c("Date","days","RG","CH","SH","GM","ESM","oyo_id","status","level","count")

#sorting status
df$status <- factor(df$status,levels=c("open","approval_required","final_amount_approval_required",
                                       "approved" ,"quality_check_required","closed_with_dispute"))

#server file
server <- shinyServer(function(input, output) {
  
  #Dropdowns for SH, GM, ESM and Property
  output$RGInput <- renderUI({
    selectInput(
      inputId = "rg",
      label = "Select a Region:",
      choices = c(levels(factor(df$RG)),"All")
    )
  })
  output$CHInput <- renderUI({
    selectInput(
      inputId = "ch",
      label = "Select a CH:",
      choices = c(levels(factor(df$CH[df$RG==input$rg])),"All")
    )
  })
  output$SHInput <- renderUI({
    selectInput(
      inputId = "sh",
      label = "Select a SH:",
      choices = c(levels(factor(df$SH[(df$CH==input$ch) & (df$RG==input$rg)])),"All")
    )
  })
  
  output$GMInput <- renderUI({
    selectInput(
      inputId = "gm",
      label = "Select a GM:",
      choices = c(levels(factor(df$GM[(df$SH==input$sh) & (df$CH==input$ch) & (df$RG==input$rg)])), "All")
    )
  })
  
  output$ESMInput <- renderUI({
    selectInput(
      inputId = "esm",
      label = "Select an ESM:",
      choices = c(levels(factor(df$ESM[(df$SH==input$sh) &(df$GM==input$gm) & (df$CH==input$ch)
                                       & (df$RG==input$rg)] )),"All")
    )
  })
  
  output$propInput <- renderUI({
    selectInput(
      inputId = "prop",
      label = "Select a property:",
      choices = c(levels(factor(df$oyo_id[(df$ESM==input$esm) & (df$GM==input$gm) &
                                            (df$SH==input$sh) & (df$CH==input$ch)
                                          & (df$RG==input$rg)])),"All")
    )
  })
  
  output$Date <- renderUI({
    dateRangeInput(
      inputId = "date",
      label = "Select a date range",
      start = "2019-06-01",
      end = "2019-06-20",
      max = "2019-06-21"
    )
  })
  
  output$levelInput <- renderUI({
    selectInput(
      inputId = "level",
      label = "Pending level",
      choices = c(levels(factor(df$level)),"All")
    )
  })
  
  #reactive table
  
  #filtering on creation date
  
  
  
  #Main Panel bar plot
  output$statePlot <- renderPlot({
    if(input$level=="All"){
      dff = df[((df$Date>=input$date[1]) & (df$Date<=input$date[2])), ]
    }else{
      dff = df[((df$Date>=input$date[1]) & (df$Date<=input$date[2]) & (df$level==input$level)), ]}
    final = aggregate(list(dff$count,dff$days),by=list(dff$RG,dff$CH,dff$SH,dff$GM,dff$ESM,
                                                       dff$oyo_id,dff$status),FUN=sum)
    colnames(final)<-c("RG","CH","SH","GM","ESM","oyo_id","status", "count","dcount")
    
    #aggregation PAN India
    dfi = aggregate(list(final$count,final$dcount),by=list(final$status),FUN=sum)
    colnames(dfi)<-c("status", "count","dcount")
    
    #aggregation RG
    dfr = aggregate(list(final$count,final$dcount),by=list(final$RG,final$status),FUN=sum)
    colnames(dfr)<-c("RG", "status", "count","dcount")
    
    #aggregation CH
    dfc = aggregate(list(final$count,final$dcount),by=list(final$CH,final$status),FUN=sum)
    colnames(dfc)<-c("CH", "status", "count","dcount")
    
    #aggregation on SH level
    dfs = aggregate(list(final$count,final$dcount),by=list(final$SH,final$status),FUN=sum)
    colnames(dfs)<-c("SH", "status", "count","dcount")
    
    #aggregation on gm level on filtered data of creation date
    dfg = aggregate(list(final$count,final$dcount),by=list(final$GM,final$status),FUN=sum)
    colnames(dfg)<-c("GM", "status", "count","dcount")
    
    #aggregation on esm level on filtered data of creation date
    dfe = aggregate(list(final$count,final$dcount),by=list(final$ESM,final$status),FUN=sum)
    colnames(dfe)<-c("ESM", "status", "count", "dcount")
    
    #plotting
    if (input$rg=="All"){
      num = dfi$dcount
      den = dfi$count
      avg = floor(num/den)
      lab = dfi$status
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=180*avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*180*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./180))
    }
    else if (input$ch=="All"){
      num = dfr$dcount[dfr$RG==input$rg]
      den = dfr$count[dfr$RG==input$rg]
      avg = floor(num/den)
      lab = dfr$status[dfr$RG==input$rg]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./10))
    }
    
    else if (input$sh=="All"){
      num = dfc$dcount[dfc$CH==input$ch]
      den = dfc$count[dfc$CH==input$ch]
      avg = floor(num/den)
      lab = dfc$status[dfc$CH==input$ch]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./5))
    }
    
    else if (input$gm=="All"){
      num = dfs$dcount[dfs$SH==input$sh]
      den = dfs$count[dfs$SH==input$sh]
      avg = floor(num/den)
      lab = dfs$status[dfs$SH==input$sh]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./1))
    }else if((input$esm=="All") & (input$prop=="All")){
      num = dfg$dcount[dfg$GM==input$gm]
      den = dfg$count[dfg$GM==input$gm]
      avg = floor(num/den)
      lab = dfg$status[dfg$GM==input$gm]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./1))
    }else if(input$prop=="All"){
      num = dfe$dcount[dfe$ESM==input$esm]
      den = dfe$count[dfe$ESM==input$esm]
      avg = floor(num/den)
      lab = dfe$status[dfe$ESM==input$esm]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg, group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./1))
      
    }else{
      num = final$dcount[(final$GM==input$gm) & (final$ESM==input$esm)& (final$oyo_id==input$prop)]
      den = final$count[(final$GM==input$gm) & (final$ESM==input$esm)& (final$oyo_id==input$prop)]
      avg = floor(num/den)
      lab = final$status[(final$GM==input$gm) & (final$ESM==input$esm)& (final$oyo_id==input$prop)]
      temp = data.frame(den,avg,lab)
      ggplot(temp)  + 
        geom_bar(aes(x=lab, y=den, fill=lab),stat="identity", colour="sienna3")+
        geom_line(aes(x=lab, y=avg,group=1),stat="identity",colour='black')+
        geom_text(aes(label=avg, x=lab, y=0.95*avg), colour="black")+
        geom_text(aes(label=den, x=lab, y=0.95*den), colour="black")+
        scale_y_continuous(sec.axis = sec_axis(~./1))
    }
  })
  
  #pie chart
  output$pie <- renderPlot({
    dff = df[((df$Date>=input$date[1]) & (df$Date<=input$date[2])), ]
    final = aggregate(dff$count,by=list(dff$RG,dff$CH,dff$SH,dff$GM,dff$ESM,
                                        dff$oyo_id,dff$level),FUN=sum)
    colnames(final)<-c("RG","CH","SH","GM","ESM","oyo_id","level", "count")
    
    #aggregation PAN India
    dfi = aggregate(final$count,by=list(final$level),FUN=sum)
    colnames(dfi)<-c("level", "count")
    
    #aggregation RG
    dfr = aggregate(final$count,by=list(final$RG,final$level),FUN=sum)
    colnames(dfr)<-c("RG", "level", "count")
    
    #aggregation CH
    dfc = aggregate(final$count,by=list(final$CH,final$level),FUN=sum)
    colnames(dfc)<-c("CH", "level", "count")
    
    #aggregation on SH level
    dfs = aggregate(final$count,by=list(final$SH,final$level),FUN=sum)
    colnames(dfs)<-c("SH", "level", "count")
    
    #aggregation on esm level on filtered data of creation date
    dfg = aggregate(final$count,by=list(final$GM,final$level),FUN=sum)
    colnames(dfg)<-c("GM","level","count")
    
    #aggregation on esm level on filtered data of creation date
    dfe = aggregate(final$count,by=list(final$ESM,final$level),FUN=sum)
    colnames(dfe)<-c("ESM","level","count")
    
    #plotting pie
    if(input$rg=="All"){
      num = dfi$count
      den = sum(dfi$count)
      per = floor(num/den*100)
      lab = dfi$level
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))}
    else if(input$ch=="All"){
      num = dfr$count[(dfr$RG==input$rg)]
      den = sum(dfr$count[(dfr$RG==input$rg)])
      per = floor(num/den*100)
      lab = dfr$level[(dfr$RG==input$rg)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))}
    else if(input$sh=="All"){
      num = dfc$count[(dfc$CH==input$ch)]
      den = sum(dfc$count[(dfc$CH==input$ch)])
      per = floor(num/den*100)
      lab = dfc$level[(dfc$CH==input$ch)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))}
    else if(input$gm=="All"){
      num = dfs$count[(dfs$SH==input$sh)]
      den = sum(dfs$count[(dfs$SH==input$sh)])
      per = floor(num/den*100)
      lab = dfs$level[(dfs$SH==input$sh)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))}
    else if((input$esm=="All") & (input$prop=="All")){
      num = dfg$count[(dfg$GM==input$gm)]
      den = sum(dfg$count[(dfg$GM==input$gm)])
      per = floor(num/den*100)
      lab = dfg$level[(dfg$GM==input$gm)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))
    }else if(input$prop=="All"){
      num = dfe$count[(dfe$ESM==input$esm)]
      den = sum(dfe$count[(dfe$ESM==input$esm)])
      per = floor(num/den*100)
      lab = dfe$level[(dfe$ESM==input$esm)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))
      
    }else{
      num = final$count[(final$oyo_id==input$prop) & (final$ESM==input$esm)
                        & (final$GM==input$gm)]
      den = sum(final$count[(final$oyo_id==input$prop) & (final$ESM==input$esm)
                            & (final$GM==input$gm)])
      per = floor(num/den*100)
      lab = final$level[(final$oyo_id==input$prop) & (final$ESM==input$esm)
                        & (final$GM==input$gm)]
      temp= data.frame(per,lab)
      ggplot(temp, aes(x="", y=per, fill=lab))+
        geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
        geom_text(aes(label = per),position = position_stack(vjust = 0.5))}
    
    
  })
  
  #main panel filtered table show
  output$ticket <- renderDataTable({
    if(input$gm=="All"){
      rnm[((rnm$SH==input$sh) & (rnm$creation_date>=input$date[1])
           & (rnm$creation_date<=input$date[2])),] 
    }
    else if((input$esm=="All") & (input$prop=="All")){
      rnm[((rnm$SH==input$sh) & (rnm$GM==input$gm) & (rnm$creation_date>=input$date[1])
           & (rnm$creation_date<=input$date[2])),]
    }else if(input$prop=="All"){
      rnm[((rnm$SH==input$sh) & (rnm$GM==input$gm) & (rnm$ESM_Name==input$esm) & 
             (rnm$creation_date>=input$date[1]) & (rnm$creation_date<=input$date[2])),]
    }else{
      rnm[((rnm$SH==input$sh) & (rnm$GM==input$gm) & (rnm$ESM_Name==input$esm) & 
             (rnm$oyo_id==input$prop)
           & (rnm$creation_date>=input$date[1]) & (rnm$creation_date<=input$date[2])),]
    }
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_ticket", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(output$ticket, file, row.names = FALSE)
    }
  )
})


