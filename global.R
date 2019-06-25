#puling data from sheet
rm(list=ls())
library(googlesheets)
library(ggplot2)
setwd("C:/Users/Shubham Srivastava/Documents/OYO/Escalations")
gs_auth()
key <- gs_url("https://docs.google.com/spreadsheets/d/1Hepp_Vw-INH3nDzrCbp8Dz9G-wTFTlyNae8FYcGNMR4/edit?ts=5d0b11b4#gid=0")
rnm <- gs_read(key, ws = "raw",range = "A1:L5250",col_names =  TRUE)


        ###preparing data from graph#######

#Count at property level
rnm$creation_date = as.Date(strptime(rnm$creation_date, "%Y-%m-%d" ))
df <- aggregate(rnm$t_id,by=list(rnm$creation_date,rnm$delta_days,rnm$Region,rnm$CH,rnm$SH,rnm$GM,rnm$ESM_Name,
                                 rnm$oyo_id,rnm$status,rnm$status1),length)
colnames(df)<- c("Date","days","RG","CH","SH","GM","ESM","oyo_id","status","level","count")
df$status <- factor(df$status,levels=c("open","approval_required","final_amount_approval_required",
                                       "approved" ,"quality_check_required","closed_with_dispute"))

