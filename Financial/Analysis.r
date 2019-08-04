install.packages("data.table")
install.packages("ggplot2")
library(data.table)
library(ggplot2)
theme_set(theme_minimal())

library(ggfortify)
library(magrittr)


source("@RemoveNoise.R")
source("@DownSample.R")

list.dirs(path = "F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv", full.names = TRUE, recursive = TRUE)


Data_folder <- "F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv"
Data_folder<- "C:/Users/mhasan8/Desktop/new"


file_location<-"F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv/6003_3/Baseline"


list_of_folder=dir(Data_folder)
list_of_folder=list.dirs(Data_folder,full.names = TRUE,recursive = FALSE)

file_name=list.files(file_location, pattern='*pp.csv',full.names = TRUE,recursive = FALSE)
file_name

list_of_folder


merged_df_NR<- data.frame(Time=numeric(),
                           PP=numeric(), 
                           NR_PP=numeric(), 
                           Session=character())
print(str(merged_df_NR))
setwd("C:/Users/soton/Documents/R")


length(list_of_folder)
for (variable1 in list_of_folder) {
  subfolder=list.dirs(variable1,full.names = TRUE,recursive = FALSE)
  subject_name=gsub("[a-zA-Z/:8]", "", variable1)
  subject_name=paste("Subject: ", subject_name)
  
  for (variable2 in subfolder) {
    
    tofind <- paste(c("Baseline","ExperimentalIT","Resting-Baseline"), collapse="|")
    criteria=stringr::str_extract(string = variable2, pattern = tofind)
    print(criteria)
    file_name=list.files(variable2, pattern='*pp.csv',full.names = TRUE,recursive = FALSE)
    
    df <- fread(file_name)
    df$V1<-NULL
    colnames(df)<-c("Time", "PP")
    
    df$NR_PP=remove_noise(df$PP)
    print(str(df))
    df_NR<-downsample_using_mean(df,c("PP", "NR_PP"))
    print(str(df_NR))
    
    if (criteria=="Baseline") {
      df_NR$Session<-"Baseline"
    }else if(criteria=="ExperimentalIT"){
      df_NR$Session="ExperimentalIT"
    }else{
      df_NR$Session="Resting-Baseline"
    }
    colnames(df_NR)<-c("Time", "PP", "NR_PP", "Session")
    merged_df_NR<-rbind(merged_df_NR,df_NR)
  }
  
}

merged_df_NR$NR_PP <- as.numeric(as.character(merged_df_NR$NR_PP))

subset_df<-merged_df_NR[c(1:100,3252:3351,6900:6999),]


ex1<-merged_df_NR[c(3252:3351),]
rb<-merged_df_NR[c(6900:6999),]


merged_df_NR <- merged_df_NR[-which(merged_df_NR$NR_PP ==""), ]


x=min(merged_df_NR$NR_PP)
x
y=max(merged_df_NR$NR_PP)


Plot_=Plot(merged_df_NR, merged_df_NR$Time, merged_df_NR$NR_PP, merged_df_NR$Session,subject_name)
ggsave("C:/Users/soton/Documents/FloridaExperiment/Plots/1.pdf", Plot_, width = 12, height = 7, units = "in")



Plot<-function(fun.data, fun.x, fun.y, session, name){
  t<-ggplot(data = fun.data) + 
    geom_line(aes(x = fun.x, y = fun.y, colour = session, group = session))+
    theme(plot.margin=unit(c(1,1,1,1),"cm"),plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(title=name ,fun.data, x="Time", y="Perinasal Perspiration [0C2]")

    
}

scale_y_continuous(breaks = seq(0,1, by =0.5), limits = c(0,1))
theme(axis.title.y = seq(0,1, by =0.1))

scale_y_

theme(axis.text.y = element_blank())

plot<-ggplot(subset_df)+
  geom_line(aes(x=Time, y=NR_PP, colour=Session, group=Session))+
  ylim(0.002, 0.006)
  
  plot

scale_y_continuous(limits = c(0,1))
  theme(axis.text.y = element_blank())+

Plot_=Plot(merged_df_NR, merged_df_NR$Time, merged_df_NR$NR_PP, merged_df_NR$Session,subject_name)
ggsave("C:/Users/mhasan8/Documents/R/Plots/1.pdf", Plot_, width = 8, height = 4.5, units = "in")



Plot<-function(fun.data, fun.x, fun.y, session, name){
  t<-ggplot(data = fun.data) + 
    geom_line(aes(x = fun.x, y = fun.y, color = session, group = 1), size = 1) +
    scale_color_manual(values = c("#FC4E07", "#E7B800", "#00CCCC")) +
    theme_minimal()+
    labs(title=name ,fun.data, x="Time", y="Perinasal Perspiration")+
    scale_y_continuous(breaks = seq(0,1, by =0.1), limits = c(0,1))
    
}

ylim(x,y)



Plot<-function(fun.data, fun.x, fun.y, session, name){
  t<-ggplot(data = fun.data, aes(x = fun.x, y = fun.y, color=session)) + 
    geom_line(aes(color = session, group=session), size = 1) +
    scale_color_manual(values = c("#FC4E07", "#E7B800", "#00CCCC")) +
    theme_minimal()+
    labs(title=name ,fun.data, x="Time", y="Perinasal Perspiration")+
    ylim(x,y)
}

scale_x_continuous(breaks =seq(0,3500, by=500), limits = c(0,3500))
scale_y_continuous(breaks = seq(0,1, by =0.1), limits = c(0,1))


p<-ggplot(merged_df_NR)+
  geom_line(aes(Time, NR_PP, colour=Session, group=Session))
p