# install.packages("data.table")
# install.packages("ggplot2")
install.packages("magrittr")
install.packages("janitor")
library(magrittr) 
library(dplyr)
library(naniar)
library(janitor)

library(data.table)
library(ggplot2)
#theme_set(theme_minimal())
# library(ggfortify)
# library(magrittr)

source("@RemoveNoise.R")
source("@DownSample.R")
setwd("C:/Users/soton/Documents/FloridaExperiment")

#list.dirs(path = "F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv", full.names = TRUE, recursive = TRUE)
#Data_folder <- "F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv"
#file_location<-"F:/Works-UH/CPL/Spring 2019/scite-financial-scenario-csv/6003_3/Baseline"
#Data_folder<- "C:/scite-intelligence-scenario-csv"
Data_folder<- "C:/ReExtracted"
list_of_folder=list.dirs(Data_folder,full.names = TRUE,recursive = FALSE)
list_of_folder
length(list_of_folder)
plot_location<-"C:/Users/mhasan8/Documents/R/Plots/Rextracted/"


#list_of_folder=dir(Data_folder)
# file_name=list.files(file_location, pattern='*pp.csv',full.names = TRUE,recursive = FALSE)
# file_name

for (variable1 in list_of_folder) {
  merged_df_NR<- data.frame(Time=numeric(),
                            PP=numeric(), 
                            NR_PP=numeric(), 
                            Session=character())
  print(str(merged_df_NR))
  
  subfolder=list.dirs(variable1,full.names = TRUE,recursive = FALSE)
  subfolder
  subject_name=gsub("[a-zA-Z/:-]", "", variable1)
  subject_title=paste("Subject: ", subject_name)
  
  for (variable2 in subfolder) {
    tofind <- paste(c("Baseline","ExperimentalIT","Resting-Baseline","Experimental", "New-Resting-Baseline"), collapse="|")
    criteria=stringr::str_extract(string = variable2, pattern = tofind)
    print(criteria)
    file_name=list.files(variable2, pattern='*pp.csv',full.names = TRUE,recursive = FALSE)
    file_name
    df <- fread(file_name)
    
    
    if (nrow(df)>10) {
    colnames(df)<-c("Frame","Time", "TimeStamp","PP")
    df$TimeStamp<-NULL
    # print(str(df))
    # 
    # df$Frame <- as.numeric(as.character(df$Frame))
    # df$Time <- as.numeric(as.character(df$Time))
    # df$PP <- as.numeric(as.character(df$PP))
    # 
    # print(str(df))
    
    
    df$NR_PP=remove_noise(df$PP)
    print(str(df))
    df_NR<-downsample_using_mean(df,c("PP", "NR_PP"))
    print(str(df_NR))
    #df_NR<-na.omit(df_NR)
    df_NR <- df_NR %>% filter(NR_PP != '')
    
    
    if (criteria=="Baseline") {
      df_NR$Session="Baseline"
    }else if(criteria=="Experimental"){
      df_NR$Session="Experimental"
    }else if(criteria=="ExperimentalIT"){
      df_NR$Session="ExperimentalIT"
    }else if(criteria=="New-Resting-Baseline"){
      df_NR$Session="New-Resting-Baseline"
    }else{
      df_NR$Session="Resting-Baseline"
    }
    colnames(df_NR)<-c("Time", "PP", "NR_PP", "Session")
    merged_df_NR<-rbind(merged_df_NR,df_NR)
    }
  }
  merged_df_NR$NR_PP <- as.numeric(as.character(merged_df_NR$NR_PP))
  merged_df_NR <- na.omit(merged_df_NR)
  merged_df_NR <- merged_df_NR %>% filter(NR_PP != '')
  location_plot_saved<-paste(plot_location,subject_name,".pdf",sep="")
  y_max=max(merged_df_NR$NR_PP)
  subject_plot=Plotting(merged_df_NR, merged_df_NR$Time, merged_df_NR$NR_PP, merged_df_NR$Session,subject_title, y_max)
  ggsave(location_plot_saved, subject_plot, width = 12, height = 7, units = "in")
  
}
#subset_df<-merged_df_NR[c(1:100,3252:3351,6900:6999),]
# ex1<-merged_df_NR[c(3252:3351),]
# rb<-merged_df_NR[c(6900:6999),]
#x=min(merged_df_NR$NR_PP)





Plotting<-function(fun.data, fun.x, fun.y, session, name, y_max){
  t<-ggplot(data = fun.data) + 
    geom_line(aes(x = fun.x, y = fun.y, colour = session, group = session))+
    theme_bw()+
    theme(plot.margin=unit(c(1,1,1,1),"cm"),plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(title=name ,fun.data, x="Time", y="Perinasal Perspiration")+
    scale_y_continuous(limits=c(0,y_max))+
    theme(legend.position = "bottom")+
    #scale_color_manual(breaks= c("Resting-Baseline", "Baseline", "ExperimentalIT", "Experimental"), values = c("red", "red","blue", "green"))
    scale_color_manual(values = c("blue", "red", "green"))
}



# scale_y_continuous(breaks = seq(0,1, by =0.5), limits = c(0,1))
# plot<-ggplot(subset_df)+
#   geom_line(aes(x=Time, y=NR_PP, colour=Session, group=Session))+
#   ylim(0.002, 0.006)
#   
#   plot
# scale_x_continuous(breaks =seq(0,3500, by=500), limits = c(0,3500))
# scale_y_continuous(breaks = seq(0,1, by =0.1), limits = c(0,1))
