##Script to back up files to another drive.##
##Author: Jason D. Gibbs##

##Libraries
library(tidyverse)
library(readxl)
library(tictoc)
tic("Total")
##Inputs##
DestinationFolder<-"H:/"

##Read in list of folders to transfer##
FoldersToTransfer<-as.data.frame(read_xlsx("LocalFileList.xlsx"))


##Transfers files that don't exist on the backup drive.
tic("New File Transfer")
for(i in 1:nrow(FoldersToTransfer)){
file.copy(from=FoldersToTransfer[i, "Path"], to=DestinationFolder, recursive = TRUE, overwrite = FALSE)
}
toc()

tic("Modified File Transfer")
for (i in 1:nrow(FoldersToTransfer)){
  if(i==1){
  FileList<-as.data.frame(list.files(FoldersToTransfer[i,"Path"], full.names = TRUE, recursive = TRUE))
  }else{
    NextList<-as.data.frame(list.files(FoldersToTransfer[i,"Path"], full.names = TRUE, recursive = TRUE))
    FileList<-rbind(FileList,NextList)
  }

}
colnames(FileList) <- c("File_Path")

##Creates a column for modified time.##
FileList$Modified_Time<-file.mtime(FileList$File_Path)
FileList$Reduced_Path<-paste0(basename(dirname(FileList[,"File_Path"])),"/", basename(FileList[,"File_Path"]))

##Destination Folder
DestinationFiles<-as.data.frame(list.files(DestinationFolder, full.names=TRUE, recursive = TRUE))
colnames(DestinationFiles) <- c("File_Path")
##Fixes the destination file path to the approriate destination path.##
DestinationFiles$File_Path<-substring(DestinationFiles$File_Path, 5)
DestinationFiles$File_Path<-paste0(DestinationFolder, DestinationFiles$File_Path)
##Pulls the modified times.
DestinationFiles$Modified_Time<-file.mtime(DestinationFiles$File_Path)
DestinationFiles$Reduced_Path<-paste0(basename(dirname(DestinationFiles[,"File_Path"])),"/", basename(DestinationFiles[,"File_Path"]))

##Finds files that exist both in the main folder and the destination folder.
##Removes the files if they exist in both locations.##
RemovalList<-list()


for(i in 1:nrow(FileList)){
  FileNameFilter<-FileList[i,"Reduced_Path"]
  DestinationFileFilter<-filter(DestinationFiles, Reduced_Path==FileNameFilter)
    if(FileList[i, "Modified_Time"]<DestinationFileFilter[1,"Modified_Time"]){
      RemovalListAdd<-i
      RemovalList<-rbind(RemovalList,RemovalListAdd)
    }
}

rownames(FileList)<-NULL
RemovalList<-sapply(RemovalList,"[[",1)
UpdatedFiles<-FileList[-RemovalList,]
rownames(UpdatedFiles)<-NULL

if(nrow(UpdatedFiles>0)){
  UpdatedFiles$Destination_Path<-NA
  for(i in 1:nrow(UpdatedFiles)){
    FilterValue<-UpdatedFiles[i, "Reduced_Path"]
    DestinationFilter<-filter(DestinationFiles, Reduced_Path==FilterValue)
    UpdatedFiles[i,"Destination_Path"]<-DestinationFilter[1,"File_Path"]
    file.copy(from=UpdatedFiles[i, "File_Path"], to=UpdatedFiles[i,"Destination_Path"], recursive = FALSE, overwrite = TRUE)
  }
  print(paste0(i," modified file(s) updated."))
}else{
  print("No modified files found.")
  
}
toc()
toc()
