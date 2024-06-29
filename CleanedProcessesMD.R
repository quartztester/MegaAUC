####Packages####
install.packages("rio") #for easily importing any file extension type
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("MESS") #Used for AUC calculation, Stackoverflow
install.packages("rgl") # For plotting 3 Dimenssions
install.packages("RcppSimdJson") #The WAY faster version of jsonlite
install.packages("ggplot2")
install.packages("ploty") #Unused
install.packages("htmlwidgets")
install.packages("webshot")
install.packages("gridExtra")
install.packages(Cairo)
install.packages("caret") #Used for confusion matrix
install.packages("openxlsx")
install.packages("PRROC")
install.packages("httr") #for donwloading from onedrive
install.packages("report") #for citing packages

library("rio")
library("jsonlite")
library("tidyverse")
#library("MESS") #from stack overflow
library("rgl")
library("RcppSimdJson")
library(data.table)
library(dtplyr)
library(precrec)
library(foreach)

library(plotly)
library(htmlwidgets)
library(webshot)
library(gridExtra)
library(Cairo) - #to save PNG in 2D graphs
library(caret)

#library(doMPI)
library(doParallel) 

####REQUIRED - Set Working Directory####
work <- "/home/emake001/MDAnalysis/test3/" #Set your working directory here (forward slashes only)

####Setting Relative absolute path, Creating Process Folders####
  folders <- c("ref", "5a", "5b", "rde")
  
  setwd(work)
  lapply(folders, function(folder) {
    dir.create(file.path(work, folder))
  })
  
  ref  <- folders[[1]]
  md5a <- folders[[2]]
  md5b <- folders[[3]]
  rde  <- folders[[4]]
####Main links file####
  lilalink <- "http://lila.science/wp-content/uploads/2023/06/lila_camera_trap_datasets.csv"
  lilalist <- rio::import(lilalink)  # Download and import data directly
  
  # Remove row 10 and select specific columns
  lilalist <- lilalist[-c(10), c('short_name', 'metadata_url_gcp', 'mdv5a_results_raw', 'mdv5b_results_raw', 'md_results_with_rde')] #Excluding V4 - Snapshot Serengeti (-10) & removing nz-trailcams (-19), keep in mind columns and rows may change with more data

####Batch Download Metadata from lila csv####
  download_and_extract <- function(urls, subdir) {
    setwd(file.path(work, subdir))
    for (x in urls) {
      download.file(x, basename(x))
      temp <- list.files(pattern = ".zip$", full.names = TRUE)
      walk(temp, unzip, exdir = ".")
      file.remove(temp)
    }
  }
  
  download_and_extract(lilalist$metadata_url, "ref")
  download_and_extract(lilalist$mdv5a_results_raw, "5a")
  download_and_extract(lilalist$mdv5b_results_raw, "5b")
  download_and_extract(lilalist$md_results_with_rde, "rde")

####Reference Prep#####
  setwd(file.path(paste(work, ref,sep = ""))) 
  temp <- list.files(pattern = ".json$", full.names = TRUE) 
  
  ref_all <- lapply(temp, function(f) {
    print(f)
    
    write_lines(gsub(pattern = "NaN", replacement = "0", read_lines(f), perl = TRUE), f) # Read lines, replace "NaN" with "0", and write back to the file
    
    json <- fload(f, flatten = TRUE)
    comb <- json$annotations %>%
      select('image_id', 'category_id') %>%
      left_join(json$categories, by = c('category_id' = 'id')) %>%
      left_join(json$images %>% select('id', 'file_name'), by = c('image_id' = 'id')) %>%
      select('name', 'file_name')
    
    comb$dataset <- str_sub(f, 3) # Add dataset name to all rows
    gc()
    return(comb)
  })
  ref_all <- as.data.frame(bind_rows(ref_all))
  
  ref_all <- ref_all[ref_all$name != "#ref!", ]  #need to remove #ref!
  ref_all$ANI <- as.integer(!(ref_all$name %in% c("human", "Human", "empty", "car")))  #labeling anything not animal as 0
  ref_all$HUM <- as.integer(ref_all$name %in% c("human", "Human")) #labeling not human as 0
  
  saveRDS(ref_all, file = file.path(work, "ref_all.RDS"))
  rm(ref_all)

####5a prep####
  setwd(file.path(paste(work,md5a,sep = "")))
  temp <- list.files(pattern = ".json$", full.names = TRUE)
  
  results <- lapply(temp, function(f) {
    print(f)
    json <- data.frame(fload(f)) %>%
      select('images.file', 'images.detections') %>%
      filter(!is.na(images.detections)) %>%
      tidyr::unnest(images.detections, keep_empty = TRUE) %>%
      mutate(category = ifelse(is.na(category), 0, category),
             conf = ifelse(is.na(conf), 0, conf),
             bbox = NULL)
  })
  
  md5a_all <- bind_rows(results)
  setwd(file.path(work))
  saveRDS(md5a_all, file = "md5a_all.RDS")
  rm(md5a_all)
  
####5b prep####
  setwd(file.path(work, md5b))
  temp <- list.files(pattern = ".json$", full.names = TRUE)
  
  results <- lapply(temp, function(f) {
    print(f)
    json <- data.frame(fload(f)) %>%
      select('images.file', 'images.detections') %>%
      filter(!is.na(images.detections)) %>%
      tidyr::unnest(images.detections, keep_empty = TRUE) %>%
      mutate(category = ifelse(is.na(category), 0, category),
             conf = ifelse(is.na(conf), 0, conf),
             bbox = NULL)
  })
  
  md5b_all <- bind_rows(results)
  setwd(file.path(work))
  saveRDS(md5b_all, file = "md5b_all.RDS")
  rm(md5b_all)
  
####rde prep####
  setwd(file.path(paste(work, rde,sep = "")))
  temp <- list.files(pattern = ".json$", full.names = TRUE)
  
  results <- lapply(temp, function(f) {
    print(f)
    json <- fload(f) %>%
      as.data.frame() %>%
      select('images.file', 'images.detections') %>%
      filter(!is.na(images.detections)) %>%
      unnest(images.detections, keep_empty = TRUE) %>%
      mutate(
        category = as.numeric(category), # Convert 'category' to numeric
        category = if_else(is.na(category), 0, category),
        conf = if_else(is.na(conf), 0, conf),
        bbox = NULL,
        images.file = gsub("\\\\", "/", images.file)
      )
  })
  
  mdrde_all <- bind_rows(results)
  setwd(file.path(work))
  saveRDS(mdrde_all,file = "mdrde_all.RDS")
  rm(mdrde_all,results)
  
####Processing in Batch - Animal####
  setwd(file.path(work))
  
    ref_all <- readRDS("ref_all.RDS")
    
    #Plotting sequence of all names
    #test <- ref_all %>%
    #  group_by(name) %>%
    #  summarise(count=n()) %>%
    #  arrange(desc(count))
    
    ref_all <- ref_all %>%
      select(file_name, ANI, dataset) %>%
      arrange(file_name, desc(ANI)) %>% #Arranging each file name from 0 to 1
      distinct(file_name, .keep_all = TRUE)  #Get rid of duplicates
    
####Joining reference####
  setwd(file.path(work))
  versions <- c('md5a_all.RDS','md5b_all.RDS','mdrde_all.RDS')
  joined <- ""
    
  for(v in 1:length(versions)){
    print(versions[v])
    md <- readRDS(versions[v])
    
    md$conf[md$category != "1"] <- 0 #Anything not labeled as animal set to 0, 100% confidence not an animal
    md <- md %>%
      lazy_dt() %>%
      group_by(images.file) %>%
      summarise(MaxValue = max(conf), Product = prod(1-conf), Sum = sum(conf)) %>%
      mutate(Sum = pmin(Sum,1),Product = 1 - Product) %>%
      as.data.table(.) %>%
      left_join(.,ref_all,by = c('images.file' = 'file_name')) #Join reference and md
    
    saveRDS(md,file = paste(c(str_sub(versions[v],end=-9),"_join.RDS"),collapse = ""))
    joined[[v]] <- paste(c(str_sub(versions[v],end=-9),"_join.RDS"),collapse = "")
    rm(md)
  }

####Alpha Analysis####
  setwd(file.path(work))
  
  alpha <- expand.grid(Alpha1 = seq(0, 1, by = 0.1), Alpha2 = seq(0, 1, by = 0.1)) %>% #Expand here depending on wanted detail by =
    mutate(Alpha3 = 1 - (Alpha1 + Alpha2)) %>%
    filter(Alpha3 >= 0) %>%
    mutate(row = row_number()) %>%
    select(-row)
  
  joined <- c("md5a_join.RDS","md5b_join.RDS","mdrde_join.RDS")
  
  #cl <- startMPIcluster() #FOR MPI
  #registerDOMPI(cl) # FOR MPI
  #add line here for nodes, rmpi
 
  numCores <- detectCores() #FOR Parrellel Operation
  registerDoParallel(numCores)
  
  ####Per Enviroment Analysis####
  MDversions <-  foreach(i=1:length(joined))%do%{ #Cycle through each version
    print(joined[i])
    dataversion <- readRDS(joined[i])

    dataversion <- dataversion[!is.na(dataversion$ANI), ]#Need to get rid of the rows with NA
    dataversion <- dataversion[dataversion$dataset != "trail_camera_images_of_new_zealand_animals_1.00.json", ] #Get rid of NZ-cam-traps
    
    uniquename <- na.omit(unique(dataversion$dataset))
    uniquename <- uniquename[-c(1,2,3,4,5,6,7,11,12,13,14,15,16,17,18)] #temporary to reduce processing time for graph testing
    
    environments <- foreach(s=1:length(uniquename))%do%{ #Cycle through each environment
      print(uniquename[s]) 
      soloenv <- dataversion %>% filter(dataset == uniquename[s])
      m <- soloenv %>% select('MaxValue','Product','Sum') %>% as.matrix()
      abs <- soloenv %>% select('ANI') %>% as.data.table()
      
      values <- foreach (a=1:nrow(alpha),.packages = c('tidyverse','data.table','precrec'))%dopar%{
        e <- as.matrix(alpha[a,]) %>% as.vector()
        
        s <- m^matrix(e,nrow(m),ncol(m), byrow = TRUE)
        newp <- s[,1] * s[,2] * s[,3]
        ForAUC <- cbind(newp,abs)
        
        sscurves <- evalmod(scores = ForAUC$newp, labels = ForAUC$ANI)
        aucs <- precrec::auc(sscurves)
        
        e <- data.table::setDF(as.list(e))
        method <- merge(aucs,e)
      }
      values <- as.data.table(bind_rows(values))
      values$set <- uniquename[s] #adding the environment into results
      values
    }
    environments <- as.data.table(bind_rows(environments))
    environments$version <- joined[i] #adding the version into results
    environments
  }
  rm(abs,dataversion,m,soloenv,enviroments,values)
  
  ####Getting AUC calculations on entire version####
  wholeversions <-  foreach(i=1:length(joined))%do%{ #Cycle through each version
    print(joined[i])
    dataversion <- readRDS(joined[i])
    
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
    dataversion <- dataversion[dataversion$dataset != "trail_camera_images_of_new_zealand_animals_1.00.json", ] #Get rid of NZ-cam-traps
    
    m <- dataversion %>% select('MaxValue','Product','Sum') %>% as.matrix()
    abs <- dataversion %>% select('ANI') %>% as.data.table()
    
    values <- foreach (a=1:nrow(alpha),.packages = c('tidyverse','data.table','precrec'))%dopar%{
      e <- as.matrix(alpha[a,]) %>% as.vector()
      
      s <- m^matrix(e,nrow(m),ncol(m), byrow = TRUE)
      newp <- s[,1] * s[,2] * s[,3]
      ForAUC <- cbind(newp,abs)
      
      sscurves <- evalmod(scores = ForAUC$newp, labels = ForAUC$ANI)
      aucs <- precrec::auc(sscurves)
      
      e <- data.table::setDF(as.list(e))
      method <- merge(aucs,e)
    }
    values <- as.data.table(bind_rows(values))
    values$set <- joined[i] #adding the version into results
    values
  }
  rm(abs,dataversion,m,soloenv,environments,values)

  ####Import Fennell####
  Fennell <- "Fennell"
  dir.create(file.path(work,Fennell))
  setwd(file.path(paste(work,Fennell,sep = "")))
  Fennell_ref <- "https://raw.githubusercontent.com/mitch-fen/FennellBeirneBurton_2022/main/InputData/images_idents_clean.csv"
  Fennell_ref <- rio::import(Fennell_ref)
  Fennell_ref <- Fennell_ref %>% select('orig_file','latin_name','common_names')
  
  Fennell_ref$ANI <- as.integer(!(Fennell_ref$latin_name %in% c("Homo sapiens", "No animal")))  #labeling anything not animal as 0
  saveRDS(Fennell_ref,"Fennell_ref.RDS")
  
  #Download <- download #insert command to download
  fennell_download <- "https://olddominion-my.sharepoint.com/:u:/g/personal/emake001_odu_edu/EQD14HJ5VrRFo2LJOejFZEQB7o1S-JffuZLCRv7X3jxEtQ?e=GPqCSb"
  fennell_download <- download.file(fennell_download, "fenneltest.json")
  
  temp <- list.files(pattern = ".json$", full.names = TRUE)
  
    Fennell_both <- lapply(temp, function(f) {#Reading into R
      print(f)
      json <- data.frame(fload(f)) %>%
        select('images.file', 'images.detections') %>%
        filter(!is.na(images.detections)) %>%
        tidyr::unnest(images.detections, keep_empty = TRUE) %>%
        mutate(images.file = sub(".*/", "", images.file), # Extract anything after the last forward slash
               category = ifelse(is.na(category), 0, category),
               conf = ifelse(is.na(conf), 0, conf),
               bbox = NULL)
    })
  
    Fennell_combined <- foreach(v=1:length(Fennell_both))%do%{#Combining Reference
      print(v)
      md <- Fennell_both[[v]]
      
      md$conf[md$category != "1"] <- 0 #Anything not labeled as animal set to 0, 100%  confidence not an animal, this can change with human
      md <- md %>%
        lazy_dt() %>%
        group_by(images.file) %>%
        summarise(MaxValue = max(conf), Product = prod(1-conf), Sum = sum(conf)) %>%
        mutate(Sum = pmin(Sum,1),Product = 1 - Product) %>%
        as.data.table(.) %>%
        left_join(.,Fennell_ref,by = c('images.file' = 'orig_file')) #Join reference and md
      md <- md %>% filter(latin_name != "Homo sapiens") # Filter out rows where latin_name is "Homo sapiens"
      md
    }
    
    FennellAuc <- foreach(v=1:length(Fennell_combined))%do%{
      
      ver <- Fennell_combined[[v]]
        
      m <- ver %>% select('MaxValue','Product','Sum') %>% as.matrix()
      abs <- ver %>% select('ANI') %>% as.data.table()
      
        values <- foreach (a=1:nrow(alpha),.packages = c('tidyverse','data.table','precrec'))%dopar%{
          e <- as.matrix(alpha[a,]) %>% as.vector()
          
          s <- m^matrix(e,nrow(m),ncol(m), byrow = TRUE)
          newp <- s[,1] * s[,2] * s[,3]
          ForAUC <- cbind(newp,abs)
          
          sscurves <- evalmod(scores = ForAUC$newp, labels = ForAUC$ANI)
          aucs <- precrec::auc(sscurves)
          
          e <- data.table::setDF(as.list(e))
          method <- merge(aucs,e)
      }
      values <- as.data.table(bind_rows(values))
      values$version <- temp[v] #need to correct this to get the right version addition
      values
    }
    rm(abs,dataversion,m,soloenv,environments,values)

--
  #####Import Dipetto####
  Dipetto <- "Dipetto"
  dir.create(file.path(work,Dipetto))
  
  #need to add somewhere to download JSONs to folder, I am copying them over for now
  
  setwd(file.path(work, Dipetto))
  temp <- list.files(pattern = ".json$", full.names = TRUE)
  
  Dipetto <- lapply(temp, function(f) {
    print(f)
    json <- data.frame(fload(f)) %>%
      select('images.file', 'images.detections') %>%
      filter(!is.na(images.detections)) %>%
      tidyr::unnest(images.detections, keep_empty = TRUE) %>%
      mutate(category = ifelse(is.na(category), 0, category),
             conf = ifelse(is.na(conf), 0, conf),
             bbox = NULL)
  })
    
  dipettoversion <- c("DiPetto_md5a","DiPetto_md5b")
  
   dipetto_labeled <-  foreach(d=1:length(Dipetto))%do%{
    print(temp[d])
    dver <- Dipetto[[d]]
    dver$images.file <- substr(dver$images.file, start = 12, stop = nchar(dver$images.file))
    dver$conf[dver$category != "1"] <- 0 #Anything not labeled as animal set to 0
    
    #Obfuscations addition?
    
    dver <- dver %>%
      lazy_dt() %>%
      group_by(images.file) %>%
      summarise(MaxValue = max(conf), Product = prod(1-conf), Sum = sum(conf)) %>%
      mutate(Sum = pmin(Sum,1),Product = 1 - Product) %>%
      as.data.table(.)
    
    first_letter <- substr(dver$images.file, 1, 1)
    dver$ANI <- ""
    dver$ANI <- ifelse(first_letter == "B", 0, ifelse(first_letter == "E", 1, dver$ANI))
    dver$version <- dipettoversion[d]
    dver
  }

   dipettoauc <- foreach(i=1:length(dipetto_labeled))%do%{
    print(dipettoversion[i])
    dver <- dipetto_labeled[[i]]
    m <- dver %>% select('MaxValue','Product','Sum') %>% as.matrix()
    abs <- dver %>% select('ANI') %>% as.data.table()
    
    values <- foreach (a=1:nrow(alpha),.packages = c('tidyverse','data.table','precrec'))%dopar%{
      e <- as.matrix(alpha[a,]) %>% as.vector()
      
      s <- m^matrix(e,nrow(m),ncol(m), byrow = TRUE)
      newp <- s[,1] * s[,2] * s[,3]
      ForAUC <- cbind(newp,abs)
      
      sscurves <- evalmod(scores = ForAUC$newp, labels = ForAUC$ANI)
      aucs <- precrec::auc(sscurves)
      
      e <- data.table::setDF(as.list(e))
      method <- merge(aucs,e)
    }
    values <- as.data.table(bind_rows(values))
    values$version <- dipettoversion[i]
    values
  }
  rm(abs,dataversion,m,soloenv,environments,values)


  ####Seperating  "trail_camera_images_of_new_zealand_animals_1.00.json"####
  setwd(file.path(work))
  
  nzcamtrap <-  foreach(i=1:2)%do%{ #Cycle through each version, excluding RDE here
    print(joined[i])
    dataversion <- readRDS(joined[i])
    
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#Need to get rid of the rows with NA
    dataversion <- dataversion %>%
                  filter(dataset == "trail_camera_images_of_new_zealand_animals_1.00.json")
    
    m <- dataversion %>% select('MaxValue','Product','Sum') %>% as.matrix()
    abs <- dataversion %>% select('ANI') %>% as.data.table()
    
    values <- foreach (a=1:nrow(alpha),.packages = c('tidyverse','data.table','precrec'))%dopar%{
      e <- as.matrix(alpha[a,]) %>% as.vector()
      
      s <- m^matrix(e,nrow(m),ncol(m), byrow = TRUE)
      newp <- s[,1] * s[,2] * s[,3]
      ForAUC <- cbind(newp,abs)
      
      sscurves <- evalmod(scores = ForAUC$newp, labels = ForAUC$ANI)
      aucs <- precrec::auc(sscurves)
      
      e <- data.table::setDF(as.list(e))
      method <- merge(aucs,e)
    }
    values <- as.data.table(bind_rows(values))
    values$set <- joined[i] #adding the version into results
    values
  }
  rm(abs,dataversion,m,soloenv,environments,values)
  
  
  
  
  ####3D Graphing Alphas FOR R USE ONLY - NOT SAVED AS PNG####
    #MegaDetector
    #REMOVEDMegaDetectorEnvs <- as.data.table(bind_rows(MDversions))
    REMOVEDMDenvplots <- foreach(e=1:length(joined))%do%{
       test <- subset(MegaDetectorEnvs, version == joined[e])
       
       chart_title <- as.character(test$version[1])
       # Create the 3D scatter plot
       plot <- plot_ly(test, x = ~V1, y = ~V2, z = ~aucs, 
                        color = ~factor(curvetypes), colors = c('blue', 'red', 'green'),
                        symbol = ~factor(set), symbols = c('circle', 'square'),
                        type = 'scatter3d', mode = 'markers', 
                        marker = list(size = 5),
                        group = ~interaction(curvetypes, set)) %>%
         layout(scene = list(xaxis = list(title = 'V1'),
                             yaxis = list(title = 'V2'),
                             zaxis = list(title = 'AUC')),
                title = chart_title)  # Set the chart title
     }
  
    #REMOVED#MegaDetectorWhole <- as.data.table(bind_rows(wholeversions))
    REMOVEDMDwholeplots <- foreach(e=1:length(joined))%do%{
      test <- subset(MegaDetectorWhole, set == joined[e])
      
      # Group the data using group_by() from dplyr
      grouped_data <- test %>% 
        group_by(curvetypes, set)
      
      # Get unique values of 'set' column for chart title
      chart_title <- as.character(unique(test$set))
      
      # Create the 3D scatter plot
      plot <- plot_ly(data = grouped_data, x = ~V1, y = ~V2, z = ~aucs, 
                      color = ~factor(curvetypes), colors = c('blue', 'red', 'green'),
                      symbol = ~factor(set), symbols = c('circle', 'square'),
                      type = 'scatter3d', mode = 'markers', 
                      marker = list(size = 5)) %>%
        layout(scene = list(xaxis = list(title = 'V1'),
                            yaxis = list(title = 'V2'),
                            zaxis = list(title = 'AUC')),
               title = paste("Set:", chart_title))  # Set the chart title
      
      # Show the plot
      plot
    }
    
    #Fennell
    #REMOVED#FennellWhole <- as.data.table(bind_rows(FennellAuc))
    REMOVEDFennellwholeplots <- foreach(f=1:length(FennellAuc))%do%{
      test <- FennellAuc[[f]]
      
      # Group the data using group_by() from dplyr
      grouped_data <- test %>% 
        group_by(curvetypes)
      
      # Get unique values of 'set' column for chart title
      chart_title <- as.character(unique(test$version[1]))
      
      # Create the 3D scatter plot
      plot <- plot_ly(data = grouped_data, x = ~V1, y = ~V2, z = ~aucs, 
                      color = ~factor(curvetypes), colors = c('blue', 'red', 'green'),
                      type = 'scatter3d', mode = 'markers', 
                      marker = list(size = 5)) %>%
        layout(scene = list(xaxis = list(title = 'V1'),
                            yaxis = list(title = 'V2'),
                            zaxis = list(title = 'AUC')),
               title = paste(chart_title))  # Set the chart title
    }
    
    #DiPetto
    #REMOVED#DiPettoWhole <- as.data.table(bind_rows(DiPettoAuc))
     RREMOVEDDiPettowholeplots <- foreach(d=1:length(FennellAuc))%do%{
    test <- DiPettoAuc[[d]]
    
    # Group the data using group_by() from dplyr
    grouped_data <- test %>% 
      group_by(curvetypes)
    
    # Get unique values of 'set' column for chart title
    chart_title <- as.character(unique(test$version[1]))
    
    # Create the 3D scatter plot
    plot <- plot_ly(data = grouped_data, x = ~V1, y = ~V2, z = ~aucs, 
                    color = ~factor(curvetypes), colors = c('blue', 'red', 'green'),
                    type = 'scatter3d', mode = 'markers', 
                    marker = list(size = 5)) %>%
      layout(scene = list(xaxis = list(title = 'V1'),
                          yaxis = list(title = 'V2'),
                          zaxis = list(title = 'AUC')),
             title = paste(chart_title))  # Set the chart title
    }
  
  
  ####2d plot wholeversions####
  setwd(work)
     
  generate_plotw <- function(curvetype, V, title_x, title_y, a,l,u) {
    wholeversions[[a]] %>%
      filter(curvetypes == curvetype) %>%
      ggplot(aes(x = !!sym(V), y = aucs)) +
      geom_point() +
      labs(title = "", x = title_x, y = title_y) #+
      #scale_y_continuous(limits = c(l,u))  # Set y-axis limits 0.935, 1
  }
  
  plot101 <- generate_plotw("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "AUROC",1,0.94,0.97)
  plot201 <- generate_plotw("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.94,0.97)
  plot301 <- generate_plotw("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.94,0.97)
  plot401 <- generate_plotw("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "PRAUC",1,0.99,1)
  plot501 <- generate_plotw("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.99,1)
  plot601 <- generate_plotw("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.99,1)
  plot111 <- generate_plotw("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.94,0.97)
  plot222 <- generate_plotw("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.94,0.97)
  plot333 <- generate_plotw("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.94,0.97)
  plot444 <- generate_plotw("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.99,1)
  plot555 <- generate_plotw("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.99,1)
  plot666 <- generate_plotw("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.99,1)

  combined_whole <- grid.arrange(plot101,plot111,
                                plot201,plot222,
                                plot301,plot333,
                                plot401,plot444,
                                plot501,plot555,
                                plot601,plot666,
                                ncol = 6, nrow = 2, top = "MegaDetector Alpha Analysis Trained Data")
  
  ggsave("Internal_Alpha_Analysis.png", combined_whole, width = 4000, height = 1080, units = "px", device = "png", type = "cairo-png")
  rm(plot101,plot111,plot201,plot222,plot301,plot333,plot401,plot444,plot501,plot555,plot601,plot666)
  
  #####2d plot Fennell####
  generate_plotf <- function(curvetype, V, title_x, title_y, a,l,u) {
    FennellAuc[[a]] %>%
    filter(curvetypes == curvetype) %>%
    ggplot(aes(x = !!sym(V), y = aucs)) +
    geom_point() +
    labs(title = "", x = title_x, y = title_y) +
    scale_y_continuous(limits = c(l,u))  # Set y-axis limits 0.935, 1
  }
  
  plot1 <- generate_plotf("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "AUROC",1,0.94,0.97)
  plot2 <- generate_plotf("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.94,0.97)
  plot3 <- generate_plotf("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.94,0.97)
  plot4 <- generate_plotf("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "PRAUC",1,0.99,1)
  plot5 <- generate_plotf("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.99,1)
  plot6 <- generate_plotf("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.99,1)
  plot11 <- generate_plotf("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.94,0.97)
  plot22 <- generate_plotf("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.94,0.97)
  plot33 <- generate_plotf("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.94,0.97)
  plot44 <- generate_plotf("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.99,1)
  plot55 <- generate_plotf("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.99,1)
  plot66 <- generate_plotf("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.99,1)
  
  combined_fennell <- grid.arrange(plot1,plot11,
                                plot2,plot22,
                                plot3,plot33,
                                plot4,plot44,
                                plot5,plot55,
                                plot6,plot66,
                                ncol = 6, nrow = 2, top = "MegaDetector Alpha Analysis Fennell")  

  ggsave("Fennell_Alpha_Analysis.png", combined_fennell,width = 4000, height = 1080, units = "px", device = "png", type = "cairo-png")
  rm(plot1,plot11,plot2,plot22,plot3,plot33,plot4,plot44,plot5,plot55,plot6,plot66)
  
  ####2d plot Dipetto####
  generate_plotd <- function(curvetype, V, title_x, title_y, a,l,u) {
    dipettoauc[[a]] %>%
      filter(curvetypes == curvetype) %>%
      ggplot(aes(x = !!sym(V), y = aucs)) +
      geom_point() +
      labs(title = "", x = title_x, y = title_y) #+
    #scale_y_continuous(limits = c(l,u))  # Set y-axis limits 0.935, 1
  }
  
  plot91 <- generate_plotd("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "AUROC",1,0.94,0.97)
  plot92 <- generate_plotd("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.94,0.97)
  plot93 <- generate_plotd("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.94,0.97)
  plot94 <- generate_plotd("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "PRAUC",1,0.99,1)
  plot95 <- generate_plotd("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.99,1)
  plot96 <- generate_plotd("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.99,1)
  plot191 <- generate_plotd("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.94,0.97)
  plot192 <- generate_plotd("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.94,0.97)
  plot193 <- generate_plotd("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.94,0.97)
  plot194 <- generate_plotd("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.99,1)
  plot195 <- generate_plotd("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.99,1)
  plot196 <- generate_plotd("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.99,1)
  
  combined_dipetto <- grid.arrange(plot91,plot191,
                                plot92,plot192,
                                plot93,plot193,
                                plot94,plot194,
                                plot95,plot195,
                                plot96,plot196,
                                ncol = 6, nrow = 2, top = "MegaDetector Alpha Analysis DiPetto")  
  
  ggsave("DiPetto_Alpha_Analysis.png", combined_dipetto,width = 4000, height = 1080, units = "px", device = "png", type = "cairo-png")
  rm(plot91,plot191,plot92,plot192,plot93,plot193,plot94,plot194,plot95,plot195,plot96,plot196)


  
  
  ##2d plot "trail_camera_images_of_new_zealand_animals_1.00.json"####
  generate_plotz <- function(curvetype, V, title_x, title_y, a,l,u) {
    nzcamtrap[[a]] %>%
      filter(curvetypes == curvetype) %>%
      ggplot(aes(x = !!sym(V), y = aucs)) +
      geom_point() +
      labs(title = "", x = title_x, y = title_y) +
      scale_y_continuous(limits = c(l,u))  # Set y-axis limits 0.935, 1
  }
  
  plota <- generate_plotz("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "AUROC",1,0.95,0.96)
  plotb <- generate_plotz("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.95,0.96)
  plotc <- generate_plotz("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.95,0.96)
  plotd <- generate_plotz("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5a")), "PRAUC",1,0.9995,1)
  plote <- generate_plotz("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5a")), "",1,0.9995,1)
  plotf <- generate_plotz("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5a")), "",1,0.9995,1)
  plotaa <- generate_plotz("ROC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.95,0.96)
  plotbb <- generate_plotz("ROC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.95,0.96)
  plotcc <- generate_plotz("ROC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.95,0.96)
  plotdd <- generate_plotz("PRC", "V1",expression(paste(alpha, " " ["1"]," ","v5b")), "",2,0.9995,1)
  plotee <- generate_plotz("PRC", "V2",expression(paste(alpha, " " ["2"]," ","v5b")), "",2,0.9995,1)
  plotff <- generate_plotz("PRC", "V3",expression(paste(alpha, " " ["3"]," ","v5b")), "",2,0.9995,1)
  
  combined_fennell <- grid.arrange(plota,plotaa,
                                   plotb,plotbb,
                                   plotc,plotcc,
                                   plotd,plotdd,
                                   plote,plotee,
                                   plotf,plotff,
                                   ncol = 6, nrow = 2, top = "MegaDetector Alpha Analysis nz-camtraps")  
  
  ggsave("nzcamtraps_Alpha_Analysis.png", combined_fennell,width = 4000, height = 1080, units = "px", device = "png", type = "cairo-png")
  rm(plota,plotaa,plotb,plotbb,plotc,plotcc,plotd,plotdd,plote,plotee,plotf,plotff)
  
####Max Analysis####

  ####Per Enivroment Max Analysis####
  setwd(file.path(work))
  versions_max <-  foreach(i=1:length(joined))%do%{ #Cycle through each version
    print(joined[i])
    dataversion <- readRDS(joined[i]) #change this to i - right now pulling the same version
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
    
    uniquename <- na.omit(unique(dataversion$dataset))
    uniquename <- uniquename[-c(1,2,3,4,5,6,7,11,12,13,14,15,16,17,18)] #temporary - limited data amount - REMOVE LATER
    
    environments <- foreach(s=1:length(uniquename))%do%{ #Cycle through each environment
      print(uniquename[s]) 
      soloenv <- dataversion %>% filter(dataset == uniquename[s])
      
      m <- soloenv %>% select('MaxValue') %>% as.matrix()
      abs <- soloenv %>% select('ANI') %>% as.data.table()
      ForAUC <- cbind(m,abs)
      
      ForAUC$MaxValue[ForAUC$MaxValue < 0] <- 0
  
      ForAUC$set <- uniquename[s] #adding the enviroment into results
      ForAUC$version <- joined[i] #adding the version into results
      ForAUC
    }
  }
  
  ####Graphing of Versions####
  foreach(v=1:3)%do%{
    print(v)
    
    #captured_plot <- recordPlot()
    switch(v,
           png("verions-5a-internal.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 300),
           png("versions-5b-internal.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 300),
           png("versions-rde-internal.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 300)
    )
    
    graphversion <- versions_max[[v]]
    
    par(mfrow = c(1, 2))
    
    versionsdata <- graphversion[[v]] #setting up initial graph
    pred <- prediction(versionsdata$MaxValue, versionsdata$ANI)
    perf1 <- performance(pred,"tpr","fpr")

    plot(perf1, main = "ROC Curve", 
         colorize=TRUE,
         #print.cutoffs.at= c(0.2,0.8),
         text.adj=c(-0.3,0.5),
         colorize.palette = rev(rainbow(256, start = 1/6, end = 6/6)))
    
     foreach(i=1:length(uniquename))%do%{
      
      versionsdata <- graphversion[[i]]
      pred <- prediction(versionsdata$MaxValue, versionsdata$ANI)
      perf1 <- performance(pred,"tpr","fpr")
      
      plot(perf1, main = "ROC Curve", 
           colorize=TRUE,
           #print.cutoffs.at= c(0.2,0.8),
           text.adj=c(-0.3,0.5),
           colorize.palette = rev(rainbow(256, start = 1/6, end = 6/6)),
           add = TRUE)
      
    }
    
    perf2 <- performance(pred,"prec","rec")

    plot(perf2, main = "Precision-Recall Curve", 
         colorize=TRUE,
         #print.cutoffs.at= c(0.2,0.8),
         text.adj=c(-0.3,0.5),
         colorize.palette = rev(rainbow(256, start = 1/6, end = 6/6)))
    
    PRC <- foreach(i=1:length(uniquename))%do%{
    
      versionsdata <- graphversion[[i]]
      pred <- prediction(versionsdata$MaxValue, versionsdata$ANI)
      perf2 <- performance(pred,"prec","rec")
  
      plot(perf2, main = "Precision-Recall Curve", 
         colorize=TRUE,
         #print.cutoffs.at= c(0.2,0.8),
         text.adj=c(-0.3,0.5),
         colorize.palette = rev(rainbow(256, start = 1/6, end = 6/6)),
         add = TRUE)
    }
    
    par(mfrow = c(1, 1))
    switch(v,
           mtext("MD-v5a", line = 2.5, cex = 1.5, adj = 0.4),
           mtext("MD-v5b", line = 2.5, cex = 1.5, adj = 0.4),
           mtext("MD-v5a & rde", line = 2.5, cex = 1.5, adj = 0.4)
    )
    #replayPlot(captured_plot)
    dev.off()
  }
  
  ####Whole Version Max Analysis####
  whole_max <-  foreach(i=1:length(joined))%do%{ #Cycle through each version
    print(joined[i])
    dataversion <- readRDS(joined[i]) #change this to i - right now pulling the same version
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
      
      m <- dataversion %>% select('MaxValue') %>% as.matrix()
      abs <- dataversion %>% select('ANI') %>% as.data.table()
      ForAUC <- cbind(m,abs)
      
      ForAUC$version <- joined[i] #adding the version into results
      ForAUC
  }
  
  ####nz-camtrap max####
  nz_max <-  foreach(i=1:2)%do%{ #Cycle through each version
    print(joined[1])
    dataversion <- readRDS(joined[i]) #change this to i - right now pulling the same version
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
    dataversion <- dataversion %>%
      filter(dataset == "trail_camera_images_of_new_zealand_animals_1.00.json")
    
    m <- dataversion %>% select('MaxValue') %>% as.matrix()
    abs <- dataversion %>% select('ANI') %>% as.data.table()
    ForAUC <- cbind(m,abs)
    
    ForAUC$version <- joined[i] #adding the version into results
    ForAUC
  }

  ####Fennell Max####
  fennell_max <-  foreach(i=1:length(Fennell_combined))%do%{ #Cycle through each version
    print(Fennell_combined[i])
    dataversion <- Fennell_combined[[i]]
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
    
    m <- dataversion %>% select('MaxValue') %>% as.matrix()
    abs <- dataversion %>% select('ANI') %>% as.data.table()
    ForAUC <- cbind(m,abs)
    ForAUC
  }

  
  ####DiPetto Max####
  dipetto_max <-  foreach(i=1:length(dipetto_labeled))%do%{ #Cycle through each version
    print(dipettoversion[i])
    dataversion <- dipetto_labeled[[i]]
    dataversion <- dataversion[!is.na(dataversion$ANI), ]#need to get rid of the rows with NA
    
    m <- dataversion %>% select('MaxValue') %>% as.matrix()
    abs <- dataversion %>% select('ANI') %>% as.data.table()
    ForAUC <- cbind(m,abs)
    ForAUC
  }
  
  ####Graphing Max Analysis####
  
  ####ROCR Actual####
  process_performance <- function(data) {
    pred <- prediction(data$MaxValue, data$ANI)
    
    # Calculate ROC AUC
    perf_roc <- performance(pred, "tpr", "fpr")
    auc_roc <- performance(pred, "auc")
    auc_value_roc <- auc_roc@y.values[[1]]
    
    # Calculate PRC AUC
    perf_prc <- performance(pred, "prec", "rec")
    auc_prc <- performance(pred, "auc", "prec")
    auc_value_prc <- auc_prc@y.values[[1]]
    
    list(pred = pred, perf_roc = perf_roc, perf_prc = perf_prc, auc_value_roc = auc_value_roc, auc_value_prc = auc_value_prc)
  }
  
  whole5a_results <- process_performance(whole_max[[1]])
  fennell5a_results <- process_performance(fennell_max[[1]])
  dipetto5a_results <- process_performance(dipetto_max[[1]])
  
  whole5b_results <- process_performance(whole_max[[2]])
  fennell5b_results <- process_performance(fennell_max[[2]])
  dipetto5b_results <- process_performance(dipetto_max[[2]])
  
  #Plotting ROC & PRC for Three Different Datasets
  
  plot_ROC <- function(perf, add = FALSE,start = 1/6,end = 6/6){
    plot(perf, 
         main = "ROC Curves", 
         colorize = TRUE,
         colorkey = FALSE,
         print.cutoffs.at = c(),
         text.adj = c(-0.3, 0.5),
         colorize.palette = rev(rainbow(256, start = start, end = end)),
         ylim = c(0, 1),
         add = add)
    
    legend("bottomright", legend = c("MD Internal", "Fennell et al.","DiPetto"), col = c("red","blue","green"), lty = 1, cex = 0.6, title = "Dataset")
  }
  
  plot_PRC <- function(perf, add = FALSE,start = 1/6,end = 6/6) {
    plot(perf, 
         main = "PRC Curves", 
         colorize = TRUE,
         colorkey = FALSE,
         print.cutoffs.at = c(),
         text.adj = c(-0.3, 0.5),
         colorize.palette = rev(rainbow(256, start = start, end = end)),
         ylim = c(0, 1),
         add = add)
        
    legend("bottomleft", legend = c("MD Internal", "Fennell et al.","DiPetto"), col = c("red","blue","green"), lty = 1, cex = 0.6, title = "Dataset")
    #points(0.7297439, 0.999834, col = "red", pch = 19)#CHECK
  }

  #5a
  png("datasets-5a-ROCPRC.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 250)
  par(mfrow = c(1, 2))

  plot_ROC(whole5a_results$perf_roc, start = 5.9/6, end = 6/6)
  plot_ROC(fennell5a_results$perf_roc, add = TRUE, start = 3.9/6, end = 4/6)
  plot_ROC(dipetto5a_results$perf_roc, add = TRUE, start = 1.9/6, end = 2/6)
  
  plot_PRC(whole5a_results$perf_prc,start = 5.9/6, end = 6/6)
  plot_PRC(fennell5a_results$perf_prc, add = TRUE, start = 3.9/6, end = 4/6)
  plot_PRC(dipetto5a_results$perf_prc, add = TRUE, start = 1.9/6, end = 2/6)
  
  par(mfrow = c(1, 1))
  mtext("MD-v5a", line = 2.5, cex = 1.5, adj = 0.4)
  dev.off()
  
  #5b
  png("datasets-5b-ROCFRC.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 250)
  par(mfrow = c(1, 2))
  
  plot_ROC(whole5b_results$perf_roc, start = 5.9/6, end = 6/6)
  plot_ROC(fennell5b_results$perf_roc, add = TRUE, start = 3.9/6, end = 4/6)
  plot_ROC(dipetto5b_results$perf_roc, add = TRUE, start = 1.9/6, end = 2/6)
  
  plot_PRC(whole5b_results$perf_prc, start = 5.9/6, end = 6/6)
  plot_PRC(fennell5b_results$perf_prc, add = TRUE, start = 3.9/6, end = 4/6)
  plot_PRC(dipetto5b_results$perf_prc, add = TRUE, start = 1.9/6, end = 2/6)
  
  par(mfrow = c(1, 1))
  mtext("MD-v5b", line = 2.5, cex = 1.5, adj = 0.4)
  dev.off()
  
  ####Confusion Matrix Data Creation####
  
  num_cores <- detectCores()
  registerDoParallel(num_cores)
  
  addrow <- data.frame(tn = integer(),
                   fp = integer(),
                   fn = integer(),
                   tp = integer(),
                   precision = integer(),
                   recall = integer(),
                   sensitivity = integer(),
                   accuaracy = integer(),
                   f1 = integer(),
                   prevelance = integer())
  

  confidence_function <- function(input){
    confidences <- foreach(i = seq(0, 1, by = 0.05),.packages = c('caret'))%dopar%{
      threshold <- i
      
      # Convert probabilities to class predictions based on the threshold
      predictions_class <- ifelse(input$MaxValue >= threshold, 1, 0)
      
      # Ensure both predictions and actual labels are factors with the same levels
      predictions_class <- factor(predictions_class, levels = c(0, 1))
      actual_labels <- factor(input$ANI, levels = c(0, 1))
      
      # Create the confusion matrix
      cm <- confusionMatrix(predictions_class, actual_labels, positive = "1")
      
      tn <- cm[["table"]][1]
      fp <- cm[["table"]][2]
      fn <- cm[["table"]][3]
      tp <- cm[["table"]][4]
      
      precision <- cm[["byClass"]][["Precision"]]
      recall <- cm[["byClass"]][["Recall"]]
      #sensitivity <- cm[["byClass"]][["Sensitivity"]]
      specificity <- cm[["byClass"]][["Specificity"]]
      #accuaracy <- cm[["overall"]][["Accuracy"]]
      f1 <- cm[["byClass"]][["F1"]]
      #prevelance <- cm[["byClass"]][["Prevalence"]]
      
      addrow <- data.frame(confidence = i,
                           tn = tn,
                           fp = fp,
                           fn = fn,
                           tp = tp,
                           precision = precision,
                           recall = recall,
                           #sensitivity = sensitivity,
                           specificity = specificity,
                           #accuaracy = accuaracy,
                           f1 = f1#,
                           #prevelance = prevelance
                           )
      
      addrow
    }
    confidences <- as.data.frame(bind_rows(confidences))
    return(confidences)
  }
  
  whole5a_confidence <- confidence_function(whole_max[[1]])
  fennell5a_confidence <- confidence_function(fennell_max[[1]])
  dipetto5a_confidence <- confidence_function(dipetto_max[[1]])
  
  whole5b_confidence <- confidence_function(whole_max[[2]])
  fennell5b_confidence <- confidence_function(fennell_max[[2]])
  dipetto5b_confidence <- confidence_function(dipetto_max[[2]])
  
  ####REMOVED - Plotting of TP,TN,FP,FN####
  #plotcm <- function(input,title, show_legend = TRUE, ylab = "Image Count",ylim = NULL){
  #  
  #  plot(input$confidence,input$tp, pch = 19, cex = 0.5,xlab = "Confidence", ylab = ylab,ylim = ylim)
  #  points(input$confidence,input$tn, pch = 1, cex = 0.5)
  #  points(input$confidence,input$fp, pch = 2, cex = 0.5)
  #  points(input$confidence,input$fn, pch = 4, cex = 0.5)
  #  title(main = title)
    
    #if (show_legend) {
     # legend("topright", legend = c("True Positive", "True Negative", "False Positive", "False Negative"), 
      #       pch = c(19, 1, 2, 4), col = "black", cex = 0.8)
    #}
  #}
  
  #par(mfrow = c(1, 3))
  #plotcm(whole5a_confidence,"MD v5a")
  #plotcm(fennell5a_confidence,"Fennell v5a")
  #plotcm(dipetto5a_confidence,"Dipetto v5a", ylim = c(0,2649153))
  
  #par(mfrow = c(1, 3))
  #plotcm(whole5b_confidence,"MD v5b")
  #plotcm(fennell5b_confidence,"Fennell v5b")
  #plotcm(dipetto5b_confidence,"Dipetto v5b")
  
  
  
  
  ####Plotting Recall, Precision, Specificity, F1 Score####
  plotsingle <- function(input1,input2,input3,value,title,show_legend = FALSE){ #add as appropriate for inputs
    plot(input1$confidence,input1[[value]], pch = 5,xlab = "Confidence", ylab = title,ylim = c(0,1)) #Diamond
    points(input2$confidence,input2[[value]], pch = 3) #Cross
    points(input3$confidence,input3[[value]], pch = 8) #Star
    title(main = title)
    
    if (show_legend) {
      legend("bottomright", legend = c("MD Internal", "Fennell et al.", "DiPetto"), 
             pch = c(5, 3, 8), 
             title = "Legend")
    }
  }
  
  #5a
  png("datasets-5a-Confidence.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 200)
  par(mfrow = c(1, 4))
  plotsingle(whole5a_confidence,fennell5a_confidence,dipetto5a_confidence,"precision","v5a Precision",show_legend = TRUE)
  plotsingle(whole5a_confidence,fennell5a_confidence,dipetto5a_confidence,"recall","v5a Recall")
  plotsingle(whole5a_confidence,fennell5a_confidence,dipetto5a_confidence,"specificity","v5a Specificity")
  plotsingle(whole5a_confidence,fennell5a_confidence,dipetto5a_confidence,"f1","v5a F1 Score")
  dev.off()
  
  #5b
  png("datasets-5b-Confidence.png", type = "cairo", height = 1080, width = 1920, units = "px", res = 200)
  par(mfrow = c(1, 4))
  plotsingle(whole5b_confidence,fennell5b_confidence,dipetto5b_confidence,"precision","v5b Precision",show_legend = TRUE)
  plotsingle(whole5b_confidence,fennell5b_confidence,dipetto5b_confidence,"recall","v5b Recall")
  plotsingle(whole5b_confidence,fennell5b_confidence,dipetto5b_confidence,"specificity","v5b Specificity")
  plotsingle(whole5b_confidence,fennell5b_confidence,dipetto5b_confidence,"f1","v5b F1 Score")
  dev.off()

  ####Excel Output####
  
  export_to_excel <- function(file_path, ...) {
    # Create a new workbook
    wb <- createWorkbook()
    
    # Get the list of data.tables and their names
    data_tables <- list(...)
    sheet_names <- names(data_tables)
    
    # If sheet names are not provided, assign default names
    if (is.null(sheet_names)) {
      sheet_names <- paste0("Sheet", seq_along(data_tables))
    }
    
    # Add each data.table to a new sheet
    for (i in seq_along(data_tables)) {
      addWorksheet(wb, sheet_names[i])
      writeData(wb, sheet_names[i], data_tables[[i]])
    }
    
    # Save the workbook
    saveWorkbook(wb, file_path, overwrite = TRUE)
  }
  
  export_to_excel("Confidences-Export.xlsx", 
                  whole5a_confidence = whole5a_confidence, 
                  whole5b_confidence = whole5b_confidence, 
                  fennell5a_confidence = fennell5a_confidence,
                  fennell5b_confidence = fennell5b_confidence,
                  dipetto5a_confidence = dipetto5a_confidence,
                  dipetto5b_confidence = dipetto5b_confidence
                  )
  