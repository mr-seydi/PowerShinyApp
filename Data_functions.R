##################Data#########################
# All the data is publicly available on https://github.com/m-a-robinson/sample-size

#If the data was not between [0,100] by linear interpolation transformed 
source("basic_functions.R")

####vGRF####
#Robinson, M.A., Vanrenterghem, J. and Pataky, T.C., 2021. Sample size estimation
#for biomechanical waveforms: Current practice, recommendations and a comparison
#to discrete power analysis. Journal of biomechanics, 122, p.110451.

# 13 trials vertical Ground reaction force
vGRF_data_Robinson <- function(type="mean"){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  #type= mean or raw
  data <- read.delim("data/Robinson2021_vGRF.txt", header=FALSE)
  if (type=="mean") {
    data_out = rowMeans(data)
  }else if(type=="raw"){
    data_out = data #column=sample curves, row=continuum_points
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}


#Phan, X., Grisbrook, T.L., Wernli, K., Stearne, S.M., Davey, P. and Ng, L., 2017.
#Running quietly reduces ground reaction force and vertical loading rate and
#alters foot strike technique. Journal of sports sciences, 35(16), pp.1636-1642.

# Time and body weight normalised vertical ground reaction force
#during the stance phase of running under normal and quiet sound conditions.
vGRF_data_Phan<- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Phan2017_vGRF.txt", header=FALSE)
  Domain <- c(0,100)
  
  if (type=="quiet") {
    quiet <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    data_out <- quiet
    
  }else if(type=="normal"){
    normal <- completed_data(x = data[,3], y = data[,4],
                               defined_domain = Domain)
    data_out <- normal
    
  }else if(type=="both"){
    
    quiet <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    normal <- completed_data(x = data[,3], y = data[,4],
                               defined_domain = Domain)
    data_out <- cbind(quiet, normal)
    colnames(data_out) <- c("Quiet", "Normal")
    
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}


####JCF####
#Barrios, J. and Willson, J., 2017. Minimum detectable change in medial
#tibiofemoral contact force parameters: derivation and application to a 
#load-altering intervention. Journal of applied biomechanics, 33(2), pp.171-175.

#Average medial tibiofemoral joint contact force during walking (1.5 m/s)
#with and without 6° laterally wedged foot orthoses. For lateral_wedge and
#no_wedge conditin

JCF_data <- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Barrios2017_JCF.txt", header=FALSE)
  Domain <- c(0,100)
  if (type=="lateral_wedge") {
    lateral_wedge <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    data_out <- lateral_wedge
    
  }else if(type=="no_wedge"){
    no_wedge <- completed_data(x = data[,3], y = data[,4],
                                            defined_domain = Domain)
    data_out <- no_wedge
      
  }else if(type=="both"){
    
    lateral_wedge <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    no_wedge <- completed_data(x = data[,3], y = data[,4],
                               defined_domain = Domain)
    data_out <- cbind(lateral_wedge, no_wedge=no_wedge)
    colnames(data_out) <- c("Lateral wedge", "No wedge")
    
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}

#### Hip Felxion Angle ####

#Bakke, D. and Besier, T., 2020. Shape model constrained scaling improves
#repeatability of gait data. Journal of biomechanics, 107, p.109838.

#Hip Felxion Angle (degree) for the left limb of two individuals obtained
#using linear scaling

Angle_data <- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Bakke2020_Angle.txt", header=FALSE)
  Domain <- c(0,100)
  
  if (type=="individual1") {
    individual1 <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    data_out <- individual1
    
  }else if(type=="individual2"){
    individual2 <- completed_data(x = data[,3], y = data[,4],
                               defined_domain = Domain)
    data_out <- individual2
    
  }else if(type=="both"){
    
    individual1 <- completed_data(x = data[,1], y = data[,2],
                                    defined_domain = Domain)
    individual2 <- completed_data(x = data[,3], y = data[,4],
                               defined_domain = Domain)
    data_out <- cbind(individual1, individual2)
    colnames(data_out) <- c("Individual 1","Individual 2")
    
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}


####Moment####

#Robinson, M.A., Donnelly, C.J., Tsao, J. and Vanrenterghem, J., 2013. Impact of
#knee modeling approach on indicators and classification of ACL injury risk. 
#Med Sci Sports Exerc, 46, pp.1269-1276.

# Mean external knee joint moments in the frontal for the DK (direct kinematic)
#and IK (inverse kinematic)

Moment_data <- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Robinson2014_Moment.txt", header=FALSE)
  
  
  if (type=="DK") {
    
    data_out <- data[,1]
    
  }else if(type=="IK"){
    
    data_out <- data[,2]
    
  }else if(type=="both"){
    
    data_out <- data
    colnames(data_out) <- c("DK","IK")
    
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}

####Muscle Force####

#Gomes, A.A., Ackermann, M., Ferreira, J.P., Orselli, M.I.V. and Sacco, I.C.,
#2017. Muscle force distribution of the lower limbs during walking in diabetic 
#individuals with and without polyneuropathy. Journal of neuroengineering and 
#rehabilitation, 14, pp.1-13.

#The force time series for ankle extensor muscle (Soleus) for control and diabetic

MF_data <- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Gomes2017_MF.txt", header=FALSE)
  Domain <- c(0,100)
  
  if (type=="control") {
    control <- completed_data(x = data[,1], y = data[,2],
                                  defined_domain = Domain)
    data_out <- control
    
  }else if(type=="diabetic"){
    diabetic <- completed_data(x = data[,3], y = data[,4],
                                  defined_domain = Domain)
    data_out <- diabetic
    
  }else if(type=="both"){
    
    control <- completed_data(x = data[,1], y = data[,2],
                                  defined_domain = Domain)
    diabetic <- completed_data(x = data[,3], y = data[,4],
                                  defined_domain = Domain)
    data_out <- cbind(control, diabetic)
    colnames(data_out) <- c("Control","Diabetic")
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}

#####EMG####



#Normalised Electromyography (EMG) For young and adult group (probably Soleus)
EMG_data <- function(type){
  if (is.null(type)) {
    return(NULL)  # Return NULL or an empty dataset
  }
  data <- read.delim("data/Bovi2011_EMG.txt", header=FALSE)
  
  
  if (type=="adult") {
    
    data_out <- data[,1]
    
  }else if(type=="young"){
    
    data_out <- data[,2]
    
  }else if(type=="both"){
    
    data_out <- data
    colnames(data_out) <- c("Adult","Young")
    
  }else{
        print("Invalid type");     return(NULL)  # Handle invalid type
  }
  return(data_out)
}


