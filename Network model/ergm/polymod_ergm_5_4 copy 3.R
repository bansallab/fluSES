#import packages
library(dplyr)
library(ergm.ego)
library(statnet)
library(network)
library(ergm)
library(igraph)

setwd("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/POLYMOD_DATA/ORIGINAL_DATA_FILES")

#initialize network
poly.net = network.initialize(7290, directed = F, hyper = FALSE, loops=FALSE, multiple = FALSE, bipartite = FALSE)

#read in participant data
poly_participants = read.delim('participants_final_v3.txt', header = TRUE, fill = TRUE)

#First dealing with gender for nodefactor and nodematch
#need to make 2 dataframes, one for egos, and one for alters, for as.egodata
part_gender = poly_participants$participant_gender 
part_gender[part_gender==""]  <- NA  #put NA's in the participant gender data where there is missing data
#change M/F to 0/1 for gender
num_part_gender = NULL #save the gender info as 0 (males) and 1 (females)
for (each_individual in part_gender){
  if (isTRUE(each_individual == 'M')){
    num_part_gender= c(num_part_gender, 0)
  }
  if (isTRUE(each_individual == 'F')){
    num_part_gender= c(num_part_gender, 1)
  }
  if (is.na(each_individual)==TRUE){
    num_part_gender= c(num_part_gender, NA)
  }
}
# put sex as a node attribute in network
poly.net %v% 'sex' = num_part_gender

ego_gender = cbind(poly_participants$global_id, num_part_gender) #cbind ego id's and gender data
ego_data_df= as.data.frame(ego_gender) # make e it a dataframe
colnames(ego_data_df)=c('egoIDcol', 'gender') # set col names

#bin participant ages
infants_toddlers = c(0, 1, 2, 3, 4)
school_age = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
adult = c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64)
elderly = c(65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100)

#bin participants ages in binned_ages
ego_age_1 = 0
ego_age_2 = 0
ego_age_3 = 0
ego_age_4 = 0

binned_ages = NULL
for (each_age in poly_participants$participant_age){
  if (each_age %in% infants_toddlers){
    binned_ages = c(binned_ages, 1)
    ego_age_1 = ego_age_1+1
  }
  if (each_age %in% school_age){
    binned_ages = c(binned_ages, 2)
    ego_age_2 = ego_age_2+1
  }
  if (each_age %in% adult){
    binned_ages = c(binned_ages, 3)
    ego_age_3 = ego_age_3+1
  }
  if (each_age %in% elderly){
    binned_ages = c(binned_ages, 4)
    ego_age_4 = ego_age_4+1
  }
  if (is.na(each_age)==TRUE ){
    binned_ages = c(binned_ages, NA)
  }
}

ego_data_df = cbind(ego_data_df, binned_ages) #cbind ego id's and gender data
ego_data_df= as.data.frame(ego_data_df) # make e it a dataframe
colnames(ego_data_df) = c('egoIDcol', 'gender', 'age')

#read in contact data
poly_contacts = read.table('contacts_final_v2_with_weights.txt', sep = ',', header = TRUE)

con_gender = poly_contacts$cnt_sex #alter gender data
con_gender[con_gender==""]  <- NA  #put NAs in empty entries
num_con_gender = NULL #make them all 0 (male) or 1 (female)
for (each_individual in con_gender){
  if (isTRUE(each_individual == 'M')){
    num_con_gender= c(num_con_gender, 0)
  }
  if (isTRUE(each_individual == 'F')){
    num_con_gender= c(num_con_gender, 1)
  }
  if (is.na(each_individual)==TRUE){
    num_con_gender= c(num_con_gender, NA)
  }
}

alter_gender= cbind(poly_contacts$global_id, num_con_gender) #cbind id that contacted the alter and the alter sex
alter_data_df = as.data.frame(alter_gender) # make a data frame
colnames(alter_data_df)=c('egoIDcol', 'gender')# set col names
num_contacts=length(alter_data_df$alter_gender) # get the number of entries in this dataframe- the number of contacts

#bin ages of alters in the same way as the ego
binned_ages_contacts = NULL
for (each_age in poly_contacts$cnt_age_mean){
  if (each_age %in% infants_toddlers){
    binned_ages_contacts = c(binned_ages_contacts, 1)
  }
  if (each_age %in% school_age){
    binned_ages_contacts = c(binned_ages_contacts, 2)
  }
  if (each_age %in% adult){
    binned_ages_contacts = c(binned_ages_contacts, 3)
  }
  if (each_age %in% elderly){
    binned_ages_contacts = c(binned_ages_contacts, 4)
  }
  if (is.na(each_age)==TRUE ){
    binned_ages_contacts = c(binned_ages_contacts, NA)
  }
}

#add ages to alter data frame
alter_data_df = cbind(alter_data_df, binned_ages_contacts)
alter_data_df = as.data.frame(alter_data_df)
colnames(alter_data_df) = c('egoIDcol', 'gender', 'age')

#make ego and alter dataframes into ego data
poly.ego = as.egodata(ego_data_df, alters = alter_data_df, egoIDcol = "egoIDcol")
num_nodes = sum(length(ego_data_df$egoIDcol), length(alter_data_df$egoIDcol))

#first, ergm.ego model with age and gender as nodefactor and nodematch
# fit.poly = ergm.ego(poly.ego~edges+nodefactor('gender')+nodefactor('age')+nodematch('gender')+nodematch('age'), control = control.ergm.ego(ergm.control = control.ergm(MCMC.interval = 20148, MCMC.burnin = 2048*16)))
# summary(fit.poly)
# mcmc.diagnostics(fit.poly)
# plot(gof(fit.poly, GOF="model"))
# plot(gof(fit.poly, GOF="degree"))
#this works- GOF not very good


#ADDING HOMOPHILY FOR CONTACT LOCATION

subset_alter_data = subset(poly_contacts, select = c(global_id, cnt_home, cnt_work, cnt_school)) # just select the columns I need
num_contacts = length(subset_alter_data$global_id) # get number of contacts
#first creating a list of 0/1 attributes for the alters
#going through each recorded contact, and saving in three separate lists where the contact occured
#assuming that individuals who go to work do not go to school
#assuming that indiviudals that go to school do not go to work (to reduce NA data, but could be easily changed)
#So, if a contact happened at home, home is 1,and then both work and school are NA, because I do not know about the contacted alter's other contacts
#if a contact happened at school, school is 1, home is NA (do not know about alter's home contacts), work is 0 (assumed individuals either work or go to school)
# if a contact happened at work, work is 1, home is NA (do not know about alter's home contacts), school is 0 (assumed to either work or attend school)
#Some reported contacting the same indiviudal in multiple places. Took these at face value (1's for the locations indicated), but can be adjusted

#empty lists
home_contacts = NULL
school_work_contacts = NULL

#go through each contact
for (each_contact in seq(1,num_contacts)){
  home = subset_alter_data$cnt_home[each_contact] # get the 0/1 value for home for this alter
  work = subset_alter_data$cnt_work[each_contact] # get the 0/1 value for work for this alter
  school = subset_alter_data$cnt_school[each_contact] # get the 0/1 value for school for this alter
  #if this alter is only contacted at home
  if (home == 1){
    if (work == 0){
      if (school == 0){
        home_contacts = c(home_contacts, 1) # home is 1
        school_work_contacts = c(school_work_contacts, NA) #work and school are NA
      }
    }
  }
  #if this alter is only contacted at work
  if (work == 1){
    if (home == 0){
      if (school == 0){
        home_contacts  =c(home_contacts, NA) # home is NA
        school_work_contacts = c(school_work_contacts, 1) # work is 1 (in this column, 1 will be a work node and 0 will be a school node )
      }
    }
  }
  #if the alter is only contacted at school
  if (school == 1){
    if (work == 0){
      if (home == 0){
        home_contacts = c(home_contacts, NA) # home is NA
        school_work_contacts = c(school_work_contacts, 0) # school is 0 in this coumn
      }
    }
  }
  #if the alter is contqacted at work and school
  if (school == 1){
    if (work == 1){
      if (home == 0){
        home_contacts = c(home_contacts, NA) # home is NA
        x = c(0, 1)
        random_assign = sample(x, size = 1, replace = TRUE)
        if (isTRUE(random_assign==0) == 1){
          school_work_contacts = c(school_work_contacts, 0) # work is 1
        }
        if(isTRUE(random_assign == 1)==1){
          school_work_contacts = c(school_work_contacts, 1) # school is 1
        }
      }
    }
  }
  #if the alter is contacted at school and home
  if (school == 1){
    if (work == 0){
      if (home == 1){
        home_contacts = c(home_contacts, 1) # home ia 1
        school_work_contacts = c(school_work_contacts, 0) # work is 0 (assumption)
      }
    }
  }
  # # if the alter is contacted at work and home
  if (school == 0){
    if (work == 1){
      if (home == 1){
        home_contacts = c(home_contacts, 1) # home is 1
        school_work_contacts = c(school_work_contacts, 1) # work is 1
      }
    }
  }
  # #if the alter is contacted in all three locations
  if (school == 1){
    if (work == 1){
      if (home == 1){
        home_contacts = c(home_contacts, 1) # home is 1
        x = c(0, 1)
        random_assign = sample(x, size = 1, replace = TRUE)
        if (isTRUE(random_assign==0) == 1){
          school_work_contacts = c(school_work_contacts, 0) # work is 1
        }
        if(isTRUE(random_assign == 1)==1){
          school_work_contacts = c(school_work_contacts, 1) # school is 1
        }
      }
    }
  }
  # # if the alter is contacted is none of these locations
  if (home == 0){
    if (work == 0){
      if (school == 0){
        home_contacts = c(home_contacts, NA) # all are NA (could be 0s?)
        school_work_contacts = c(school_work_contacts, NA)
      }
    }
  }
}





alter_data_df = cbind(alter_data_df, home_contacts, school_work_contacts) # add the home, work, and school records to the alter data frame
alter_data_df = as.data.frame(alter_data_df)
colnames(alter_data_df) = c('egoIDcol', 'gender', 'age', 'home', 'school_work')

#next, do the same thing for egos
#egos can have multipe locations- they will have a 1 in the locaiton category if they ever recorded a contact in that location. Otherwise, they have a 0 (we should know about all of the egos contacts)
home_0 = rep(NA, length(ego_data_df$egoIDcol)) # created lists of NAs to fill
school_work_0 = rep(NA, length(ego_data_df$egoIDcol))
ego_data_df = cbind(ego_data_df, home_0, school_work_0) # add the zero lists to the ego dataframe to fill
ego_data_df = as.data.frame(ego_data_df)
colnames(ego_data_df)= c('egoIDcol', 'gender', 'age', 'home', 'school_work')



for (each_ego in ego_data_df$egoIDcol){#go through each ego
  if (isTRUE(each_ego %in% subset_alter_data$global_id)==TRUE){ # if the ego is in the alter dataframe (reported any contacts)
    indices_of_ego = which(subset_alter_data$global_id == each_ego) # find the indices in the alter data where the ego of interest has contacts
    ego_home_contacts = NULL # empty lists to save the home contacts
    ego_work_contacts = NULL # empty list to save the work contacts
    ego_school_contacts = NULL
    for (each_index in indices_of_ego){ # go through each alter entry (at the indices found in indices_of_ego step)
      home = subset_alter_data$cnt_home[each_index] #home is the home column at this location
      ego_home_contacts = c(ego_home_contacts, home) # add the home record (0/1) to the home list
      work = subset_alter_data$cnt_work[each_index] # work is the work column at this location
      ego_work_contacts = c(ego_work_contacts, work) # add the work record (0/1) to the work list
      school = subset_alter_data$cnt_school[each_index] # school is the school column at this location
      ego_school_contacts = c(ego_school_contacts, school) # add the school record (0/1) to the school list
    }
    ego_index = which(ego_data_df$egoIDcol==each_ego) # get the index of the ego in the ego data frame
    if(sum(ego_home_contacts)>1){ # if there is at least 1 home contact
      ego_data_df$home[ego_index] = 1 # put a 1 for the ego in the ego data frame
    }
    else{ # if there is not at least 1 home contact
      ego_data_df$home[ego_index] = 0 # ego's home contact =0
    }
    if(sum(ego_work_contacts)>1){ # if there is at least 1 work contact
      if (sum(ego_school_contacts)==0){
        ego_data_df$school_work[ego_index] = 1 # ego's work contact = 1
      }
    }
    
    if(sum(ego_school_contacts)>1){ # if there is at least 1 school contact
      if (sum(ego_work_contacts)==0){
        ego_data_df$school_work[ego_index] = 0 # ego's school contact = 1
      }
    }
    if(sum(ego_work_contacts)>1){ # if there is at least 1 work contact
      if (sum(ego_school_contacts)>1){
        x = c(0, 1)
        random_assign = sample(x, size = 1, replace = TRUE)
        ego_data_df$school_work[ego_index] = random_assign 
      }
    }
    if(sum(ego_work_contacts)==0){ 
      if (sum(ego_school_contacts)==0){
        ego_data_df$school_work[ego_index] = NA
      }
    }
  }
}

poly.ego = as.egodata(ego_data_df, alters = alter_data_df, egoIDcol = "egoIDcol") # ego data and alter data as ego data

#reclassify contacts in unexpected places
age1_work = NULL
age1_school = NULL
age2_work = NULL
age2_school = NULL
age3_work = NULL
age3_school = NULL
age4_work = NULL
age4_school = NULL


for (each_ego in ego_data_df$egoIDcol){
  ego_index =  which(ego_data_df$egoIDcol == each_ego)
  ego_age = ego_data_df$age[ego_index]
  ego_school_work = ego_data_df$school_work[ego_index]
  if (isTRUE(ego_age==1)==T){
    if (isTRUE(ego_school_work == 0)==T){
      age1_school = c(age1_school, each_ego)
    }
    if (isTRUE(ego_school_work == 1)==T){
      age1_work = c(age1_work, each_ego)
    }
  }
  if (isTRUE(ego_age==2)==T){
    if (isTRUE(ego_school_work == 0)==T){
      age2_school = c(age2_school, each_ego)
    }
    if (isTRUE(ego_school_work == 1)==T){
      age2_work = c(age2_work, each_ego)
    }
  }
  if (isTRUE(ego_age==3)==T){
    if (isTRUE(ego_school_work == 0)==T){
      age3_school = c(age3_school, each_ego)
    }
    if (isTRUE(ego_school_work == 1)==T){
      age3_work = c(age3_work, each_ego)
    }
  }
  if (isTRUE(ego_age==4)==T){
    if (isTRUE(ego_school_work == 0)==T){
      age4_school = c(age4_school, each_ego)
    }
    if (isTRUE(ego_school_work == 1)==T){
      age4_work = c(age4_work, each_ego)
    }
  }
}
num_age1_work = length(age1_work)
num_age1_school = length(age1_school)
num_age2_work = length(age2_work)
num_age2_school = length(age2_school)
num_age3_work = length(age3_work)
num_age3_school = length(age3_school)
num_age4_work = length(age4_work)
num_age4_school = length(age4_school)


for (each_node in age1_work){
  print(each_node)
  index_of_node = which(ego_data_df$egoIDcol == each_node)
  print(index_of_node)
  print(ego_data_df$school_work[index_of_node])
  ego_data_df$school_work[index_of_node] = NA
  print(ego_data_df$school_work[index_of_node])
}

for (each_node in age2_work){
  print(each_node)
  index_of_node = which(ego_data_df$egoIDcol == each_node)
  print(index_of_node)
  print(ego_data_df$school_work[index_of_node])
  ego_data_df$school_work[index_of_node] = NA
  print(ego_data_df$school_work[index_of_node])
}

for (each_node in age3_school){
  print(each_node)
  index_of_node = which(ego_data_df$egoIDcol == each_node)
  print(index_of_node)
  print(ego_data_df$school_work[index_of_node])
  ego_data_df$school_work[index_of_node] = 1
  print(ego_data_df$school_work[index_of_node])
}
for (each_node in age4_school){
  print(each_node)
  index_of_node = which(ego_data_df$egoIDcol == each_node)
  print(index_of_node)
  print(ego_data_df$school_work[index_of_node])
  ego_data_df$school_work[index_of_node] = 1
  print(ego_data_df$school_work[index_of_node])
}

#education: give each work ego's contacts the same educaiton level as the ego
ego_edu = poly_participants$participant_education
binned_ego_edu = NULL
low_edu = c(0, 1)
medium_edu = c(2, 3, 4, 8)
high_edu = c(5, 6, 7)
for (each_edu in ego_edu){
  if (each_edu %in% low_edu){
    binned_ego_edu = c(binned_ego_edu, 1)
  }
  if (each_edu %in% medium_edu){
    binned_ego_edu = c(binned_ego_edu, 2)
  }
  if (each_edu %in% high_edu){
    binned_ego_edu = c(binned_ego_edu, 3)
  }
  if (is.na(each_edu)==TRUE){
    binned_ego_edu = c(binned_ego_edu, NA)
  }
}





edu = binned_ego_edu
ego_data_df = cbind(ego_data_df, edu)
alter_edu = rep(NA, length(alter_data_df$egoIDcol))
edu = alter_edu
alter_data_df = cbind(alter_data_df, edu)

num_edu_1_1 = 0
num_edu_2_2 = 0
num_edu_3_3 = 0

for (each_ego in ego_data_df$egoIDcol){
  ego_index = which(ego_data_df$egoIDcol==each_ego)
  ego_school_work_status = ego_data_df$school_work[ego_index]
  ego_education_status = ego_data_df$edu[ego_index]
  if (isTRUE(ego_school_work_status == 1)==T){
    ego_contacts = which(alter_data_df == each_ego)
    for (each_contact in ego_contacts){
      alter_school_work_status = alter_data_df$school_work[each_contact]
      if (isTRUE(alter_school_work_status==1)==T){
        alter_data_df$edu[each_contact] = ego_education_status
      }
    }
  }
}

#next: education level is a household attribute. If a contact happens at home, the alter gets the same education level as the ego
#what to do if home and school/work - shouldn't matter, they get the same edu value either way
#find the home and work alters. 
# home_and_work_alters = NULL
# for (each_contact in seq(1, length(alter_data_df$egoIDcol))){
#   home_status = alter_data_df$home[each_contact]
#   school_work_status = alter_data_df$school_work[each_contact]
#   if (isTRUE(home_status==1)==T){
#     if (isTRUE(school_work_status == 1)==T){
#       home_and_work_alters = c(home_and_work_alters, each_contact)
#     }
#   }
# }


for (each_ego in ego_data_df$egoIDcol){
  ego_index = which(ego_data_df$egoIDcol==each_ego)
  ego_home_status = ego_data_df$home[ego_index]
  ego_education_status = ego_data_df$edu[ego_index]
  if (isTRUE(ego_home_status == 1)==T){
    ego_contacts = which(alter_data_df == each_ego)
    for (each_contact in ego_contacts){
      alter_home_status = alter_data_df$home[each_contact]
      if (isTRUE(alter_home_status==1)==T){
        alter_data_df$edu[each_contact] = ego_education_status
      }
    }
  }
}










poly.ego = as.egodata(ego_data_df, alters = alter_data_df, egoIDcol = "egoIDcol")

#fit.poly.edu = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work', base = 1)+nodefactor('edu')+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 100000, MCMC.burnin = 1000000)))
# fit.poly.edu2 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work')+nodefactor('edu')+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 100000, MCMC.burnin = 10000000, MCMC.return.stats = T)))
# #fit.poly.edu3 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work', base = 0)+nodefactor('edu')+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 100000, MCMC.burnin = 10000000, MCMC.return.stats = T)))
# fit.poly.edu4 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:3)+nodefactor('school_work')+nodefactor('edu')+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu5 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:3)+nodefactor('school_work')+nodefactor('edu', base = 2:3)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu6 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work')+nodefactor('edu', base = 2:3)+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu7 = ergm.ego(poly.ego~edges+nodefactor('age', base = 2)+nodefactor('school_work', base = 0)+nodefactor('edu', base = 2:3)+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work', diff=T)+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu8 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work')+nodefactor('edu', base = 2:3)+nodematch('age', diff = T)+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu9 = ergm.ego(poly.ego~edges+nodefactor('age', base = 1:2)+nodefactor('school_work')+nodefactor('edu', base = 2:3)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 10000, MCMC.burnin = 100000, MCMC.return.stats = T)))
# fit.poly.edu10 = ergm.ego(poly.ego~edges+nodefactor('age', base = c(1,2, 4))+nodefactor('school_work')+nodefactor('edu', base = 2:3)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 1000000, MCMC.burnin = 10000000, MCMC.return.stats = T)))

###best model
#fit.poly.edu11 = ergm.ego(poly.ego~edges+nodefactor('age', base = c(1,2))+nodefactor('school_work')+nodefactor('edu', base = 2)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 1000000, MCMC.burnin = 1000000, MCMC.return.stats = T)))

#checking if adding gender back in messes up this model
#THIS IS NOW THE BEST MODEL!!!!!!!!!!!
fit.poly.edu11.2 = ergm.ego(poly.ego~edges+nodefactor('gender', base = 1)+nodefactor('age', base = c(1,2))+nodefactor('school_work')+nodefactor('edu', base = 2)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 1000000, MCMC.burnin = 1000000, MCMC.return.stats = T)))



summary(fit.poly.edu11.2)
mcmc.diagnostics(fit.poly.edu11.2)
par(mfrow=c(4,4))
plot(mcmc.diagnostics(fit.poly.edu11.2))
dev.off()
gof(fit.poly.edu11)
plot(gof(fit.poly.edu11.2))
plot(gof(fit.poly.edu11.2, GOF = "degree"))


VIF.ERGM(fit.poly.edu11.2)


num_sims = 10
poly.sim = simulate(fit.poly.edu11.2, nsim = num_sims, popsize = 7290, verbose = TRUE)

for (each_sim in seq(1, num_sims)){
  #print(poly.sim[each_sim])
  sim = as.matrix(poly.sim[each_sim][[1]], matrix.type= "edgelist")
  edgelist = cbind(sim[,1], sim[,2])
  edgelist = as.data.frame(edgelist)
  colnames(edgelist) = c("node1", "node2")
  filename_edgelist = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_nets/sim_net_",each_sim,".txt", sep = "")
  write.table(edgelist, file = filename_edgelist, sep = ',', row.names = F, col.names = F)
  
  all_nodes = seq(1, 7290)
  empty_vals = rep(0, 7290)
  age_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(age_attribute) = c('node', 'age')
  for (each_node in all_nodes){
    age_attribute$node[each_node] = each_node
    age = poly.sim[each_sim][[1]]$val[[each_node]]$age
    age_attribute$age[each_node] = age
  }
  filename_age = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_net_age_attributes/sim_net_age_attributes_",each_sim,".txt", sep = "")
  write.table(age_attribute, file = filename_age, sep = ',', row.names = F, col.names = F)
  
  gender_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(gender_attribute) = c('node', 'gender')
  for (each_node in all_nodes){
    gender_attribute$node[each_node] = each_node
    gender = poly.sim[each_sim][[1]]$val[[each_node]]$gender
    gender_attribute$gender[each_node] = gender
  }
  filename_gender = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_net_gender_attributes/sim_net_gender_attributes_",each_sim,".txt", sep = "")
  write.table(gender_attribute, file = filename_gender, sep = ',', row.names = F, col.names = F)
  
  school_work_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(school_work_attribute) = c('node', 'school_work')
  for (each_node in all_nodes){
    school_work_attribute$node[each_node] = each_node
    school_work = poly.sim[each_sim][[1]]$val[[each_node]]$school_work
    school_work_attribute$school_work[each_node] = school_work
  }
  filename_school_work = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_net_school_work_attributes/sim_net_school_work_attributes_",each_sim,".txt", sep = "")
  write.table(school_work_attribute, file =filename_school_work, sep = ',', row.names = F, col.names = F)
  
  home_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(home_attribute) = c('node', 'home')
  for (each_node in all_nodes){
    home_attribute$node[each_node] = each_node
    home = poly.sim[each_sim][[1]]$val[[each_node]]$home
    home_attribute$home[each_node] = home
  }
  filename_home = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_net_home_attributes/sim_net_home_attributes_",each_sim,".txt", sep = "")
  write.table(home_attribute, file = filename_home, sep = ',', row.names = F, col.names = F)
  
  edu_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(edu_attribute) = c('node', 'edu')
  for (each_node in all_nodes){
    edu_attribute$node[each_node] = each_node
    edu = poly.sim[each_sim][[1]]$val[[each_node]]$edu
    edu_attribute$edu[each_node] = edu
  }
  filename_edu = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/sim_net_edu_attributes/sim_net_edu_attributes_",each_sim,".txt", sep = "")
  write.table(edu_attribute, file = filename_edu, sep = ',', row.names = F, col.names = F)
}

VIF.ERGM<-function(my.ergm){
  require(ergm)
  ###simulate from posterior distribution of ERGM--toggle nsim for more robustness/less computation time. Default is 1,000
  m2<-simulate(my.ergm,statsonly=TRUE,nsim=1000)
  
  
  cor.mat<-cor(m2) #calculate correlation matrix
  corr5<-cor.mat[-c(1),-c(1)] ##omit edges term
  VIFS<-matrix(0,nr=1,nc=ncol(corr5)+1)
  
  for(i in 1:ncol(corr5)){
    
    gvec<-as.vector(corr5[-c(i),i]) ##create vector of correlations between covariate of interest and other covariates in the model
    tgvec<-t(gvec)    
    xcor<-solve(corr5[-c(i),-c(i)]) ##create square matrix of correlations between covariates in the model other than the one of interest
    Rsq<-tgvec%*%xcor%*%gvec
    VIFS[1,i]<-1/(1-Rsq)
  }
  
  colnames(VIFS)<-names(my.ergm$coef[-1])
  message("Higher values indicate greater correlation.\nVIF > 20 is concerning, VIF > 100 indicates severe collinearity.")
  VIFS
}
VIF.ERGM(fit.poly.edu)
VIF.ERGM(fit.poly.edu2)
VIF.ERGM(fit.poly.edu3)

VIF.ERGM(fit.poly.edu11.2)


###############look at degree distributions
#overall degree distribution
list_degree = NULL #save the degree of each node
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  list_degree = c(list_degree, length(indices_of_ego))
}
hist(list_degree, breaks = 100, main = "Overall degree distribution", xlab = "Degree")
mean(list_degree)

#overall degree distribution by gender
list_male_degree = NULL
list_female_degree = NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  gender_of_ego = ego_data_df$gender[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(gender_of_ego == 0)==TRUE){
    list_male_degree = c(list_male_degree, length(indices_of_ego))
  }
  if (isTRUE(gender_of_ego == 1)==TRUE){
    list_female_degree = c(list_female_degree, length(indices_of_ego))
  }
}
hist(list_male_degree, breaks = 50, main = "Male degree distribution", xlab = "Male degree")
hist(list_female_degree, breaks = 50, main = "Female degree distribution", xlab = "Female degree")
mean(list_male_degree)
mean(list_female_degree)

#overall degree distribution of each age group
list_age1_degree = NULL #save the degree of each node
list_age2_degree = NULL
list_age3_degree = NULL
list_age4_degree = NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  age_of_ego = ego_data_df$age[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(age_of_ego == 1)==TRUE){
    list_age1_degree = c(list_age1_degree, length(indices_of_ego))
  }
  if (isTRUE(age_of_ego == 2)==TRUE){
    list_age2_degree = c(list_age2_degree, length(indices_of_ego))
  }
  if (isTRUE(age_of_ego == 3)==TRUE){
    list_age3_degree = c(list_age3_degree, length(indices_of_ego))
  }
  if (isTRUE(age_of_ego == 4)==TRUE){
    list_age4_degree = c(list_age4_degree, length(indices_of_ego))
  }
}
hist(list_age1_degree, breaks = 50, main = "Infant-toddler (age 1) degree distribution", xlab = "Infant degree")
hist(list_age2_degree, breaks = 50, main = "School age (age 2) degree distribution", xlab = "School age degree")
hist(list_age3_degree, breaks = 50, main = "Adult (age 3) degree distribution", xlab = "Adult degree")
hist(list_age4_degree, breaks = 50, main = "Elderly (age 4) degree distribution", xlab = "Elderly degree")

mean(list_age1_degree)
mean(list_age2_degree)
mean(list_age3_degree)
mean(list_age4_degree)
mean(list_age5_degree)
mean(list_age6_degree)


list_school_degree = NULL
list_work_degree = NULL

for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  school_work_of_ego = ego_data_df$school_work[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(school_work_of_ego == 0)==TRUE){
    list_school_degree = c(list_school_degree, length(indices_of_ego))
  }
  if (isTRUE(school_work_of_ego == 1)==TRUE){
    list_work_degree = c(list_work_degree, length(indices_of_ego))
  }
}

hist(list_school_degree, breaks = 50, main = "School degree distribution", xlab = "School degree")
hist(list_work_degree, breaks = 50, main = "Work degree distribution", xlab = "Work degree")

list_home_degree=NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  home_of_ego = ego_data_df$home[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(home_of_ego == 1)==TRUE){
    list_home_degree = c(list_home_degree, length(indices_of_ego))
  }
}
hist(list_home_degree, breaks = 50, main = "Home degree distribution", xlab = "Home degree")

list_edu1_degree = NULL
list_edu2_degree = NULL
list_edu3_degree = NULL

for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  edu_of_ego = ego_data_df$edu[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(edu_of_ego == 1)==TRUE){
    list_edu1_degree = c(list_edu1_degree, length(indices_of_ego))
  }
  if (isTRUE(edu_of_ego == 2)==TRUE){
    list_edu2_degree = c(list_edu2_degree, length(indices_of_ego))
  }
  if (isTRUE(edu_of_ego == 3)==TRUE){
    list_edu3_degree = c(list_edu3_degree, length(indices_of_ego))
  }
}

hist(list_edu1_degree, breaks = 50, main = "Edu 1 degree distribution", xlab = "Edu 1 degree")
hist(list_edu2_degree, breaks = 50, main = "Edu 2 degree distribution", xlab = "Edu 2 degree")
hist(list_edu3_degree, breaks = 50, main = "Edu 3 degree distribution", xlab = "Edu 3 degree")


#homophily degree
#HOMOPHILY DEGREE

#gender homophily degree
list_male_male_degree = NULL
list_female_female_degree = NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  gender_of_ego = ego_data_df$gender[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(gender_of_ego == 0)==TRUE){
    num_male_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$gender[each_index]==0)==TRUE){
        num_male_contacts = num_male_contacts+1
      }
    }
    list_male_male_degree = c(list_male_male_degree, num_male_contacts)
  }
  if (isTRUE(gender_of_ego == 1)==TRUE){
    num_female_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$gender[each_index]==1)==TRUE){
        num_female_contacts = num_female_contacts+1
      }
    }
    list_female_female_degree = c(list_female_female_degree, num_female_contacts)
  }
}
hist(list_male_male_degree, breaks = 50, main = "Male-male degree distribution", xlab = "Male degree")
hist(list_female_female_degree, breaks = 50, main = "Female-female degree distribution", xlab = "Female degree")
mean(list_male_degree)
mean(list_female_degree)


#age homophily degre 
list_1_1_degree = NULL
list_2_2_degree = NULL
list_3_3_degree = NULL
list_4_4_degree = NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  age_of_ego = ego_data_df$age[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(age_of_ego == 1)==TRUE){
    num_age1_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$age[each_index]==1)==TRUE){
        num_age1_contacts = num_age1_contacts+1
      }
    }
    list_1_1_degree = c(list_1_1_degree, num_age1_contacts)
  }
  if (isTRUE(age_of_ego == 2)==TRUE){
    num_age2_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$age[each_index]==2)==TRUE){
        num_age2_contacts = num_age2_contacts+1
      }
    }
    list_2_2_degree = c(list_2_2_degree, num_age2_contacts)
  }
  if (isTRUE(age_of_ego == 3)==TRUE){
    num_age3_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$age[each_index]==3)==TRUE){
        num_age3_contacts = num_age3_contacts+1
      }
    }
    list_3_3_degree = c(list_3_3_degree, num_age3_contacts)
  }
  if (isTRUE(age_of_ego == 4)==TRUE){
    num_age4_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$age[each_index]==4)==TRUE){
        num_age4_contacts = num_age4_contacts+1
      }
    }
    list_4_4_degree = c(list_4_4_degree, num_age4_contacts)
  }
}
hist(list_1_1_degree, breaks = 50, main = "Age1-age1 degree distribution", xlab = "Age 1 degree")
hist(list_2_2_degree, breaks = 50, main = "Age2-age2 degree distribution", xlab = "Age 2 degree")
hist(list_3_3_degree, breaks = 50, main = "Age3-age3 degree distribution", xlab = "Age 3 degree")
hist(list_4_4_degree, breaks = 50, main = "Age4-age4 degree distribution", xlab = "Age 4 degree")
mean(list_1_1_degree)
mean(list_2_2_degree)
mean(list_3_3_degree)
mean(list_4_4_degree)


list_school_school_degree = NULL
list_work_work_degree = NULL

for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  school_work_of_ego = ego_data_df$school_work[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(school_work_of_ego == 0)==TRUE){
    num_school_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$school_work[each_index]==0)==TRUE){
        num_school_contacts = num_school_contacts+1
      }
    }
    list_school_school_degree = c(list_school_school_degree, num_school_contacts)
  }
  if (isTRUE(school_work_of_ego == 1)==TRUE){
    num_work_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$school_work[each_index]==1)==TRUE){
        num_work_contacts = num_work_contacts+1
      }
    }
    list_work_work_degree = c(list_work_work_degree, num_work_contacts)
  }
}

hist(list_school_school_degree, breaks = 50, main = "School-school degree distribution", xlab = "School degree")
hist(list_work_work_degree, breaks = 50, main = "Work-work degree distribution", xlab = "Work degree")

list_home_home_degree = NULL

for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  home_of_ego = ego_data_df$home[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(home_of_ego == 1)==TRUE){
    num_home_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$home[each_index]==1)==TRUE){
        num_home_contacts = num_home_contacts+1
      }
    }
    list_home_home_degree = c(list_home_home_degree, num_home_contacts)
  }
}
hist(list_home_home_degree, breaks = 50, main = "Home-home degree distribution", xlab = "Home degree")


list_edu1_1_degree = NULL
list_edu2_2_degree = NULL
list_edu3_3_degree = NULL
for (each_ego in ego_data_df$egoIDcol){#go through each ego
  index_of_ego = which(ego_data_df$egoIDcol == each_ego)
  edu_of_ego = ego_data_df$edu[index_of_ego]
  indices_of_ego = which(alter_data_df$egoIDcol == each_ego) # get the indices where that ego is found in the alter data
  if (isTRUE(edu_of_ego == 1)==TRUE){
    num_edu1_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$edu[each_index]==1)==TRUE){
        num_edu1_contacts = num_edu1_contacts+1
      }
    }
    list_edu1_1_degree = c(list_edu1_1_degree, num_edu1_contacts)
  }
  if (isTRUE(edu_of_ego == 2)==TRUE){
    num_edu2_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$edu[each_index]==2)==TRUE){
        num_edu2_contacts = num_edu2_contacts+1
      }
    }
    list_edu2_2_degree = c(list_edu2_2_degree, num_edu2_contacts)
  }
  if (isTRUE(edu_of_ego == 3)==TRUE){
    num_edu3_contacts = 0
    for (each_index in indices_of_ego){
      if (isTRUE(alter_data_df$edu[each_index]==3)==TRUE){
        num_edu3_contacts = num_edu3_contacts+1
      }
    }
    list_edu3_3_degree = c(list_edu3_3_degree, num_edu3_contacts)
  }
}

hist(list_edu1_1_degree, breaks = 50, main = "Edu1- edu1 degree distribution", xlab = "Edu 1 degree")
hist(list_edu2_2_degree, breaks = 50, main = "Edu2- edu2 degree distribution", xlab = "Edu 2 degree")
hist(list_edu3_3_degree, breaks = 50, main = "Edu3- edu3 degree distribution", xlab = "Edu 3 degree")
mean(list_edu1_1_degree)
mean(list_edu2_2_degree)
mean(list_edu3_3_degree)


# #seelct subset of nodes from ermg for gephi analysis
# sim = as.matrix(poly.sim[1][[1]], matrix.type= "edgelist")
# edgelist = cbind(sim[,1], sim[,2])
# edgelist = as.data.frame(edgelist)
# colnames(edgelist) = c("node1", "node2")
# nodes_chosen= sample(edgelist$node1, 50)
# nodes_chosen = unique(nodes_chosen) # randomly choose a number of unique nodes
# 
# node1_edgelist = NULL
# node2_edgelist = NULL
# 
# for (each_contact in seq(1,length(edgelist$node1))){
#   node1 = edgelist$node1[each_contact]
#   node2 = edgelist$node2[each_contact]
#   if (node1 %in% nodes_chosen){
#     node1_edgelist = c(node1_edgelist, node1)
#     node2_edgelist = c(node2_edgelist, node2)
#   }
#   if (node2 %in% nodes_chosen){
#     node1_edgelist = c(node1_edgelist, node1)
#     node2_edgelist = c(node2_edgelist, node2)
#   }
# }
# subset_edgelist = cbind(node1_edgelist, node2_edgelist)
# colnames(subset_edgelist) = c('node1', 'node2')
# subset_edgelist = as.data.frame(subset_edgelist)
# 
# 
# write.table(subset_edgelist, file = '/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_behavior_change_results/subset_edgelist.csv', sep = ',', row.names = F, col.names = F)

#density plots
net1_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_0.txt', sep = ',')
net2_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_1.txt', sep = ',')
net3_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_2.txt', sep = ',')
net4_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_3.txt', sep = ',')
net5_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_4.txt', sep = ',')
net6_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_5.txt', sep = ',')
net7_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_6.txt', sep = ',')
net8_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_7.txt', sep = ',')
net9_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_8.txt', sep = ',')
net10_overall_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/net_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_degree), xlab = "Degree", main = "Degree distributions", ylim = c(0, 0.07), lwd = 4, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, col = 'black')
lines(density(net1_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net2_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net3_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net4_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net5_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net6_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net7_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net8_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net9_overall_degree_dist), col = 'gray', lwd = 1)
lines(density(net10_overall_degree_dist), col = 'gay', lwd = 1)
legend(x=35, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1, lwd = 2, cex =1.5, pt.cex = 1)

net1_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_0.txt', sep = ',')
net2_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_1.txt', sep = ',')
net3_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_2.txt', sep = ',')
net4_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_3.txt', sep = ',')
net5_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_4.txt', sep = ',')
net6_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_5.txt', sep = ',')
net7_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_6.txt', sep = ',')
net8_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_7.txt', sep = ',')
net9_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_8.txt', sep = ',')
net10_male_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_male_degree), xlab = "Overall male degree", main = "Overall male degree distribution", ylim = c(0, 0.069), lwd = 4)
lines(density(net1_male_degree_dist), col = 'gray')
lines(density(net2_male_degree_dist), col = 'gray')
lines(density(net3_male_degree_dist), col = 'gray')
lines(density(net4_male_degree_dist), col = 'gray')
lines(density(net5_male_degree_dist), col = 'gray')
lines(density(net6_male_degree_dist), col = 'gray')
lines(density(net7_male_degree_dist), col = 'gray')
lines(density(net8_male_degree_dist), col = 'gray')
lines(density(net9_male_degree_dist), col = 'gray')
lines(density(net10_male_degree_dist), col = 'gray')
legend(x=60, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_0.txt', sep = ',')
net2_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_1.txt', sep = ',')
net3_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_2.txt', sep = ',')
net4_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_3.txt', sep = ',')
net5_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_4.txt', sep = ',')
net6_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_5.txt', sep = ',')
net7_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_6.txt', sep = ',')
net8_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_7.txt', sep = ',')
net9_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_8.txt', sep = ',')
net10_female_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_female_degree), xlab = "Overall female degree", main = "Overall female degree distribution", ylim = c(0, 0.069), lwd = 4)
lines(density(net1_female_degree_dist), col = 'gray')
lines(density(net2_female_degree_dist), col = 'gray')
lines(density(net3_female_degree_dist), col = 'gray')
lines(density(net4_female_degree_dist), col = 'gray')
lines(density(net5_female_degree_dist), col = 'gray')
lines(density(net6_female_degree_dist), col = 'gray')
lines(density(net7_female_degree_dist), col = 'gray')
lines(density(net8_female_degree_dist), col = 'gray')
lines(density(net9_female_degree_dist), col = 'gray')
lines(density(net10_female_degree_dist), col = 'gray')
legend(x=55, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_0.txt', sep = ',')
net2_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_1.txt', sep = ',')
net3_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_2.txt', sep = ',')
net4_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_3.txt', sep = ',')
net5_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_4.txt', sep = ',')
net6_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_5.txt', sep = ',')
net7_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_6.txt', sep = ',')
net8_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_7.txt', sep = ',')
net9_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_8.txt', sep = ',')
net10_age1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_age1_degree), xlab = "Overall age 1 degree", main = "Overall age 1 degree distribution", ylim = c(0, 0.18), lwd = 4)
lines(density(net1_age1_degree_dist), col = 'gray')
lines(density(net2_age1_degree_dist), col = 'gray')
lines(density(net3_age1_degree_dist), col = 'gray')
lines(density(net4_age1_degree_dist), col = 'gray')
lines(density(net5_age1_degree_dist), col = 'gray')
lines(density(net6_age1_degree_dist), col = 'gray')
lines(density(net7_age1_degree_dist), col = 'gray')
lines(density(net8_age1_degree_dist), col = 'gray')
lines(density(net9_age1_degree_dist), col = 'gray')
lines(density(net10_age1_degree_dist), col = 'gray')
legend(x=30, y = 0.11, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

net1_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_0.txt', sep = ',')
net2_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_1.txt', sep = ',')
net3_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_2.txt', sep = ',')
net4_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_3.txt', sep = ',')
net5_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_4.txt', sep = ',')
net6_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_5.txt', sep = ',')
net7_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_6.txt', sep = ',')
net8_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_7.txt', sep = ',')
net9_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_8.txt', sep = ',')
net10_age2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_age2_degree), xlab = "Overall age 2 degree", main = "Overall age 2 degree distribution", ylim = c(0, 0.06), lwd = 4)
lines(density(net1_age2_degree_dist), col = 'gray')
lines(density(net2_age2_degree_dist), col = 'gray')
lines(density(net3_age2_degree_dist), col = 'gray')
lines(density(net4_age2_degree_dist), col = 'gray')
lines(density(net5_age2_degree_dist), col = 'gray')
lines(density(net6_age2_degree_dist), col = 'gray')
lines(density(net7_age2_degree_dist), col = 'gray')
lines(density(net8_age2_degree_dist), col = 'gray')
lines(density(net9_age2_degree_dist), col = 'gray')
lines(density(net10_age2_degree_dist), col = 'gray')
legend(x=35, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_0.txt', sep = ',')
net2_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_1.txt', sep = ',')
net3_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_2.txt', sep = ',')
net4_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_3.txt', sep = ',')
net5_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_4.txt', sep = ',')
net6_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_5.txt', sep = ',')
net7_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_6.txt', sep = ',')
net8_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_7.txt', sep = ',')
net9_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_8.txt', sep = ',')
net10_age3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_age3_degree), xlab = "Overall age 3 degree", main = "Overall age 3 degree distribution", ylim = c(0, 0.10), lwd = 4)
lines(density(net1_age3_degree_dist), col = 'gray')
lines(density(net2_age3_degree_dist), col = 'gray')
lines(density(net3_age3_degree_dist), col = 'gray')
lines(density(net4_age3_degree_dist), col = 'gray')
lines(density(net5_age3_degree_dist), col = 'gray')
lines(density(net6_age3_degree_dist), col = 'gray')
lines(density(net7_age3_degree_dist), col = 'gray')
lines(density(net8_age3_degree_dist), col = 'gray')
lines(density(net9_age3_degree_dist), col = 'gray')
lines(density(net10_age3_degree_dist), col = 'gray')
legend(x=40, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_0.txt', sep = ',')
net2_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_1.txt', sep = ',')
net3_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_2.txt', sep = ',')
net4_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_3.txt', sep = ',')
net5_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_4.txt', sep = ',')
net6_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_5.txt', sep = ',')
net7_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_6.txt', sep = ',')
net8_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_7.txt', sep = ',')
net9_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_8.txt', sep = ',')
net10_age4_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_age4_degree), xlab = "Overall age 4 degree", main = "Overall age 4 degree distribution", lwd = 4)
lines(density(net1_age4_degree_dist), col = 'gray')
lines(density(net2_age4_degree_dist), col = 'gray')
lines(density(net3_age4_degree_dist), col = 'gray')
lines(density(net4_age4_degree_dist), col = 'gray')
lines(density(net5_age4_degree_dist), col = 'gray')
lines(density(net6_age4_degree_dist), col = 'gray')
lines(density(net7_age4_degree_dist), col = 'gray')
lines(density(net8_age4_degree_dist), col = 'gray')
lines(density(net9_age4_degree_dist), col = 'gray')
lines(density(net10_age4_degree_dist), col = 'gray')
legend(x=30, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

net1_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_0.txt', sep = ',')
net2_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_1.txt', sep = ',')
net3_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_2.txt', sep = ',')
net4_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_3.txt', sep = ',')
net5_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_4.txt', sep = ',')
net6_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_5.txt', sep = ',')
net7_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_6.txt', sep = ',')
net8_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_7.txt', sep = ',')
net9_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_8.txt', sep = ',')
net10_home_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_home_degree), xlab = "Overall home degree", main = "Overall home degree distribution", ylim = c(0, 0.09), lwd = 4)
lines(density(net1_home_degree_dist), col = 'gray')
lines(density(net2_home_degree_dist), col = 'gray')
lines(density(net3_home_degree_dist), col = 'gray')
lines(density(net4_home_degree_dist), col = 'gray')
lines(density(net5_home_degree_dist), col = 'gray')
lines(density(net6_home_degree_dist), col = 'gray')
lines(density(net7_home_degree_dist), col = 'gray')
lines(density(net8_home_degree_dist), col = 'gray')
lines(density(net9_home_degree_dist), col = 'gray')
lines(density(net10_home_degree_dist), col = 'gray')
legend(x=30, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

net1_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_0.txt', sep = ',')
net2_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_1.txt', sep = ',')
net3_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_2.txt', sep = ',')
net4_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_3.txt', sep = ',')
net5_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_4.txt', sep = ',')
net6_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_5.txt', sep = ',')
net7_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_6.txt', sep = ',')
net8_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_7.txt', sep = ',')
net9_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_8.txt', sep = ',')
net10_school_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_school_degree), xlab = "Overall school degree", main = "Overall school degree distribution", ylim = c(0, 0.05), lwd = 4)
lines(density(net1_school_degree_dist), col = 'gray')
lines(density(net2_school_degree_dist), col = 'gray')
lines(density(net3_school_degree_dist), col = 'gray')
lines(density(net4_school_degree_dist), col = 'gray')
lines(density(net5_school_degree_dist), col = 'gray')
lines(density(net6_school_degree_dist), col = 'gray')
lines(density(net7_school_degree_dist), col = 'gray')
lines(density(net8_school_degree_dist), col = 'gray')
lines(density(net9_school_degree_dist), col = 'gray')
lines(density(net10_school_degree_dist), col = 'gray')
legend(x=40, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

net1_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_0.txt', sep = ',')
net2_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_1.txt', sep = ',')
net3_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_2.txt', sep = ',')
net4_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_3.txt', sep = ',')
net5_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_4.txt', sep = ',')
net6_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_5.txt', sep = ',')
net7_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_6.txt', sep = ',')
net8_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_7.txt', sep = ',')
net9_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_8.txt', sep = ',')
net10_work_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_work_degree), xlab = "Overall work degree", main = "Overall work degree distribution", ylim = c(0, 0.08), lwd = 4)
lines(density(net1_work_degree_dist), col = 'gray')
lines(density(net2_work_degree_dist), col = 'gray')
lines(density(net3_work_degree_dist), col = 'gray')
lines(density(net4_work_degree_dist), col = 'gray')
lines(density(net5_work_degree_dist), col = 'gray')
lines(density(net6_work_degree_dist), col = 'gray')
lines(density(net7_work_degree_dist), col = 'gray')
lines(density(net8_work_degree_dist), col = 'gray')
lines(density(net9_work_degree_dist), col = 'gray')
lines(density(net10_work_degree_dist), col = 'gray')
legend(x=50, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_0.txt', sep = ',')
net2_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_1.txt', sep = ',')
net3_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_2.txt', sep = ',')
net4_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_3.txt', sep = ',')
net5_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_4.txt', sep = ',')
net6_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_5.txt', sep = ',')
net7_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_6.txt', sep = ',')
net8_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_7.txt', sep = ',')
net9_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_8.txt', sep = ',')
net10_edu1_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_edu1_degree), xlab = "Overall edu1 degree", main = "Overall edu1 degree distribution", ylim = c(0, 0.10), lwd = 4)
lines(density(net1_edu1_degree_dist), col = 'gray')
lines(density(net2_edu1_degree_dist), col = 'gray')
lines(density(net3_edu1_degree_dist), col = 'gray')
lines(density(net4_edu1_degree_dist), col = 'gray')
lines(density(net5_edu1_degree_dist), col = 'gray')
lines(density(net6_edu1_degree_dist), col = 'gray')
lines(density(net7_edu1_degree_dist), col = 'gray')
lines(density(net8_edu1_degree_dist), col = 'gray')
lines(density(net9_edu1_degree_dist), col = 'gray')
lines(density(net10_edu1_degree_dist), col = 'gray')
legend(x=30, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_0.txt', sep = ',')
net2_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_1.txt', sep = ',')
net3_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_2.txt', sep = ',')
net4_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_3.txt', sep = ',')
net5_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_4.txt', sep = ',')
net6_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_5.txt', sep = ',')
net7_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_6.txt', sep = ',')
net8_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_7.txt', sep = ',')
net9_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_8.txt', sep = ',')
net10_edu2_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_edu2_degree), xlab = "Overall edu2 degree", main = "Overall edu2 degree distribution", ylim = c(0, 0.09), lwd = 4)
lines(density(net1_edu2_degree_dist), col = 'gray')
lines(density(net2_edu2_degree_dist), col = 'gray')
lines(density(net3_edu2_degree_dist), col = 'gray')
lines(density(net4_edu2_degree_dist), col = 'gray')
lines(density(net5_edu2_degree_dist), col = 'gray')
lines(density(net6_edu2_degree_dist), col = 'gray')
lines(density(net7_edu2_degree_dist), col = 'gray')
lines(density(net8_edu2_degree_dist), col = 'gray')
lines(density(net9_edu2_degree_dist), col = 'gray')
lines(density(net10_edu2_degree_dist), col = 'gray')
legend(x=50, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_0.txt', sep = ',')
net2_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_1.txt', sep = ',')
net3_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_2.txt', sep = ',')
net4_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_3.txt', sep = ',')
net5_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_4.txt', sep = ',')
net6_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_5.txt', sep = ',')
net7_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_6.txt', sep = ',')
net8_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_7.txt', sep = ',')
net9_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_8.txt', sep = ',')
net10_edu3_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_overall_degree_distributions/net_9.txt', sep = ',')
plot(density(list_edu3_degree), xlab = "Overall edu3 degree", main = "Overall edu3 degree distribution", ylim = c(0, 0.08), lwd = 4)
lines(density(net1_edu3_degree_dist), col = 'gray')
lines(density(net2_edu3_degree_dist), col = 'gray')
lines(density(net3_edu3_degree_dist), col = 'gray')
lines(density(net4_edu3_degree_dist), col = 'gray')
lines(density(net5_edu3_degree_dist), col = 'gray')
lines(density(net6_edu3_degree_dist), col = 'gray')
lines(density(net7_edu3_degree_dist), col = 'gray')
lines(density(net8_edu3_degree_dist), col = 'gray')
lines(density(net9_edu3_degree_dist), col = 'gray')
lines(density(net10_edu3_degree_dist), col = 'gray')
legend(x=30, y = 0.05, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_0.txt', sep = ',')
net2_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_1.txt', sep = ',')
net3_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_2.txt', sep = ',')
net4_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_3.txt', sep = ',')
net5_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_4.txt', sep = ',')
net6_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_5.txt', sep = ',')
net7_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_6.txt', sep = ',')
net8_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_7.txt', sep = ',')
net9_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_8.txt', sep = ',')
net10_male_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/male_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_male_male_degree), xlab = "Male assortative degree", main = "Assortative male degree distribution", ylim = c(0, 0.12), lwd = 4)
lines(density(net1_male_assort_degree_dist), col = 'gray')
lines(density(net2_male_assort_degree_dist), col = 'gray')
lines(density(net3_male_assort_degree_dist), col = 'gray')
lines(density(net4_male_assort_degree_dist), col = 'gray')
lines(density(net5_male_assort_degree_dist), col = 'gray')
lines(density(net6_male_assort_degree_dist), col = 'gray')
lines(density(net7_male_assort_degree_dist), col = 'gray')
lines(density(net8_male_assort_degree_dist), col = 'gray')
lines(density(net9_male_assort_degree_dist), col = 'gray')
lines(density(net10_male_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_0.txt', sep = ',')
net2_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_1.txt', sep = ',')
net3_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_2.txt', sep = ',')
net4_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_3.txt', sep = ',')
net5_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_4.txt', sep = ',')
net6_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_5.txt', sep = ',')
net7_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_6.txt', sep = ',')
net8_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_7.txt', sep = ',')
net9_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_8.txt', sep = ',')
net10_female_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/female_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_female_female_degree), xlab = "Female assortative degree", main = "Assortative female degree distribution", ylim = c(0, 0.12), lwd = 4)
lines(density(net1_female_assort_degree_dist), col = 'gray')
lines(density(net2_female_assort_degree_dist), col = 'gray')
lines(density(net3_female_assort_degree_dist), col = 'gray')
lines(density(net4_female_assort_degree_dist), col = 'gray')
lines(density(net5_female_assort_degree_dist), col = 'gray')
lines(density(net6_female_assort_degree_dist), col = 'gray')
lines(density(net7_female_assort_degree_dist), col = 'gray')
lines(density(net8_female_assort_degree_dist), col = 'gray')
lines(density(net9_female_assort_degree_dist), col = 'gray')
lines(density(net10_female_assort_degree_dist), col = 'gray')
legend(x=40, y = 0.08, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_0.txt', sep = ',')
net2_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_1.txt', sep = ',')
net3_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_2.txt', sep = ',')
net4_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_3.txt', sep = ',')
net5_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_4.txt', sep = ',')
net6_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_5.txt', sep = ',')
net7_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_6.txt', sep = ',')
net8_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_7.txt', sep = ',')
net9_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_8.txt', sep = ',')
net10_age1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age1_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_1_1_degree), xlab = "Age 1 assortative degree", main = "Assortative age 1 degree distribution", ylim  =c(0, 1.1), lwd = 4)
lines(density(net1_age1_assort_degree_dist), col = 'gray')
lines(density(net2_age1_assort_degree_dist), col = 'gray')
lines(density(net3_age1_assort_degree_dist), col = 'gray')
lines(density(net4_age1_assort_degree_dist), col = 'gray')
lines(density(net5_age1_assort_degree_dist), col = 'gray')
lines(density(net6_age1_assort_degree_dist), col = 'gray')
lines(density(net7_age1_assort_degree_dist), col = 'gray')
lines(density(net8_age1_assort_degree_dist), col = 'gray')
lines(density(net9_age1_assort_degree_dist), col = 'gray')
lines(density(net10_age1_assort_degree_dist), col = 'gray')
legend(x=20, y = 0.7, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_0.txt', sep = ',')
net2_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_1.txt', sep = ',')
net3_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_2.txt', sep = ',')
net4_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_3.txt', sep = ',')
net5_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_4.txt', sep = ',')
net6_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_5.txt', sep = ',')
net7_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_6.txt', sep = ',')
net8_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_7.txt', sep = ',')
net9_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_8.txt', sep = ',')
net10_age2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age2_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_2_2_degree), xlab = "Age 2 assortative degree", main = "Assortative age 2 degree distribution", ylim = c(0, 0.1), lwd = 4)
lines(density(net1_age2_assort_degree_dist), col = 'gray')
lines(density(net2_age2_assort_degree_dist), col = 'gray')
lines(density(net3_age2_assort_degree_dist), col = 'gray')
lines(density(net4_age2_assort_degree_dist), col = 'gray')
lines(density(net5_age2_assort_degree_dist), col = 'gray')
lines(density(net6_age2_assort_degree_dist), col = 'gray')
lines(density(net7_age2_assort_degree_dist), col = 'gray')
lines(density(net8_age2_assort_degree_dist), col = 'gray')
lines(density(net9_age2_assort_degree_dist), col = 'gray')
lines(density(net10_age2_assort_degree_dist), col = 'gray')
legend(x=25, y = 0.06, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_0.txt', sep = ',')
net2_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_1.txt', sep = ',')
net3_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_2.txt', sep = ',')
net4_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_3.txt', sep = ',')
net5_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_4.txt', sep = ',')
net6_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_5.txt', sep = ',')
net7_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_6.txt', sep = ',')
net8_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_7.txt', sep = ',')
net9_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_8.txt', sep = ',')
net10_age3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age3_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_3_3_degree), xlab = "Age 3 assortative degree", main = "Assortative age 3 degree distribution", ylim = c(0, 0.1), lwd = 4)
lines(density(net1_age3_assort_degree_dist), col = 'gray')
lines(density(net2_age3_assort_degree_dist), col = 'gray')
lines(density(net3_age3_assort_degree_dist), col = 'gray')
lines(density(net4_age3_assort_degree_dist), col = 'gray')
lines(density(net5_age3_assort_degree_dist), col = 'gray')
lines(density(net6_age3_assort_degree_dist), col = 'gray')
lines(density(net7_age3_assort_degree_dist), col = 'gray')
lines(density(net8_age3_assort_degree_dist), col = 'gray')
lines(density(net9_age3_assort_degree_dist), col = 'gray')
lines(density(net10_age3_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.06, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_0.txt', sep = ',')
net2_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_1.txt', sep = ',')
net3_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_2.txt', sep = ',')
net4_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_3.txt', sep = ',')
net5_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_4.txt', sep = ',')
net6_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_5.txt', sep = ',')
net7_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_6.txt', sep = ',')
net8_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_7.txt', sep = ',')
net9_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_8.txt', sep = ',')
net10_age4_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/age4_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_4_4_degree), xlab = "Age 4 assortative degree", main = "Assortative age 4 degree distribution", ylim = c(0, 1), lwd = 4)
lines(density(net1_age4_assort_degree_dist), col = 'gray')
lines(density(net2_age4_assort_degree_dist), col = 'gray')
lines(density(net3_age4_assort_degree_dist), col = 'gray')
lines(density(net4_age4_assort_degree_dist), col = 'gray')
lines(density(net5_age4_assort_degree_dist), col = 'gray')
lines(density(net6_age4_assort_degree_dist), col = 'gray')
lines(density(net7_age4_assort_degree_dist), col = 'gray')
lines(density(net8_age4_assort_degree_dist), col = 'gray')
lines(density(net9_age4_assort_degree_dist), col = 'gray')
lines(density(net10_age4_assort_degree_dist), col = 'gray')
legend(x=10, y = 0.8, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_0.txt', sep = ',')
net2_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_1.txt', sep = ',')
net3_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_2.txt', sep = ',')
net4_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_3.txt', sep = ',')
net5_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_4.txt', sep = ',')
net6_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_5.txt', sep = ',')
net7_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_6.txt', sep = ',')
net8_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_7.txt', sep = ',')
net9_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_8.txt', sep = ',')
net10_home_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/home_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_home_home_degree), xlab = "home assortative degree", main = "Assortative home degree distribution", lwd = 4)
lines(density(net1_home_assort_degree_dist), col = 'gray')
lines(density(net2_home_assort_degree_dist), col = 'gray')
lines(density(net3_home_assort_degree_dist), col = 'gray')
lines(density(net4_home_assort_degree_dist), col = 'gray')
lines(density(net5_home_assort_degree_dist), col = 'gray')
lines(density(net6_home_assort_degree_dist), col = 'gray')
lines(density(net7_home_assort_degree_dist), col = 'gray')
lines(density(net8_home_assort_degree_dist), col = 'gray')
lines(density(net9_home_assort_degree_dist), col = 'gray')
lines(density(net10_home_assort_degree_dist), col = 'gray')
legend(x=20, y = 0.3, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_0.txt', sep = ',')
net2_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_1.txt', sep = ',')
net3_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_2.txt', sep = ',')
net4_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_3.txt', sep = ',')
net5_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_4.txt', sep = ',')
net6_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_5.txt', sep = ',')
net7_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_6.txt', sep = ',')
net8_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_7.txt', sep = ',')
net9_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_8.txt', sep = ',')
net10_school_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/school_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_school_school_degree), xlab = "school assortative degree", main = "Assortative school degree distribution", ylim = c(0, 0.085), lwd = 4)
lines(density(net1_school_assort_degree_dist), col = 'gray')
lines(density(net2_school_assort_degree_dist), col = 'gray')
lines(density(net3_school_assort_degree_dist), col = 'gray')
lines(density(net4_school_assort_degree_dist), col = 'gray')
lines(density(net5_school_assort_degree_dist), col = 'gray')
lines(density(net6_school_assort_degree_dist), col = 'gray')
lines(density(net7_school_assort_degree_dist), col = 'gray')
lines(density(net8_school_assort_degree_dist), col = 'gray')
lines(density(net9_school_assort_degree_dist), col = 'gray')
lines(density(net10_school_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.06, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_0.txt', sep = ',')
net2_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_1.txt', sep = ',')
net3_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_2.txt', sep = ',')
net4_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_3.txt', sep = ',')
net5_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_4.txt', sep = ',')
net6_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_5.txt', sep = ',')
net7_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_6.txt', sep = ',')
net8_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_7.txt', sep = ',')
net9_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_8.txt', sep = ',')
net10_work_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/work_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_work_work_degree), xlab = "work assortative degree", main = "Assortative work degree distribution", ylim = c(0, 0.12), lwd =4)
lines(density(net1_work_assort_degree_dist), col = 'gray')
lines(density(net2_work_assort_degree_dist), col = 'gray')
lines(density(net3_work_assort_degree_dist), col = 'gray')
lines(density(net4_work_assort_degree_dist), col = 'gray')
lines(density(net5_work_assort_degree_dist), col = 'gray')
lines(density(net6_work_assort_degree_dist), col = 'gray')
lines(density(net7_work_assort_degree_dist), col = 'gray')
lines(density(net8_work_assort_degree_dist), col = 'gray')
lines(density(net9_work_assort_degree_dist), col = 'gray')
lines(density(net10_work_assort_degree_dist), col = 'gray')
legend(x=40, y = 0.06, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)



net1_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_0.txt', sep = ',')
net2_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_1.txt', sep = ',')
net3_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_2.txt', sep = ',')
net4_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_3.txt', sep = ',')
net5_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_4.txt', sep = ',')
net6_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_5.txt', sep = ',')
net7_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_6.txt', sep = ',')
net8_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_7.txt', sep = ',')
net9_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_8.txt', sep = ',')
net10_edu1_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu1_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_edu1_1_degree), xlab = "edu1 assortative degree", main = "Assortative edu1 degree distribution", lwd = 4)
lines(density(net1_edu1_assort_degree_dist), col = 'gray')
lines(density(net2_edu1_assort_degree_dist), col = 'gray')
lines(density(net3_edu1_assort_degree_dist), col = 'gray')
lines(density(net4_edu1_assort_degree_dist), col = 'gray')
lines(density(net5_edu1_assort_degree_dist), col = 'gray')
lines(density(net6_edu1_assort_degree_dist), col = 'gray')
lines(density(net7_edu1_assort_degree_dist), col = 'gray')
lines(density(net8_edu1_assort_degree_dist), col = 'gray')
lines(density(net9_edu1_assort_degree_dist), col = 'gray')
lines(density(net10_edu1_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.12, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

net1_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_0.txt', sep = ',')
net2_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_1.txt', sep = ',')
net3_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_2.txt', sep = ',')
net4_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_3.txt', sep = ',')
net5_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_4.txt', sep = ',')
net6_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_5.txt', sep = ',')
net7_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_6.txt', sep = ',')
net8_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_7.txt', sep = ',')
net9_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_8.txt', sep = ',')
net10_edu2_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu2_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_edu2_2_degree), xlab = "edu2 assortative degree", main = "Assortative edu2 degree distribution", lwd = 4)
lines(density(net1_edu2_assort_degree_dist), col = 'gray')
lines(density(net2_edu2_assort_degree_dist), col = 'gray')
lines(density(net3_edu2_assort_degree_dist), col = 'gray')
lines(density(net4_edu2_assort_degree_dist), col = 'gray')
lines(density(net5_edu2_assort_degree_dist), col = 'gray')
lines(density(net6_edu2_assort_degree_dist), col = 'gray')
lines(density(net7_edu2_assort_degree_dist), col = 'gray')
lines(density(net8_edu2_assort_degree_dist), col = 'gray')
lines(density(net9_edu2_assort_degree_dist), col = 'gray')
lines(density(net10_edu2_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.12, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)


net1_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_0.txt', sep = ',')
net2_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_1.txt', sep = ',')
net3_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_2.txt', sep = ',')
net4_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_3.txt', sep = ',')
net5_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_4.txt', sep = ',')
net6_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_5.txt', sep = ',')
net7_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_6.txt', sep = ',')
net8_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_7.txt', sep = ',')
net9_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_8.txt', sep = ',')
net10_edu3_assort_degree_dist = scan('/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/sim_runs_normalized_absenteeism_vs_edu_7_5/best_model_files_5_2_to_5_7/5_7_modified_model/checking_outputs/edu3_assortative_degree_dist/net_9.txt', sep = ',')
plot(density(list_edu3_3_degree), xlab = "edu3 assortative degree", main = "Assortative edu3 degree distribution", lwd = 4)
lines(density(net1_edu3_assort_degree_dist), col = 'gray')
lines(density(net2_edu3_assort_degree_dist), col = 'gray')
lines(density(net3_edu3_assort_degree_dist), col = 'gray')
lines(density(net4_edu3_assort_degree_dist), col = 'gray')
lines(density(net5_edu3_assort_degree_dist), col = 'gray')
lines(density(net6_edu3_assort_degree_dist), col = 'gray')
lines(density(net7_edu3_assort_degree_dist), col = 'gray')
lines(density(net8_edu3_assort_degree_dist), col = 'gray')
lines(density(net9_edu3_assort_degree_dist), col = 'gray')
lines(density(net10_edu3_assort_degree_dist), col = 'gray')
legend(x=30, y = 0.12, legend = c("POLYMOD data","Simulated networks"), col = c("black", "gray"), lty = 1)

#checking household size
sim = simulate(fit.poly.edu11.2, nsim = 1)
sim$mel
sim$gal
sim$val
sim$iel
sim$oel

hh_size = NULL
hh_num1 = NULL
hh_num2 = NULL
hh_num3 = NULL
hh_num4 = NULL
hh_adults_per_children = NULL
hh_adults_no_children = NULL
hh_children_per_adult = NULL
hh_children_no_adult = NULL


for (each_entry in seq(1, length(ego_data_df$egoIDcol))){
  node_id = ego_data_df$egoIDcol[each_entry]
  home_status = ego_data_df$home[each_entry]
  node_age = ego_data_df$age[each_entry]
  if (isTRUE(home_status==1)==T){
    num_age1 = 0
    num_age2 = 0
    num_age3 = 0
    num_age4 = 0
    if (isTRUE(node_age == 1)==T){
      num_age1 = num_age1+1
    }
    if (isTRUE(node_age == 2)==T){
      num_age2 = num_age2+1
    }
    if (isTRUE(node_age == 3)==T){
      num_age3 = num_age3+1
    }
    if (isTRUE(node_age == 4)==T){
      num_age4 = num_age4+1
    }
    neighbor_indices = which(alter_data_df$egoIDcol == node_id)
    for (each_neighbor in neighbor_indices){
      neighbor_home_status = alter_data_df$home[each_neighbor]
      neighbor_age = alter_data_df$age[each_neighbor]
      if (isTRUE(neighbor_home_status == 1)==T){
        if (isTRUE(neighbor_age == 1)==T){
          num_age1 = num_age1+1
        }
        if (isTRUE(neighbor_age == 2)==T){
          num_age2 = num_age2+1
        }
        if (isTRUE(neighbor_age == 3)==T){
          num_age3 = num_age3+1
        }
        if (isTRUE(neighbor_age == 4)==T){
          num_age4 = num_age4+1
        }
      }
    }
    hh_num1 = c(hh_num1, num_age1)
    hh_num2 = c(hh_num2, num_age2)
    hh_num3 = c(hh_num3, num_age3)
    hh_num4 = c(hh_num1, num_age4)
    hh_size = c(hh_size, sum(num_age1, num_age2, num_age3, num_age4))
    if (isTRUE(sum(num_age1+num_age2)>0)==T){
      hh_adults_per_children = c(hh_adults_per_children, (sum(num_age3+num_age4)/sum(num_age1+num_age2)))
    }
    if (isTRUE(sum(num_age1+num_age2)==0)==T){
      hh_adults_no_children = c(hh_adults_no_children, (sum(num_age3+num_age4)))
    }
    if (isTRUE(sum(num_age3+num_age3)>0)==T){
      hh_children_per_adult = c(hh_children_per_adult, (sum(num_age1+num_age2)/sum(num_age3+num_age4)))
    }
    if (isTRUE(sum(num_age3+num_age4)==0)==T){
      hh_children_no_adult = c(hh_children_no_adult, (sum(num_age1+num_age2)))
    }
  }
  
}

boxplot(hh_num1, xlab = "number of age 1 individuals", main = "Age 1 individuals in each household")
boxplot(hh_num2, xlab = "Number of age 2 individuals", main = "Age 2 individuals in each household")
boxplot(hh_num3, xlab = "Number of age 3 individuals", main = "Age 3 individuals in each household")
boxplot(hh_num4, xlab = "Number of age 4 individuals", main = "Age 4 individuals in each household")
boxplot(hh_size, xlab = "Number of individuals in each household", main = "Household size")

boxplot(hh_adults_per_children, xlab = "Number of adults/number of children", main = "Number of adults per children where there is at least 1 child", yaxp = c(0, 18, 18))
boxplot(hh_adults_no_children, xlab = "Number of adults", main = "Number of adults in households with no children")

boxplot(hh_children_per_adult, xlab = "Number of children/number of adults", main = "Number of children per adults where there is at least 1 adult")
boxplot(hh_children_no_adult, xlab = "Number of children", main = "Number of children in households with no adults")




#seelct subset of nodes from ermg for gephi analysis
sim = as.matrix(poly.sim[1][[1]], matrix.type= "edgelist")
edgelist = cbind(sim[,1], sim[,2])
edgelist = as.data.frame(edgelist)
colnames(edgelist) = c("Source", "Target")
nodes_chosen= sample(edgelist$node1, 50)
nodes_chosen = unique(nodes_chosen) # randomly choose a number of unique nodes

node1_edgelist = NULL
node2_edgelist = NULL

for (each_contact in seq(1,length(edgelist$node1))){
  node1 = edgelist$node1[each_contact]
  node2 = edgelist$node2[each_contact]
  if (node1 %in% nodes_chosen){
    node1_edgelist = c(node1_edgelist, node1)
    node2_edgelist = c(node2_edgelist, node2)
  }
  if (node2 %in% nodes_chosen){
    node1_edgelist = c(node1_edgelist, node1)
    node2_edgelist = c(node2_edgelist, node2)
  }
}
subset_edgelist = cbind(node1_edgelist, node2_edgelist)
colnames(subset_edgelist) = c('node1', 'node2')
subset_edgelist = as.data.frame(subset_edgelist)

write.table(edgelist, file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/full_sim_net_edgelist_for_gephi_6_7.csv", sep = ",", row.names = F)

age_attributes = as.data.frame(cbind(ego_data_df$egoIDcol, ego_data_df$egoIDcol, ego_data_df$age))
colnames(age_attributes)=c('Id', 'Label', 'Age')
write.table(age_attributes, file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/full_sim_net_age_attributes_for_gephi_6_7.csv", sep = ",", row.names = F)












