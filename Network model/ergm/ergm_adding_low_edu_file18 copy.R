rm(list=ls())
#import packages
library(dplyr)
library(ergm.ego)

library(network)
library(ergm)

library(ergm.ego)

setwd("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/POLYMOD_DATA/ORIGINAL_DATA_FILES")

#initialize network
poly.net = network.initialize(7290, directed = F, hyper = FALSE, loops=FALSE, multiple = FALSE, bipartite = FALSE)

#read in participant data
poly_participants = read.delim('participants_final_v3.txt', header = TRUE, fill = TRUE)


#read in contact data
poly_contacts = read.table('contacts_final_v2_with_weights.txt', sep = ',', header = TRUE)






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

num_edu_1_alters = length(subset(alter_data_df, edu == 1)$edu)
num_edu_2_alters = length(subset(alter_data_df, edu == 2)$edu)
num_edu_3_alters = length(subset(alter_data_df, edu == 3)$edu)

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

print(num_edu_1_alters)
print(num_edu_2_alters)
print(num_edu_3_alters)

num_edu_1_alters = length(subset(alter_data_df, edu == 1)$edu)
num_edu_2_alters = length(subset(alter_data_df, edu == 2)$edu)
num_edu_3_alters = length(subset(alter_data_df, edu == 3)$edu)


#################### changing ego_data_df and alter_data_df to give different edu level distributions
low_edu = 0
med_edu = 0
high_edu = 0
total_nodes = 
  for (each_ego_edu in ego_data_df$edu){
    if (isTRUE(each_ego_edu == 1)==TRUE){
      low_edu = low_edu + 1
    }
    if (isTRUE(each_ego_edu == 2)==TRUE){
      med_edu = med_edu + 1
    }
    if (isTRUE(each_ego_edu == 3)==TRUE){
      high_edu = high_edu + 1
    }
  }

low_edu_ego_df = subset(ego_data_df, edu == 1)
low_edu_alter_df = subset(alter_data_df, edu == 1)
num_low_edu_ego = length(low_edu_ego_df$egoIDcol)
num_low_edu_alter = length(low_edu_alter_df$egoIDcol)

# 20% low edu

multiply_low_edu_df = rbind(low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df, low_edu_ego_df)

length(multiply_low_edu_df$egoIDcol)/(7290+(length(low_edu_ego_df$egoIDcol)*21))

# need to reclassify these egos as new egos, grab the alters of these 5* high educaiton nodes and 5* them also
#then do ergm

max(ego_data_df$egoIDcol)

new_ego_ids = seq(50038, 50037+(length(multiply_low_edu_df$egoIDcol)))


multiply_high_edu_df_w_new_ids = cbind(multiply_low_edu_df, new_ego_ids) # still need to rbind this to main ego df



zero_list = rep(0, length(ego_data_df$egoIDcol))

ego_data_df = cbind(ego_data_df, zero_list)

colnames(ego_data_df) = c('egoIDcol', 'gender', 'age', 'home', 'school_work', 'edu', 'new_ego_ids')

ego_data_df$new_ego_ids = ego_data_df$egoIDcol

full_ego_data = rbind(ego_data_df, multiply_high_edu_df_w_new_ids)

high_edu_df = subset(full_ego_data, edu == 3)

length(high_edu_df$egoIDcol)/length(full_ego_data$egoIDcol)



na_list = rep(0, length(alter_data_df$egoIDcol))


alter_data_df = cbind(alter_data_df, na_list)

colnames(alter_data_df) = c('egoIDcol', 'gender', 'age', 'home', 'school_work', 'edu', 'new_ego_id_alt')

typeof(alter_data_df$new_ego_id_alt[3])

# go through each added ego. get its alters. connect alter to the new ego id (create an NA column and add to it). 
#save these "new alters" in their own df to be rbinded to the main alter df


new_alter_df = NULL
for (each_ego in seq(1, length(multiply_high_edu_df_w_new_ids$egoIDcol))){
  original_ego_id = multiply_high_edu_df_w_new_ids$egoIDcol[each_ego]
  new_ego_id = multiply_high_edu_df_w_new_ids$new_ego_ids[each_ego]
  indices_of_each_egos_alters = which(alter_data_df$egoIDcol==original_ego_id)
  for (each_alter_index in indices_of_each_egos_alters){
    #alter_data_df$new_ego_id_alt[each_alter_index] = new_ego_id
    each_alter_data = alter_data_df[each_alter_index,]
    
    each_alter_data$new_ego_id_alt= new_ego_id
    new_alter_df = rbind(new_alter_df, each_alter_data)
  }
}


alter_data_df$new_ego_id_alt = alter_data_df$egoIDcol

full_alter_data = rbind(alter_data_df, new_alter_df)


ready_ego_data = full_ego_data



ready_ego_data = subset(full_ego_data, select=-c(egoIDcol))
colnames(ready_ego_data) = c('gender', 'age', 'home', 'school_work', 'edu', 'egoIDcol')

ready_alter_data = subset(full_alter_data, select=-c(egoIDcol))
colnames(ready_alter_data) = c('gender', 'age', 'home', 'school_work', 'edu', 'egoIDcol')


length(subset(ready_ego_data, edu ==1)$edu)
length(ready_ego_data$egoIDcol)

count1 = 0
count2 = 0
count3 = 0
total_count = 0
for (each in ready_ego_data$edu){
  total_count = total_count+1
  if (isTRUE(each == 1)==TRUE){
    count1 = count1+1
  }
  if (isTRUE(each == 2)==TRUE){
    count2 = count2+1
  }
  if (isTRUE(each == 3)==TRUE){
    count3 = count3+1
  }
}

count1 = 0
count2 = 0
count3 = 0
total_count = 0
for (each in alter_data_df$edu){
  total_count = total_count+1
  if (isTRUE(each == 1)==TRUE){
    count1 = count1+1
  }
  if (isTRUE(each == 2)==TRUE){
    count2 = count2+1
  }
  if (isTRUE(each == 3)==TRUE){
    count3 = count3+1
  }
}

poly.ego = as.egodata(ready_ego_data, alters = ready_alter_data, egoIDcol = "egoIDcol")


fit.poly.edu11.2 = ergm.ego(poly.ego~edges+nodefactor('gender', base = 1)+nodefactor('age', base = c(1,2))+nodefactor('school_work')+nodefactor('edu', base = 3)+nodematch('age')+nodematch('home')+nodematch('school_work')+nodematch('edu', diff = T), control = control.ergm.ego(ergm.control = control.ergm(MCMLE.maxit = 1000, MCMC.interval = 1000000, MCMC.burnin = 1000000, MCMC.return.stats = T)))


pop_size = length(ready_ego_data$egoIDcol)

num_sims = 10
poly.sim = simulate(fit.poly.edu11.2, nsim = num_sims, popsize =pop_size, verbose = TRUE)

for (each_sim in seq(1, num_sims)){
  #print(poly.sim[each_sim])
  sim = as.matrix(poly.sim[each_sim][[1]], matrix.type= "edgelist")
  edgelist = cbind(sim[,1], sim[,2])
  edgelist = as.data.frame(edgelist)
  colnames(edgelist) = c("node1", "node2")
  filename_edgelist = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_",each_sim,".txt", sep = "")
  write.table(edgelist, file = filename_edgelist, sep = ',', row.names = F, col.names = F)
  
  all_nodes = seq(1, pop_size)
  empty_vals = rep(0, pop_size)
  age_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(age_attribute) = c('node', 'age')
  for (each_node in all_nodes){
    age_attribute$node[each_node] = each_node
    age = poly.sim[each_sim][[1]]$val[[each_node]]$age
    age_attribute$age[each_node] = age
  }
  filename_age = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_age_attributes_",each_sim,".txt", sep = "")
  write.table(age_attribute, file = filename_age, sep = ',', row.names = F, col.names = F)
  
  gender_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(gender_attribute) = c('node', 'gender')
  for (each_node in all_nodes){
    gender_attribute$node[each_node] = each_node
    gender = poly.sim[each_sim][[1]]$val[[each_node]]$gender
    gender_attribute$gender[each_node] = gender
  }
  filename_gender = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_gender_attributes_",each_sim,".txt", sep = "")
  write.table(gender_attribute, file = filename_gender, sep = ',', row.names = F, col.names = F)
  
  school_work_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(school_work_attribute) = c('node', 'school_work')
  for (each_node in all_nodes){
    school_work_attribute$node[each_node] = each_node
    school_work = poly.sim[each_sim][[1]]$val[[each_node]]$school_work
    school_work_attribute$school_work[each_node] = school_work
  }
  filename_school_work = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_school_work_attributes_",each_sim,".txt", sep = "")
  write.table(school_work_attribute, file =filename_school_work, sep = ',', row.names = F, col.names = F)
  
  home_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(home_attribute) = c('node', 'home')
  for (each_node in all_nodes){
    home_attribute$node[each_node] = each_node
    home = poly.sim[each_sim][[1]]$val[[each_node]]$home
    home_attribute$home[each_node] = home
  }
  filename_home = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_home_attributes_",each_sim,".txt", sep = "")
  write.table(home_attribute, file = filename_home, sep = ',', row.names = F, col.names = F)
  
  edu_attribute= as.data.frame(cbind(empty_vals, empty_vals))
  colnames(edu_attribute) = c('node', 'edu')
  for (each_node in all_nodes){
    edu_attribute$node[each_node] = each_node
    edu = poly.sim[each_sim][[1]]$val[[each_node]]$edu
    edu_attribute$edu[each_node] = edu
  }
  filename_edu = paste("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/ergm_nets_with_varying_prop_low_edu/low_edu_higher9/sim_net_edu_attributes_",each_sim,".txt", sep = "")
  write.table(edu_attribute, file = filename_edu, sep = ',', row.names = F, col.names = F)
}