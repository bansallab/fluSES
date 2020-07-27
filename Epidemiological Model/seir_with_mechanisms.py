#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 20 13:16:17 2019

@author: admin
"""

#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Run this on Py 27

11/13

Updates on this file: 
    
    - simulations can run on regular network or ergm network - check! 
    - fix vaccination code so the general and ses vaccinate equal numbers
    - add positive control for healthcare utilization
    - add positive control for susceptibiltiy
    - chanbe absenteeism code to be low vs other




"""

#master file of functions for experiments on unweighted networks with the same lenght of b

import networkx as nx
import random as rnd
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
import errno
import itertools



def create_unweighted_sim_net(net_number, prop_low_edu):
    """
    Read in simulated network file unweighted, add the node attributes
    """
    #create simulated network
   
 
    file_name = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_"+str(net_number)+".txt"
    G = nx.Graph()
    G = nx.read_edgelist(file_name, delimiter=',', nodetype=int)
    
    #add in nodes that are not in the simulation (with no edges)
    max_node = max(nx.nodes(G))
    
    for each_node in range(1, max_node):
        if not each_node in nx.nodes(G):
            G.add_node(each_node)

    #5/7 modified model is the new ERGM where I added gender back in!
    age_file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_age_attributes_"+str(net_number)+".txt"
    gender_file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_gender_attributes_"+str(net_number)+".txt"
    home_file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_home_attributes_"+str(net_number)+".txt"
    school_work_file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_school_work_attributes_"+str(net_number)+".txt"
    edu_file = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/selected_ergm_nets_with_varying_prop_low_edu/low_edu_"+str(prop_low_edu)+"_percent/sim_net_edu_attributes_"+str(net_number)+".txt"
    
    #create dicionary of ages, levels: 1(infant), 2(child), 3(adult), 4(elderly), NA
    age_dict = {}
    with open(age_file) as a:
        for line in a:
            (key, val) = line.split(',')
            if val.strip() != 'NA':
                age_dict[int(key)] = int(val.strip())
            else:
                age_dict[int(key)] = val
                
    #create dictionary of genders, levels: NA, 0(male), 1 (female)
    gender_dict = {}
    with open(gender_file) as b:
        for line in b:
            (key, val) = line.split(',')
            if val.strip() != 'NA':
                gender_dict[int(key)] = int(val.strip())
            else:
                gender_dict[int(key)] = val
                
    # create dictionary of school/work status, levels: 0 (school), 1(work), NA        
    school_work_dict = {}
    with open(school_work_file) as c:
        for line in c:
            (key, val) = line.split(',')
            if not val.strip() in ('NA', '"NA"'):
                school_work_dict[int(key)] = int(val.strip())
            else:
                school_work_dict[int(key)] = val

   
    #create dictionary of home, levels: 0 (not home), 1 (home), NA
    home_dict = {}
    with open(home_file) as e:
        for line in e:
            (key, val) = line.split(',')
            if val.strip() != 'NA':
                home_dict[int(key)] = int(val.strip())
            else:
                home_dict[int(key)] = val
                
                
        #create dictionary of home, levels: 0 (not home), 1 (home), NA
    edu_dict = {}
    with open(edu_file) as f:
        for line in f:
            (key, val) = line.split(',')
            if val.strip() != 'NA':
                edu_dict[int(key)] = int(val.strip())
            else:
                edu_dict[int(key)] = val
    
    
#    #create dictionary of eduction attributes, levels: 1 (low), 2 (medium), 3 (high), NA   
#    num_low_edu_nodes = int(prop_low_edu*7290.0)
#    all_nodes = nx.nodes(G)
#    random_low_nodes = rnd.sample(all_nodes, num_low_edu_nodes)
#    other_nodes = [each_node for each_node in all_nodes if not each_node in random_low_nodes]
#    num_other_nodes = len(other_nodes)
#    num_medium_nodes = int(num_other_nodes/2)
#    num_high_nodes = len(other_nodes) - num_medium_nodes
#    random_medium_nodes = rnd.sample(other_nodes, num_medium_nodes)
#    random_high_nodes = [each_node for each_node in other_nodes if not each_node in random_medium_nodes]
#    
#    edu_dict = {each_low_edu_node :1 for each_low_edu_node in random_low_nodes}
#    for each_med_node in random_medium_nodes:
#        edu_dict[each_med_node] = 2
#    for each_high_node in random_high_nodes:
#        edu_dict[each_high_node] = 3
  
    

    #set all node attributes
    nx.set_node_attributes(G,  age_dict, "age")
    nx.set_node_attributes(G,  gender_dict, "gender")
    nx.set_node_attributes(G, school_work_dict, "school_work")
    nx.set_node_attributes(G,  home_dict, "home")
    nx.set_node_attributes(G, edu_dict,  "edu")
    
    return G



def create_regular_net(ergm_net):
    num_nodes = nx.number_of_nodes(ergm_net)
    all_degrees = []
    for each_node in nx.nodes(ergm_net):
        all_degrees.append(ergm_net.degree(each_node))
    mean_degree = int(np.mean(all_degrees))
    
    reg_graph = nx.random_regular_graph(mean_degree, num_nodes)
    return reg_graph

def vacc_a_priori_general(net, edu_dict, total_num_to_vacc):
    
    vacc_nodes = rnd.sample(nx.nodes(net), total_num_to_vacc)

    return vacc_nodes

def vacc_a_priori_by_ses(net, edu_dict, num_low_to_vacc, num_other_to_vacc):
    
    low_edu_nodes = [each_node for each_node, each_edu in edu_dict.iteritems() if each_edu==1]
    other_edu_nodes = [each_node for each_node, each_edu in edu_dict.iteritems() if each_edu!=1]
    
    
    vacc_low_edu_nodes = rnd.sample(low_edu_nodes, num_low_to_vacc)
    vacc_other_edu_nodes = rnd.sample(other_edu_nodes, num_other_to_vacc)
    
    
    return vacc_low_edu_nodes, vacc_other_edu_nodes
    

    
############################################################################################################################################
def choose_patient_0_and_set_attributes(net):
    """
    Start infection status dictionaries and randomly choose one individual to be patient zero
    """
    all_nodes = nx.nodes(net) #all nodes
    S_status = {} #dictionaries to keep track of status of each node
    E_status = {} #each dictionary is node1:0, node1:0,...
    I_status = {} #0 indicates the node is not in that status, 1 indicates the node is in that status
    R_status = {}
    for each_node in all_nodes:
        S_status[each_node] = 1 #dict of 1's for every node (all start susceptible)
        E_status[each_node] = 0 #0 for every node 
        I_status[each_node] = 0 #0 for every node
        R_status[each_node] = 0 #0 for every node
    patient_0 = np.random.choice(all_nodes)  #randomly select a node to be initially infected
    S_status[patient_0] = 0 #infect patient 0 )
    I_status[patient_0] = 1
    
    return patient_0, S_status, E_status, I_status, R_status    
    
############################################################################################################################################
    
def seir_same_length_bc_unweighted_net(num_time_steps, net, S_status, E_status, I_status, R_status, beta, alpha,  types_behavior_change, percent_that_change_behavior, patient_0, num_small_outbreaks, prop_low_edu, beta_low_edu, low_edu_gamma, other_edu_gamma, num_low_to_vacc, num_other_to_vacc, total_num_to_vacc, absenteeism_dict, dict_delayed_recovery_nodes, dict_increased_susceptibility_nodes):
    """
    SEIR code where the length of the change is behavior is the same as the infectious period. Edges are restored when an indiviudal recovers. 
    """
    print types_behavior_change
    t_values = range(0, num_time_steps+1) #set t values
    num_s = [0] * (num_time_steps+1) #initiate number of indiviudals in each category list
    num_e = [0] * (num_time_steps+1)
    num_i = [0] * (num_time_steps+1)
    num_r = [0] * (num_time_steps+1)
    num_s[0] = len(net) -1 #number of starting susceptible indiviudals is population - 1
    num_e[0] = 0 #number of exposed = 0
    num_i[0] = 1 #number of infected = 1
    num_r[0] = 0 #number of recovered = 0
    
    all_nodes = nx.nodes(net)

    list_num_i = [1]
    incidence = [1]
    
    who_becomes_infected_at_each_timestep = {}
    who_caused_the_infection = {}
    who_changes_behavior_with_who_at_each_timestep = {}
    who_delayed_recovery = []
    who_increased_susceptibility = []
    
    nodes_ever_infected = [patient_0] # keep track of all of the nodes every infected
    who_becomes_infected_at_each_timestep[0]=[patient_0]
    removed_edges_when_I_full = {} # this dict keeps track of what edges are removed from each node, so they can be restored wehn behavior change is ended
    
    list_num_bc = []
    
    all_children_and_their_caregivers = {}
    num_ts = 0
    
    clustering_coefs =[]
    
    
    edu_dict= nx.get_node_attributes(net, 'edu')
    
    num_vacced = 0
    who_vacced = []
    
    
        
    if 1 in types_behavior_change:
        
        
        vacc_nodes = vacc_a_priori_general(net, edu_dict, total_num_to_vacc)
        
        for each_vacc_node in vacc_nodes:
            net.remove_node(each_vacc_node)
        num_vacced = len(vacc_nodes)
        who_vacced = vacc_nodes
        num_s = [0] * (num_time_steps+1) #initiate number of indiviudals in each category list
        num_e = [0] * (num_time_steps+1)
        num_i = [0] * (num_time_steps+1)
        num_r = [0] * (num_time_steps+1)
        num_s[0] = len(net) -1 #number of starting susceptible indiviudals is population - 1
        num_e[0] = 0 #number of exposed = 0
        num_i[0] = 1 #number of infected = 1
        num_r[0] = 0 #number of recovered = 0
        
        all_nodes = nx.nodes(net)
    
        list_num_i = [1]
        incidence = [1]
        
        who_becomes_infected_at_each_timestep = {}
        who_caused_the_infection = {}
        who_changes_behavior_with_who_at_each_timestep = {}
        who_delayed_recovery = []
        who_increased_susceptibility = []
        
        nodes_ever_infected = [patient_0] # keep track of all of the nodes every infected
            
        removed_edges_when_I_full = {} # this dict keeps track of what edges are removed from each node, so they can be restored wehn behavior change is ended
        
        list_num_bc = []
        
        all_children_and_their_caregivers = {}
        num_ts = 0
        
        clustering_coefs =[]

        edu_dict= nx.get_node_attributes(net, 'edu')
        
    if 2 in types_behavior_change:
        vacc_low_edu_nodes, vacc_other_edu_nodes = vacc_a_priori_by_ses(net, edu_dict, num_low_to_vacc, num_other_to_vacc)
        for each_vacc_node in vacc_low_edu_nodes:
            net.remove_node(each_vacc_node)
        for each_vacc_node in vacc_other_edu_nodes:
            net.remove_node(each_vacc_node)
        num_vacced = len(vacc_low_edu_nodes)+len(vacc_other_edu_nodes)
        who_vacced = vacc_low_edu_nodes + vacc_other_edu_nodes
        num_s = [0] * (num_time_steps+1) #initiate number of indiviudals in each category list
        num_e = [0] * (num_time_steps+1)
        num_i = [0] * (num_time_steps+1)
        num_r = [0] * (num_time_steps+1)
        num_s[0] = len(net) -1 #number of starting susceptible indiviudals is population - 1
        num_e[0] = 0 #number of exposed = 0
        num_i[0] = 1 #number of infected = 1
        num_r[0] = 0 #number of recovered = 0
        
        all_nodes = nx.nodes(net)
    
        list_num_i = [1]
        incidence = [1]
        
        who_becomes_infected_at_each_timestep = {}
        who_caused_the_infection = {}
        who_changes_behavior_with_who_at_each_timestep = {}
        who_delayed_recovery = []
        who_increased_susceptibility = []
        
        nodes_ever_infected = [patient_0] # keep track of all of the nodes every infected
            
        removed_edges_when_I_full = {} # this dict keeps track of what edges are removed from each node, so they can be restored wehn behavior change is ended
        
        list_num_bc = []
        
        all_children_and_their_caregivers = {}
        num_ts = 0
        
        clustering_coefs =[]

        edu_dict= nx.get_node_attributes(net, 'edu')
        
        
  
  
    for t in t_values[1:]: #for each time step
        num_ts = num_ts+1
        ##### S -> E
        S_nodes = [node for node in nx.nodes(net) if S_status[node]==1] #go through network at this timepoint, and identify all susceptible nodes
        
        s_to_e_nodes = []
        for each_s_node in S_nodes: 
            #if each_s_node in nodes_that_have_been_vacc:
                #print 'problem', each_s_node
            if 7 in types_behavior_change:
                #print 'happening - 7'
                all_neigh_s = list(net.neighbors(each_s_node)) #get the neighbors of each S node
                neigh_I_status_dict = {node:I_status[node] for node in all_neigh_s}
                neigh_I_status_list = [I_status[node] for node in all_neigh_s]
                num_I_neigh = sum(neigh_I_status_list)
                if num_I_neigh>0:
                
                    if dict_increased_susceptibility_nodes[each_s_node] == 1:
                     
                        to_put_in_exponential = float(-beta_low_edu)*float(num_I_neigh) 
                        prob_of_exposure = ((1.0)- math.exp(to_put_in_exponential)) # calculate the probability of becoming infected using the number of infected neighbors      
                        who_increased_susceptibility.append(each_s_node)
                    if dict_increased_susceptibility_nodes[each_s_node] != 1:
                       
                        to_put_in_exponential = float(-beta)*float(num_I_neigh) 
                        prob_of_exposure = ((1.0)- math.exp(to_put_in_exponential)) 
                        
    
                    r_s = rnd.random() #randomly select a value between 0 and 1
                    while r_s == 0:
                        r_s = rnd.random()
                    if r_s < prob_of_exposure: #if the randomly selected value is less than the probability of infection
                        s_to_e_nodes.append(each_s_node)
                        who_could_have_caused_this_infection = []
                        for each_neighbor_node, each_neighbor_i_status in neigh_I_status_dict.iteritems():
                            if each_neighbor_i_status ==1:
                                who_could_have_caused_this_infection.append(each_neighbor_node)
                        who_caused_the_infection[each_s_node] = who_could_have_caused_this_infection
                        
                    
                        
            if 8 in types_behavior_change:
                #print 'happening-8'
                all_neigh_s = list(net.neighbors(each_s_node)) #get the neighbors of each S node
                neigh_I_status_dict = {node:I_status[node] for node in all_neigh_s}
                neigh_I_status_list = [I_status[node] for node in all_neigh_s]
                num_I_neigh = sum(neigh_I_status_list)
                if num_I_neigh>0:
                
                    if dict_increased_susceptibility_nodes[each_s_node] == 1:
                     
                        to_put_in_exponential = float(-beta_low_edu)*float(num_I_neigh) 
                        prob_of_exposure = ((1.0)- math.exp(to_put_in_exponential)) # calculate the probability of becoming infected using the number of infected neighbors      
                            
                        who_increased_susceptibility.append(each_s_node)
                    if dict_increased_susceptibility_nodes[each_s_node] != 1:
                       
                        to_put_in_exponential = float(-beta)*float(num_I_neigh) 
                        prob_of_exposure = ((1.0)- math.exp(to_put_in_exponential)) 
                        
    
                    r_s = rnd.random() #randomly select a value between 0 and 1
                    while r_s == 0:
                        r_s = rnd.random()
                    if r_s < prob_of_exposure: #if the randomly selected value is less than the probability of infection
                        s_to_e_nodes.append(each_s_node)
                        who_could_have_caused_this_infection = []
                        for each_neighbor_node, each_neighbor_i_status in neigh_I_status_dict.iteritems():
                            if each_neighbor_i_status ==1:
                                who_could_have_caused_this_infection.append(each_neighbor_node)
                        who_caused_the_infection[each_s_node] = who_could_have_caused_this_infection   
               
                        
                
            if not 7 in types_behavior_change:
                if not 8 in types_behavior_change:
                    #print 'happening-other sus'
                #print(each_s_node)
                    all_neigh_s = list(net.neighbors(each_s_node)) #get the neighbors of each S node
                    
                    neigh_I_status_dict = {node:I_status[node] for node in all_neigh_s}
                    neigh_I_status_list = [I_status[node] for node in all_neigh_s]
                    
                    num_I_neigh = sum(neigh_I_status_list)
                    to_put_in_exponential = float(-beta)*float(num_I_neigh) 
                    prob_of_exposure = ((1.0)- math.exp(to_put_in_exponential)) # calculate the probability of becoming infected using the number of infected neighbors      
                    r_s = rnd.random() #randomly select a value between 0 and 1
                    while r_s == 0:
                        r_s = rnd.random()
                    if r_s < prob_of_exposure: #if the randomly selected value is less than the probability of infection
                        #print(each_s_node)
                        s_to_e_nodes.append(each_s_node)
                        who_could_have_caused_this_infection = []
                        for each_neighbor_node, each_neighbor_i_status in neigh_I_status_dict.iteritems():
                            if each_neighbor_i_status ==1:
                                who_could_have_caused_this_infection.append(each_neighbor_node)
                        who_caused_the_infection[each_s_node] = who_could_have_caused_this_infection
        
       ###### E -> I
        E_nodes = [node for node in nx.nodes(net) if E_status[node]==1]#go through the network and find all exposed nodes
        
        num_e[t] = sum(E_status.values())
        rand_nums = [rnd.random() for each in range(0, len(E_nodes))]#go through each exposed node, randomly transition to infected at level alpha
        e_to_i_nodes = [each_e_node for each_e_node, rand_num in zip(E_nodes, rand_nums) if rand_num< alpha] 
    
   
        incidence.append(len(e_to_i_nodes))
        who_becomes_infected_at_each_timestep[t] = e_to_i_nodes
        
        # I -> R
        
        I_nodes = [node for node in nx.nodes(net) if I_status[node]] #go thorugh network and collect all infected nodes

              
        if 3 in types_behavior_change:
            net, removed_edges_when_I, who_changes_behavior_and_with_who_at_this_timestep = absenteeism(net, e_to_i_nodes, percent_that_change_behavior, absenteeism_dict)
            num_who_change_behavior = len(who_changes_behavior_and_with_who_at_this_timestep.keys())
            list_num_bc.append(num_who_change_behavior)
            who_changes_behavior_with_who_at_each_timestep[t] = who_changes_behavior_and_with_who_at_this_timestep
            for node, edges in removed_edges_when_I.iteritems(): # keep track of what edges are removed from which node so the edges can be restored upon recovery
                removed_edges_when_I_full[node] = edges
            
        if 4 in types_behavior_change:
            net, removed_edges_when_I, who_changes_behavior_and_with_who_at_this_timestep = absenteeism(net, e_to_i_nodes, percent_that_change_behavior, absenteeism_dict)
            num_who_change_behavior = len(who_changes_behavior_and_with_who_at_this_timestep.keys())
            list_num_bc.append(num_who_change_behavior)
            who_changes_behavior_with_who_at_each_timestep[t] = who_changes_behavior_and_with_who_at_this_timestep
            for node, edges in removed_edges_when_I.iteritems(): # keep track of what edges are removed from which node so the edges can be restored upon recovery
                removed_edges_when_I_full[node] = edges        
      
        


        num_i[t] = sum(I_status.values()) #all the infected nodes in the network at t is num_i[t]
        list_num_i.append(num_i[t]) 
        
              
        if 5 in types_behavior_change:
            #print 'happening-5'
            
            delayed_recovery_I_nodes = [each_node for each_node in I_nodes if dict_delayed_recovery_nodes[each_node] == 1]
            other_I_nodes = [each_node for each_node in I_nodes if dict_delayed_recovery_nodes[each_node] == 0]
            
            for each in delayed_recovery_I_nodes:
                who_delayed_recovery.append(each)
            
            
            delayed_r_rand_nums = [rnd.random() for each in range(0, len(delayed_recovery_I_nodes))]
            other_rand_nums = [rnd.random() for each in range(0, len(other_I_nodes))]
           
            delayed_r_i_to_r_nodes = [each_i_node for each_i_node, rand_num in zip(delayed_recovery_I_nodes, delayed_r_rand_nums) if rand_num< low_edu_gamma]
            other_i_to_r_nodes = [each_i_node for each_i_node, rand_num in zip(other_I_nodes, other_rand_nums) if rand_num< other_edu_gamma]
        
        
        
        if 6 in types_behavior_change:
            #print 'happening-6'
            
            delayed_recovery_I_nodes = [each_node for each_node in I_nodes if dict_delayed_recovery_nodes[each_node] == 1]
            other_I_nodes = [each_node for each_node in I_nodes if dict_delayed_recovery_nodes[each_node] == 0]
            
            for each in delayed_recovery_I_nodes:
                who_delayed_recovery.append(each)
                
            delayed_r_rand_nums = [rnd.random() for each in range(0, len(delayed_recovery_I_nodes))]
            other_rand_nums = [rnd.random() for each in range(0, len(other_I_nodes))]
           
            delayed_r_i_to_r_nodes = [each_i_node for each_i_node, rand_num in zip(delayed_recovery_I_nodes, delayed_r_rand_nums) if rand_num< low_edu_gamma]
            other_i_to_r_nodes = [each_i_node for each_i_node, rand_num in zip(other_I_nodes, other_rand_nums) if rand_num< other_edu_gamma]

          
        if not 5 in types_behavior_change:
            if not 6 in types_behavior_change:
                #print 'happening-other recovery'
                rand_nums = [rnd.random() for each in range(0, len(I_nodes))] #go through each i node and randomly choose if it will recover at level gamma 
                i_to_r_nodes = [each_i_node for each_i_node, rand_num in zip(I_nodes, rand_nums) if rand_num< other_edu_gamma]
                #print len(i_to_r_nodes)
        

                
                                
        num_r[t] =sum(R_status.values())
        
        #print 's to e', s_to_e_nodes
        #print 'e to i', e_to_i_nodes
        for each_node in s_to_e_nodes:
            S_status[each_node] = 0 # suscpeitble nodes become infected
            E_status[each_node] = 1
            
        num_s[t] = sum(S_status.values())  
        
      
        num_exposed = len(s_to_e_nodes)
        #num_removed_from_s_nodes_at_ts = num_vacc_nodes+num_exposed
        #print num_removed_from_s_nodes_at_ts
        S_nodes_after = [node for node in nx.nodes(net) if S_status[node]==1] #go through network at this timepoint, and identify all susceptible nodes
        #print 'num s nodes after ts', len(S_nodes_after)
        
        for each_node in e_to_i_nodes: # exposed nodes become infected
            E_status[each_node] = 0
            I_status[each_node] = 1
            nodes_ever_infected.append(each_node)
            
        num_edges_restored = 0
        
        if 5 in types_behavior_change:
            for each_node in delayed_r_i_to_r_nodes: # infected nodes recover
                I_status[each_node] = 0
                R_status[each_node] = 1
                if each_node in removed_edges_when_I_full.keys():
                    removed_edges = removed_edges_when_I_full[each_node] # access the full dict of which edges were removed from which node
                    for each_removed_edge in removed_edges:
                        net.add_edge(each_node, each_removed_edge) #restore the edge between the focal node and all neighbors when the focal individual recovers                     
                        num_edges_restored = num_edges_restored+1

                        
            for each_node in other_i_to_r_nodes: # infected nodes recover
                I_status[each_node] = 0
                R_status[each_node] = 1
                if each_node in removed_edges_when_I_full.keys():
                    removed_edges = removed_edges_when_I_full[each_node] # access the full dict of which edges were removed from which node
                    for each_removed_edge in removed_edges:
                        net.add_edge(each_node, each_removed_edge) #restore the edge between the focal node and all neighbors when the focal individual recovers                     
                        num_edges_restored = num_edges_restored+1

        if 6 in types_behavior_change:
            for each_node in delayed_r_i_to_r_nodes: # infected nodes recover
                I_status[each_node] = 0
                R_status[each_node] = 1
                if each_node in removed_edges_when_I_full.keys():
                    removed_edges = removed_edges_when_I_full[each_node] # access the full dict of which edges were removed from which node
                    for each_removed_edge in removed_edges:
                        net.add_edge(each_node, each_removed_edge) #restore the edge between the focal node and all neighbors when the focal individual recovers                     
                        num_edges_restored = num_edges_restored+1

                        
            for each_node in other_i_to_r_nodes: # infected nodes recover
                I_status[each_node] = 0
                R_status[each_node] = 1
                if each_node in removed_edges_when_I_full.keys():
                    removed_edges = removed_edges_when_I_full[each_node] # access the full dict of which edges were removed from which node
                    for each_removed_edge in removed_edges:
                        net.add_edge(each_node, each_removed_edge) #restore the edge between the focal node and all neighbors when the focal individual recovers                     
                        num_edges_restored = num_edges_restored+1
                        
            
        if not 5 in types_behavior_change:
            if not 6 in types_behavior_change:
                for each_node in i_to_r_nodes: # infected nodes recover
                    I_status[each_node] = 0
                    R_status[each_node] = 1
                    if each_node in removed_edges_when_I_full.keys():
                        removed_edges = removed_edges_when_I_full[each_node] # access the full dict of which edges were removed from which node
                        for each_removed_edge in removed_edges:
                            net.add_edge(each_node, each_removed_edge) #restore the edge between the focal node and all neighbors when the focal individual recovers                     
                            num_edges_restored = num_edges_restored+1
              

        
        if sum(E_nodes)+sum(I_nodes) == 0:
            break
    
    num_nodes = nx.number_of_nodes(net)
    list_percent_i = [float(each_num_i)/float(num_nodes) for each_num_i in num_i]

    return t_values, num_s, num_e, num_i, num_r, list_num_i, list_percent_i, num_small_outbreaks, who_becomes_infected_at_each_timestep, who_caused_the_infection, who_changes_behavior_with_who_at_each_timestep, list_num_bc, num_ts, clustering_coefs, incidence, num_vacced, who_vacced, who_delayed_recovery, who_increased_susceptibility 


    
####################### BEHAVIOR CHANGE FUNCTIONS  ####################################################################################################################
def absenteeism(net, e_to_i_nodes, percent_that_change_behavior, absenteeism_dict):
    """


    """ 
    
    removed_edges_when_I = {} 
    who_changes_behavior_and_with_who_at_this_timestep={}
    num_edges_removed = 0
    edu_attributes = nx.get_node_attributes(net, 'edu') 
    school_work_attributes = nx.get_node_attributes(net, "school_work") # save work attributes as work_attributes
    #the percent that are able to change their behavior (take paid sick days) are different depending on educaiton level (based on piper et al. paper). Those that change behavior still reduce their contactby the same amount (couldn't find any other paper on this)
    #print nodes_that_have_been_vacc 
    #print edu_attributes
    #print school_work_attributes
    #print edu_attributes
    for each_i_node in e_to_i_nodes: # go through each infected node
        
        does_node_have_psd = absenteeism_dict[each_i_node]
        if does_node_have_psd == 1:
       
                     
            percent = np.random.normal(0.91, .0167, 1)# randomly choose a percent from a poisson destribution with mean of 91 
            percent = percent*100
            if percent>100: # If the chosen percent is over 100%, jsut make it 100%
                percent = 100
            
            #if school_work_attributes[each_i_node] == 1: # if the I node is a work node
            neighbors = list(net.neighbors(each_i_node)) # get the neighbors of that I node
            num_neighbors = len(neighbors)
           
            #work_neighbors = [each_neighbor for each_neighbor in neighbors if school_work_attributes[each_neighbor] == 1]
            #num_work_neighbors = len(work_neighbors) # find the number of work neighbors
    
            if num_neighbors>0: # if there is at least 1 work neighbor
                percent_reduced = percent *.01 # multiply the chosen percent by 0.01 
                num_edges_to_remove = int(num_neighbors*percent_reduced) # multiply the percent times the number of work neighbors and make an int- this is the number of work edges that must be removed            
                
                edges_to_remove = rnd.sample(neighbors, num_edges_to_remove) # randomly choose the required number of work neighbors to lose the edge between them            
                
                for each_neighbor in edges_to_remove: # go through each neighbor to be removed
                    net.remove_edge(each_i_node, each_neighbor) # remove the edge between the I node and the selected neighbor    
                    num_edges_removed =num_edges_removed+1
                removed_edges_when_I[each_i_node] = edges_to_remove 
                who_changes_behavior_and_with_who_at_this_timestep[each_i_node]=edges_to_remove 
                
        
      
    return net, removed_edges_when_I, who_changes_behavior_and_with_who_at_this_timestep

    


    
############################################################################################################################################
def find_epi_size(num_r, net, num_ts):
    """
    Using num recovered, find epi size
    """
    num_in_net = nx.number_of_nodes(net)
    epi_size =  float(num_r[num_ts])/float(num_in_net)
    return epi_size 
    
############################################################################################################################################
def find_epi_peak(t_values, list_percent_i, list_num_i, net):
    """
    Using num infected, find the size and timing of the epidemic peak
    """

    epi_peak = max(list_num_i)
    percent_epi_peak = max(list_percent_i)
    
    #find epidemic start date- first date that there are more than 5% of the
    start_amount =  .1*(sum(list_num_i)/len(t_values))
    start_day = next(each[0] for each in enumerate(list_num_i) if each[1] > start_amount)
  
    time_step_of_epi_peak = list_num_i.index(epi_peak)
    time_step_of_real_epi_peak = time_step_of_epi_peak-start_day
    
    
    return epi_peak, percent_epi_peak, time_step_of_real_epi_peak
    
############################################################################################################################################    
def create_dir(filename):
    if not os.path.exists(os.path.dirname(filename)):
        try:
            os.makedirs(os.path.dirname(filename))
        except OSError as exc: 
            if exc.errno != errno.EEXIST:
                raise

        
    
############################################################################################################################################
def run_more_times_unweighted(each_type_of_net, net_number, num_time_steps, beta, alpha, num_sims, types_behavior_change, percent_that_change_behavior, prop_low_edu, beta_low_edu, low_edu_gamma, other_edu_gamma, low_edu_vacc_rate, high_edu_vacc_rate, low_ses_percent_that_change_behavior):
    """
    run multiple number of times 
    """
    for each_sim in range(0, num_sims):
        print 'type of net', each_type_of_net
        print "prop low edu", prop_low_edu
        print "type bc", types_behavior_change
        print "net number", net_number
        print "sim", each_sim
        
        
        net = create_unweighted_sim_net(net_number, prop_low_edu)
        num_nodes = nx.number_of_nodes(net)
        edu_dict = nx.get_node_attributes(net, 'edu')
        low_edu_nodes = [each_node for each_node, each_edu in edu_dict.iteritems() if each_edu==1]
        other_edu_nodes = [each_node for each_node, each_edu in edu_dict.iteritems() if each_edu!=1]
        
        num_low_edu_nodes = len(low_edu_nodes)
        
        
        num_low_to_vacc = None
        num_other_to_vacc = None
        total_num_to_vacc = None
        if 1 in types_behavior_change:
            num_low_to_vacc = int(low_edu_vacc_rate*len(low_edu_nodes))
            
            num_other_to_vacc = int(other_edu_vacc_rate*len(other_edu_nodes))
      
            total_num_to_vacc = num_low_to_vacc+num_other_to_vacc
            print total_num_to_vacc
        if 2 in types_behavior_change:
            num_low_to_vacc = int(low_edu_vacc_rate*len(low_edu_nodes))
            
            num_other_to_vacc = int(other_edu_vacc_rate*len(other_edu_nodes))
      
            total_num_to_vacc = num_low_to_vacc+num_other_to_vacc
        
            
        
        
        
        num_low_ses_with_psd = int(low_ses_percent_that_change_behavior*len(low_edu_nodes))
        num_other_ses_with_psd = int(percent_that_change_behavior*len(other_edu_nodes))
        total_with_psd = num_low_ses_with_psd +num_other_ses_with_psd
        
        
        if each_type_of_net == 'regular':
            net = create_regular_net(net)
            
        # randomly distribute paid sick days- randomly select percent_that_change_behavior of all nodes and give them 
        # a 1 in a dictionary
        absenteeism_dict = {each_node:0 for each_node in nx.nodes(net)}
        if 3 in types_behavior_change:
            general_psd_nodes = list(np.random.choice(nx.nodes(net), total_with_psd, replace = False))
            
            for each_node in general_psd_nodes:
                absenteeism_dict[each_node]=1
            
        # ses distributed psd
        if 4 in types_behavior_change:
            low_ses_psd_nodes =  list(np.random.choice(low_edu_nodes, num_low_ses_with_psd, replace = False))
            other_ses_psd_nodes =  list(np.random.choice(other_edu_nodes, num_other_ses_with_psd, replace = False))
            for each_node in low_ses_psd_nodes:
                absenteeism_dict[each_node]=1
            for each_node in other_ses_psd_nodes:
                absenteeism_dict[each_node]=1
        
        
            
        
        # generally distributed delayed recovery
        # randomly choose the nodes to have delayed recovery- choose the number of low edu nodes
        dict_delayed_recovery_nodes = {each_node:0 for each_node in nx.nodes(net)}
        if 5 in types_behavior_change:
            delayed_recovery_nodes = list(np.random.choice(nx.nodes(net), num_low_edu_nodes, replace = False))
            
            for each_node in delayed_recovery_nodes:
                dict_delayed_recovery_nodes[each_node]=1
        
        if 6 in types_behavior_change:
            for each_node in low_edu_nodes:
                dict_delayed_recovery_nodes[each_node]=1
        
        
            
        dict_increased_susceptibility_nodes = {each_node:0 for each_node in nx.nodes(net)}
        if 7 in types_behavior_change:
            increased_sus_nodes = list(np.random.choice(nx.nodes(net), num_low_edu_nodes, replace = False))
            
            for each_node in increased_sus_nodes:
                dict_increased_susceptibility_nodes[each_node]=1
        
        if 8 in types_behavior_change:
            for each_node in low_edu_nodes:
                dict_increased_susceptibility_nodes[each_node]=1
                    
            

        num_small_outbreaks = 0
        original_num_edges = nx.number_of_edges(net)
        patient_0, S_status, E_status, I_status, R_status  = choose_patient_0_and_set_attributes(net)
        
        t_values, num_s, num_e, num_i, num_r, list_num_i, list_percent_i, num_small_outbreaks, who_becomes_infected_at_each_timestep, who_caused_the_infection, who_changes_behavior_with_who_at_each_timestep, list_num_bc, num_ts, clustering_coefs, incidence, num_vacced, who_vacced, who_delayed_recovery, who_increased_susceptibility  = seir_same_length_bc_unweighted_net(num_time_steps, net, S_status, E_status, I_status, R_status, beta, alpha, types_behavior_change, percent_that_change_behavior, patient_0, num_small_outbreaks, prop_low_edu, beta_low_edu, low_edu_gamma, other_edu_gamma, num_low_to_vacc, num_other_to_vacc, total_num_to_vacc, absenteeism_dict, dict_delayed_recovery_nodes, dict_increased_susceptibility_nodes)

        
        vacc_df = pd.DataFrame({'who_vacced':list(np.unique(who_vacced))})
        delayed_recovery_df = pd.DataFrame({'who_delayed':list(np.unique(who_delayed_recovery))})
        increased_sus_df = pd.DataFrame({'who_increased':list(np.unique(who_increased_susceptibility))})
        
        #print type(who_becomes_infected_at_each_timestep)
        
        num_infected_at_each_ts = {}
        for each_ts, each_list_i in who_becomes_infected_at_each_timestep.items():
            if not each_ts ==0:
                num_infected_at_each_ts[each_ts]=len(each_list_i)
        
        
        list_infected_nodes = []
        for each_ts, each_list_infected in who_becomes_infected_at_each_timestep.iteritems():
            for each_node in each_list_infected:
                    list_infected_nodes.append(each_node)
                    
        num_i = len(list_infected_nodes)
        if each_type_of_net == 'regular':
            if num_i > (0.05*num_nodes):
                prop_i = num_i/float(num_nodes)
                
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/epi_size/net"+str(net_number)+"_epi_sizes.txt"
                create_dir(filename)
                f= open(filename,"a+")
                f.write(str(each_sim)+" %s\n" %prop_i)
                
        if each_type_of_net == 'ergm':
            list_infected_nodes = []
            list_low_edu_i = []
            list_other_edu_i = []
            for each_ts, each_list_infected in who_becomes_infected_at_each_timestep.iteritems():
                
                low_edu_i = [each_i_node for each_i_node in each_list_infected if edu_dict[each_i_node]==1]
                other_edu_i = [each_i_node for each_i_node in each_list_infected if edu_dict[each_i_node]!=1]
                for each_node in each_list_infected:
                    list_infected_nodes.append(each_node)
                for each_low_node in low_edu_i:
                    list_low_edu_i.append(each_low_node)
                for each_other_node in other_edu_i:
                    list_other_edu_i.append(each_other_node)
            num_i = len(list_infected_nodes)
            
            if num_i > (0.05*num_nodes):
                prop_i = num_i/float(num_nodes)
                prop_low_i = len(list_low_edu_i)/float(num_i)
                prop_other_i = len(list_other_edu_i)/float(num_i)
#                
#                
#                
#                
#                
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/net_"+str(net_number)+"/epi_sizes.txt"
                create_dir(filename)
                f= open(filename,"a+")
                f.write(str(each_sim)+" %s\n" %prop_i)
                
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/net_"+str(net_number)+"/prop_low_i.txt"
                create_dir(filename)
                f= open(filename,"a+")
                f.write(str(each_sim)+" %s\n" %prop_low_i)
                
                
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/net_"+str(net_number)+"/prop_other_i.txt"
                create_dir(filename)
                f= open(filename,"a+")
                f.write(str(each_sim)+" %s\n" %prop_other_i)
                
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/net_"+str(net_number)+"/list_infected_low_edu/sim_"+str(each_sim)+"_low_edu_i.txt"
                create_dir(filename)
                f= open(filename,"w+") 
                for each_low_edu_i in list_low_edu_i:
                    f.write(str(each_low_edu_i)+"\n")
                    
                filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/final_mechanism_epi_sims_1_8/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/net_"+str(net_number)+"/list_infected_other_edu/sim_"+str(each_sim)+"_other_edu_i.txt"
                create_dir(filename)
                f= open(filename,"w+") 
                for each_other_edu_i in list_other_edu_i:
                    f.write(str(each_other_edu_i)+"\n")                        
                    
                
        
        
       
        

            
#        epi_size = find_epi_size(num_r, net, num_ts)
#        
#        
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/epi_size/net"+str(net_number)+"_epi_sizes.txt"
#        create_dir(filename)
#        f= open(filename,"a+")
#        f.write(str(each_sim)+" %s\n" %epi_size)


#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/incidence_identities_ts/net_"+str(net_number)+"/sim_"+str(each_sim)+".txt"
#        create_dir(filename)
#        f= open(filename,"w+") 
#        for t, list_infected_nodes in who_becomes_infected_at_each_timestep.iteritems():
#            f.write(str(t)+":"+str(list_infected_nodes)+"\n")
#           
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/behavior_change_identities_ts/net_"+str(net_number)+"/sim_"+str(each_sim)+".txt"
#        create_dir(filename)
#        f= open(filename,"w+")
#        for t, dict_bc_nodes_and_neighbors in who_changes_behavior_with_who_at_each_timestep.iteritems():
#            f.write(str(t)+":"+str(dict_bc_nodes_and_neighbors)+"\n")  
#
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_infected_who/net_"+str(net_number)+"/sim_"+str(each_sim)+".txt"
#        create_dir(filename)
#        f= open(filename,"w+")
#        for infected_node, infected_neighbors in who_caused_the_infection.iteritems():
#            f.write(str(infected_node)+":"+str(infected_neighbors)+"\n")
#            
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_vacced/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv"
#        create_dir(filename)
#        vacc_df.to_csv("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_vacced/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv")   
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_delayed_recovery/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv"
#        create_dir(filename)
#        delayed_recovery_df.to_csv("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_delayed_recovery/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv")   
#        filename = "/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_increased_susceptibility/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv"
#        create_dir(filename)
#        increased_sus_df.to_csv("/Users/admin/Dropbox (Bansal Lab)/Casey_Bansal_Lab/ergm_behavior_change_project/update_corrected_all_mechanism_epi_sims/prop_low_edu_"+str(prop_low_edu)+"/network_type_"+str(each_type_of_net)+"/type_bc_"+str(types_behavior_change)+"/who_increased_susceptibility/net_"+str(net_number)+"/sim_"+str(each_sim)+".csv")   
##                    
#        
# 
    return 
    

    

############################################################################################################################################

if __name__ == '__main__':
    #### I edit the values in this section to control which experiment is run! things that need to be set- type of behavior change, whether the experiment is done on the unweighted or weighted network (reduced duration experiment only), whether or not the length of behavior change is the same or different from the infectious period
    
    
    types_of_net = ['ergm']
    
    num_nets = 5
    
    
    ####### vaccination
    #overall_vacc_rate = 0.41
    low_edu_vacc_rate = 0.33
    other_edu_vacc_rate = 0.41
    # general vaccination = 1
    # ses vaccination = 2
    
    
    ######## absenteeism
    percent_that_change_behavior = 0.741 # assign probability of becoming absent prior to simulation. 
    low_ses_percent_that_change_behavior = 0.28
    #Then when that individual becomes infected, if they are a yes for behavior change, they change behavior
    # Distributing access to paid sick days- consistent numbers of access to PSD across experiemnts
    # The resulting number that are absent may be different, but that is a function of the contact network and epi sim
    # general sabsenteeism = 3
    # ses absenteeism = 4
    
    
    
    
    # hc utilization
    # general delayed recovery: 5
    # ses_delayed recovery: 6
    other_edu_gamma = 0.21
    low_edu_gamma = 0.14
    
    
    # susceptibility
    # general susceptibility: 7
    # ses susceptibility:8
    beta_low_edu = 0.08
    beta = 0.04
  

    
    alpha = 0.5

   
    num_time_steps = 200
    num_sims = 200
    
    
    
    #types_behavior_change = [[3], [5], [6], [7], [8]]#[3, 6], [3, 7], [3, 8], [6, 7], [6, 8], [7, 8], [3, 6, 7], [3, 6, 8], [3, 7, 8], [6, 7, 8]
    #types_behavior_change = [[3, 6], [3, 7], [3, 8], [6, 7], [6, 8], [7, 8], [3, 6, 7], [3, 6, 8], [3, 7, 8], [6, 7, 8]]
  #  types_behavior_change = [[0], [1], [3], [5], [7], [1, 3, 5, 7]]
    types_behavior_change = [[0], [1], [3], [5], [7], [1, 3, 5, 7], [2], [4], [6], [8], [2, 4, 6, 8]]  
    #types_behavior_change = [ [6],  [8],[2, 4, 6, 8]]
    
    props_low_edu = [60]
#0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 
# using files up to 80 percent. need to do 90 percent later when the ergms are done
    for each_type_of_net in types_of_net: 
        for prop_low_edu in props_low_edu:
            for each_types_bc in types_behavior_change:
                for net_number in range(1, num_nets+1):
                        run_more_times_unweighted(each_type_of_net, net_number, num_time_steps, beta, alpha,num_sims, each_types_bc, percent_that_change_behavior, prop_low_edu, beta_low_edu, low_edu_gamma, other_edu_gamma, low_edu_vacc_rate, other_edu_vacc_rate, low_ses_percent_that_change_behavior)
                
         
