append_nested_object <- function(object_a, object_b){
  appended_object <- lapply(seq_along(object_a), function(i) append(object_a[[i]], object_b[[i]]))
  names(appended_object) = names(object_a)
  return(appended_object)  
}

bind_collated_realisations <- function(collated_object_a, collated_object_b){
  collated_object = list()
  collated_object$parcel_nums_used = append_nested_object(collated_object_a$parcel_nums_used, collated_object_b$parcel_nums_used)
  collated_object$parcel_set_outcomes = append(collated_object_a$parcel_set_outcomes, collated_object_b$parcel_set_outcomes)     
  collated_object$landscape_loss = append_nested_object(collated_object_a$landscape_loss, collated_object_b$landscape_loss)         
  collated_object$net_program_loss = append_nested_object(collated_object_a$net_program_loss, collated_object_b$net_program_loss)              
  collated_object$landscape_rel_to_counter = append(collated_object_a$landscape_rel_to_counter, collated_object_b$landscape_rel_to_counter)    
  collated_object$program_cfac_sums = append(collated_object_a$program_cfac_sums, collated_object_b$program_cfac_sums)      
  collated_object$program_cfac_sum_rel_initial = append(collated_object_a$program_cfac_sum_rel_initial, collated_object_b$program_cfac_sum_rel_initial) 
  collated_object$landscape_cfac_sum = collated_object_a$landscape_cfac_sum
  collated_object$program_NNL = append_nested_object(collated_object_a$program_NNL, collated_object_b$program_NNL)       
  collated_object$system_NNL = append_nested_object(collated_object_a$system_NNL, collated_object_b$system_NNL)        
  collated_object$cfac_trajs = collated_object_a$cfac_trajs           
  collated_object$parcel_set_NNL = append_nested_object(collated_object_a$parcel_set_NNL, collated_object_b$parcel_set_NNL)   
  collated_object$program_sums = append_nested_object(collated_object_a$program_sums, collated_object_b$program_sums)            
  collated_object$realisation_num = collated_object_a$realisation_num + collated_object_b$realisation_num            
  collated_object$collated_offsets = append_nested_object(collated_object_a$collated_offsets, collated_object_b$collated_offsets)
  collated_object$collated_devs = append_nested_object(collated_object_a$collated_devs, collated_object_b$collated_devs)        
  collated_object$collated_illegal_clearing = append_nested_object(collated_object_a$collated_illegal_clearing, collated_object_b$collated_illegal_clearing) 
  collated_object$collated_dev_credit = append_nested_object(collated_object_a$collated_dev_credit, collated_object_b$collated_dev_credit)       
  collated_object$collated_offset_bank = append_nested_object(collated_object_a$collated_offset_bank, collated_object_b$collated_offset_bank)     
  collated_object$site_offset_gains = append_nested_object(collated_object_a$site_offset_gains, collated_object_b$site_offset_gains)         
  collated_object$site_dev_losses = append_nested_object(collated_object_a$site_dev_losses, collated_object_b$site_dev_losses)         
  collated_object$offset_bank_gains = append_nested_object(collated_object_a$offset_bank_gains, collated_object_b$offset_bank_gains)          
  collated_object$net_illegal_clearing = append_nested_object(collated_object_a$net_illegal_clearing, collated_object_b$net_illegal_clearing)
  collated_object$dev_credit_losses = append_nested_object(collated_object_a$dev_credit_losses, collated_object_b$dev_credit_losses)       
  collated_object$net_offset_gains =  append(collated_object_a$net_offset_gains, collated_object_b$net_offset_gains)       
  collated_object$net_dev_losses = append(collated_object_a$net_dev_losses, collated_object_b$net_dev_losses)             
  collated_object$net_program_outcomes = append(collated_object_a$net_program_outcomes, collated_object_b$net_program_outcomes)    
  collated_object$net_landscape = append(collated_object_a$net_landscape, collated_object_b$net_landscape) 
  return(collated_object)
}