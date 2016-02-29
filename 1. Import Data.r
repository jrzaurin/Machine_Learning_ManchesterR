library(plyr)
library(dplyr)
setwd("//cht-gs1/clinicaloutcomes$/Tom/R/Machine_Learning_ManchesterR")
source('functions.r')
arrhythmia = read.csv('arrhythmia.csv')
dim(arrhythmia)
ldply(arrhythmia, class)
# Make folds vector to specify holdout set

set.seed(120)    
folds = sample(1:10, nrow(arrhythmia ), replace = TRUE)
response = arrhythmia$abnormal



fmla <- abnormal ~  sex + di_width_ragged_r_wave + di_width_diphasic_derivation_of_r_wave + 
    di_width_ragged_p_wave + di_width_diphasic_derivation_of_p_wave + 
    di_width_ragged_t_wave + di_width_diphasic_derivation_of_t_wave + 
    dii_width_ragged_r_wave + dii_width_diphasic_derivation_of_r_wave + 
    dii_width_ragged_p_wave + dii_width_diphasic_derivation_of_p_wave + 
    dii_width_ragged_t_wave + dii_width_diphasic_derivation_of_t_wave + 
    diii_width_ragged_r_wave + diii_width_diphasic_derivation_of_r_wave + 
    diii_width_ragged_p_wave + diii_width_diphasic_derivation_of_p_wave + 
    diii_width_ragged_t_wave + diii_width_diphasic_derivation_of_t_wave + 
    avr_width_sp_wave + avr_width_ragged_r_wave + avr_width_diphasic_derivation_of_r_wave + 
    avr_width_ragged_p_wave + avr_width_diphasic_derivation_of_p_wave + 
    avr_width_ragged_t_wave + avr_width_diphasic_derivation_of_t_wave + 
    avl_width_diphasic_derivation_of_r_wave + avl_width_ragged_p_wave + 
    avl_width_diphasic_derivation_of_p_wave + avl_width_ragged_t_wave + 
     avf_width_ragged_r_wave + 
    avf_width_diphasic_derivation_of_r_wave + avf_width_diphasic_derivation_of_p_wave + 
    avf_width_ragged_t_wave + avf_width_diphasic_derivation_of_t_wave + 
    v1_width_ragged_r_wave + v1_width_diphasic_derivation_of_r_wave + 
    v1_width_ragged_p_wave + v1_width_diphasic_derivation_of_p_wave + 
    v1_width_ragged_t_wave + v1_width_diphasic_derivation_of_t_wave + 
    v2_width_ragged_r_wave + v2_width_diphasic_derivation_of_r_wave + 
    v2_width_ragged_p_wave + v2_width_diphasic_derivation_of_p_wave + 
    v2_width_ragged_t_wave + v2_width_diphasic_derivation_of_t_wave + 
    v3_width_ragged_r_wave + v3_width_diphasic_derivation_of_r_wave + 
    v3_width_ragged_p_wave + v3_width_diphasic_derivation_of_p_wave + 
    v3_width_ragged_t_wave + v3_width_diphasic_derivation_of_t_wave + 
    v4_width_ragged_r_wave + v4_width_diphasic_derivation_of_r_wave + 
    v4_width_ragged_t_wave + v4_width_diphasic_derivation_of_t_wave + 
    v5_width_diphasic_derivation_of_r_wave + v5_width_diphasic_derivation_of_p_wave + 
    v5_width_diphasic_derivation_of_t_wave + v6_width_ragged_r_wave + 
    v6_width_diphasic_derivation_of_r_wave + v6_width_ragged_p_wave + 
    v6_width_diphasic_derivation_of_t_wave + avr_amp_sp_wave + 
    age + height + weight + qrs_duration + p_r_interval + q_t_interval + 
    t_interval + p_interval + qrs + t + p + qrst + heart_rate + 
    di_width_q_wave + di_width_r_wave + di_width_s_wave + di_width_rp_wave + 
    di_width_number_of_intrinsic_deflections + dii_width_q_wave + 
    dii_width_r_wave + dii_width_s_wave + dii_width_rp_wave + 
    dii_width_sp_wave + dii_width_number_of_intrinsic_deflections + 
    diii_width_q_wave + diii_width_r_wave + diii_width_s_wave + 
    diii_width_rp_wave + diii_width_sp_wave + diii_width_number_of_intrinsic_deflections + 
    avr_width_q_wave + avr_width_r_wave + avr_width_s_wave + 
    avr_width_rp_wave + avr_width_number_of_intrinsic_deflections + 
    avl_width_q_wave + avl_width_r_wave + avl_width_s_wave + 
    avl_width_rp_wave + avl_width_number_of_intrinsic_deflections + 
    avf_width_q_wave + avf_width_r_wave + avf_width_s_wave + 
    avf_width_rp_wave + avf_width_sp_wave + avf_width_number_of_intrinsic_deflections + 
    v1_width_q_wave + v1_width_r_wave + v1_width_s_wave + v1_width_rp_wave + 
    v1_width_sp_wave + v1_width_number_of_intrinsic_deflections + 
    v2_width_q_wave + v2_width_r_wave + v2_width_s_wave + v2_width_rp_wave + 
    v2_width_sp_wave + v2_width_number_of_intrinsic_deflections + 
    v3_width_q_wave + v3_width_r_wave + v3_width_s_wave + v3_width_rp_wave + 
    v3_width_sp_wave + v3_width_number_of_intrinsic_deflections + 
    v4_width_q_wave + v4_width_r_wave + v4_width_s_wave + v4_width_rp_wave + 
    v4_width_sp_wave + v4_width_number_of_intrinsic_deflections + 
    v5_width_q_wave + v5_width_r_wave + v5_width_s_wave + v5_width_rp_wave + 
    v5_width_number_of_intrinsic_deflections + v6_width_q_wave + 
    v6_width_r_wave + v6_width_s_wave + v6_width_rp_wave + v6_width_number_of_intrinsic_deflections + 
    di_amp_jj_wave + di_amp_q_wave + di_amp_r_wave + di_amp_s_wave + 
    di_amp_rp_wave + di_amp_p_wave + di_amp_t_wave + di_amp_qrsa + 
    di_amp_qrsta + dii_amp_jj_wave + dii_amp_q_wave + dii_amp_r_wave + 
    dii_amp_s_wave + dii_amp_rp_wave + dii_amp_sp_wave + dii_amp_p_wave + 
    dii_amp_t_wave + dii_amp_qrsa + dii_amp_qrsta + diii_amp_jj_wave + 
    diii_amp_q_wave + diii_amp_r_wave + diii_amp_s_wave + diii_amp_rp_wave + 
    diii_amp_sp_wave + diii_amp_p_wave + diii_amp_t_wave + diii_amp_qrsa + 
    diii_amp_qrsta + avr_amp_jj_wave + avr_amp_q_wave + avr_amp_r_wave + 
    avr_amp_s_wave + avr_amp_rp_wave + avr_amp_p_wave + avr_amp_t_wave + 
    avr_amp_qrsa + avr_amp_qrsta + avl_amp_jj_wave + avl_amp_q_wave + 
    avl_amp_r_wave + avl_amp_s_wave + avl_amp_rp_wave + avl_amp_p_wave + 
    avl_amp_t_wave + avl_amp_qrsa + avl_amp_qrsta + avf_amp_jj_wave + 
    avf_amp_q_wave + avf_amp_r_wave + avf_amp_s_wave + avf_amp_rp_wave + 
    avf_amp_sp_wave + avf_amp_p_wave + avf_amp_t_wave + avf_amp_qrsa + 
    avf_amp_qrsta + v1_amp_jj_wave + v1_amp_q_wave + v1_amp_r_wave + 
    v1_amp_s_wave + v1_amp_rp_wave + v1_amp_sp_wave + v1_amp_p_wave + 
    v1_amp_t_wave + v1_amp_qrsa + v1_amp_qrsta + v2_amp_jj_wave + 
    v2_amp_q_wave + v2_amp_r_wave + v2_amp_s_wave + v2_amp_rp_wave + 
    v2_amp_sp_wave + v2_amp_p_wave + v2_amp_t_wave + v2_amp_qrsa + 
    v2_amp_qrsta + v3_amp_jj_wave + v3_amp_q_wave + v3_amp_r_wave + 
    v3_amp_s_wave + v3_amp_rp_wave + v3_amp_sp_wave + v3_amp_p_wave + 
    v3_amp_t_wave + v3_amp_qrsa + v3_amp_qrsta + v4_amp_jj_wave + 
    v4_amp_q_wave + v4_amp_r_wave + v4_amp_s_wave + v4_amp_rp_wave + 
    v4_amp_sp_wave + v4_amp_p_wave + v4_amp_t_wave + v4_amp_qrsa + 
    v4_amp_qrsta + v5_amp_jj_wave + v5_amp_q_wave + v5_amp_r_wave + 
    v5_amp_s_wave + v5_amp_rp_wave + v5_amp_p_wave + v5_amp_t_wave + 
    v5_amp_qrsa + v5_amp_qrsta + v6_amp_jj_wave + v6_amp_q_wave + 
    v6_amp_r_wave + v6_amp_s_wave + v6_amp_rp_wave + v6_amp_p_wave + 
    v6_amp_t_wave + v6_amp_qrsa + v6_amp_qrsta

# Make sparse model matrix
mm = Matrix::sparse.model.matrix( fmla, data = arrhythmia)

