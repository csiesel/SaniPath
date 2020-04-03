# ****************************************************************************************
# SaniPath Lusaka Deployment
# ****************************************************************************************
# 

## Additonal Pathways ----
# sample type = 6.. floodwater 
# 2: types
# col_f_type:
        # 1 regular (15)
        # 2 near latrine (15)

# sample type = 7.. latrine swabs
# separate pathways
# col_l_toilet_pubshare
        # 1 public (10)
        # 2 shared (compound) (10)

# sample type = 11.. other drinking water
# # separate pathways
# col_odw_type:
        # 1 bore hole (15)
        # 2 shallow well (15)

# extra variables/ pathways
# ***************************************************************
# | sample | lab | h | c | s | pathway/ variable                |
# |:------:|:---:|:-:|:-:|:-:|:--------------------------------:|
# |    x   |  x  | x | x | x | flood water (normal)             |       
# |    x   |  x  |   |   |   | flood water (near latrine)       |
# |    x   |  x  | x | x | x | latrine (public)                 |
# |    x   |  x  | x | x | x | latrine (shared)                 |
# |    x   |  x  | x | x | x | odw - borehole                   |
# |    x   |  x  | x | x | x | odw - shallow well               |
# |        |     | x | x | x | bw source                        |




# extra variables ####

#### HH Survey ####

# other drinking water
h_dwbore_a
h_dwbore_a_other
h_dwbore_c
h_dwbore_c_other
h_dwwell_a
h_dwwell_a_other
h_dwwell_c
h_dwwell_c_other


h_bw_source
        # 1	Municipal Water
        # 2	Borehole Water
        # 3	Shallow Well Water
        # 4	Other
        # 5	Do not know
        # 6	Not applicable / unable to collect information
h_bw_source_other
h_bw_source_na


# shared latrines 
h_ls
h_ls_a
h_ls_a_other
h_ls_c
h_ls_c_other



#### School Survey ####
s_dwbore_c
s_dwbore_c_note
s_dwbore_c_note_2
s_dwbore_c_3
s_dwbore_c_2
s_dwbore_c_1
s_dwbore_c_0
s_dwbore_c_na
s_dwbore_a
s_dwbore_a_note
s_dwbore_a_note_2
s_dwbore_a_3
s_dwbore_a_2
s_dwbore_a_1
s_dwbore_a_0
s_dwbore_a_na
s_dwwell_c
s_dwwell_c_note
s_dwwell_c_note_2
s_dwwell_c_3
s_dwwell_c_2
s_dwwell_c_1
s_dwwell_c_0
s_dwwell_c_na
s_dwwell_a
s_dwwell_a_note
s_dwwell_a_note_2
s_dwwell_a_3
s_dwwell_a_2
s_dwwell_a_1
s_dwwell_a_0
s_dwwell_a_na

# bathing water source
s_bw_source
# note
s_bw_source_3
# municipal water
s_bw_source_2
# borehole water
s_bw_source_1
# shallow well water
s_bw_source_0
# other


# shared latrine
s_sl_c
s_sl_c_note
s_sl_c_note_2
s_sl_c_3
s_sl_c_2
s_sl_c_1
s_sl_c_0
s_sl_c_na
s_sl_a
s_sl_a_note
s_sl_a_note_2
s_sl_a_3
s_sl_a_2
s_sl_a_1
s_sl_a_0
s_sl_a_na



#### Community Survey ####

# drinking water
c_dwbore_a
c_dwbore_a_note
c_dwbore_a_note_2
c_dwbore_a_3
c_dwbore_a_2
c_dwbore_a_1
c_dwbore_a_0
c_dwbore_a_na
c_dwwell_a
c_dwwell_a_note
c_dwwell_a_note_2
c_dwwell_a_3
c_dwwell_a_2
c_dwwell_a_1
c_dwwell_a_0
c_dwwell_a_na

c_dwbore_c_note
c_dwbore_c_note_2
c_dwbore_c_3
c_dwbore_c_2
c_dwbore_c_1
c_dwbore_c_0
c_dwbore_c_na
c_dwwell_c
c_dwwell_c_note
c_dwwell_c_note_2
c_dwwell_c_3
c_dwwell_c_2
c_dwwell_c_1
c_dwwell_c_0
c_dwwell_c_na


# bathing water
c_bw_source
c_bw_source_3
c_bw_source_2
c_bw_source_1
c_bw_source_0

# latrine
c_sl_a
c_sl_a_note
c_sl_a_note_2
c_sl_a_3
c_sl_a_2
c_sl_a_1
c_sl_a_0
c_sl_a_na
c_sl_c
c_sl_c_note
c_sl_c_note_2
c_sl_c_3
c_sl_c_2
c_sl_c_1
c_sl_c_0
c_sl_c_na

