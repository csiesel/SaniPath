# functions for graph generation - for markdown file

# additions to get the data for graphs.



# function to graph cumulative frequency
# 1. standard questions
# 2. y/n questions


# ************************************************************************************************************
# standard questions
# ************************************************************************************************************

# freq.comb.neighb("d", dplynum = 2, adults = T)
# freq.comb.neighb("d", dplynum = 2, adults = F)

##
# xx.var <- "d"
# dplynum <- 2

# dplunum refers to the deployment id in the meta file

freq.comb.neighb.data <- function(xx.var, dplynum, adults = T) {
        # data ----
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>% select(neighborhood, "coding" = var) %>%
                group_by(neighborhood) %>%
                summarise(a3 = sum(coding==1, na.rm = T),
                          a2 = sum(coding==2, na.rm = T),
                          a1 = sum(coding==3, na.rm = T),
                          a0 = sum(coding==4, na.rm = T),
                          exposed = sum(sum(coding==1, na.rm = T), sum(coding==2, na.rm = T), sum(coding==3, na.rm = T) ) ,
                          not_exposed = sum(coding==4, na.rm = T),
                          exclude = sum(sum(coding==5, na.rm = T), sum(coding==6, na.rm = T)),
                          na = sum(is.na(coding)),
                          sum1 = n(),
                          sum=(exposed+not_exposed) ) %>%
                select(neighborhood, a3, a2, a1, a0, exposed, not_exposed, sum) %>% 
                data.frame() -> df1


        # SS
        var <-  paste0("s_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        
        df <- df.s %>% 
                left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>%
                select(., neighborhood, matches(varspaste)) 
        
        df %>% group_by(neighborhood) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                exposed = (a3+a2+a1),
                                                not_exposed = a0,
                                                sum = sum(a3, a2, a1, a0) ) %>%
                # select(citylabel, exposed, not_exposed, sum) %>% 
                data.frame() -> df2


        # CC
        var <-  paste0("c_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        
        df <- df.c %>% 
                left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>%
                select(., neighborhood, matches(varspaste))    
        
        df %>% group_by(neighborhood) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                exposed = (a3+a2+a1),
                                                not_exposed = a0,
                                                sum = sum(a3, a2, a1, a0)) %>%
                # select(citylabel, exposed, not_exposed, sum) %>% 
                data.frame()  -> df3

        # adding all 3 df together
        df.comb <- data.frame()
        neighbvector <- meta_neighb$neighborhood[meta_neighb$deployment_id == dplynum]

        # combine and add all three data frames
        df.comb <- bind_rows(df1, df2)
        df.comb <- bind_rows(df.comb, df3)
        
        df.comb <- aggregate(. ~ neighborhood, data=df.comb, FUN=sum)
        

        #sort
        df.comb <- df.comb[order(match(df.comb$neighborhood, meta_neighb$neighborhood[meta_neighb$deployment_id == dplynum])), ]
        df.comb$neighborhood <- factor(df.comb$neighborhood, levels = df.comb$neighborhood)
        
        
        print(df.comb)
}



# ************************************************************************************************************
# y/n questions
# ************************************************************************************************************

# freq.comb.yn("dw_e_wt")
# freq.comb.yn("p_e")
# freq.comb.yn("pl_a_th")
# freq.comb.yn("pl_a_tu")
# freq.comb.yn("pl_a_tw")
# freq.comb.yn("pl_a_tf")

# xx.var <- "dw_e_wt"

# freq.comb.yn.neighb("dw_e_wt", 2)


freq.comb.yn.neighb.data <- function(xx.var, dplynum) {
        # data ----
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>% 
                select(neighborhood, "coding" = var) %>%
                group_by(neighborhood) %>%
                summarise(a1 = sum(coding==1, na.rm = T),
                          a0 = sum(coding==2, na.rm = T),
                          sum=(a1+a0) ) %>%
                data.frame() -> df1
        
        
        # SS
        var <-  paste0("s_", xx.var)
        vars <- if(xx.var == "p_e") c(paste0(var,"_2"), paste0(var,"_1")) else c(paste0(var,"_1"), paste0(var,"_0"))
        varspaste <- paste(vars, collapse = "|")
        df <- df.s %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>% 
                select(neighborhood, matches(varspaste)) 
        
        df %>% group_by(neighborhood) %>% summarise(a1 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                sum = sum(a1, a0) ) %>%
                data.frame() -> df2

        
        # CC
        var <-  paste0("c_", xx.var)
        vars <- if(xx.var == "p_e") c(paste0(var,"_2"), paste0(var,"_1")) else c(paste0(var,"_1"), paste0(var,"_0"))
        varspaste <- paste(vars, collapse = "|")
        df <- df.c %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")], by=c("neighb_num" = "neighb_UID")) %>%
                filter(dply_num == dplynum) %>% 
                select(neighborhood, matches(varspaste)) 
        
        df %>% group_by(neighborhood) %>% summarise(a1 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                sum = sum(a1, a0)) %>%
                data.frame()  -> df3

        # combine and add all three data frames
        df.comb <- bind_rows(df1, df2)
        df.comb <- bind_rows(df.comb, df3)
        
        df.comb <- aggregate(. ~ neighborhood, data=df.comb, FUN=sum)
        
        df.comb$yes <- df.comb$a1 / df.comb$sum
        df.comb$no <- df.comb$a0 / df.comb$sum
        
        neighbvector <- meta_neighb$neighborhood[meta_neighb$deployment_id == dplynum]
        
        #sort
        df.comb <- df.comb[order(match(df.comb$neighborhood, meta_neighb$neighborhood[meta_neighb$deployment_id == dplynum])), ]
        df.comb$neighborhood <- factor(df.comb$neighborhood, levels = df.comb$neighborhood)
        
        print(df.comb)        

}

