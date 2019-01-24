
# Load_libraries ----------------------------------------------------------
# 
# library(RSQLite)
# library(safer)
# library(tidyverse)
# library(DBI)
# library(data.table)



# function to extract and decrypt SQL -------------------------------------

mod_decrypt_sql<-function(input,output,session,sql_con,sql_table,decode_file){
    
    ns<-session$ns
    
    table<-reactive({
    
        decrypt_file(infile = decode_file,outfile = 'www/login/tinyfile.txt')
        key <-read_file('www/login/tinyfile.txt')
        file.remove('www/login/tinyfile.txt')
        
        
        table<-sql %>% 
            dbReadTable(sql_table)
        
        table %>% setDT()
        
        table[, (colnames(table)) := lapply(.SD, as.character), .SDcols = colnames(table)]
        
        table %>% setDF()
        
        for(i in 1:nrow(table)){
            for(j in 1:ncol(table)){
                table[i,j]<-decrypt_string(as.character(table[i,j]),key)
            }
        }
        table %>% setDT()
        
        rm(key,i,j)
        dbDisconnect(sql)
        
    })
    
        
    return(table())

}


# Normal Function ---------------------------------------------------------


decrypt_sql<-function(sql_con,sql_table,decode_file){
    
        decrypt_file(infile = decode_file,outfile = 'www/login/tinyfile.txt')
        key <-read_file('www/login/tinyfile.txt')
        file.remove('www/login/tinyfile.txt')
        
        sql<-dbConnect(SQLite(),
                       sql_con)
        
        
        table<-sql %>% 
            dbReadTable(sql_table)
        
        table %>% setDT()
        
        if(nrow(table)>0){
        
        table[, (colnames(table)) := lapply(.SD, as.character), .SDcols = colnames(table)]
        
        table %>% setDF()
        
        for(i in 1:nrow(table)){
            for(j in 1:ncol(table)){
                table[i,j]<-decrypt_string(as.character(table[i,j]),key)
            }
        }
        table %>% setDT()
        
        rm(key,i,j)
        
        }
        
        dbDisconnect(sql)
        
        return(table)
    
}

# Function to encrypt a table ---------------------------------------------


mod_encrypt_sql<-function(input,output,session,table,decode_file){
    
    table<-reactive({
        table %>% setDF()
        
        decrypt_file(infile = decode_file,outfile = 'www/login/tinyfile.txt')
        key <-read_file('www/login/tinyfile.txt')
        file.remove('www/login/tinyfile.txt')
        
        table %>% setDT()
        if(nrow(table)>0){
        table[, (colnames(table)) := lapply(.SD, as.character), .SDcols = colnames(table)]
        
        table %>% setDF()
        
        for(i in 1:nrow(table)){
            for(j in 1:ncol(table)){
                table[i,j]<-encrypt_string(as.character(table[i,j]),key)
            }
        }
        
        table %>% setDT()
        
        rm(key,i,j)
        }
    })

    return(table)

}


# Normal_encrypt_function -------------------------------------------------


encrypt_sql<-function(table,decode_file){
    
        table %>% setDF()
        
        decrypt_file(infile = decode_file,outfile = 'www/login/tinyfile.txt')
        key <-read_file('www/login/tinyfile.txt')
        file.remove('www/login/tinyfile.txt')
        
        
        table %>% setDT()
        if(nrow(table)>0){
        table[, (colnames(table)) := lapply(.SD, as.character), .SDcols = colnames(table)]
        
        table %>% setDF()
        
        for(i in 1:nrow(table)){
            for(j in 1:ncol(table)){
                table[i,j]<-encrypt_string(as.character(table[i,j]),key)
            }
        }
        
        table %>% setDT()
        
        rm(key,i,j)    
        }
    
        return(table)
    
}

# whatIneed<-decrypt_sql('www/main_data.sqlite',
#             sql_table = 'emp_user',decode_file = 'www/login/user.bin')
# 
# 
# whatIhave<-encrypt_sql(whatIneed,'www/login/user.bin')
