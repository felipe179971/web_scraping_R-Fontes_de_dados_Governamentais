#Pacotes e Funções##############################################################
carregando_pacotes<-function(){
  packages<-c("rvest","stringr", "dplyr", "openxlsx","stringr","xml2")
  
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  );rm(packages,package.check)
}
carregando_pacotes();rm(carregando_pacotes)
`%nin%` = Negate(`%in%`)
verificar_paginas<-function(html){
  x=html %>%
    rvest::html_nodes('div div section div ul') %>%
    rvest::html_text()%>%
    dplyr::last()%>%
    scan(text=.,what = "")%>%{
      a=.;b=a[length(a)-1]
      as.numeric(b)
    }
  if(stringr::str_detect(as.character(x),"Inf")){
    x=1
  }
  return(x)
}
#Executando#####################################################################
cjd=17
p_cjd=1
d=1

html_cj_dados <- rvest::read_html("http://www.dados.df.gov.br/dataset")
#Entrar em todas as paginas (Conjunto de Dados)---------------------------------
paginas_cj_dados=verificar_paginas(html=html_cj_dados)
tryCatch(
  {
    #for(cjd in 1:paginas_cj_dados){
    for(cjd in 1:paginas_cj_dados){
      #Entrando em todos os conjuntos de dados de cada pg---------------------------
      link_cj_dados=NA
      tryCatch({link_cj_dados=rvest::read_html(paste0("http://www.dados.df.gov.br/dataset?q=&sort=&page=",cjd))%>%
        rvest::html_nodes('div div section div ul li a')%>%
        html_attr('href')%>%unique()},error = function(e) {cat("Error:", conditionMessage(e), "\n")})
      if(all(!is.na(link_cj_dados))){
        if(any(!stringr::str_detect(link_cj_dados,"dataset"))){link_cj_dados<-link_cj_dados[-which(!stringr::str_detect(link_cj_dados,"dataset"))]}
        #procurar<-c("/dataset/testeeeee","/dataset/atendimento","/dataset/cursos-tecnicos")
        #if(any(link_cj_dados%in%c(procurar))){link_cj_dados<-link_cj_dados[-c(which(link_cj_dados%in%c(procurar)))]}
        if(any(link_cj_dados%>%stringr::str_detect("q[=][&]sort[=][&]page"))){link_cj_dados<-link_cj_dados[-c(which(link_cj_dados%>%stringr::str_detect("q[=][&]sort[=][&]page")))]}
        for(p_cjd in 1:length(link_cj_dados)){
          
          link_cj_dados_dentro<-ifelse(link_cj_dados[p_cjd]%>%stringr::str_detect("http[:]")|link_cj_dados[p_cjd]%>%stringr::str_detect("https[:]"),link_cj_dados[p_cjd],paste0("http://www.dados.df.gov.br",link_cj_dados[p_cjd],collapse = "/"))
          link_p_cjd=NA
          tryCatch({link_p_cjd=rvest::read_html(link_cj_dados_dentro)},error = function(e) {cat("Error:", conditionMessage(e), "\n")})
          if(!is.na(link_p_cjd)){
            #Coletando as informacoes da organizacao
            Organizacao=link_p_cjd%>%
              rvest::html_elements("div aside div div h3")%>%rvest::html_text2()
            Conjunto_de_Dados=link_p_cjd%>%
              rvest::html_elements("article div h1")%>%rvest::html_text2()
            #Pegando cada um dos datasets-----------------------------------------------
            link_dataset=link_p_cjd%>%  
              rvest::html_nodes("ul li a")%>%
              html_attr('href')%>%stringr::str_remove_all("[\n]")%>%unique()
        
            if(any(link_dataset%>%stringr::str_detect("/dataset/"))){link_dataset<-link_dataset[which(link_dataset%>%stringr::str_detect("/dataset/"))]}
            if(any(link_dataset%>%stringr::str_detect("http[:]"))){link_dataset<-link_dataset[-which(link_dataset%>%stringr::str_detect("http[:]"))]}
            if(any(link_dataset%>%stringr::str_detect("https[:]"))){link_dataset<-link_dataset[-which(link_dataset%>%stringr::str_detect("https[:]"))]}
            if(any(link_dataset%>%stringr::str_detect("plus[.]google|twitter[.]com|facebook[.]com|landpage"))){link_dataset<-link_dataset[-which(link_dataset%>%stringr::str_detect("plus[.]google|twitter[.]com|facebook[.]com|landpage"))]}
            link_dataset<-ifelse(link_dataset%>%stringr::str_detect("http[:]")|link_dataset%>%stringr::str_detect("https[:]"),link_dataset,paste0("www.dados.df.gov.br",link_dataset))
            procurar1<-c("www.dados.df.gov.br/dataset/",paste0(paste0("www.dados.df.gov.br/dataset/",c("groups","activity")),link_cj_dados[p_cjd]%>%stringr::str_remove("dataset/")),ifelse(stringr::str_detect(link_cj_dados_dentro,"http[:]"),stringr::str_remove(link_cj_dados_dentro,"http[:]//"),ifelse(stringr::str_detect(link_cj_dados_dentro,"https[:]"),stringr::str_remove(link_cj_dados_dentro,"https[:]//"),link_cj_dados_dentro )))
            if(any(link_dataset%in%procurar1)){link_dataset<-link_dataset[-which(link_dataset%in%procurar1)]}
            link_dataset<-link_dataset[link_dataset%>%stringr::str_detect("dados[.]df[.]gov[.]br")]
            for(d in 1:length(link_dataset)){
              id=paste(cjd,p_cjd,d,sep="_")
              suppressWarnings(dir.create('separado.DF'))
              suppressWarnings(dir.create(paste0('separado.DF/',cjd)))
              
              if(id%in%c(dir(paste0('separado.DF/',cjd))%>%stringr::str_remove_all("[.]xlsx"))){
                print(paste0("Já fiz id ",id))
              }else{
                html_link=NA
                link_dataset_cada_um=link_dataset[d]
                if(!link_dataset_cada_um%>%stringr::str_detect("http[:]")){link_dataset_cada_um=paste0("http://",link_dataset_cada_um)}
                tryCatch({html_link=rvest::read_html(link_dataset_cada_um)},error = function(e) {cat("Error:", conditionMessage(e), "\n")})
                if(!is.na(html_link)){
                  
                  Dataset=html_link%>%
                    rvest::html_nodes("div section div h1")%>%
                    rvest::html_text2()%>%
                    dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
                  
                  Descricao=html_link%>%
                    rvest::html_nodes("div section div div blockquote")%>%
                    rvest::html_text2()
                  if(any(Descricao=="")){Descricao=Descricao[-which(Descricao=="")]}
                  Descricao=Descricao%>%dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
                  if(is.na(Descricao)){
                    Descricao=html_link%>%
                      rvest::html_nodes("div section div div p")%>%
                      rvest::html_text2()
                    if(any(Descricao=="")){Descricao=Descricao[-which(Descricao=="")]}
                    Descricao=Descricao%>%dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
                  }
                  if(is.na(Dataset)){
                    Formato=NA
                  }else{
                    Formato=html_link%>% 
                      rvest::html_node("div div section table")%>% 
                      rvest::html_table()%>%dplyr::rename(campo=1)%>%dplyr::filter(campo=="format")%>%
                      dplyr::select(2)%>%dplyr::pull()
                    if(length(Formato)==0){
                      Formato=html_link%>% 
                        rvest::html_node("div div section table")%>% 
                        rvest::html_table()%>%dplyr::rename(campo=1)%>%dplyr::filter(campo=="Formato")%>%
                        dplyr::select(2)%>%dplyr::pull()
                    }
                    
                    x=data.frame(id,Organizacao,Conjunto_de_Dados,Dataset,Formato,Descricao,link=link_dataset[d])
                    #Salvando na pasta--------------------------------------------------------
                    openxlsx::write.xlsx(x,paste0("separado.DF/",cjd,"/",id,".xlsx"))
                    #rm(x,id) 
                  }
                  print(paste0("ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
                  
                }else{
                  print(paste0("ERROR ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
                  openxlsx::write.xlsx(data.frame(a=NA),paste0("separado.DF/ERROR_",id,".xlsx"))
                }
                
              }
              
              #Se tiver o arquivo de erro e o normal, excluir o de erro
              if(paste0(cjd,".xlsx")%in%dir(paste0('separado.DF/',cjd))&paste0("ERROR_",cjd,".xlsx")%in%dir("separado.DF/")){
                file.remove(paste0("separado.DF/ERROR_",cjd,".xlsx"))
                print(paste0("APAGUEI: ERROR_",cjd,".xlsx"))
              }
              
            }
          }else{
            print(paste0("ERROR ID=",paste(cjd,p_cjd,sep="_"),"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",'/',"]","{","/","}"))
            openxlsx::write.xlsx(data.frame(a=NA),paste0("separado.DF/ERROR_",paste(cjd,p_cjd,sep="_",".xlsx")))
          }
          
        }
        
      }else{
        print(paste0("ERROR ID=",cjd,"[cjd=",cjd,'/',"][p_cjd=",'/',"][d=",'/',"]","{","/","}"))
        openxlsx::write.xlsx(data.frame(a=NA),paste0("separado.DF/ERROR_",id,".xlsx"))
      }
      
    }
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    print(paste0("ERROR ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
    openxlsx::write.xlsx(data.frame(a=NA),paste0("separado.DF/ERROR_",id,".xlsx"))
    
  }
)
#-------------------------------------------------------------------------------
#Empilhando tudo----------------------------------------------------------------

pastas<-dir("separado.DF/")
pastas<-pastas[!stringr::str_detect(pastas,".xlsx")]
dados=NA
for(i in 1:length(pastas)){
  arquivos=dir(paste0("separado.DF/",pastas[i]))
  for(j in 1:length(arquivos)){
    if(paste0("separado.DF/",pastas[i],"/",arquivos[j])%nin%c(paste0("separado.DF/",pastas[i],"/",c("9999999999.xlsx")))){
      a=openxlsx::read.xlsx(paste0("separado.DF/",pastas[i],"/",arquivos[j]))
      if(all(is.na(dados))){dados=a}else{dados=rbind(dados,a)%>%unique()}
      print(paste0("separado.DF/",pastas[i],"/",arquivos[j])) 
    }
  }
}

a=dados
a$id1=NA;a$id2=NA;a$id3=NA
for(i in 1:nrow(a)){
  a$id1[i]=strsplit(a$id[i], "_", fixed=T)[[1]][1]
  a$id2[i]=strsplit(a$id[i], "_", fixed=T)[[1]][2]
  a$id3[i]=strsplit(a$id[i], "_", fixed=T)[[1]][3]
  print(paste0(i,"/",nrow(a)))
}


a=a%>%dplyr::mutate(id1=as.numeric(id1),id2=as.numeric(id2),id3=as.numeric(id3))%>%
  dplyr::arrange(id1,id2,id3,Organizacao,Conjunto_de_Dados)
openxlsx::write.xlsx(a%>%dplyr::select(-c(id1,id2,id3)),"Mapeados_DF.xlsx",overwrite=TRUE)

