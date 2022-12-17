#Pacotes e Funções##############################################################
carregando_pacotes<-function(){
  packages<-c("rvest","stringr", "dplyr", "openxlsx","stringr")
  
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

verificar_paginas<-function(html){
  x=html %>%
    rvest::html_nodes('div div section div ul') %>%
    rvest::html_text()%>%
    strsplit(" ")%>%
    lapply(function(x) as.numeric(x))%>%
    unlist()%>%
    max(na.rm=T)%>%suppressWarnings()
  if(stringr::str_detect(as.character(x),"Inf")){
    x=1
  }
  return(x)
}
#Executando#####################################################################
cjd=550
p_cjd=20
d=1

html_cj_dados <- rvest::read_html("https://dev.dados.gov.br/dataset")
#Entrar em todas as paginas (Conjunto de Dados)---------------------------------
paginas_cj_dados=verificar_paginas(html=html_cj_dados)
tryCatch(
  {
    #for(cjd in 1:paginas_cj_dados){
    for(cjd in 1:paginas_cj_dados){
      #Entrando em todos os conjuntos de dados de cada pg---------------------------
      link_cj_dados=rvest::read_html(paste0("https://dev.dados.gov.br/dataset?q=&sort=&page=",cjd))%>%
        rvest::html_nodes('div div section div ul li a')%>%
        html_attr('href')%>%unique()
      if(any(!stringr::str_detect(link_cj_dados,"dataset"))){link_cj_dados<-link_cj_dados[-which(!stringr::str_detect(link_cj_dados,"dataset"))]}
      #procurar<-c("/dataset/testeeeee","/dataset/atendimento","/dataset/cursos-tecnicos")
      #if(any(link_cj_dados%in%c(procurar))){link_cj_dados<-link_cj_dados[-c(which(link_cj_dados%in%c(procurar)))]}
      if(any(link_cj_dados%>%stringr::str_detect("q[=][&]sort[=][&]page"))){link_cj_dados<-link_cj_dados[-c(which(link_cj_dados%>%stringr::str_detect("q[=][&]sort[=][&]page")))]}
      for(p_cjd in 1:length(link_cj_dados)){
        link_cj_dados_dentro=paste0("https://dev.dados.gov.br",link_cj_dados[p_cjd],collapse = "/")
        #Coletando as informacoes da organizacao
        Organizacao=rvest::read_html(link_cj_dados_dentro)%>%
          rvest::html_nodes("div aside div section h1")%>%
          rvest::html_text()%>%
          dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
        Conjunto_de_Dados=rvest::read_html(link_cj_dados_dentro)%>%
          rvest::html_nodes("div aside section div div h1")%>%
          rvest::html_text()%>%
          dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
        #Pegando cada um dos datasets-----------------------------------------------
        link_dataset=rvest::read_html(link_cj_dados_dentro)%>%
          rvest::html_nodes("div div div div article div section ul li a")%>%
          html_attr('href')%>%stringr::str_remove_all("[\n]")%>%unique()
        if(any(link_dataset%>%stringr::str_detect("/dataset/"))){link_dataset<-link_dataset[which(link_dataset%>%stringr::str_detect("/dataset/"))]}
        if(any(link_dataset%>%stringr::str_detect("plus[.]google|twitter[.]com|facebook[.]com|landpage"))){link_dataset<-link_dataset[-which(link_dataset%>%stringr::str_detect("plus[.]google|twitter[.]com|facebook[.]com|landpage"))]}
        link_dataset<-ifelse(link_dataset%>%stringr::str_detect("http[:]"),link_dataset,paste0("https://dev.dados.gov.br",link_dataset))
        procurar<-c("https://dev.dados.gov.br/dataset/",paste0(paste0("https://dev.dados.gov.br/dataset/",c("groups","activity")),link_cj_dados[p_cjd]%>%stringr::str_remove("dataset/")),link_cj_dados_dentro)
        if(any(link_dataset%in%procurar)){link_dataset<-link_dataset[-which(link_dataset%in%procurar)]}
        for(d in 1:length(link_dataset)){
          id=paste(cjd,p_cjd,d,sep="_")
          if(id%in%c(dir("separado/")%>%stringr::str_remove_all("[.]xlsx"))){
            print(paste0("Já fiz id ",id))
          }else{
            html_link=NA
            tryCatch({html_link=rvest::read_html(link_dataset[d])},error = function(e) {cat("Error:", conditionMessage(e), "\n")})
            if(!is.na(html_link)){
              Dataset=rvest::read_html(link_dataset[d])%>%
                rvest::html_nodes("div section div h1")%>%
                rvest::html_text2()%>%
                dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
              
              Descricao=rvest::read_html(link_dataset[d])%>%
                rvest::html_nodes("div section div div blockquote")%>%
                rvest::html_text2()
              if(any(Descricao=="")){Descricao=Descricao[-which(Descricao=="")]}
              Descricao=Descricao%>%dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
              if(is.na(Descricao)){
                Descricao=rvest::read_html(link_dataset[d])%>%
                  rvest::html_nodes("div section div div p")%>%
                  rvest::html_text2()
                if(any(Descricao=="")){Descricao=Descricao[-which(Descricao=="")]}
                Descricao=Descricao%>%dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
              }

              Formato=rvest::read_html(link_dataset[d])%>% 
                rvest::html_node("div div section table")%>% 
                rvest::html_table()%>%dplyr::rename(campo=1)%>%dplyr::filter(campo=="format")%>%
                dplyr::select(2)%>%dplyr::pull()
              x=data.frame(id,Organizacao,Conjunto_de_Dados,Dataset,Formato,Descricao,link=link_dataset[d])
              #Salvando na pasta--------------------------------------------------------
              suppressWarnings(dir.create('separado'))
              openxlsx::write.xlsx(x,paste0("separado/",id,".xlsx"))
              print(paste0("ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
              rm(x,id) 
            }else{
              print(paste0("ERROR ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
              openxlsx::write.xlsx(data.frame(a=NA),paste0("separado/ERROR_",id,".xlsx"))
            }

          }
        }
      }
    }
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    print(paste0("ERROR ID=",id,"[cjd=",cjd,'/',paginas_cj_dados,"][p_cjd=",p_cjd,'/',length(link_cj_dados),"][d=",d,'/',length(link_dataset),"]","{",Conjunto_de_Dados,"/",Conjunto_de_Dados,"}"))
    openxlsx::write.xlsx(data.frame(a=NA),paste0("separado/ERROR_",id,".xlsx"))
  }
)
