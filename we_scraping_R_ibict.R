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
    rvest::html_nodes('div div article div div ul') %>%
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

{
  prefixo_site1="https://dev.dados.gov.br"
  #Link Inicial com todas as organizacoes encontradas
  html_organizacoes <- rvest::read_html("https://dev.dados.gov.br/organization")
  ##Verificando quantas paginas tenho
  paginas_organizacoes=verificar_paginas(html_organizacoes) 
  if(!"we_scraping_R_ibict.xlsx"%in%dir()){
    resultado=data.frame(id=0,organizacao=0,titulo=0,link=0,`Pg Inicial`=0,Org=0,`Pg Org`=0,Bases=0)
  }else{
    resultado=openxlsx::read.xlsx("we_scraping_R_ibict.xlsx")
  }
  
  
  o=1
  io=1
  iop=1
  b=1
  for(o in 1:paginas_organizacoes){
    tryCatch(
      {
        link_organizacao=rvest::read_html(paste0("https://dev.dados.gov.br/organization?q=&sort=&page=",o))%>%
          rvest::html_nodes('div div article div ul li a')%>%
          html_attr('href')
        if(any(!stringr::str_detect(link_organizacao,"organization"))){link_organizacao<-link_organizacao[-which(!stringr::str_detect(link_organizacao,"organization"))]}
        link_organizacao<-paste0(prefixo_site1,link_organizacao)
        #Inside Ornazination website
        #for(io in 1){
        for(io in 3:length(link_organizacao)){
          html_insede_organizacoes <- rvest::read_html(link_organizacao[io])
          ##Verificando quantas paginas tenho
          paginas_insede_organizacoes=verificar_paginas(html=html_insede_organizacoes)
          ##Em cada pagina, entro e vejo quantos links tenho
          #for(iop in 1){
          for(iop in 72:paginas_insede_organizacoes){
            #vendos os links de cada pagina
            link_base=rvest::read_html(paste0(link_organizacao[io],"?q=&sort=&page=",iop))%>%
              rvest::html_nodes('div div article div ul li div h3 a')%>%
              html_attr('href')
            if(any(!stringr::str_detect(link_base,"dataset"))){link_base<-link_base[-which(!stringr::str_detect(link_base,"dataset"))]}
            link_base<-paste0(prefixo_site1,link_base)  
            #dentro do site pego as informacoes que quero da base
            #for(b in 1){
            for(b in 1:length(link_base)){
              #Se ja fiz, pula
              id=paste0(o,io,iop,b)
              if(id%in%c(resultado$id)){
                print(paste0("Já fiz id ",id))
              }else{
                html_base <- rvest::read_html(link_base[b])
                organizacao=rvest::read_html(paste0(link_organizacao[io],"?q=&sort=&page=",iop))%>%
                  rvest::html_nodes("div aside div section h1")%>%
                  rvest::html_text()%>%
                  dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
                titulo=html_base%>%
                  rvest::html_nodes("div aside section div div h1")%>%
                  rvest::html_text()%>%
                  dplyr::first()%>%stringr::str_remove_all("[\n]")%>%trimws()
                link=link_base[b]
                x=data.frame(id,organizacao,titulo,link,`Pg Inicial`=o,Org=io,`Pg Org`=iop,Bases=b)
                resultado<-rbind(resultado,x)
                print(paste0("ID=",id," Pg Inicial[o=",o,'/',paginas_organizacoes,"] Organizacao[io=",io,'/',length(link_organizacao),"] Pg Org[iop=",iop,'/',paginas_insede_organizacoes,"] Bases[b=",b,'/',length(link_base),"{",organizacao,"}"))
                rm(x,html_base,titulo,link,organizacao)
              }
            }
          }
          
        }
        
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        print(paste0("ERROR ID=",id," Pg Inicial[o=",o,'/',paginas_organizacoes,"] Organizacao[io=",io,'/',length(link_organizacao),"] Pg Org[iop=",iop,'/',paginas_insede_organizacoes,"] Bases[b=",b,'/',length(link_base),"{",organizacao,"}"))
        
      }
    )
  }
  
  #Salvando em excel
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Bases")
  openxlsx::writeDataTable(wb,sheet = "Bases", resultado,tableStyle ="TableStyleMedium2")
  openxlsx::saveWorkbook(wb, "we_scraping_R_ibict.xlsx", overwrite = TRUE)
  
}
