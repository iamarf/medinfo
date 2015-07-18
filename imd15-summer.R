raw.tidy <- function(){
   # Questa funzione serve ad estrarre i dati sulle attività
   # degli studenti memorizzate nella piattaforma Moodle
   
   # Gli oggetti
   
   # tm: tempo dell'ultima consegna (in unix time)
   # dok: data.frame con autori, file, tempo iscrizione,
   #      ultima modifica
   
   # Carico la libreria che mi consente di processare
   # i file in formato XML
   library(XML)
   
   # Estraggo i dati esportati dalla piattaforma Moodle
   doc <- xmlTreeParse("files.xml")
   
   # Estraggo il nodo primario dei dati in formato XML
   r <- xmlRoot(doc)
   
   # Extracting userid
   ii <- sapply(getNodeSet(r, "//userid"), xmlValue)
   # Estraggo i nomi degli studenti...
   aa <- sapply(getNodeSet(r, "//author"), xmlValue)
   # ... i file con i loro elaborati...
   cc <- sapply(getNodeSet(r, "//contenthash"), xmlValue)
   # ... la data di iscrizione...
   tc <- sapply(getNodeSet(r, "//timecreated"), xmlValue)
   # ... l'ultima data di invio degli elaborati 
   tm <<- sapply(getNodeSet(r, "//timemodified"), xmlValue)
   
   # Trasformazione dei dati temporali in formato leggibile
   ttc <- lapply(as.numeric(tc), as.POSIXct, origin = "1970-01-01")
   tttc <- lapply(ttc, format,"%a %b %d %X")
   ttttc <- as.character(tttc)
   ttm <- lapply(as.numeric(tm), as.POSIXct, origin = "1970-01-01")
   tttm <- lapply(ttm, format,"%a %b %d %X")
   ttttm <- as.character(tttm)
   
   # Composizione del tutto in una tabella
   ddd <- cbind(ii, aa, cc, tc, ttttc, ttttm)
   dddd <- data.frame(ddd)
   
   # Eliminazione delle false iscrizioni
   ifn <- ifelse(dddd[,"aa"] == "$@NULL@$", TRUE, FALSE)
   
   # Calcolo della tabella pulita
   dok <<-dddd[!ifn,]
   names(dok) <<- c("userid","Nome","hash.file","Unix.time", "Iscrizione","Consegna")
   dok$Argomento <<- NA
   dok$Valutazione.Ricerca <<- NA
   dok$Valutazione.Testo <<- NA
   
}




get.stud <- function(chars){
   ## Returns data of a student
   ## chars contains a substring of its name
   
   subset(dok, Nome == dok$Nome[grepl(paste(".*", chars, ".*", sep = ""), dok$Nome, perl = TRUE, ignore.case =TRUE)])
}


get.stud.id <- function(id){
   ## Returns data of a student
   ## chars contains a substring of its name
   
   subset(dok, userid == id)
}

# now try to find the directory XX: ./files/XX/hash...

launch.lo <- function(chars){
   ## Launch LibreOffice on text written by student whose name contains string char
   ## CAREFUL: if more then one text are there [shouldn't!] the first one is selected!
   ## In such case, look the output of get.student, take note of the userid of multiple files
   ## and use launch.lo.id(userid)
   
   fa <- list.files("files/", recursive = TRUE)
   ff <- as.character(get.stud(chars)[["hash.file"]])
   dd <- dirname(file.path(fa[grepl(paste(".*/", ff[1], sep = ""), list.files("files/", recursive = TRUE), perl = TRUE)]))
   file.copy(paste("./files/", dd, "/", ff[1], sep = ""), paste("./files/", dd, "/tmp.odt", sep = ""))
   system2("/usr/bin/libreoffice" , paste("./files/", dd, "/tmp.odt", sep = ""))
   file.remove(paste("./files/", dd, "/tmp.odt", sep = ""))
}

launch.lo.id <- function(id){
   ## Launch LibreOffice on text written by student whose name contains string char
   ## CAREFUL: if more then one text are there [shouldn't!] the first one is selected!
   
   fa <- list.files("files/", recursive = TRUE)
   ff <- as.character(get.stud.id(id)[["hash.file"]])
   dd <- dirname(file.path(fa[grepl(paste(".*/", ff[1], sep = ""), list.files("files/", recursive = TRUE), perl = TRUE)]))
   file.copy(paste("./files/", dd, "/", ff[1], sep = ""), paste("./files/", dd, "/tmp.odt", sep = ""))
   system2("/usr/bin/libreoffice" , paste("./files/", dd, "/tmp.odt", sep = ""))
   file.remove(paste("./files/", dd, "/tmp.odt", sep = ""))
}

# function for setting topic, biblographic search grade, free text grade


set.valutazione <- function(chars, arg, ric, test){
   dok$Argomento[grepl(paste(".*", chars, ".*", sep = ""), dok$Nome, perl = TRUE, ignore.case =TRUE)] <<- arg
   dok$Valutazione.Ricerca[grepl(paste(".*", chars, ".*", sep = ""), dok$Nome, perl = TRUE, ignore.case =TRUE)] <<- ric
   dok$Valutazione.Testo[grepl(paste(".*", chars, ".*", sep = ""), dok$Nome, perl = TRUE, ignore.case =TRUE)] <<- test
}

get.graded <- function(){
   ## Returns Topic, bibliographic search and text grades
   subset(dok, !is.na(Valutazione.Ricerca), select = c(Nome, Argomento, Valutazione.Ricerca, Valutazione.Testo))
}

get.not.graded <- function(){
   ## Returns Topic, bibliographic search and text grades
   subset(dok, is.na(Valutazione.Ricerca), select = c(Nome, Argomento, Valutazione.Ricerca, Valutazione.Testo))
}

## Exctracting firstname and lastname
## I have files.xml but I need users.xml - the link is the userid

split.name <- function(chars){
   
   # Recupera i dati dello studente nel cui nome e cognome, così come memorizzato
   # in files.xml, si suppone vi sia la stringa chars con get.stud();
   # da questa si ricava lo userid che ci consente di recuperare da users.xml (anagrafica Moodle)
   # nome, cognome, email e matricola, rispettivamente nei nodi XML
   # firstname, lastname, email e username (!)
   
   library(XML)
   
   uid <- as.character(get.stud(chars)["userid"][[1]])
   users <- xmlTreeParse("users.xml")
   
   usr <- xmlRoot(users)
   
   nn <- getNodeSet(usr, paste("//users/user[@id=",uid,"]"))
   
   c(xmlValue(getNodeSet(nn[[1]], "//firstname")[[1]]), xmlValue(getNodeSet(nn[[1]], "//lastname")[[1]]), xmlValue(getNodeSet(nn[[1]], "//email")[[1]]), xmlValue(getNodeSet(nn[[1]], "//username")[[1]]))
}

## Returns the number of graded assignments
how.many.graded <- function(){
   length(dok$Argomento[!is.na(dok$Argomento)])
}

## Returns first n rows of dok data frame, giving for each student:
## name, topic, pubmed search grade, text grade
head.dok <- function(n = 30){
   head(subset(dok, select = c(Nome, Argomento, Valutazione.Ricerca, Valutazione.Testo)),n)
}

build.message <- function(nome, email, voto, ind = NA){
   if(is.na(ind)) {
      ff <- file("./MESSAGGI/messaggio", "w")
   } else {
      ff <- file(paste("./MESSAGGI/messaggio-", as.character(ind), sep = ""), "w")
   }
   cat("From: Andreas <andreas.formiconi@gmail.com>\n", file = ff)
   cat(paste("To: ", email, "\n", sep = ""), file = ff)
   cat("Cc: Andreas <andreas.formiconi@gmail.com>\n", file = ff)
   cat("Subject: Esame di informatica\n\n", file = ff)
   cat("Cara/o ", nome, ",\n\n", file = ff, sep = "")
   cat("il tuo voto è ", voto, ".\n\n", file = ff, sep = "")
   cat("Ti puoi iscrivere ad uno dei prossimi appelli, nel modo seguente:\n\n", file = ff)
   cat("1) Ti iscrivi in uno dei quattro appelli su https://sol.unifi.it \n", file = ff)
   cat("   Questo è fondamentale affinché il sistema di Ateneo produca il verbale elettronico. \n\n", file = ff)
   cat("2) Prenoti in questo modulo http://doodle.com/xawbgy8kvvkgt68m \n", file = ff)
   cat("   il momento nel quale preferisci presentarti nell'appello che hai scelto. \n", file = ff)
   cat("   Attenzione: questo serve solo a minimizzare le attese ma non sostituisce \n", file = ff)
   cat("   la prenotazione nel sistema di Ateneo - vanno fatte ambedue le cose. \n\n", file = ff)
   cat("Tuttavia, se non hai ricevuto da me un'altra email nella quale ti chiedo  \n", file = ff)
   cat("espressamente di presentarti per dei chiarimenti sugli elaborati, se il voto \n", file = ff)
   cat("che ti ho assegnato ti va bene e se non ti interessa la firma sul libretto celeste, \n", file = ff)
   cat("allora non importa che ti presenti ma devi iscriverti su https://sol.unifi.it \n", file = ff)
   cat("e inviarmi un'email nel giorno dell'appello. \n\n", file = ff)
   
   cat("Questa è la pagina di riferimento per l'esame: http://iamarf.org/orari-ricevimenti-e-appelli/ \n", file = ff)
   cat("dove verranno pubblicati anche eventuali aggiornamenti. \n\n", file = ff)
   cat("Ciao\n", "Andreas Formiconi \n", sep="", file = ff)
   close(ff)
}

grade.calc <- function(rg, tg){
   # rg: ricerca grade
   # tg: testo grade
   
   if(is.na(rg) || is.na(tg)) {
      grade <- paste("Voto non calcolabile - ricerca: ", rg, "testo: ", tg, sep = " ")
   } else if(rg == "A" && tg == "A") {
      grade <- "30 lode"
   } else if(rg == "A" && tg == "B") {
      grade <- "30 lode"
   } else if(rg == "B" && tg == "A") {
      grade <- "30 lode"
   } else if(rg == "B" && tg == "B") {
      grade <- "30"
   } else if(rg == "B" && tg == "C") {
      grade <- "30"
   } else if(rg == "C" && tg == "B") {
      grade <- "30"
   } else if(rg == "C" && tg == "C") {
      grade <- "28"
   } else if(rg == "C" && tg == "D") {
      grade <- "28"
   } else if(rg == "D" && tg == "C") {
      grade <- "28"
   } else if(rg == "A" && tg == "C") {
      grade <- "30"
   } else if(rg == "C" && tg == "A") {
      grade <- "30"
   } else if(rg == "B" && tg == "D") {
      grade <- "28"
   } else if(rg == "D" && tg == "B") {
      grade <- "28"
   } else if(rg == "B" && tg == "E") {
      grade <- "26"
   } else if(rg == "E" && tg == "B") {
      grade <- "26"
   } else {
      grade <- paste("Voto non calcolabile - ricerca: ", rg, "testo: ", tg, sep = " ")
   }
   grade
}

all.grades <- function(){
   # produce una lista di tutti i voti calcolati
   
   dd <- unique(head.dok(nrow(dok)))
   ddd <- data.frame(Nome = character(nrow(dd)), Voto = character(nrow(dd)), stringsAsFactors = FALSE)
   for (ii in 1:nrow(dd)) {
      rr = dd[ii,]
      gg <- grade.calc(rr$Valutazione.Ricerca, rr$Valutazione.Testo)
      #print(paste(rr$Nome, gg, sep = ": "))
      ddd[ii,] <- c(as.character(rr$Nome), gg)
   }
   ddd
}
simpleCap <- function(x) {
   s <- strsplit(x, " ")[[1]]
   s <- tolower(s)
   paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}

cap.nomi <- function(ntt){
   for(ii in nrow(tt)){ntt$Nome[ii] <- simpleCap(ntt$Nome[ii])}
   
}

test.message <- function(tt){
   
   ## Compone i messaggi per tutti gli studenti compresi nel 
   ## data frame tt e li salva in file di nome
   ## messaggio-1, messaggio-2... raccolti nella
   ## cartella ./MESSAGGI
   
   for(ind in 1:nrow(tt)){
      file.show(paste("./MESSAGGI/messaggio-", as.character(ind), sep = ""))
      readkey()
   }
}

readkey <- function()
{
   
   ## Interrompe un ciclo chiedendo se continuare o smettere
   
   cat ("Press [enter] to continue")
   line <- readline()
   if(line == "s"){stop("Basta!")}
}

send.mail <- function(data, first, last){
   
        ## Compone e invia un'email agli studenti compresi fra first e last nel
        ## data frame "data"

        ## Esempio: send.mail(tidy.all.particolari,100,200)

   for(ii in first:last){
      print(paste(ii , data$Nome[ii], data$Cognome[ii], sep = " "))
      build.message(as.character(data$Nome[ii]), data$Email[ii], data$Voto[ii])
      system2("./mess.sh", as.character(data$Email[ii]))
      Sys.sleep(rr[ii])
   }
}

hist.arg <- function(td, freq.option = TRUE) {
   
   ## Fa l'istogramma degli argomenti scelti dagli studenti 
   ## utilizzando il display disponibile
   
   ## Vedi l'uso di questa funzione in hist.freq.pdf.2 che produce
   ## una figura completa
   
   ## tf: data set
   ## freq.option: se TRUE frequenze, se FALSE percentuali
   
   
   par(cex = 0.9)
   if(freq.option == TRUE){
      ff <- 1
      tol <- length(levels(as.factor(tidy.all.particolari$Argomento))) + 0.5
      hh <- hist(as.numeric(td$Argomento), breaks = seq(from = 0.5, to = tol, by = 1), labels = TRUE, freq = TRUE, main = "", xlab = "Argomento scelto", ylab = "Numero di studenti", axes = FALSE)
   } else if (freq.option ==FALSE) {
      ff <- nrow(td)
      tol <- length(levels(as.factor(tidy.all.particolari$Argomento))) + 0.5
      hh <- hist(as.numeric(td$Argomento), breaks = seq(from = 0.5, to = tol, by = 1), labels = TRUE, freq = FALSE, main = "", xlab = "Argomento scelto", ylab = "Percentuale di studenti", axes = FALSE)
   } else {
      stop("Sbagliato parametro freq: deve essere TRUE o FALSE")
   }

   text(15, 135/ff, "SLI \t\t Software libero", adj = c(0,1), cex = 0.8)
   text(15, 130/ff, "OER \t Open Educational Resources", adj = c(0,1), cex = 0.8)
   text(15, 125/ff, "LSI \t\t Letteratura scientifica", adj = c(0,1), cex = 0.8)
   text(15, 120/ff, "CLC \t\t Cloud computing", adj = c(0,1), cex = 0.8)
   text(15, 115/ff, "SNE \t Social network", adj = c(0,1), cex = 0.8)
   text(15, 110/ff, "ELI \t\t Elaborazione immagini", adj = c(0,1), cex = 0.8)
   text(15, 105/ff, "RSS \t Web feed", adj = c(0,1), cex = 0.8)
   text(15, 100/ff, "HLI \t\t Hardware libero", adj = c(0,1), cex = 0.8)
   text(15, 95/ff , "P2P \t Comunicazione P2P (email...)", adj = c(0,1), cex = 0.8)
   text(15, 90/ff , "RIF \t\t Riflessione generale", adj = c(0,1), cex = 0.8)
   text(15, 85/ff , "HSN \t Hacking social network", adj = c(0,1), cex = 0.8)
   text(15, 80/ff , "HLI \t\t Hacking Linux", adj = c(0,1), cex = 0.8)
   text(15, 75/ff , "HHP \t Hacking Hot Potatoes", adj = c(0,1), cex = 0.8)
   text(15, 70/ff , "DAU \t Diritto d'autore", adj = c(0,1), cex = 0.8)
   text(15, 65/ff , "HCA \t Hacking Calc (Libreoffice)", adj = c(0,1), cex = 0.8)
   text(15, 60/ff , "SEC \t Sicurezza", adj = c(0,1), cex = 0.8)
   text(15, 55/ff , "ELT \t Elaborazione testi", adj = c(0,1), cex = 0.8)
   text(15, 50/ff , "ELA \t Elaborazione audio", adj = c(0,1), cex = 0.8)
   text(15, 45/ff , "IME \t\t Informatica in medicina", adj = c(0,1), cex = 0.8)
   text(15, 40/ff , "DWE \t Deep web", adj = c(0,1), cex = 0.8)
   text(15, 35/ff , "SDI \t\t Sopravvivenza digitale", adj = c(0,1), cex = 0.8)

   if(freq.option == TRUE){
         text(5, 135/ff , "Numero di studenti che hanno", adj = c(0,1), cex = 1.2)
      } else {
         text(5, 135/ff , "Percentuale di studenti che hanno", adj = c(0,1), cex = 1.2)
   }
   text(5, 128/ff , "scelto gli argomenti in questa", adj = c(0,1), cex = 1.2)
   text(5, 121/ff , "tabella per un tema libero", adj = c(0,1), cex = 1.2)
   
   par(cex = 0.7)
   lbl <- c("SLI","OER","LSI","CLC","SNE","ELI","RSS","HLI","P2P","RIF","HSN","HLI","HHP","DAU","HCA","SEC","VAR","ELA","IME","DWE","SDI")
   xlbl <- seq(from = 1, to = 21)
   axis(1, at = xlbl, labels = lbl, tick = FALSE)
}
hist.arg.pdf.2 <- function(fl, td) {
   
   ## Produce un file PDF con due histogrammi degli argomenti scelti 
   ## dagli studenti, il primo con le frequenze e il secondo con le 
   ## percentuali.
   
   ## fl: nome del file da scrivere
   ## td: data set
   
   ## Usa la funzione hist.arg 
   
   pdf(fl, width = 10, height = 7, paper = "a4r")
   hist.arg(td, TRUE)
   hist.arg(td, FALSE)
   dev.off()
}

write.table_with_header <- function(x, file, header, ...){
   cat(header, '\n',  file = file)
   write.table(x, file, append = T, ...)
}

copy.verbali <- function(filer, filew = "lista.0.csv") {
        
        ## Funzione di sviluppo e test per la manipolazione dei
        ## verbali online di Unifi
        
        ## Legge un file CSV in formato verbale Unifi
        ## lo trasforma in un data frame su cui si possa lavorare
        ## lo riconoduce ad un formato CSV verbale Unifi
        ## se produce un file identico all'orginale vuol dire
        ## che le operazioni sono corrette
        ## È servito a mettere a punto il codice giusto
        
        ## filer: nome file da leggere
        ## filew: nome file da scrivere
        
        ## I fase: legge il  file CSV e produce un data frame 
        ##         pulito utilizzabile
        
        library(dplyr)
        
        ## Legge l'header
        hd <- readLines(filer, 2)
        ## e lo piazza nel file hd.csv
        writeLines(hd, "hd.csv")
        
        ### Produzione dei file CSV da inviare al sistema di verbalizzazione d'ateneo
        
        ## legge il file con i seguenti parametri: 
        ## skip: skippando la prima linea (contiene il codice dell'esame,
        ## header: dichiarando che la prima riga (dopo lo skip) contiene i nomi 
        ##         delle variabili,
        ## sep: il carattere separatore dei campi è ";"
        ## check.names: non rendere sintatticamente validi i nomi delle variabili
        ##              non lo sono ma tanto vanno riscritti tal quali nel 
        ##              formato CSV di Unifi
        ## na.strings: forza l'intepretazione dei campi con "" o " " come NA
        
        lls <- read.table("lista.csv", skip = 1, header = TRUE, sep = ";", check.names = FALSE, na.strings = c("", " "))
        
        ## elimina il prefisso "="" nei valori numerici
        lls <- mutate(lls, MATRICOLA = gsub("=", "", MATRICOLA), "DATA PRENOTAZIONE" = gsub("=", "", `DATA PRENOTAZIONE`), "VOTO (in trentesimi)" = gsub("=", "", `VOTO (in trentesimi)`), "DATA ESITO" = gsub("=", "", `DATA ESITO`))
        
        ## Preparo il data frame per la scrittura su file facendo a mano il quoting
        ## delle veriabili e aggiungendo il prefisso "=" ai valori numerici dopo averli quotati
        lls <- mutate(lls, MATRICOLA = gsub("(.*)", "=\"\\1\"", MATRICOLA), COGNOME = gsub("(.*)", "\"\\1\"", COGNOME), NOME = gsub("(.*)", "\"\\1\"", NOME), COMMENTO = gsub("(.*)", "\"\\1\"", COMMENTO), "COD CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `COD CDL STUDENTE`), "CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `CDL STUDENTE`), "DATA PRENOTAZIONE" = gsub("(.*)", "=\"\\1\"", `DATA PRENOTAZIONE`), "COD INSEGNAMENTO" = gsub("(.*)", "\"\\1\"", `COD INSEGNAMENTO`), INSEGNAMENTO = gsub("(.*)", "\"\\1\"", INSEGNAMENTO), "VOTO (in trentesimi)" = gsub("(.*)", "=\"\\1\"", `VOTO (in trentesimi)`), "DATA ESITO" = gsub("(.*)", "=\"\\1\"", `DATA ESITO`), "A.A. PRECEDENTE" = gsub("(.*)", "\"\\1\"", `A.A. PRECEDENTE`), ARGOMENTI = gsub("(.*)", "\"\\1\"", ARGOMENTI), "PRESA VISIONE" = gsub("(.*)", "\"\\1\"", `PRESA VISIONE`), VERBALIZZATO = gsub("(.*)", "\"\\1\"", VERBALIZZATO), EMAIL = gsub("(.*)", "\"\\1\"", EMAIL))
        
        ## Correggo ritogliendo il prefisso "=" a eventuali casi "Non assegnato"
        lls <- mutate(lls, "VOTO (in trentesimi)" = gsub("=\"Non assegnato\"", "\"Non assegnato\"", `VOTO (in trentesimi)`))
        
        ## trasformo gli <NA> in ""
        lls[is.na(lls)] <- "\"\""
        
        write.table(lls, "l.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ";")
        
        ## Riattacco l'header, con lo script bash
        ## header.sh:
        ## cat hd.csv l.csv > $1
        ## dove $1 è il nome del file da scrivere
        system2("./header.sh", filew)
}

read.lista.verbali <- function(filer) {
        
        ## Legge un file CSV in formato verbale Unifi e
        ## lo trasforma in un data frame su cui si possa lavorare
        
        ## Value:
        ## Un data frame dei verbali con il voto non assegnato

        ## Messa a punto lavorando sulla funzione test copy.verbali()
        
        ## filer: nome file da leggere
        
        library(dplyr)
        
        ## legge il file con i seguenti parametri: 
        ## skip: skippando la prima linea (contiene il codice dell'esame,
        ## header: dichiarando che la prima riga (dopo lo skip) contiene i nomi 
        ##         delle variabili,
        ## sep: il carattere separatore dei campi è ";"
        ## check.names: non rendere sintatticamente validi i nomi delle variabili
        ##              non lo sono ma tanto vanno riscritti tal quali nel 
        ##              formato CSV di Unifi
        ## na.strings: forza l'intepretazione dei campi con "" o " " come NA
        
        lls <- read.table("lista.csv", skip = 1, header = TRUE, sep = ";", check.names = FALSE, na.strings = c("", " "), stringsAsFactors = FALSE)
        
        ## elimina il prefisso "="" nei valori numerici
        lls <- mutate(lls, MATRICOLA = gsub("=", "", MATRICOLA), "DATA PRENOTAZIONE" = gsub("=", "", `DATA PRENOTAZIONE`), "VOTO (in trentesimi)" = gsub("=", "", `VOTO (in trentesimi)`), "DATA ESITO" = gsub("=", "", `DATA ESITO`))
        
        lls <- filter(lls, `VOTO (in trentesimi)` == "Non assegnato")
        lls
}

read.header.verbali <- function(filer) {
        
        ## Legge l'haeader del file CSV in formato verbale Unifi

        ## Value
        ## Due righe in formato character con l'header
        ## 1: codice esame
        ## 2: nomi variabili
        
        library(dplyr)
        
        ## Legge l'header
        hd <- readLines(filer, 2)
        hd[1] <- gsub("(.*)", "\\1;;;;;;;;;;;;;;", gsub("=", "", gsub("\"", "", hd[1])))
        hd[2] <- gsub("\"", "", hd[2])
        hd
}

build.verbali <- function(lista, tidy.data) {
        
        ## Esegue il merge dei tidy data e della lista verbali
        ## sui numeri di matricola
        ## Copia i voti nella colonna appropriata
        ## e seleziona solo le colonne che servono per essere
        ## successivamente importate come CSV nel sistema
        ## di ateneo
        
        ## Value
        ## Un data frame con le righe pronte ad essere
        ## trasformate nel file CSV
        
        library(dplyr)
        
        lv <- merge(lista, tidy.all.particolari, by.x = "MATRICOLA", by.y = "Matricola")

        lv <- select(mutate(lv, `VOTO (in trentesimi)` = Voto), -Nome, -Cognome, -Email, -Argomento, -Valutazione.Ricerca, -Valutazione.Testo, -Voto)
        
        lv
}

write.verbali.old <- function(header, verbali, filew = "lista.0.csv") {
        
        ## Scrive il dara frame verbali in un file CSV 
        ## secondo il formato verbale Unifi
        
        ## header: vettore di caratteri con l'header del file CSV
        ##         come determinato dal sistema d'Ateneo
        ## verbali: data frame con i dati ddei verbali
        ## filew: nome file del file CSV da scrivere
        
        
        library(dplyr)
        

        ## Scrive l'header nel file hd.csv
        writeLines(header, "hd.csv")
          
        ## Preparo il data frame per la scrittura su file facendo a mano il quoting
        ## delle veriabili e aggiungendo il prefisso "=" ai valori numerici dopo averli quotati
        verbali <- mutate(verbali, MATRICOLA = gsub("(.*)", "=\"\\1\"", MATRICOLA), COGNOME = gsub("(.*)", "\"\\1\"", COGNOME), NOME = gsub("(.*)", "\"\\1\"", NOME), COMMENTO = gsub("(.*)", "\"\\1\"", COMMENTO), "COD CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `COD CDL STUDENTE`), "CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `CDL STUDENTE`), "DATA PRENOTAZIONE" = gsub("(.*)", "=\"\\1\"", `DATA PRENOTAZIONE`), "COD INSEGNAMENTO" = gsub("(.*)", "\"\\1\"", `COD INSEGNAMENTO`), INSEGNAMENTO = gsub("(.*)", "\"\\1\"", INSEGNAMENTO), "VOTO (in trentesimi)" = gsub("(.*)", "=\"\\1\"", `VOTO (in trentesimi)`), "DATA ESITO" = gsub("(.*)", "=\"\\1\"", `DATA ESITO`), "A.A. PRECEDENTE" = gsub("(.*)", "\"\\1\"", `A.A. PRECEDENTE`), ARGOMENTI = gsub("(.*)", "\"\\1\"", ARGOMENTI), "PRESA VISIONE" = gsub("(.*)", "\"\\1\"", `PRESA VISIONE`), VERBALIZZATO = gsub("(.*)", "\"\\1\"", VERBALIZZATO), EMAIL = gsub("(.*)", "\"\\1\"", EMAIL))
        
        ## Correggo ritogliendo il prefisso "=" a eventuali casi "Non assegnato"
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("=\"Non assegnato\"", "\"Non assegnato\"", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("30 lode", "30 e Lode", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("30", "30/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("29", "29/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("28", "28/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("27", "27/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("26", "26/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("25", "25/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("24", "24/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("23", "23/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("22", "22/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("21", "21/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("20", "20/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("19", "19/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("18", "18/30", `VOTO (in trentesimi)`))
        
        ## trasformo gli <NA> in ""
        verbali[is.na(verbali)] <- "\"\""
        
        ## scrivo il data frame così preparato nel file l.csv
        write.table(verbali, "l.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ";")
        
        ## Riattacco l'header, con lo script bash
        ## header.sh:
        ## cat hd.csv l.csv > $1
        ## dove $1 è il nome del file da scrivere
        system2("./header.sh", filew)
}

write.verbali <- function(header, verbali, filew = "lista.0.csv") {
        
        ## Scrive il dara frame verbali in un file CSV 
        ## secondo il formato verbale Unifi
        
        ## header: vettore di caratteri con l'header del file CSV
        ##         come determinato dal sistema d'Ateneo
        ## verbali: data frame con i dati ddei verbali
        ## filew: nome file del file CSV da scrivere
        
        
        library(dplyr)
        
        
        ## Scrive l'header nel file hd.csv
        writeLines(header, "hd.csv")
        
        ## Preparo il data frame per la scrittura su file facendo a mano il quoting
        ## delle veriabili e aggiungendo il prefisso "=" ai valori numerici dopo averli quotati
        ## verbali <- mutate(verbali, MATRICOLA = gsub("(.*)", "=\"\\1\"", MATRICOLA), COGNOME = gsub("(.*)", "\"\\1\"", COGNOME), NOME = gsub("(.*)", "\"\\1\"", NOME), COMMENTO = gsub("(.*)", "\"\\1\"", COMMENTO), "COD CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `COD CDL STUDENTE`), "CDL STUDENTE" = gsub("(.*)", "\"\\1\"", `CDL STUDENTE`), "DATA PRENOTAZIONE" = gsub("(.*)", "=\"\\1\"", `DATA PRENOTAZIONE`), "COD INSEGNAMENTO" = gsub("(.*)", "\"\\1\"", `COD INSEGNAMENTO`), INSEGNAMENTO = gsub("(.*)", "\"\\1\"", INSEGNAMENTO), "VOTO (in trentesimi)" = gsub("(.*)", "=\"\\1\"", `VOTO (in trentesimi)`), "DATA ESITO" = gsub("(.*)", "=\"\\1\"", `DATA ESITO`), "A.A. PRECEDENTE" = gsub("(.*)", "\"\\1\"", `A.A. PRECEDENTE`), ARGOMENTI = gsub("(.*)", "\"\\1\"", ARGOMENTI), "PRESA VISIONE" = gsub("(.*)", "\"\\1\"", `PRESA VISIONE`), VERBALIZZATO = gsub("(.*)", "\"\\1\"", VERBALIZZATO), EMAIL = gsub("(.*)", "\"\\1\"", EMAIL))
        
        ## Correggo ritogliendo il prefisso "=" a eventuali casi "Non assegnato"
        ## verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("=\"Non assegnato\"", "\"Non assegnato\"", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("30 lode", "30 e Lode", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("30$", "30/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("29", "29/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("28", "28/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("27", "27/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("26", "26/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("25", "25/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("24", "24/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("23", "23/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("22", "22/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("21", "21/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("20", "20/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("19", "19/30", `VOTO (in trentesimi)`))
        verbali <- mutate(verbali, "VOTO (in trentesimi)" = gsub("18", "18/30", `VOTO (in trentesimi)`))
        
        #verbali <- transform(verbali, `A.A. PRECEDENTE` = as.character(`A.A. PRECEDENTE`))  
        #verbali <- transform(verbali, ARGOMENTI = as.character(ARGOMENTI))
        #verbali <- transform(verbali, `PRESA VISIONE` = as.character(`PRESA VISIONE`))  
        #verbali <- transform(verbali, VERBALIZZATO = as.character(VERBALIZZATO))

        ## DA INIZIALIZZARE PER BENE FUORI DI QUI, PRIMA...
        for(ii in nrow(verbali)) {
                verbali$`A.A. PRECEDENTE` <- ""
                #verbali$`ARGOMENTI` <- "Domande varie"
                verbali$`PRESA VISIONE` <- ""
                verbali$`VERBALIZZATO` <- ""
        }
        
        ## trasformo gli <NA> in blank
        verbali[is.na(verbali)] <- ""
        
        ## scrivo il data frame così preparato nel file l.csv
        write.table(verbali, "l.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ";")
        
        ## Riattacco l'header, con lo script bash
        ## header.sh:
        ## cat hd.csv l.csv > $1
        ## dove $1 è il nome del file da scrivere
        system2("./header.sh", filew)
}

fill.verbali <- function(tidy.data, verbali) {
        
        ## Compila i verbali con i numeri di matricola
        ## e gli argomenti
        
        for(ii in 1:nrow(verbali)) {
                cc <- tidy.all.particolari$Argomento[
                        grepl(paste(".*", verbali$MATRICOLA[ii],".*",sep = ""), 
                              tidy.all.particolari$Matricola, 
                              perl = TRUE, 
                              ignore.case =TRUE
                              )
                        ]
                aa <- as.character(kw[[as.numeric(cc),2]])
                verbali$ARGOMENTI[ii] <- 
                        paste(aa, 
                              "Tecniche di controllo del campo di ricerca in Pubmed.",
                              sep = " "
                        )
        }
        verbali
}

lista2verbali <- function(tidy.data, 
                          nome.lista = "lista.csv", 
                          nome.verbali = "verbali.csv"){
        
        ## Genera un set di verbali a partire da una lista
        ## di studenti iscritti ad un appello esportati
        ## in un file CSV
        ## Anche i verbali vengono scritti in un file
        ## CSV che può essere esportato direttamente
        ## nel sistea di ateneo
        
        lista <- read.lista.verbali(nome.lista)
        header <- read.header.verbali(nome.lista)
        verbali <- build.verbali(lista, tidy.data)
        verbali <- fill.verbali(tidy.data, verbali)
        write.verbali(header, verbali, nome.verbali)
}

