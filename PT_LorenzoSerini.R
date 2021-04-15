
install.packages("igraph")

## Load package
library("igraph")
library("stats")


Graph <- setRefClass("Graph", fields = list(num_nodi = "numeric", matrice = "matrix"),
                     
                     methods = list(
                       
                      
                       crea_matrix = function(num_nodi){ #permette di creare una matrice di adiacenza con il numero dei nodi richiesti
                         num_nodi <<- num_nodi
                         matrice <<- matrix(0)
                         matrice <<- fix(matrice)
                        colnames(matrice) <<- c(1:dim(matrice)[1])
                        rownames(matrice) <<- c(1:dim(matrice)[1])
                         if((dim(matrice)[1] * dim(matrice)[1]) != (num_nodi*num_nodi)){stop("Hai creato una matrice di adiacenza che non corrisponde al numero dei nodi")}
                         for(v in 1:dim(matrice)[1]){
                           for(i in 1:dim(matrice)[1]){
                             if(is.na(matrice[v,i]) == TRUE){
                               return(stop("Le matrici di adiacenza devono essere composte da 0 o dal peso dell'arco. NA non è accettato !"))
                             }
                           }
                         }
                       },
                       
                       print_matrix = function(){#stampa a video la matrice di adiacenza
                         print(matrice)
                         },
                       
                       arco_xy = function(x, y){#indica se esiste un arco da x a y
                         found <- FALSE
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                           for(z in 1:dim(matrice)[2]){
                             if(colnames(matrice)[z] == y){
                               found <- TRUE
                               if(matrice[c,z] != 0){
                               print("Il vertice x è collegato con il vertice y")
                               }else{print("Il vertice x non è collegato con il vertice y")}
                             break()
                             }
                           }
                           }
                         }
                         if(found == FALSE){print("Uno dei due vertici inseriti non esiste!")}
                       },
                       
                       
                       elenco_vertici_y = function(x){#stampa a video tutti i vertici y tali che vi sia un arco da x a y
                         exist <- FALSE
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             exist <- TRUE
                         for (y in 1:dim(matrice)[1]) {
                           if(matrice[x, y] != 0){print(colnames(matrice)[y])}
                         }
                           }
                         }
                         if(exist == FALSE){stop("Il vertice rischiesto non esiste!")}
                       },
                       
                       aggungi_vertice = function(vertice){#se non esiste aggiunge il vertice richiesto
                         
                         add <- TRUE
                         for(i in 1:dim(matrice)[1]){
                           if((colnames(matrice)[i])==vertice){
                             add <- FALSE
                             break()
                           }
                           }
                         
                         if(add == FALSE){
                           stop("Questo vertice è gia presente")
                         }else{
                           num_nodi <<- num_nodi + 1
                           matrice <<- fix(matrice)
                           colnames(matrice)[num_nodi] <<- vertice
                           rownames(matrice)[num_nodi] <<- vertice
                         }
                       },
                       
                       elimina_vertice = function(vertice){#se c'è elimina il vertice richiesto
                         delete <- FALSE
                         for(i in 1:dim(matrice)[1]){
                           if(isTRUE((colnames(matrice)[i])==vertice)){
                             #num_nodi <<- num_nodi - 1
                             matrice <<- matrice[-i, -i]
                             num_nodi<<- num_nodi - 1
                             delete <- TRUE
                            # break()
                           }
                         }
                         if(delete == FALSE){stop("Questo vertice non esiste")}
                       },
                       
                       
                       aggiungi_arco = function(x, y, peso){#se il peso di un aro è uguale a 0 (cioè non esiste), aggiunge l'arco da x a y con il peso richiesto
                         exist <- FALSE
                         if(peso == 0){stop("Il peso non può essere uguale a 0, se vuoi eliminare l'arco usa la funzione elimina_arco!")}
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             for(z in 1:dim(matrice)[2]){
                               if(colnames(matrice)[z] == y){
                                 exist <- TRUE
                         if(matrice[[c, z]] == 0){matrice[c, z] <<- peso}
                         else{stop("l'arco è gia esistente, se vuoi modificare il suo valore richiama il metodo cambia_valore_arco")}
                               }}}}
                         
                         if (exist==FALSE){stop("Uno dei due vertici non esiste !")}         
                         },
                       
                       elimina_arco = function(x, y){#se esiste elimina l'arco da x a y
                         exist <- FALSE
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             for(z in 1:dim(matrice)[2]){
                               if(colnames(matrice)[z] == y){
                                 exist <- TRUE
                         if(matrice[[c, z]] != 0){matrice[c, z] <<- 0}
                         else{stop("l'arco indicato non esistente")}
                               }}}}
                         
                         if (exist==FALSE){stop("Uno dei due vertici non esiste !")} 
                         
                       },
                       
                       
                       valore_nodo_x = function(x){ #stampa a video il valore (somma del valore degli archi uscenti e entranti) del nodo x
                         exist <- FALSE
                         valore <- 0
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             exist <- TRUE
                              for(i in 1:dim(matrice)[2]){
                                valore <- (valore + matrice[i,x])
                                if(x != i ){(valore <- (valore + matrice[x,i]))}
                              }
                           }
                         }
                         if(exist == FALSE){stop("Il nodo inserito non esiste!")}
                         print(valore)
                       },
                       
                       valore_arco = function(x, y){#stampa a video il peso dell'arco da x a y
                         exist <- FALSE
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             for(z in 1:dim(matrice)[2]){
                               if(colnames(matrice)[z] == y){
                         print(matrice[[c, z]])
                                 exist <- TRUE
                               }}}}
                         
                         if(exist == FALSE){stop("Uno dei due nodi non esiste!")}
                       },
                       
                       cambia_valore_arco = function(x, y, change){#se esiste, cambia il peso dell'arco da x a y in change
                         exist <- FALSE
                         
                         if(change == 0){stop("0 non ammesso; per eliminare l'arco usare la funzione elimina_arco")}
                         
                         for(c in 1:dim(matrice)[1]){
                           if((rownames(matrice)[c])==x){
                             for(z in 1:dim(matrice)[2]){
                               if(colnames(matrice)[z] == y){
                                 matrice[c,z] <<- change
                                 exist <- TRUE
                               }}}}
                         
                         if(exist == FALSE){stop("Uno dei due nodi inseriti non esiste!")}
                       },
                       
                       rappresentazione_grafo = function(){#permette di visualizzare il grafo inserito
                         vettore <- vector("numeric", dim(matrice)[1]*dim(matrice)[1]*2)
                         contatore <- 1
                         for(i in 1:dim(matrice)[1]){
                           for(v in 1:dim(matrice)[1]){
                             
                             if(matrice[i,v] != 0){
                               vettore[contatore] <- i
                               vettore[contatore+1] <- v
                               contatore <- (contatore + 2)
                             }
                             else{
                               vettore <- vettore[-contatore]
                               vettore <- vettore[-contatore]
                             }
                           }
                         }
                         g <- make_empty_graph() %>%
                           add_vertices(dim(matrice)[1], color = "white") %>%
                           add_edges(vettore)
                         g
                         E(g)[[]]
                         plot(g)
                       },
                       
                       lista_adiacenza = function(){#stampa a video la raffigurazione del grafo secondo lista di adiacenza
                         lista_a <- list()
                           for(i in 1:dim(matrice)[1]){
                             vec <- vector("numeric", dim(matrice)[1])
                             for(x in 1:dim(matrice)[1]){
                               if(matrice[i,x] != 0) vec[x] <- x
                             }
                             lista_a[[i]] <- which(vec>0) #metto nella lista gli indici > 0 quindi solo se collegati
                           }
                           print(lista_a)
                       },
                       
                       edge_list = function(){#stampa a video la raffigurazione del grafo secondo edge list
                         edge_list <- list()
                         conta <- 1
                         for(i in 1:dim(matrice)[1]){
                           #vec <- vector("numeric", dim(matrice)[1])
                           vec <- vector("numeric", 3)
                           for(x in 1:dim(matrice)[1]){
                             if(matrice[i,x] != 0){
                               vec[1] <- rownames(matrice)[i]
                               vec[2] <- colnames(matrice)[x]
                               vec[3] <- matrice[[i,x]]
                               edge_list[[conta]] <- vec
                               conta <- conta+1
                             }
                           }
                         }
                         print(edge_list)
                       },
                       
                       BFS = function(nodo){#Attraversamento in ampiezza partendo dal nodo indicato
                         exist <- FALSE
                         for(x in 1:dim(matrice)[1]){
                           if(rownames(matrice)[1]==nodo){
                             exist <- TRUE
                           }
                         }
                         if(exist==FALSE){stop("Il nodo inserito non esiste")}
                       
                           bfs <- vector()
                         vec <- vector("numeric", dim(matrice)[1])
                         conta <- 2
                         #bfs[1] <- nodo
                        bfs[1] <- rownames(matrice)[nodo]
                         vec[nodo] <- nodo
                        print(bfs)
                         while(length(bfs) > 0){
                           for(y in 1:dim(matrice)[1]){
                             if(matrice[nodo,y] != 0 && y!=vec[y]){
                              # bfs[conta] <- y
                               bfs[conta] <- colnames(matrice)[y]
                               vec[y] <- y 
                               conta <- conta +1
                             }
                           }
                           
                           bfs <- bfs[-1]
                           print(bfs)
                          nodo <- bfs[1]
                          conta <- conta -1
                         }
                         },
                       
                       Dijkstra = function(init_node) {#Algoritmo di Dijktra partendo dal nodo indicato
                         
                         v1 <- c()
                         v2 <- c()
                         w <- c()
                         visited <- c()
                         
                         for(i in 1:dim(matrice)[1]){
                         for(y in 1:dim(matrice)[1]){
                             if(matrice[i,y]!=0){
                               v1 = c(v1, i)
                              v2 = c(v2, y)
                               w = c(w, matrice[i,y])
                             }
                           }
                         }
                         
                         graph <- data.frame(v1, v2, w)
                         
                         # Stop conditions.
                         
                         # Assicurarsi che init_node sia uno scalare numerico
                         # che esiste nel grafico.
                         if(!is.numeric(init_node) && !(init_node %in% graph[, 1])){
                           stop()
                         }
                         
                         # Dijkstra algorithm.
                         # Inizializzazione dell'algoritmo.
                         unvisited = unique(graph[, 1])
                         unvisited = sort(unvisited)  # Ordinamento dei nodi unici.
                         distance = unvisited
                         
                         
                         # Inizializzazione del distance vector.
                         index = match(init_node, unvisited)
                         distance[] = Inf
                         distance[index] = 0
                         
                         # Iterare fino ad ottenere la distanza minima tra il
                         # nodo iniziale e il resto del grafico.
                         while(length(unvisited) > 0){
                           
                           #Ottenere il nodo corrente.
                           idx_min_unvisited = which.min(distance[unvisited])
                           current_node = unvisited[idx_min_unvisited]
                           
                           visited <- c(visited, current_node)
                           
                           # Rimuovere il nodo corrente dalla univisited list.
                           unvisited = unvisited[-idx_min_unvisited]
                           
                           # Creare due booleane mask in modo da ottenere i
                           # vicini non visitati del nodo attuale.
                           neighbor_mask = graph[, 1] == current_node
                           unvisited_mask = graph[neighbor_mask, 2] %in% unvisited
                           reduced_graph = graph[neighbor_mask, 2:3][unvisited_mask, ]
                           
                           # Aggiornamento delle distanze dei vicini non visitati del
                           # nodo attuale.
                           d_i = distance[current_node]         # Distanza del nodo corrente i.
                           d_k = distance[reduced_graph[, 1]]   # Distanza attuale di tutti i vicini non visitati (k) di i.
                           d_ik = reduced_graph[, 2]            #Distanza dal nodo i al nodo k.
                           distance[reduced_graph[, 1]] = pmin(d_k, d_i + d_ik)
                           
                         }
                         
                         print(visited)
                         return(sort(distance))
                       },
                       
                       
                       Kruskal = function() {#Algoritmo di Kruskal
                         
                         
                         v1 <- c()
                         v2 <- c()
                         w <- c()
                         
                         for(i in 1:dim(matrice)[1]){
                           for(y in 1:dim(matrice)[1]){
                             if(matrice[i,y]!=0){
                               v1 = c(v1, i)
                               v2 = c(v2, y)
                               w = c(w, matrice[i,y])
                             }}}
                         
                         wiki_graph <- data.frame(v1, v2, w)
                         
                         for(i in 1:dim(wiki_graph)[1] - 1){
                           for(x in 2 : dim(wiki_graph)[1]){
                             if(isTRUE(wiki_graph[i,1]==wiki_graph[x,2])&&isTRUE(wiki_graph[i,2]==wiki_graph[x,1])&&isTRUE(wiki_graph[i,3]==wiki_graph[x,3])){
                               wiki_graph <- wiki_graph[-x,]
                             }}}
                         
                         sort <- sort(wiki_graph$w)
                         sort <- unique(sort)
                         
                         
                         accopVertex <- c(1:dim(matrice)[1])
                         singolVertex <- c()
                         f <- 1
                         v1 <- c()
                         v2 <- c()
                         w <- c()
                         delete <- c()
                         
                         while(length(accopVertex)!=0){
                           
                           sub <- subset(wiki_graph, w %in% sort[f])
                           
                           for(y in 1:dim(sub)[1]){
                             for(i in 1:length(accopVertex)){
                               if((isTRUE(sub[y,1]==accopVertex[i])||isTRUE(sub[y,2]==accopVertex[i]))&&isTRUE(sub[y,1]!=sub[y,2])){
                                 v1 <- c(v1, sub[y,1])
                                 v2 <- c(v2, sub[y,2])
                                 w <- c(w,sub[y,3])
                                 singolVertex <- c(singolVertex, sub[y,1], sub[y,2])
                                 
                                 if(length(singolVertex)>2){
                                 for(x in 1:length(singolVertex)-2){
                                  
                                    
                                     if(isTRUE(sub[y,1]==singolVertex[x])||isTRUE(sub[y,2]==singolVertex[x])){  
                                       
                                       if(isTRUE(sub[y,1]==singolVertex[x])){
                                         
                                         delete <- c(delete,sub[y,1], sub[y,2])
                                         
                                         if(x%%2==0){
                                           delete <- c(delete, singolVertex[(x-1)])
                                           
                                         }else{
                                           delete <- c(delete, singolVertex[(x+1)])

                                         }
                                       }
                                       
                                       if(isTRUE(sub[y,2]==singolVertex[x])){
                                         
                                         delete <- c(delete,sub[y,1], sub[y,2])
                                         
                                         if(x%%2==0){
                                           delete <- c(delete, singolVertex[(x-1)])
                                           
                                         }else{
                                           delete <- c(delete, singolVertex[(x+1)])
                                         }
                                       }
                                       
                                       delete <- unique(delete)
                                       
                                       for(g in 1:length(delete)){
                                         for(h in 1:length(accopVertex)){
                                           if(isTRUE(delete[g]==accopVertex[h])){
                                             accopVertex <- accopVertex[-h]
                                           }}}
                                       delete <- NULL
                                     }
                                   }
                                  }
                                 break()
                               }}}
                           f <- f+1}
                         visited <- data.frame(v1,v2,w)
                         return(visited)
                       }
                      
                     )
)


Grafo <- Graph()
Grafo$crea_matrix(3)

Grafo$print_matrix()


Grafo$arco_xy(1,3)
Grafo$arco_xy(4,1)


Grafo$elenco_vertici_y(1)
Grafo$elenco_vertici_y(3)
Grafo$elenco_vertici_y(4)

Grafo$aggungi_vertice(7)

Grafo$elimina_vertice(1)

Grafo$aggiungi_arco(2,1,10)

Grafo$elimina_arco(3,3)
Grafo$elimina_arco(1,2)

Grafo$valore_nodo_x(1)
Grafo$valore_nodo_x(2)
Grafo$valore_nodo_x(3)

Grafo$valore_arco(1,1)
Grafo$valore_arco(4,3)

Grafo$cambia_valore_arco(4,1,2)
Grafo$cambia_valore_arco(3,5,2)


Grafo$rappresentazione_grafo()

Grafo$lista_adiacenza()

Grafo$edge_list()

Grafo$BFS(1)

Grafo$Dijkstra(1)

Grafo$Kruskal()
