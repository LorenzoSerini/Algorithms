# Algorithms
Algorithms è un programma in cui sono stati sviluppati i seguenti algoritmi: Breadth-first search (BFS), Algoritmo di Dijkstra, Algoritmo di Kruskal. Gli algoritmi sono perfettamente funzionanti e il risultato derivante dal loro utilizzo è stampabile a video tramite diverse modalità tra le quali: Matrice di adiacenza, Lista di adiacenza, Edge list.

Nel programma viene implementata una classe RC di nome Graph.
Per usare tale classe deve essere inizialmente invocato il metodo crea_matrix(numero_nodi) attraverso il quale, grazie alla funzione fix, si potrà creare un grafo rappresentandolo, in modo molto semplice, tramite una matrice di adiacenza.

Successivamente possono essere invocati tutti i metodi richiesti dalla consegna attraverso le chiamate:
1) verificare se c'è un arco dal vertice x al vertice y = Grafo$arco_xy(vertice_x, vertice_y);
2) elencare tutti i vertici y tali che vi sia un arco dal vertice x al vertice y = Grafo$elenco_vertici_y(y);
3) aggiungere il vertice x, se non c'è = Grafo$aggungi_vertice(vertice_x);
4) rimuovere il vertice x, se c'è = Grafo$elimina_vertice(vertice_x);
5) aggiungere l'arco dal vertice x al vertice y,
se non c'è = Grafo$aggiungi_arco(vertice_x,vertice_y,peso);
6) rimuovere l'arco dal vertice x al vertice y,se c'è = Grafo$elimina_arco(vertice_x, vertice_y);
7) restituire il valore associato al vertice x = Grafo$valore_nodo_x(vertice_x);
8) restituire il valore associato all'arco (x, y) = Grafo$valore_arco(vertice_x, vertice_y);
9) impostare il valore associato
all'arco (x, y) a v = Grafo$cambia_valore_arco(vertice_x, vertice_y, peso).

Una volta inserito, il grafo può essere stampato a video come:
1) Matrice di adiacenza = Grafo$print_matrix();
2) Lista di adiacenza = Grafo$lista_adiacenza();
3) Edge list = Grafo$edge_list().

Il grafo può essere visualizzato attraverso l’invocazione Grafo$rappresentazione_grafo().

Possono essere utilizzati i seguenti Algoritmi:
1) Breadth-first search (BFS) = Grafo$BFS(nodo_partenza);
2) Algoritmo di Dijkstra = Grafo$Dijkstra(nodo_partenza);
3) Algoritmo di Kruskal = Grafo$Kruskal().
