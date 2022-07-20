(*definizione tipo grafo*)
type 'a graph = Grafo of ('a * 'a) list;;

let grafo1 = Grafo [(1,2);(1,3);
                    (2,3);(2,4);(2,5);(2,7);
                    (3,6);(3,7);
                    (4,5);(4,7);
                    (5,7);
                    (6,7)]
;;

let grafo2 = Grafo [(1,2);(1,3);(1,4);(1,5);
                    (2,3);(2,5);(2,12);
                    (3,4);(3,5);
                    (4,5);(4,12);
                    (5,6);(5,7);(5,8);(5,9);(5,10);
                    (6,7);(6,8);(6,9);(6,10);
                    (7,8);(7,9);(7,10);
                    (8,4);(8,9);(8,10);(8,13);
                    (9,10);(9,13);
                    (11,12);(11,13);
                    (12,13);
                    (13,14);
                    (14,8);(14,15)]
;;     

let grafo3 = Grafo [(1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);
                    (2,3);(2,4);(2,5);(2,6);(2,7);(2,8);
                    (3,4);(3,5);(3,6);(3,7);(3,8);
                    (4,5);(4,6);(4,7);(4,8);
                    (5,6);(5,7);(5,8);
                    (6,7);(6,8);
                    (7,8)]
;;


(*funzione che prende come argomento un grafo, e ritorna una lista contenente tutti i suoli valori distinti *)
(*'a graph -> 'a list*)
let nodi (Grafo arcs) = 
  let setadd x set = if List.mem x set then set
                      else x::set
        in let rec aux = function
              [] -> []
              | (x,y)::resto -> setadd x (setadd y (aux resto))
        in aux arcs
;;

(*funzione successori di un nodo di un grafo orientato*)
(*prende in input un grafo e un valore(nodo), ritorna una lista contentente tutti i nodi
    che sono una coppia con il valore nodo input nel grafo*)
(*'a graph -> 'a -> 'a list*)
let rec successori (Grafo arcs) n = 
  match arcs with
  [] -> []
  | (x,y)::resto -> if x = n then y::successori (Grafo resto) n
                      else if y = n then x::successori (Grafo resto) n
                      else successori (Grafo resto) n
;;

(*funzione che controlla se un nodo è vicino di un altro*)
(*prende in input un grafo e una coppia di valori, ritorna true se i due valori
   corrispondono a una coppie definita nel grafo*, false altrimenti*)
(*'a graph -> 'a -> 'a -> bool*)
let rec vicini (Grafo arcs) n1 n2 =
  match arcs with
  [] -> false
  |(x,y)::resto -> if x = y then vicini (Grafo resto) n1 n2
                    else if x == n1 then y == n2 || vicini (Grafo resto) n1 n2
                    else if x == n2 then y == n1 || vicini (Grafo resto) n1 n2
                    else vicini (Grafo resto) n1 n2
;;

(*'a graph -> 'a list -> 'a -> bool*)
(*funzione che controlla se una cricca di un grafo può essere espansa da un nodo input m;
   ritorna true se la cricca è massimale(non espandibile), false altrimenti *)
let rec massimale grafo cricca m =
  match cricca with
  [] -> false
  | n::resto -> if not(vicini grafo n m) then true
                else massimale grafo resto m
;;

exception NessunaCricca;;
exception ErroreDimensione;;

let rec stampalista = function [] -> print_newline()
    | x::rest -> print_int(x); print_string("; "); stampalista rest
;;

(*ricerca in ampiezza nel grafo dei cammini la possibile cricca*)
(*prende come parametri un grafo, un valore k e un nodo iniziale, utilizza
   la funzione ausiliare cerca per effettuare la bfs;
   ritorna una lista contenente i nodi della cricca di dimensione k*)
(*'a graph -> int -> 'a list -> 'a list*)
let bfs (Grafo arcs) k inizio =
  let rec cerca visitati cricca = function
    [] -> raise NessunaCricca
    | n::resto -> if (List.length cricca) = k then cricca
                  else if List.mem n visitati then cerca visitati cricca resto
                  else if massimale (Grafo arcs) cricca n
                    then cerca (n::visitati) cricca (resto @ (successori (Grafo arcs) n))
                  else cerca (n::visitati) (n::cricca) (resto @ (successori (Grafo arcs) n))
  in cerca [] [] [inizio]
;;

(*bfs con uso di funzione di stampalista interna*)
let stampabfs (Grafo arcs) k inizio =
  let rec cerca visitati cricca = function
    [] -> raise NessunaCricca
    | n::resto -> stampalista visitati;
                  stampalista cricca;
                  print_newline ();
                  if (List.length cricca) = k then cricca
                  else if List.mem n visitati then cerca visitati cricca resto
                  else if massimale (Grafo arcs) cricca n
                    then cerca (n::visitati) cricca (resto @ (successori (Grafo arcs) n))
                  else cerca (n::visitati) (n::cricca) (resto @ (successori (Grafo arcs) n))
  in cerca [] inizio[(List.hd inizio)]
;;

(*funzione che cerca una cricca di un grafo a partire da uno specifico nodo*)
(*'a graph -> int -> 'a -> 'a list*)
let cerca_cricca_da (Grafo arcs) k inizio =
  if k < 2 then raise ErroreDimensione
  else stampabfs (Grafo arcs) k inizio
;;

(*cerca_bfs prende in input un grafo, un valore int k, e una lista di nodi;
   cerca una cricca tramite la bfs partendo dal primo nodo della lista nodi input,
   per poi eseguirla sul resto della lista; ritorna una lista contentente i nodi della cricca*)
(*'a graph -> int -> 'a list -> 'a list*)
let rec cerca_bfs grafo k = function
  [] -> raise NessunaCricca
  | n::resto -> try List.rev (stampabfs grafo k n)
                with NessunaCricca -> cerca_bfs grafo k resto
;;

(*cerca un cricca in grafo: dato un grafo e un valore int k, applica cerca_bfs*)
(*'a graph -> int -> 'a list*)
let cerca_cricca grafo k =
  if k < 2 then raise ErroreDimensione
  else cerca_bfs grafo k (nodi grafo)
;;

cerca_cricca grafo1 4;;
