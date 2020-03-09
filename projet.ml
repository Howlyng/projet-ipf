(* Les tests sont dans les commentaires *)

(*[split_string s] retourne la liste de caracteres correspondant a la chaine [s] *)
let split_string s =
  let rec aux i n =
    if (i >= n) then []
    else (String.get s i)::(aux (i+1) n)
  in
  aux 0 (String.length s);;

(* 
   [parse_input ()] lit deux lignes sur l'entree standard du programme.
   La premiere doit comporter un entier, la seconde une chaine de caracteres.
   Retourne un couple compose de l'entier de la premiere ligne et de la liste des caracteres de la seconde.

   Leve l'exception [Failure "int of string"] si la premiere ligne ne represente pas un entier.
*)
let parse_input()=
  let nb_antennas = int_of_string (read_line()) in
  let phrase = read_line () in
  (nb_antennas,split_string phrase);;


(*PHASE 1*)

(*
  char est un caractere en lettre majuscule ou ' ' dans les cas que nous utiliserons
  [letter_to_number char]  renvoie un entier qui correspond a la place de la lettre sur la roue
  letter_to_number 'T' =  20
*)
let letter_to_number char =
  if (char = ' ') then 0 else ( Char.code char - 64);; (* le -64 correspond a la différence entre les codes ascii et ce que nous voulons *)
  
(* 
   n est un entier entre 0 et 26 dans les cas que nous utiliserons
   [number_to_letter n] renvoie le caractere a la place n sur la roue
   number_to_letter 26 = 'Z'
*)
let  number_to_letter n =
  if (n = 0) then ' ' else (Char.chr (n+64));;

(*
  char est un caractere entre ' ' et 'Z' dans les cas que nous utiliserons
  n est un entier positif ( les cas négatifs ne sont pas traités car les fonctions qui appellent cette fonction ne peuvent pas avoir appeler un entier négatifs )
  [create_list char n] genere une liste de caractere de n fois le caractere char puis le caractere 'E' pour finir
  create_list 'S' 3 = ['S';'S';'S';'E']
*)
let rec create_list char n = match n with
    |0-> ['E']
    |n-> char::(create_list char (n-1));;

(*
  char est un caractere entre ' ' et 'Z' dans les cas que nous utilisons
  n un entier entre 0 et 26 dans les cas que nous utilisons
  [code_attendu char n] renvoie le code attendu pour envoyer le caractere char si la roue se trouve sur le caractere a la place n
  code_attendu 'Y' 5 = ['P';'P';'P';'P';'P';'P';'P';'E']
*)
let code_attendu char n =
  let m = letter_to_number char in
  if ((m-n) < -13) then (create_list 'N' (27-(n-m)))
  else if ((m-n) < 0) then (create_list 'P' (n-m))
  else if ((m-n) < 14) then (create_list 'N' (m-n))
  else (create_list 'P' (27-(m-n)));;

(*phrase est une liste de caractere entre ' ' et 'Z' dans les cas de l'exercice
  [phase1 phrase] renvoie le code attendu pour ecrire phrase sous forme d'une liste de caractere selon les données de l'exercice
  phase1 ['A';'B'] = ['N';'E';'N';'E']
*)
let phase1 phrase =
  let rec aux phrase n = match phrase with
    (* phrase est la phrase a ecrire, n la place de la roue*)
    |[]->[]
    |p::q -> (code_attendu p n)::(aux q (letter_to_number p))
  in
  List.concat (aux phrase 0);;

(*PHASE 2*)

(* 
   char est un caractere entre ' ' et 'Z'
   n est un entier entre 0 et  26
   [dist char n ] calcule la distance entre le caractere char et la place n de la roue 
   dist 'R' 6 = 12
*)
let dist char n =
  let m = letter_to_number char in
  if ((n-m) < -13) then (27-(m-n))
  else if ((n-m) < 0) then (m-n)
  else if ((n-m) < 14) then (n-m)
  else (27-(n-m));;

(* 
   n est un entier positif ( les fonctions appelant cette fonction ne peuvent pas appeler autre chose 
   [create_place_roue n] cree une liste de n 0 qui sont le calibrages des n roues au debut du message
   create_place_roue 10 = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
*)
let rec create_place_roue n = match n with
  |0->[]
  |n-> 0::(create_place_roue (n-1));;

(* calcul du min sur int (la fonction n'est pas typé en théorie mais on ne l'utilise jamais autrement) *)
let minimum a b = if a > b then b else a;;

(* 
   list une liste ( dans notre cas de caracteres )
   n un entier entre 0 et la taille de la liste - 1
   [select list n] renvoie l'element a la nieme position de la liste list
   retourne une exception failure si n plus grand que la taille de la liste, mais le cas ne peut pas etre appelé
   select [1;2;3] 1 = 2
*)
let rec select list n = if n = 0 then (List.hd list) else select (List.tl list) (n-1);;


(*
  char un caractere entre ' ' et 'Z'
  list_n une liste d'entier entre 0 et 26
  [dist_min char list_n] renvoie le numero de la roue a la distance minimale du caractere char
  dist_min 'T' [1;8;6;4;9;24;5] = 4
*)
let dist_min char list_n =
  let rec aux char list min m m0 = match list with
    |[]-> m0
    |p::q -> let a = dist char p in
	     if (a < min) then aux char q a (m+1) m else aux char q min (m+1) m0
  in
  aux char list_n 14 0 (-1);;

(*
  n un entier positif
  [number_to_list_of_char n] transforme un entier en une list de char correspondant aux chiffres qui composent le nombre n
  number_to_list_of_char 687 = ['6'; '8'; '7']
*)
let rec number_to_list_of_char n =
  if n<10 then [Char.chr (n+48)] else (number_to_list_of_char (n/10))@(number_to_list_of_char (n mod 10));;
    
(* liste_n une liste detaillant la position des roues
   char le caractere a envoyer
   m l'antenne/la roue equipee

   [code_attendu2 list_n char m]  renvoie le code pour afficher le caractere char
   code_attendu2 [5;8;3;21;16;9] 'G' 3 = ['S'; '1'; 'P'; 'E']
*)
let code_attendu2 list_n char m =
  let a = dist_min char list_n in
  if (a = m) then code_attendu char (select list_n m)
  else ['S']@(number_to_list_of_char a)@(code_attendu char (select list_n a));;

(*
  char est un caractere entre ' ' et 'Z' dans les cas que nous utiliserons
  list est une liste de caractere ( entre ' ' et 'Z' toujours )
  [put char list n] remplace l'element  à la place n dans la liste list par char ( dans notre cas, ce sont toujours des listes de caracteres
  renvoie une exception:failure si n superieur a la longueur de la liste
  put 'R' ['T';'Y'] 0 = ['R';'Y']
*)
let rec put char list n =
  if n=0 then char::(List.tl list) else (List.hd list)::(put char (List.tl list) (n-1));;
  
(*
  liste_n une liste detaillant la position des roues
  char un caractere entre ' ' et 'Z' dans les cas que nous utiliserons
  [bouge_roue list_n char] renvoie une liste détaillant la configuration des roues apres un tour l'envoi du caractere char
  bouge_roue [5;7;1;10;23;9] 'K' =  [5; 7; 1; 11; 23; 9]
*)
let bouge_roue list_n char =
  let a = dist_min char list_n in
  put (letter_to_number char) list_n a;;
  
(* n est un entier positif correspondant au nombre d'antenne 
   phrase est une liste de caracteres correspondant au message a envoyer
   [phase2 n phrase] renvoie le code a renvoyer pour generer la phrase avec n antennes 
   phase2 5 ['A';'Z'] = ['N'; 'E'; 'S'; '1'; 'P'; 'E']
*)
let phase2 n phrase =
  let rec aux list_n phrase m = match phrase with
      (* list_n la position des roues
	 phrase la phrase à ecrire
	 m la roue sur laquelle on est 
      *)
    |[]->[]
    |p::q-> (code_attendu2 list_n p m)::(aux (bouge_roue list_n p) q (dist_min p list_n))
  in
  if (n=1) then phase1 phrase (* si il n'y a qu'une antenne on se sert de la résolution de la phase 1*)
  else List.concat (aux (create_place_roue n) phrase 0);;

(*
  char_list une liste de caractere
  [convert_to_string char_list] converti char_list en une chaine de caractere
  convert_to_string ['N'; 'E'; 'S'; '1'; 'P'; 'E'] = "NES1PE"
*)
let convert_to_string char_list = List.fold_left (fun x y -> x^(Char.escaped y)) "" char_list;;

(*c'est la fonction finale demandé dans l'exercice : elle lit l'entrée standard et renvoie la chaine de caractere necessaire pour envoyer le message *)
let fonction_finale() =
  let (a,b) = parse_input() in (* on se sert de parse_input() pour lire l'entrée standard *)
  let c = phase2 a b in (* on genere la liste de caracteres correspondant *)
  let d = convert_to_string c in (* on genere une chaine de caractere correspondante a la reponse attendue *)
  Printf.printf "%s \n" d;; (* on renvoie sur l'entree standard *)

fonction_finale();;




