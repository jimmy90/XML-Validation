
#load "str.cma";;

(* Structure document XML *)

type id= (*ID of*) string ;;
type balises= OUVRANTE of id
	      |FERMANTE of id;;
type documentXML= DOCUMENTXML of (entry list)
and entry= DATA of string
	   |ENTRY of balises*(entry list);;
(*
<contacts>
	<contact>
 		<prenom> Jimmy </prenom>
		<ville> olivet </ville>
	<contact>
 </contacts>
*)
let essai= DOCUMENTXML [
ENTRY (OUVRANTE "contacts",[
ENTRY (OUVRANTE "contact",[
ENTRY (OUVRANTE "prenom",[DATA "Jimmy"]);
ENTRY (FERMANTE "prenom",[DATA ""]);
ENTRY (OUVRANTE "ville",[DATA "olivet"]);
ENTRY (FERMANTE "ville",[DATA ""])] );
ENTRY (FERMANTE "contact", [DATA ""])]);
ENTRY (FERMANTE "contacts",[DATA ""])];;


(* Structure de la DTD *)

type elements= ELEMENTS of (occurence*atom) list
	      |ALL of (occurence * atom) list
	      |ONE_OF_ALL of (occurence * atom) list 
and occurence= ATOM1 
	       | ATOM_ADD 
	       | ATOM_MULT 
	       | ATOM_01
and atom= IDENTIFIANT of id 
	  |ELEMENT of elements;;

type documentDTD = DOCUMENTDTD of (description list)
and description= id * model
and model= EMPTY | PCDATA | MODEL of elements;;
 
let dtd_expl = DOCUMENTDTD [
("contacts", MODEL (ELEMENTS [(ATOM_MULT, IDENTIFIANT "contact")])); 
("contact", MODEL (ALL [(ATOM1, IDENTIFIANT "prenom"); (ATOM1, IDENTIFIANT "ville")]));
("prenom", PCDATA);
("ville", PCDATA) ];;



 (* Quelques Exceptions *)
 exception Fail of string;;
 let error_xml= Fail "Not a XML file";;
 let error_dtd= Fail "Not a DTD file";; (*Inutile nan? Vu qu'on suppose la DTD correcte*)
 
(* Verification de la structure du fichier xml  *)
(* Explications plus détaillé de la méthode à venir  *)
 let depiler l= (List.tl l);; (* Depiler une pile en retirant la tete *)
 let rules_xml liste=
   let rec aux stack ops l= (* l est la liste à evaluer; stack est la pile d'expression contenant le nom des balises; ops est la pile contenant les symboles du genre "<" "</" *)
     match l with
	 []-> if ((stack=[])&&(ops=[])) then true else raise error_xml
       |"<"::t-> if (ops=[]) then (aux stack ("<"::ops) t) else raise error_xml
       |">"::t-> if ((stack=[])&&(ops=[])) then raise error_xml else  if ((List.hd ops)="<") then (aux stack (depiler ops) t) else raise error_xml
       |"</"::t-> if ((ops<>[])&&(stack=[])) then raise error_xml else 
	   begin  match t with
	       []->raise error_xml
	     |a::[]->raise error_xml
	     |a::">"::c -> if (a=(List.hd stack)) then (aux (depiler stack) ops c) else raise error_xml
	     |_-> raise error_xml
	   end 
       |n::">"::t -> if (ops=[]) then raise error_xml else (aux (n::stack) ops (">"::t))
       |n::"<"::t -> if ((ops=[])&&(stack<>[])) then (aux stack ops ("<"::t)) else raise error_xml  (*Si dans la liste donné en exemple la place de "eee" rend 
le fichier xml faux alors supprimer cette ligne svp dans le cas contraire laisser la ligne  *)
       |n::"</"::t ->  if ((ops=[])&&(stack<>[])) then (aux stack ops ("</"::t)) else raise error_xml 
       |_-> raise error_xml
   in aux [] [] liste;;
 let exemple=  ["<";"contacts";">";"<";"contact";">";"eee";"<";"prenom";">";"Jimmy";"</";"prenom";">"
;"<";"ville";">";"Olivet";"</";"ville";">";"</";"contact";">";"</";"contacts";">"];;
rules_xml exemple;;
 let rec nom_balises l=
   match l with 
       []->[]
     |"<"::a -> (List.hd a)::(nom_balises (List.tl a))
     |"</"::a -> (List.hd a)::(nom_balises (List.tl a))
     |a::b -> (nom_balises b);;

nom_balises exemple;;

(* Methode permettant de retirer la 1ere occurence de s dans une liste l *)

 let rec remove s l=
   match l with
       []->[]
     |a::b when a=s -> b
     |a::b -> a::(remove s b);;

(* Méthode permettant de retourner  la tete si celle-ci n'est pas identique à l'element suivant,  si la tete et le suivant sont 
identiques on renvoie une liste contenant la tete et on continue avec le meme principe pour la suite
 - Exemple: si on a une liste l= ["m";"b";"v";"v";"r";"d"] la fonction renverra ["m"] et si on a une liste l2=["m";"m";"s";"s";"t";"p";"j";"t"] elle renverra
 ["m","s","t"] *)

 let rec sous_liste r =
   match r  with
     |[]-> []
     |a::b -> begin 
       match b with
	   []->[]
	 |c::d -> if (c=a) then (a::(sous_liste d)) else [a]
     end;;
(* l'argument m est le nom de la balise *)
 let sous_liste_final l m=
   let rec aux liste  m=
     match liste with 
	 []->[]
       |t::q -> if (t=m) then [] else t::(aux q m) in aux (sous_liste l) m;; 

(* Méthode permettant de determiner le nombre d'apparition d'une balise dans un doccument xml *)

 let nb_apparition a l=
   let rec aux a l res=
     match l with 
	 []-> (res/2)
       |t::q -> if (t=a) then (aux a q (res+1)) else (aux a q res) in aux a l 0;;

(* Méthode permettant de resumer un document xml à peu pres à l'image d'une DTD et donne le nombre d'apparition de chaque balise*)

 let resume_xml l=
   let rec aux liste=
   match liste with
       []->[]
     |a::b::[]->[(a,(["PCDATA"],(nb_apparition a l)))]
     |a::b -> if (List.hd b)=a then (a,(["PCDATA"],(nb_apparition a l)))::(aux (List.tl b)) else (a,((sous_liste_final b a),(nb_apparition a l)))::(aux (remove a b)) in aux (nom_balises l);;

(* Supprime les doublons c'est à dire chaque couple obtenu par par la fonction précedente qui se retrouve au minimum 2 est suprimé de telle sorte qu'il 
reste qu'une seule occurence *)

 let resume_xml_final l=
   let rec aux liste=
   match liste with 
       []->[]
     |a::b -> a::(aux (remove a b)) in aux (resume_xml l);;

(* Exemple de ce que fait la fonction *)
(* L' argument de la fonction ci dessous (<<exemple>>) est une variable globale définit plus haut *)

resume_xml_final ["<";"contacts";">";"<";"contact";">";"<";"prenom";">";"Jimmy";"</";"prenom";">";"<";"ville";">";"Olivet";"</";"ville";">";"</";"contact";">";"<";"contact";">";"<";"prenom";">";"Tennessy";"</";"prenom";">";"<";"ville";">";"Courtenay";"</";"ville";">";"</";"contact";">";"</";"contacts";">"] ;;


(*
(*Creation de la DTD à partir d'un fichier *)

let rec getLastElement l = match l with
  [] -> ""
  | hd::[] -> hd
  | hd::tl -> getLastElement tl;;

(*Renvois un couple (ATOM, id) à partir de, par exemple, telephone?*)
let getCoupleDtd s = match (Str.last_chars s 1) with
  "?" -> (ATOM_01, IDENTIFIANT(ID (Str.string_before s ((String.length s)-1))))
  | "*" -> (ATOM_MULT, IDENTIFIANT(ID  (Str.string_before s ((String.length s)-1))))
  | "+" -> (ATOM_ADD, IDENTIFIANT(ID  (Str.string_before s ((String.length s)-1))))
  | _ -> (ATOM1, IDENTIFIANT(ID  (Str.string_before s ((String.length s)))));;

(*Renvois une liste des couples ( par exemple, à partir de (prenom,nom,telephone?)) *)
let rec getListElement l = match l with
  [] -> []
  | (Str.Delim hd)::tl -> getListElement tl
  | (Str.Text hd)::tl -> (getCoupleDtd hd)::(getListElement tl);;

let getListComplete l ( id)= 
  match l with 
    [] -> raise error_dtd
    |hd::[] -> (ID id, MODEL (ELEMENTS (getListElement l))) 
    |hd::tl -> let lDelim = (List.hd (List.tl l)) in match l with
     (Str.Text hd)::tl when lDelim=(Str.Delim ",") -> (ID id, MODEL (ALL (getListElement l)))
    | (Str.Text hd)::tl when lDelim=(Str.Delim "|") -> (ID id, MODEL (ONE_OF_ALL (getListElement l)))
    | (Str.Text hd)::tl when hd="#PCDATA" -> (ID id, PCDATA)
    |  _ -> (ID id, MODEL (ELEMENTS (getListElement l))) ;;

let getLineDtd s = 
let base = Str.split(Str.regexp "[< >]") s in 
let  id =  (List.hd (List.tl base)) in 
let d1 = Str.split(Str.regexp "[()]") (getLastElement base) in 
let d2 = ( (Str.full_split(Str.regexp "[,|]") (List.hd  d1))) in (getListComplete d2 (id));; 

let read_file filename = 
  let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;



let rec generateDtd l = match l with 
      [] ->  []
      | hd::tl -> (getLineDtd hd)::(generateDtd tl);;

let getFullDtd f = DOCUMENTDTD (generateDtd (read_file f));;
*)