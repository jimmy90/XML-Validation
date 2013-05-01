#load "str.cma";;
                                         (* LECTURE DE FICHIER, EXISTENCE *)
 exception Fail of string;;
 let error_dtd= Fail "Not a DTD file";;
 let exist_file s= Sys.file_exists s;;
type split_result= Delim of string | Text of string;;
let rec string_of_result l=
match l with 
[]->[]
  |(Str.Delim a)::b -> a::(string_of_result b)
  |(Str.Text a)::b -> a::(string_of_result b) ;;
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


                                    (* TYPAGE DES DIFFERENTS DOCUMENTS (XML & DTD)  *)
(* XML *)
type documentXML= entry list
and entry=DATA of string
	   |ENTRY of string*(entry list);;

(* DTD *)
type ide=string;;
type elements= ELEMENTS of (occurrence*atom)list
	      |ALL of (occurrence * atom) list
	      |ONE_OF_ALL of (occurrence * atom) list 
and occurrence= ATOM1 
	       | ATOM_ADD 
	       | ATOM_MULT 
	       | ATOM_01
and atom= IDENTIFIANT of ide 
	  |ELEMENT of elements;;

type documentDTD = (description list)
and description= string * model
and model= EMPTY | PCDATA | MODEL of elements;;

                                          (* VERIFICATION SYNTAXIQUE XML *)
(* Concatenation des elements de la liste en une chaine *)
let rec getXmlString l = match l with
  [] -> ""
  | hd::tl -> (hd^(getXmlString tl));;
(* String privé de son 1er caractère *)
let without_the_first s=(String.sub s 1 ((String.length s)-1));;

(* Rassemble les chevrons et le slash *)
let rec rassembleSlash l = match l with
  [] -> []
  | hd::tl when (hd = "<") -> 
    begin
      match tl with 
	  [] -> [hd]
	|t::q when (String.sub t 0 1)="/" -> "</"::(rassembleSlash ((without_the_first t)::q))
	|t::q -> hd::(rassembleSlash (t::q))
    end 
  | hd::tl -> if ((String.sub hd 0 1)="/") then "/"::(without_the_first hd)::(rassembleSlash tl) else hd::(rassembleSlash (tl));;

let getListXml xml =rassembleSlash ( string_of_result(Str.full_split(Str.regexp "[<>]") (getXmlString(read_file xml))));;

(* Verification de la structure du fichier xml  *)

 let depiler l= (List.tl l);; (* Depiler une pile en retirant la tete *)
 let rules_xml liste=
   let rec aux stack ops l= (* l est la liste à evaluer; stack est la pile d'expression contenant le nom des balises; ops est la pile contenant les symboles du genre "<" "</" *)
     match l with
	 []-> if ((stack=[])&&(ops=[])) then true else false
       |"<"::t-> if (ops=[]) then (aux stack ("<"::ops) t) else false
       |">"::t-> if ((stack=[])&&(ops=[])) then false else  if ((List.hd ops)="<") then (aux stack (depiler ops) t) else false
       |"</"::t-> if ((ops<>[])&&(stack=[])) then false else 
	   begin  match t with
	       []-> false
	     |a::[]-> false
	     |a::">"::c -> if (a=(List.hd stack)) then (aux (depiler stack) ops c) else false
	     |_-> false
	   end 
       |n::">"::t -> if (ops=[]) then false else (aux (n::stack) ops (">"::t))
       |n::"<"::t -> if ((ops=[])&&(stack<>[])) then (aux stack ops ("<"::t)) else false
       |n::"</"::t ->  if ((ops=[])&&(stack<>[])) then (aux stack ops ("</"::t)) else false 
       |_-> false
   in aux [] [] liste;;
                                          (* CONSTRUCTION DE L'ARBRE XML *)
 let getFullEntry xml= let rec aux xml l m=
match xml with
[]->[]
  |[hd] -> [DATA hd]
  |"<"::hd::">"::tl -> if (l=[])&&(m=[]) then aux tl l (hd::m) else aux tl (l@["<";hd;">"]) m
  |"</"::hd::">"::tl -> if (List.hd m)=hd then (ENTRY (hd, aux l [] []))::aux tl [] [] else aux tl (l@["</";hd;">"]) m
  |hd::tl -> aux tl (l@[hd]) m 
									   in aux (getListXml xml) [] [];;
 let getFullXml xml= ((getFullEntry xml):documentXML);;

                                          (* CONSTRUCTION DE L'ARBRE DTD *)

let rec getLastElement l = match l with
  [] -> ""
  | hd::[] -> hd
  | hd::tl -> getLastElement tl;;

(*Renvois un couple (ATOM, id) à partir de, par exemple, telephone?*)
let getCoupleDtd s = match (Str.last_chars s 1) with
  "?" -> (ATOM_01, IDENTIFIANT((Str.string_before s ((String.length s)-1))))
  | "*" -> (ATOM_MULT, IDENTIFIANT(  (Str.string_before s ((String.length s)-1))))
  | "+" -> (ATOM_ADD, IDENTIFIANT(  (Str.string_before s ((String.length s)-1))))
  | _ -> (ATOM1, IDENTIFIANT( (Str.string_before s ((String.length s)))));;


(*Renvois une liste des couples ( par exemple, à partir de (prenom,nom,telephone?)) *)
let rec getListElement l = match l with
  [] -> []
  | (Str.Delim hd)::tl -> getListElement tl
  | (Str.Text hd)::tl -> (getCoupleDtd hd)::(getListElement tl);;

let getListComplete l ( id)= 
  match l with 
    [] -> raise error_dtd
    | (Str.Text hd)::tl when hd="#PCDATA" -> ((id, PCDATA):description)
    |hd::[] -> (( id, MODEL (ELEMENTS (getListElement l))):description)
    |hd::tl -> let lDelim = (List.hd (List.tl l)) in match l with
        (Str.Text hd)::tl when lDelim=(Str.Delim ",") -> (( id, MODEL (ALL (getListElement l))):description)
        | (Str.Text hd)::tl when lDelim=(Str.Delim "|") -> ((id, MODEL (ONE_OF_ALL (getListElement l))):description)
        |  _ -> ((id, MODEL (ELEMENTS (getListElement l))):description) ;;

let getLineDtd s = 
let base = Str.split(Str.regexp "[< >]") s in 
let  id =  (List.hd (List.tl base)) in 
let d1 = Str.split(Str.regexp "[()]") (getLastElement base) in 
let d2 = ( (Str.full_split(Str.regexp "[,|]") (List.hd  d1))) in (getListComplete d2 (id));; 




let rec generateDtd l = match l with 
      [] ->  []
      | hd::tl -> (getLineDtd hd)::(generateDtd tl);;

let getFullDtd f =((generateDtd (read_file f)):documentDTD);;
                                       (*  VERIFICATION STRUCTURELLE XML-DTD *)
                 (* Fonctions intermediaires *)
(* Methodes retirant des doublons ou une seule occurence*)
let rec remove_one_occur s l = match l with []->[] | a::b -> if a=s then remove_one_occur s b else a::(remove_one_occur s b);;
let rec remove_doublons s =match s with []-> [] | a::b -> a::(remove_doublons (remove_one_occur a b));;

(* Retourne le nom des balises contenues dans une entry list *)
let rec retrouver xml=
match xml with
  |[]->[]
  |[DATA a] -> []
  |(ENTRY (a,b))::t-> [a]@(retrouver t)
  |_ -> failwith "00";;
(* Retrouve toutes les balises dans le document XML *)
let retrouver_toutXml (xml:documentXML)=
remove_doublons(let rec aux liste res= 
  match liste with   
      [] -> res 
      |DATA(x)::q -> aux q (res) 
      |ENTRY(x,y)::q -> aux q (x::(aux y (res)))
           in aux xml []) ;;
let retrouver_toutXml_doublons (xml:documentXML)=
let rec aux liste res= 
  match liste with   
      [] -> res 
      |DATA(x)::q -> aux q (res) 
      |ENTRY(x,y)::q -> aux q (x::(aux y (res)))
           in aux xml [] ;;

(* Retourne les balises contenue dans s sans les doublons *)
let rec contenu_balise (xml:documentXML) s=
remove_doublons(match xml with
[]->[]
  |[DATA a] -> []
  |(ENTRY (a,b))::t-> if (a=s) then retrouver b else (contenu_balise b s)@(contenu_balise t s)
  |_-> []);;

(* Nombre de balises dans une balise *)
let nb_atome (xml:documentXML) s= let aux l = (List.length l) in aux (contenu_balise xml s);;

(* Nombre d'apparition d'une balise dans un document xml *)
let nb_occurence (xml:documentXML) s= let rec aux xml s res=
match xml with
[]->res 
  |a::b -> if a=s then aux b s (res+1) else aux b s res  
			in aux (retrouver_toutXml_doublons xml) s 0;;
let rec in_the s l=match l with []->false | a::b -> if a=s then true else in_the s b;;

(* Retrouve tous les id dans un document DTD *)
let rec retrouver_toutDtd (dtd:documentDTD) = 
remove_doublons( match dtd with 
  |[] -> [] 
  |(x,_)::q-> x::(retrouver_toutDtd q));;

(* Test pour savoir si 2 listes comptent les memes elements *)
let equal_list (xml:documentXML) (dtd:documentDTD)=
if (List.length (retrouver_toutXml xml))<>(List.length (retrouver_toutDtd dtd)) then false else
    let rec aux x d= match x with
	[]->true
      |a::b -> if (in_the a d) then aux b d else false in aux (retrouver_toutXml xml) (retrouver_toutDtd dtd)  ;;

(* Validation des elements de type ALL & ONE_OF_ALL *)
let rec traite_all d xml x=
match d with
[]->true
  |(ATOM1, IDENTIFIANT i)::q->if (in_the i (contenu_balise xml x)) then (traite_all q xml x) else false
  |(ATOM_ADD,IDENTIFIANT i)::q->if (in_the i (contenu_balise xml x))&&((nb_atome xml i)>0) then (traite_all q xml x) else false
  |(ATOM_01,IDENTIFIANT i)::q-> if (in_the i (contenu_balise xml x))&&((nb_occurence xml i)<=1) then(traite_all q xml x) else false
  |(ATOM_MULT,IDENTIFIANT i)::q->if in_the i (contenu_balise xml x) then (traite_all q xml x) else false
  |_ -> false ;;
let rec traite_or d xml x=
match d with
[]->false
|(ATOM1, IDENTIFIANT i)::q->if (in_the i (contenu_balise xml x)) then true else false||(traite_or q xml x)
  |(ATOM_ADD,IDENTIFIANT i)::q->if (in_the i (contenu_balise xml x))&&((nb_atome xml i)>0) then true else false||(traite_or q xml x)
  |(ATOM_01,IDENTIFIANT i)::q-> if (in_the i (contenu_balise xml x))&&((nb_occurence xml i)<=1) then true else false||(traite_or q xml x)
  |(ATOM_MULT,IDENTIFIANT i)::q->if in_the i (contenu_balise xml x) then true else false||(traite_or q xml x)
  |_ -> false ;;

                      (*Validation complete*)
let validation xml dtd= let rec aux xml dtd =
(*(equal_list (getFullXml "testXml.txt") (getFullDtd "dtdTest.txt"))&&*)(match dtd with 
[]-> true
    |(x, PCDATA)::q -> if (contenu_balise xml x)=[] then aux xml q else false
    |(x, (MODEL (ALL y)))::q -> if (traite_all y xml x)&&((List.length y)<=(nb_atome xml x)) then aux xml q else false
   |(x, (MODEL (ONE_OF_ALL y))):: q-> if (traite_or y xml x) then aux xml q else false
    |(x, (MODEL (ELEMENTS y)))::q ->
      begin match y with 
	[]-> aux xml q
	    |[(ATOM_MULT,IDENTIFIANT i)]->if (contenu_balise xml x)=[i] then aux xml q else false
	    |[(ATOM_ADD,IDENTIFIANT i)]->if ((contenu_balise xml x)=[i])&&((nb_atome xml i)>0) then aux xml q else false
	    |[(ATOM_01,IDENTIFIANT i)]-> if ((contenu_balise xml x)=[i])&&((nb_occurence xml i)<=1) then aux xml q else false
	    |_ -> false
      end
    |_ -> false)
in aux (getFullXml xml) (getFullDtd dtd);;
 

if((Array.length Sys.argv) = 3) then 
  begin if(exist_file (Sys.argv.(1)) && exist_file(Sys.argv.(2))) then
    begin if(rules_xml (getListXml Sys.argv.(1))) then
      begin if(validation Sys.argv.(1) Sys.argv.(2)) then
        print_string "Validation reussie"
      else
        print_string "Echec de la validation"
      end
    else
      print_string "Echec de la validation"
    end
  else
    print_string "Fichiers non present"
  end
else
  print_string "Syntaxe : ocaml Projet.ml XML DTD";;

