#load "str.cma";;

(* Structure document XML *)

type id= string ;;
type balises= OUVRANTE of id
	      |FERMANTE of id;;
type documentXML= DOCUMENTXML of (entry list)
and entry= DATA of string
	   |ENTRY of balises*(entry list);;
(*
<contacts>
 <prenom> Jimmy </prenom>
<ville> olivet </ville>
 </contact>
*)
let essai= DOCUMENTXML [
ENTRY (OUVRANTE "contacts",[
ENTRY (OUVRANTE "prenom",[DATA "Jimmy"]);
ENTRY (FERMANTE "prenom",[DATA ""]);
ENTRY (OUVRANTE "ville",[DATA "olivet"]);
ENTRY (FERMANTE "ville",[DATA ""])] );
ENTRY (FERMANTE "contacts",[DATA ""])];;
(* Structure de la DTD *)

type elements= ELEMENTS of occurence*atom
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
and model= EMPTY | PCDATA of string | MODEL of elements;;
 
 (* Quelques Exceptions *)
 exception Fail of string;;
 let error_xml= Fail "Not a XML file";;
 let error_dtd= Fail "Not a DTD file";;
#load "str.cma";;

(* Structure document XML *)

type id= string ;;
type balises= OUVRANTE of id
	      |FERMANTE of id;;
type documentXML= DOCUMENTXML of (entry list)
and entry= DATA of string
	   |ENTRY of balises*(entry list);;
(*
<contacts>
 <prenom> Jimmy </prenom>
<ville> olivet </ville>
 </contact>
*)
let essai= DOCUMENTXML [
ENTRY (OUVRANTE "contacts",[
ENTRY (OUVRANTE "prenom",[DATA "Jimmy"]);
ENTRY (FERMANTE "prenom",[DATA ""]);
ENTRY (OUVRANTE "ville",[DATA "olivet"]);
ENTRY (FERMANTE "ville",[DATA ""])] );
ENTRY (FERMANTE "contacts",[DATA ""])];;
(* Structure de la DTD *)

type elements= ELEMENTS of occurence*atom
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
and model= EMPTY | PCDATA of string | MODEL of elements;;
 
 (* Quelques Exceptions *)
 exception Fail of string;;
 let error_xml= Fail "Not a XML file";;
 let error_dtd= Fail "Not a DTD file";;

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
 rules_xml  ["<";"contacts";">";"<";"contact";">";"eee";"<";"prenom";">";"Jimmy";"</";"prenom";">"
;"<";"ville";">";"Olivet";"</";"ville";">";"</";"contact";">";"</";"contacts";">"];;
