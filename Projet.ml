#load "str.cma";;

(* Structure document XML *)

type id= string ;;
type balises= OUVRANTE of id
	      |FERMANTE of id;;
type documentXML= DOCUMENTXML of (entry list)
and entry= DATA of string
	   |ENTRY of balises*(entry list);;

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
