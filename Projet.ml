
#load "str.cma";;
                (*  LECTURE FICHIER & EXCEPTION  *)

 exception Fail of string;;
 let error_xml= Fail "Not a XML file";;
 let error_dtd = Fail "Error DTD";;

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


(* Structure document XML *)

type documentXML= DOCUMENTXML of (entry list)
and entry=Vide  |DATA of string
	   |ENTRY of string*(entry list);;
(*
<contacts>
	<contact>
 		<prenom> Jimmy </prenom>
		<ville> olivet </ville>
                 <tel> 0600000 </tel>
	<contact>
        <contact>
                <prenom> Ten </prenom>
		<ville> Courtenay </ville>
        </contact>
 </contacts>
*)
let essai= DOCUMENTXML[ENTRY ( "contacts",[ENTRY ("contact",[ENTRY ("prenom",[DATA "Jimmy"]);ENTRY ("ville",[DATA "Olivet"]);ENTRY ("tel",[DATA "06000000"])]);
ENTRY ("contact",[ENTRY ("prenom",[DATA "Ten"]);ENTRY ("ville",[DATA "Courtenay"])])])];;
                                     (* VERIFICATION DE LA SYNTAXE *) 
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
 let exemple= getListXml "testXml.txt" ;;
rules_xml exemple;;
 let getFullEntry xml= let rec aux xml l m=
match xml with
[]->[]
  |[hd] -> [DATA hd]
  |"<"::hd::">"::tl -> if (l=[])&&(m=[]) then aux tl l (hd::m) else aux tl (l@["<";hd;">"]) m
  |"</"::hd::">"::tl -> if (List.hd m)=hd then (ENTRY (hd, aux l [] []))::aux tl [] [] else aux tl (l@["</";hd;">"]) m
  |hd::tl -> aux tl (l@[hd]) m 
									   in aux xml [] [];;
 let getFullXml xml= DOCUMENTXML [(getFullEntry xml)];;
                                                    (* STRUCTURE DE LA  DTD *)
type ide=string;;
type elements= ELEMENTS of (occurence*atom) list
	      |ALL of (occurence * atom) list
	      |ONE_OF_ALL of (occurence * atom) list 
and occurence= ATOM1 
	       | ATOM_ADD 
	       | ATOM_MULT 
	       | ATOM_01
and atom= IDENTIFIANT of ide 
	  |ELEMENT of elements;;

type documentDTD = DOCUMENTDTD of (description list)
and description= ide * model
and model= EMPTY | PCDATA | MODEL of elements;;
 
let dtd_expl = DOCUMENTDTD [
("contacts", MODEL (ELEMENTS [(ATOM_MULT, IDENTIFIANT "contact")])); 
("contact", MODEL (ALL [(ATOM1, IDENTIFIANT "prenom"); (ATOM1, IDENTIFIANT "ville")]));
("prenom", PCDATA);
("ville", PCDATA) ];;



 
                                         (*Creation de la DTD à partir d'un fichier *)

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
    | (Str.Text hd)::tl when hd="#PCDATA" -> (id, PCDATA)
    |hd::[] -> ( id, MODEL (ELEMENTS (getListElement l))) 
    |hd::tl -> let lDelim = (List.hd (List.tl l)) in match l with
        (Str.Text hd)::tl when lDelim=(Str.Delim ",") -> ( id, MODEL (ALL (getListElement l)))
        | (Str.Text hd)::tl when lDelim=(Str.Delim "|") -> (id, MODEL (ONE_OF_ALL (getListElement l)))
        |  _ -> (id, MODEL (ELEMENTS (getListElement l))) ;;

let getLineDtd s = 
let base = Str.split(Str.regexp "[< >]") s in 
let  id =  (List.hd (List.tl base)) in 
let d1 = Str.split(Str.regexp "[()]") (getLastElement base) in 
let d2 = ( (Str.full_split(Str.regexp "[,|]") (List.hd  d1))) in (getListComplete d2 (id));; 




let rec generateDtd l = match l with 
      [] ->  []
      | hd::tl -> (getLineDtd hd)::(generateDtd tl);;

let getFullDtd f = DOCUMENTDTD (generateDtd (read_file f));;



getListXml "testXml.txt";;
