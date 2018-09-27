type trie =
    | Node of char * trie list (* node has a character, and a association list of children *)
    | TerminalNode of char * trie list (* node has a character, and a list of children *)
    | RootNode of trie list
    | Empty

let getChildren = function 
    | Node (_, c) | TerminalNode (_, c) | RootNode c -> c
    | Empty -> [];;

let getChar = function 
    | Node (c, _) | TerminalNode (c, _)  -> c
    | _ -> failwith "should never happen";;

(* given a list of nodes, find the one that has the char in question. Return None if not found *)
let getNodeForChar char = List.find_opt (fun node -> char = (getChar node));;

let getChild char trie = getChildren trie |> (getNodeForChar char);;

let removeNodeWithChar char = List.filter (fun node -> char <> (getChar node));;

let murderChild char trie = getChildren trie |> (removeNodeWithChar char);;

(* split str into a tuple composed of the first letter and the rest.
 * For example, chop "hello" -->  ('h', "ello")
 *)
let chop str = (String.get str 0, String.sub str 1 ((String.length str)-1));;

let rec insert (trie:trie) (word:string) = 
    match (String.length word) with
    | 0 -> trie
    | _ -> (
            let (firstLetter, remnant) = chop word in
            let isTerminal = String.length word = 1 in
            let node:trie = match (getChild firstLetter trie) with
                | Some (Node (c,children) as node) -> if isTerminal then TerminalNode (c,children) else node
                | Some (TerminalNode _ as node) -> node
                | None -> if isTerminal 
                    then TerminalNode (firstLetter, [])
                    else Node (firstLetter, [])
                | _ -> failwith "should never happen" in
            let newChildren =  (insert node remnant) :: (murderChild firstLetter trie) in
            match trie with 
                | Node (c, _) -> Node (c, newChildren)
                | TerminalNode (c, _) -> TerminalNode (c, newChildren)
                | Empty | RootNode _ -> RootNode newChildren
    );;

let toStr c = String.make 1 c;; 
let combine = String.concat ", ";;
let flatMap fn trie = (List.map fn trie) |> List.flatten;;

(* 
  fn x --> [ x, 2x, 3x ]
  List.map fn [ 1,2 3 ] ----> [ [1,2,3], [2,4,6], [3,6,9] ]
  List.flatMap fn [ 1,2,3,2,4,6,3,6,9]
 *)

let rec getWords ?(acc="") (trie:trie) = match trie with 
    | Empty -> []
    | RootNode children -> flatMap getWords children 
    | Node (c, children) -> flatMap (getWords ~acc:(acc ^ (toStr c))) children
    | TerminalNode (c, children) -> 
        let word = acc ^ (toStr c) in
        word :: (flatMap (getWords ~acc:word) children)
        ;;

let rec printTrie ?(acc="") (trie:trie) = match getWords trie with
    | [] -> "Empty Tree, but heres an emoji: 🍂"
    | lst -> combine lst;;

let rec hasPrefix trie word = 
    let (first,remnant) = chop word  in
    match getChild first trie with
        | None -> false
        | Some node -> if remnant = "" then true else (hasPrefix node remnant);;

let rec hasWord trie word =
    let (first,remnant) = chop word  in
    if remnant <> "" then match getChild first trie with
        | None -> false
        | Some node -> (hasWord node remnant)
    else match getChild first trie with
        | Some (TerminalNode _) -> true
        | _ -> false;;

let makeTree = List.fold_left (fun acc elem -> insert acc elem) Empty;;


let tree = makeTree  [ "cat"; "dot"; "jake"; "hi"; "ok";];;
(* 
 C A T
 B G O
 R K D
*)

let board = [ 
    ['C'; 'A'; 'T';]; 
    ['B'; 'G'; 'O';]; 
    ['O'; 'K'; 'D';]; 
];;


(* let rec findForChar board visited x y = 
    if visited[x][y] do nothing *)

