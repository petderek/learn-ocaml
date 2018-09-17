type trie =
    | Node of char * (char * trie) list (* node has a character, and a association list of children *)
    | TerminalNode of char * (char * trie) list (* node has a character, and a list of children *)
    | RootNode of (char * trie) list
    | Empty

let getChildren = function 
    | Node (_, c) | TerminalNode (_, c) | RootNode c -> c
    | Empty -> [];;

let rec insert (trie:trie) (word:string) = 
    match (String.length word) with
    | 0 -> trie
    | _ -> (
            let firstLetter = String.get word 0 in
            let isTerminal = String.length word = 1 in
            let remnant = String.sub word 1 ((String.length word)-1) in
            let childs: (char * trie) list = getChildren trie in
            let node:trie = match (List.assoc_opt firstLetter childs) with
                | Some (Node (c,children) as node) -> if isTerminal then TerminalNode (c,children) else node
                | Some (TerminalNode _ as node) -> node
                | None -> if isTerminal 
                    then TerminalNode (firstLetter, [])
                    else Node (firstLetter, []) in
            let newChildren = (firstLetter, (insert node remnant)) :: (List.remove_assoc firstLetter childs) in
            match trie with 
                | Node (c, _) -> Node (c, newChildren)
                | TerminalNode (c, _) -> TerminalNode (c, newChildren)
                | Empty | RootNode _ -> RootNode newChildren
    );;

let rec printTrie trie = match trie with
    | Empty -> "Empty Tree, but heres an emoji: 🍂"
    | RootNode children -> String.concat ", " (List.map (fun (a,t) -> printTrie t) children)
    | Node (c, children) -> (String.make 1 c) ^ (String.concat ", " (List.map (fun (a,t) -> printTrie t) children))
    | TerminalNode (c, children) -> (String.make 1 c) ^ " and its a word" ^ (String.concat ", " (List.map (fun (a,t) -> printTrie t) children));;

let rec hasPrefix trie word = 
    let first = String.get word 0 in
    let remnant = String.sub word 1 ((String.length word)-1) in
    let kids = getChildren trie in
    match List.assoc_opt first kids with
        | None -> false
        | Some node -> if remnant = "" then true else (hasPrefix node remnant);;

let rec hasWord trie word =
    let first = String.get word 0 in
    let remnant = String.sub word 1 ((String.length word)-1) in
    let kids = getChildren trie in
    if remnant <> "" then match List.assoc_opt first kids with
        | None -> false
        | Some node -> (hasWord node remnant)
    else match List.assoc_opt first kids with
        | Some (TerminalNode _) -> true
        | _ -> false;;

let tree = insert (insert (insert (insert Empty "helo") "helicopter") "jake") "jakiro";;

print_endline ("Look: " ^ (printTrie tree));;

print_endline (Printf.sprintf "Do we have a prefix: %B" (hasPrefix tree "helic"));;

print_endline (Printf.sprintf "Do we have a word: %B" (hasWord tree "jakiro"));;

print_endline (Printf.sprintf "Do we have a word? We shouldn't: %B" (hasWord tree "jakir"));;


