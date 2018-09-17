type trie =
    | Node of char * (char * trie) list (* node has a character, and a association list of children *)
    | TerminalNode of char * (char * trie) list (* node has a character, and a list of children *)
    | RootNode of (char * trie) list
    | Empty

let rec insert (trie:trie) (word:string) = 
    match (String.length word) with
    | 0 -> Node ('q', []);
    | _ -> (
            let firstLetter = String.get word 0 in
            let isTerminal = String.length word = 1 in
            let remnant = String.sub word 1 ((String.length word)-1) in
            (print_endline ((String.make 1 firstLetter)));
            let childs: (char * trie) list = match trie with
                | Node (_, children) -> children
                | TerminalNode (_, children) -> children
                | RootNode children -> children
                | Empty -> [] in
            let node:trie = match (List.assoc_opt firstLetter childs) with
                | Some (Node (c,children) as node) -> if isTerminal 
                    then TerminalNode (c,children)
                    else node
                | Some (TerminalNode _ as node) -> node
                | None -> if isTerminal 
                    then TerminalNode (firstLetter, [])
                    else Node (firstLetter, []) in
            let moddedNode = if remnant = "" then node else (insert node remnant) in
            let newChildren = (firstLetter, moddedNode) :: (List.remove_assoc firstLetter childs) in
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


let tree = insert (insert Empty "helo") "jake";;

print_endline ("Look: " ^ (printTrie tree));;



