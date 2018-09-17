type trie =
    | Node of char * (char * trie) list (* node has a character, and a association list of children *)
    | TerminalNode of char * (char * trie) list (* node has a character, and a list of children *)
    | RootNode of (char * trie) list
    | Empty

let insert trie word = 
    let firstLetter = String.get word 1 in
    match (String.length word) with
    | 0 -> ();
    | _ -> (
            let isTerminal = String.length word = 1 in
            let remnant = String.sub word 1 ((String.length word)-1) in
            let node = match List.assoc_opt firstLetter trie with
                | Some (c,children) as node -> if isTerminal 
                    then TerminalNode c,children
                    else node
                | None -> if isTerminal 
                    then TerminalNode firstLetter, [] 
                    else Node firstLetter, [] in
            let newChildren = Node (List.remove_assoc firstLetter) @ (insert node remnant) in
            match trie with 
                | Node c,_ -> Node c,newChildren
                | TerminalNode c,_ -> TerminalNode c,newChildren
                | Empty | RootNode _ -> RootNode newChildren
           (* TerminalNode firstLetter *  *)
    )

