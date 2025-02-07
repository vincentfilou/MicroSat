open System.IO

let load_cnf filePath = 
    let lines = System.IO.File.ReadLines(filePath) in
        List.ofSeq(lines) 
              |> List.map (fun x -> x.Trim() )
              |> List.filter (fun x-> String.length x > 0)
              |> List.filter (fun x -> x[0] <> 'p' && x[0] <> 'c' && x[0] <> '%' && x[0] <> '0')
              |> List.map (fun x -> x.Split(' '))
              |> List.map (fun str -> (List.map (fun x-> x|>int) (List.ofArray(str)) ))
              |> List.map (fun str -> (List.filter (fun x-> x <> 0) str))

let units formula =
    formula |> (List.filter (fun f -> List.length f = 1))
            |> List.concat

let delete_clause_containing lit formula =
    formula |> List.filter (fun clause -> not(List.contains lit clause))

let remove_occurences lit formula=
    formula|> List.map (fun clause -> (List.filter (fun x-> x<>lit) clause))

let rec unit_propag formula = 
    match  ((units formula) |>List.tryHead) with
    | Some x -> (unit_propag (formula |> (delete_clause_containing x)|>remove_occurences -x))
    | None -> formula

let pure_lits formula =
    let all_lits = (formula |> List.concat) in 
    all_lits |> List.filter (fun x-> not(List.contains -x all_lits))
    
let rec pure_lits_elim formula =
    match ((pure_lits formula) |>List.tryHead) with
    | Some x -> pure_lits_elim (delete_clause_containing x formula)
    | None -> formula

let contains_empty_clause formula =
    (formula |> List.filter (fun clause -> List.length clause = 0) |> List.length) > 0

let is_empty formula = List.length formula = 0

let get_literal formula= 
    (formula |> List.concat) |> List.tryHead

let merge list_formula list_literals=
    List.append ((list_formula |> List.filter (fun formula -> List.exists (fun x -> (List.contains x list_literals)|| (List.contains -x list_literals)) (formula|> List.concat))) |> List.concat ) [list_literals]

let excluded list_formula list_literals=
    list_formula |> List.filter (fun formula -> List.forall (fun x -> not( (List.contains x list_literals) || (List.contains -x list_literals ))) (formula|> List.concat))

let rec build_partitions partition formula=
    match formula with
    |[] -> partition
    | clause::lst -> build_partitions (List.append [(merge partition clause)] (excluded partition clause)) lst

let rec split acc lst size =
    match lst with
    | [] -> [|acc|]
    | f :: lst0 -> (if List.length(acc) = size then (Array.concat [[|acc|] ;(split [] lst size)]) else (split (f::acc) lst0 size))

let rec dpll formula result =
    let f0 = unit_propag formula in
    let f1 = pure_lits_elim f0 in
    if is_empty f1 then Some result
    else (if (contains_empty_clause f1) then None
            else (match (get_literal f1) with
                    | Some x -> (match (dpll ([x]::f1) (x::result)) with
                                    | Some res -> Some res
                                    | None         ->  (match (dpll ([-x]::f1) (-x::result)) with
                                                        | Some res -> Some res
                                                        | None -> None)
                                    )
                    | None -> None))

let arr = 
    Directory.GetFiles("Bench/uf20-91/", "*.cnf") 
    |> Array.Parallel.map (fun x -> (x,load_cnf x)) 
    |> Array.Parallel.map (fun x -> match (dpll (snd x) []) with
                                                            | Some res-> printfn "%s SAT" (fst x)
                                                            | None -> printfn "%s UNSAT" (fst x))