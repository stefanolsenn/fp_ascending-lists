let rec insert alist key = match alist with
                           |[] -> [key]
                           |h::t when key > h -> [h] @ insert t key
                           |h::t when key <= h -> [key] @ alist
                           |_-> failwith "Not possible"
                           
let l1 = [1;1;2]
let l2 = [1;2;4]
                      
let rec plus list1 list2 =
        match list1 with
        | [] -> list2
        | h :: t -> insert list2 h |> plus t
