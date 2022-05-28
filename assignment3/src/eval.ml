open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

let extend env x v = (x,v)::env

let rec lookup env x = 
 	match env with
 	| [] -> raise UndefinedVar
 	| (y,v)::env' -> if x=y then v else (lookup env' x)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Number n -> Int_Val n
    | True -> Bool_Val True
    | False -> Bool_Val False
    | Var -> lookup env Var
    | Plus (e1,e2)-> 
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> Int_Val (i+j)
    	| _ -> raise TypeError) 
    	
    | Minus (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> Int_Val i, Int_Val j
    	| _ -> raise TypeError) 
    	let n3 = n1-n2 in
    	n3
    | Times (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> Int_Val i, Int_Val j
    	| _ -> raise TypeError) 
    	let n3 = n1*n2 in
    	n3
    | Div (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> if j=0 (raise DivByZeroError) else (Int_Val i, Int_Val j)
    	| _ -> raise TypeError) 
    	let n3 = n1/n2 in
    	n3
    | Mod (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> if j=0 (raise DivByZeroError) else (Int_Val i, Int_Val j)
    	| _ -> raise TypeError) 
    	let n3 = n1 mod n2 in
    	n3
    | Eq (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> (if i=j (Bool_Val (true)) else (Bool_Val(false)))
    	| Bool_Val i, Bool_Val j -> (if i=j (Bool_Val(true)) else (Bool_Val(false)))
    	| _ -> raise TypeError)
    | Leq (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> if i<=j (Bool_Val(true)) else (Bool_Val(false))
    	| _ -> raise TypeError)
    | Lt (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    	| Int_Val i, Int_Val j -> if i<j (Bool_Val(true)) else (Bool_Val(false))
    	| _ -> raise TypeError)
    | Not (e1) ->
    	let n1 = eval_expr e1 in
    	(match n1 with
    	| Bool_Val (true) -> Bool_Val (false)
    	| Bool_Val (false) -> Bool_Val (true)
    	| _ -> raise TypeError)
    | And (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    		| Bool_Val(true), Bool_Val(true) -> Bool_Val(true)
    		| Bool_Val(true), Bool_Val(false) -> Bool_Val(false)
    		| Bool_Val(false), Bool_Val(true) -> Bool_Val(false)
    		| Bool_Val(false), Bool_Val(false) -> Bool_Val(false)
    		| _ -> raise TypeError)
    | Or (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1,n2 with
    		| Bool_Val(true), Bool_Val(true) -> Bool_Val(true)
    		| Bool_Val(true), Bool_Val(false) -> Bool_Val(true)
    		| Bool_Val(false), Bool_Val(true) -> Bool_Val(true)
    		| Bool_Val(false), Bool_Val(false) -> Bool_Val(false)
    		| _ -> raise TypeError) 
    | Fun (x,e1) -> Closure(env,x,e1)
	| App (e1,e2) ->
    	let n1 = eval_expr e1 in
    	let n2 = eval_expr e2 in
    	(match n1 with
    	| Closure (env',x,e1) -> eval_expr e1 (extend env' x n2) 
    	| _ -> raise TypeError)

    

 
let rec exists env assgnVar x = 
 	match env with
 	| [] -> raise UndefinedVar
 	| (y,v)::env' -> if assgnVar=y then (match x,v with
 										| Int_Val n1, Int_Val n2 -> (y,x)::env'
 										| Bool_Val b1, Bool_Val b2 -> (y,x)::env'
 										| _ -> raise TypeError)  else (exists env' x)


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
	match c with
	| Skip -> env
	| Comp(com1,com2) -> eval_command com2 (eval_command com1 env)
	| Declare(dtype, x) -> 
		(match dtype with
		|Int_Type -> extend env x (Int_Val 0)
		|Bool_Type -> extend env x Bool_Val(false)
		|Lambda_Type -> extend env x (Closure (env, "x", Var "x")))

	| Assg(st,e) -> 
		let x = eval_expr e env in
		exists env st x 
	| Cond(ex, com1, com2) -> 
		(match eval_expr ex env with
			|Bool_Val b1 -> if true then (eval_command com1 env) else (eval_command com2 e)
			|_ -> raise TypeError)
	| While(ex, com) ->
		(match eval_expr ex env with
			|Bool_Val b1-> if true then eval_command c (eval_command com env) else env 
			|_ -> raise TypeError)
	| For(ex,com) ->
	
	eval_command (For(Number(n-1),com)) (eval_command com env)
	
		
    []
	
