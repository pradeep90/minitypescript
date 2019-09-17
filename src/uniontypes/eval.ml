(** Large step evaluation semantics. *)

open Syntax

exception Runtime_error of string

(** [runtime_error msg] reports a runtime error by raising [Runtime_error msg] *)
let runtime_error msg = raise (Runtime_error msg)

(** [lookup_value x env] looks up the value of [x] in environment [env]. *)
let lookup_value x env =
  try List.assoc x env with Not_found -> runtime_error ("unknown variable " ^ x)

(** [eval env e] evaluates expression [e] in environment [env]. *)
let rec eval env = function
  | Var x -> lookup_value x env
  | StringLiteral _ as e -> e
  | Int _ as e -> e
  | Plus (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 + k2)
	 | _, _ -> runtime_error "integers expected in addition")
  | Minus (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 - k2)
	 | _, _ -> runtime_error "integers expected in subtraction")
  | Times (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Int (k1 * k2)
	 | _, _ -> runtime_error "integers expected in multiplication")
  | Divide (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int 0 -> runtime_error "division by zero"
	 | Int k1, Int k2 -> Int (k1 / k2)
	 | _, _ -> runtime_error "integers expeced in quotient")
  | Bool _ as e -> e
  | Equal (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Bool (k1 = k2)
	 | _, _ -> runtime_error "integers expected in equality")
  | Less (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Int k1, Int k2 -> Bool (k1 < k2)
	 | _, _ -> runtime_error "integers expected in comparison")
  | And (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Bool b1, Bool b2 -> Bool (b1 && b2)
	 | _, _ -> runtime_error "boolean values expected in conjunction")
  | Or (e1, e2) ->
      (match eval env e1, eval env e2 with
	   Bool b1, Bool b2 -> Bool (b1 || b2)
	 | _, _ -> runtime_error "boolean values expected in disjunction")
  | Not b ->
      (match eval env b with
	   Bool b -> Bool (not b)
	 | _ -> runtime_error "boolean values expected in negation")
  | If (e1, e2, e3) ->
      (match eval env e1 with
	   Bool true -> eval env e2
	 | Bool false -> eval env e3
	 | _ -> runtime_error "boolean value expected in conditional")
  | Fun (f, x, ty, e) ->
     let stored_types = Type_check.get_stored_types env
     in
     let ty_fun = Type_check.substitute_params_maybe stored_types ty
     in
     let rec c = Closure ((f,c)::env, x, e, ty_fun) in c
  | TFun (name, kind, e) -> TFun (name, kind, eval env e)
  | Closure (env', x, e, ty) ->
     let stored_types = Type_check.get_stored_types env
     in
     Closure ((List.map Type_check.encode_type_application stored_types) @ env', x, e, ty)
  | Let (x, e1, e2) -> eval ((x, eval env e1)::env) e2
  | App (e1, e2) ->
      (match eval env e1 with
	   Closure (env', x, e, _) -> eval ((x,eval env e2)::env') e
         | Record [("fst", ef1); ("snd", ef2)] ->
            let ef1' = eval env ef1
            and ef2' = eval env ef2
            in
            (* TODO: Merge this with the Match branch. *)
            (match eval env e2 with
             | Left (_, _, e') -> eval env (App (ef1', e'))
             | Right (_, _, e') -> eval env (App (ef2', e'))
             | _ as e' when is_value e' ->
                let stored_types = Type_check.get_stored_types env
                in
                let ty_arg = Type_check.substitute_params_maybe stored_types (Type_check.type_of [] e')
                and ty_ef1' = Type_check.substitute_params_maybe stored_types (Type_check.type_of [] ef1')
                and ty_ef2' = Type_check.substitute_params_maybe stored_types (Type_check.type_of [] ef2')
                in
                if Type_check.is_valid_argument ty_arg ty_ef1'
                then eval env (App (ef1', e'))
                else if Type_check.is_valid_argument ty_arg ty_ef2'
                then eval env (App (ef2', e'))
                else
                  runtime_error (Printf.sprintf "App got a value that was not a subtype of the union type; this should not have passed the typechecker - ty_arg: %s; ty_ef1': %s; ty_ef2': %s" (string_of_type ty_arg) (string_of_type ty_ef1') (string_of_type ty_ef2'))
             | _ -> runtime_error "App expected Left or Right or a subtype value as argument")
	 | _ -> runtime_error "invalid application")
  | TApp (e, ty) -> (match eval env e with
                     | TFun (name, _, e') ->
                        eval ((Type_check.encode_type_application (name, ty))::env) e'
                     | _ -> runtime_error "TApp expected TFun"
                    )
  | Record rs ->
      Record (List.map (fun (l,e) -> (l, eval env e)) rs)
  | Project (e, l) ->
      (match eval env e with
	   Record vs -> eval env (snd (List.find (fun (l',_) -> l = l') vs))
	 | _ -> runtime_error "record expected")
  | Left (ty1, ty2, e) -> Left (ty1, ty2, eval env e)
  | Right (ty1, ty2, e) -> Right (ty1, ty2, eval env e)
  | Match (e, ty1, n1, e1, ty2, n2, e2) ->
     (match eval env e with
      | Left (_, _, e') -> eval ((n1, e')::env) e1
      | Right (_, _, e') -> eval ((n2, e')::env) e2
      | _ as e' when is_value e' ->
         let stored_types = Type_check.get_stored_types env
         in
         let ty_e' = Type_check.type_of stored_types e'
         in
         let ty1_concrete = Type_check.substitute_params_maybe stored_types ty1
         and ty2_concrete = Type_check.substitute_params_maybe stored_types ty2
         in
         if Type_check.subtype ty_e' ty1_concrete
         then eval ((n1, e')::env) e1
         else if Type_check.subtype ty_e' ty2_concrete
         then eval ((n2, e')::env) e2
         else runtime_error (Printf.sprintf "match got a value that was not a subtype of the union type; this should not have passed the typechecker; ty_e': %s; ty1_concrete: %s; ty2_concrete: %s" (string_of_type ty_e') (string_of_type ty1_concrete) (string_of_type ty2_concrete))
      | _ -> runtime_error "expected Left or Right or a subtype value as argument to match")
