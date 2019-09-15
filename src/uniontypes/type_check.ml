(** Type checking with record subtyping *)

open Syntax

exception Type_error of string

(** [type_error msg] spro¾i izjemo [Type_error msg] *)
let type_error msg = raise (Type_error msg)

(** [occurs x lst] returns [true] if [x] appears as a key in the
    associative array [lst] *)
let occurs x lst = List.exists (fun (y,_) -> x = y) lst

(** [check_labels lst] check whether all elements of [lst] are distinct. *)
let rec check_labels = function
    [] -> ()
  | l :: ls ->
      if List.mem l ls then type_error ("label " ^ l ^ " occurs more than once") ;
      check_labels ls

(** [lookup_type x ctx] looks up the type of [x] in context [ctx]. *)
let lookup_type x ctx =
  try List.assoc x ctx with Not_found -> type_error ("unknown variable " ^ x)

(** [type_of ctx e] returns the type of [e] in context [ctx]. *)
let rec type_of ctx = function
    Var x -> lookup_type x ctx
  | Int _ -> TInt
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Divide (e1, e2) -> check ctx e1 TInt; check ctx e2 TInt; TInt
  | Bool _ -> TBool
  | Equal (e1, e2)
  | Less (e1, e2) -> check ctx e1 TInt; check ctx e2 TInt; TBool
  | And (e1, e2)
  | Or (e1, e2) -> check ctx e1 TBool; check ctx e2 TBool; TBool
  | Not e -> check ctx e TBool; TBool
  | If (e1, e2, e3) ->
      check ctx e1 TBool;
      let ty2 = type_of ctx e2 in
      let ty3 = type_of ctx e3 in
	if subtype ty2 ty3 then ty3
	else if subtype ty3 ty2 then ty2
	else type_error "incompatible types in conditional"
  | Fun (f, x, ty, e) ->
     (match ty with
      | TArrow (ty1, ty2) ->
         check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) e ty2 ;
         TArrow (ty1, ty2)
      | _ -> type_error ("expected function type but got " ^ string_of_type ty))
  | TFun (name, kind, e) -> TForAll (name, kind, type_of ctx e)
  | Closure (_, _, _, ty) -> ty
  | Let (x, e1, e2) -> type_of ((x, type_of ctx e1)::ctx) e2
  | App (e1, e2) ->
     (match type_of ctx e1 with
	TArrow (ty1, ty2) -> check ctx e2 ty1; ty2
      | TRecord [("fst", ty1); ("snd", ty2)] as ty ->
         let ty_arg = type_of ctx e2
         in
         (match ty_arg with
          | TUnion _ ->
             let in_union, out_union = function_type_union true ty, function_type_union false ty
             in check ctx e2 in_union;
                out_union
          | _ -> (match matching_function_type ty_arg ty with
                  | Some (TArrow (_, ty_out)) -> ty_out
                  | _ -> type_error (Printf.sprintf "App: expected %s to be a subtype of an arrow type in %s" (string_of_type ty_arg) (string_of_type ty))))
      | _ as ty -> type_error ("App: expected function but got " ^ string_of_type ty))
  | TApp (e1, ty_arg) ->
     (match type_of ctx e1 with
      | TForAll (name, _, ty) -> substitute_params_maybe ((name, ty_arg)::ctx) ty
      | _ as ty -> type_error ("expected `forall`, but got " ^ string_of_type ty)
     )
  | Record rs ->
      check_labels (List.map fst  rs) ;
      TRecord (List.map (fun (l, e) -> (l, type_of ctx e)) rs)
  | Project (e, l) ->
      (match type_of ctx e with
	   TRecord ts ->
	     (try List.assoc l ts with
		  Not_found -> type_error ("no such field " ^ l))
	 | _ -> type_error "record expected" )
  | Left (ty1, ty2, e) -> check ctx e ty1; TUnion (ty1, ty2)
  | Right (ty1, ty2, e) -> check ctx e ty2; TUnion (ty1, ty2)
  | Match (e, ty1, n1, e1, ty2, n2, e2) ->
     check ctx e (TUnion (ty1, ty2));
     let ty_left = type_of ((n1, ty1)::ctx) e1
     and ty_right = type_of ((n2, ty2)::ctx) e2
     in
     if subtype ty_left ty_right then ty_right
     else if subtype ty_right ty_left then ty_left
     else type_error (Printf.sprintf "incompatible types in match %s and %s" (string_of_type ty_left) (string_of_type ty_right))

(** [check ctx e ty] checks whether [e] can be given type [ty] in
    context [ctx]. *)
and check ctx e ty =
  if not (subtype (type_of ctx e) ty) then type_error ("incompatible types; " ^ (string_of_type (type_of ctx e)) ^ " is not a subtype of " ^ (string_of_type ty))

(** [subtype ty1 ty2] returns [true] if [ty1] is a subtype of [ty2]. *)
and subtype ty1 ty2 =
  (ty1 = ty2) ||
    (match ty1, ty2 with
	 TArrow (u1, v1), TArrow (u2, v2) ->
	   (subtype u2 u1) && (subtype v1 v2)
       | TRecord ts1, TRecord ts2 ->
	   List.for_all
	     (fun (l,ty) -> List.exists (fun (l',ty') -> l = l' && subtype ty' ty) ts1)
	     ts2
       | TForAll (n1, k1, ty1), TForAll (n2, k2, ty2) ->
          subtype (substitute_params_maybe [(n1, TParam n2)] ty1) ty2
       | TUnion (ty1, ty2), TUnion (ty1', ty2') ->
          (subtype ty1 ty1') && (subtype ty2 ty2')
       | ty1, TUnion (ty1', ty2') ->
          (subtype ty1 ty1') || (subtype ty1 ty2')
       | _, _ -> false
    )

(** [is_valid_argument ty_arg ty_fn] returns [true] if [ty_arg] has can be a valid input to [ty_fn]. *)
and is_valid_argument ty_arg ty_fn =
  match ty_fn with
    TArrow (ty1, _) -> subtype ty_arg ty1
  | TRecord [("fst", ty_fst); ("snd", ty_snd)] ->
     is_valid_argument ty_arg ty_fst || is_valid_argument ty_arg ty_snd
  | _ -> type_error ("expected function or record of functions but got " ^ string_of_type ty_fn)

(** [function_type_union ty1 ty2] for [(A -> B & C -> D) & E -> F] returns [((A|C)|E, (B|D)|F)]. *)
and function_type_union choose_input_type ty =
  match ty with
  | TArrow (ty1, ty2) -> if choose_input_type then ty1 else ty2
  | TRecord [("fst", ty_fst); ("snd", ty_snd)] -> TUnion (function_type_union choose_input_type ty_fst, function_type_union choose_input_type ty_snd)
  | _ -> type_error ("expected function or record of functions but got " ^ string_of_type ty)

(** [matching_function_type ty_arg ty] returns the function type in [ty] that matches [ty_arg] or None. *)
and matching_function_type ty_arg ty =
  match ty with
  | TArrow (ty1, ty2) -> if subtype ty_arg ty1 then Some ty else None
  | TRecord [("fst", ty_fst); ("snd", ty_snd)] ->
     (match matching_function_type ty_arg ty_fst with
     | Some _ as ty' -> ty'
     | None -> matching_function_type ty_arg ty_snd)
  | _ -> type_error ("matching_function_type expected function or record of functions but got " ^ string_of_type ty)

(** [substitute_params_maybe ctx ty] returns [ty] with type parameters replaced by
   their definitions from [ctx], if available. *)
and substitute_params_maybe ctx ty =
  match ty with
  | TInt | TBool -> ty
  | TParam name -> (try List.assoc name ctx with Not_found -> ty)
  | TArrow (ty_in, ty_out) -> TArrow (substitute_params_maybe ctx ty_in, substitute_params_maybe ctx ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, substitute_params_maybe ctx ty')) tss)
  | TForAll (name, kind, ty) ->
     TForAll (name, kind, substitute_params_maybe ((name, TParam name)::ctx) ty)
  | TUnion (ty1, ty2) -> TUnion (substitute_params_maybe ctx ty1, substitute_params_maybe ctx ty2)