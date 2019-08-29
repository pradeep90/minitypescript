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
  | Fun (f, x, ty1, ty2, e) ->
     let ty1_concrete = make_alias_param (substitute_aliases_maybe ctx ty1) in
     let ty2_concrete = make_alias_param (substitute_aliases_maybe ctx ty2) in
      check ((f, TArrow(ty1_concrete,ty2_concrete)) :: (x, ty1_concrete) :: ctx) e ty2_concrete ;
      TArrow (ty1_concrete, ty2_concrete)
  | Closure _ -> assert false
  | Let (x, e1, e2) -> type_of ((x, type_of ctx e1)::ctx) e2
  | App (e1, e2) ->
      (match type_of ctx e1 with
	   TArrow (ty1, ty2) ->
            let unified_ctx = unify_constraints [] (param_constraints ty1 (type_of ctx e2)) in
            check ctx e2 (substitute_params_maybe unified_ctx ty1);
            (substitute_params_maybe unified_ctx ty2)
	 | _ -> type_error "function expected")
  | Record rs ->
      check_labels (List.map fst  rs) ;
      TRecord (List.map (fun (l, e) -> (l, type_of ctx e)) rs)
  | Project (e, l) ->
      (match type_of ctx e with
	   TRecord ts ->
	     (try List.assoc l ts with
		  Not_found -> type_error ("no such field " ^ l))
	 | _ -> type_error "record expected" )

(** [check ctx e ty] checks whether [e] can be given type [ty] in
    context [ctx]. *)
and check ctx e ty =
  if not (subtype (type_of ctx e) ty) then type_error ("incompatible types; " ^ (string_of_type (type_of ctx e)) ^ " is not a subtype of " ^ (string_of_type ty))

(** [sybtype ty1 ty2] returns [true] if [ty1] is a subtype of [ty2]. *)
and subtype ty1 ty2 =
  (ty1 = ty2) ||
    (match ty1, ty2 with
	 TArrow (u1, v1), TArrow (u2, v2) ->
	   (subtype u2 u1) && (subtype v1 v2)
       | TRecord ts1, TRecord ts2 ->
	   List.for_all
	     (fun (l,ty) -> List.exists (fun (l',ty') -> l = l' && subtype ty' ty) ts1)
	     ts2
       | _, _ -> false
    )

(** [substitute_aliases_maybe ctx ty] returns [ty] with type aliases replaced by
   their definitions from [ctx], if available. *)
and substitute_aliases_maybe ctx ty =
  match ty with
  | TInt | TBool | TParam _ -> ty
  | TAlias name -> (try List.assoc name ctx with Not_found -> ty)
  | TArrow (ty_in, ty_out) -> TArrow (substitute_aliases_maybe ctx ty_in, substitute_aliases_maybe ctx ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, substitute_aliases_maybe ctx ty')) tss)

(** [has_no_aliases ty] returns true iff there are no type aliases within [ty]. *)
and has_no_aliases ty =
  match ty with
  | TInt | TBool | TParam _ -> true
  | TAlias _ -> false
  | TArrow (ty_in, ty_out) -> has_no_aliases ty_in && has_no_aliases ty_out
  | TRecord tss -> List.for_all (fun (l, ty') -> has_no_aliases ty') tss

(** [has_no_parameters ty] returns true iff there are no type parameters within [ty]. *)
and has_no_parameters ty =
  match ty with
  | TInt | TBool | TAlias _ -> true
  | TParam _ -> false
  | TArrow (ty_in, ty_out) -> has_no_parameters ty_in && has_no_parameters ty_out
  | TRecord tss -> List.for_all (fun (l, ty') -> has_no_parameters ty') tss

(** [make_alias_param ty] returns [ty] with type aliases turned into type
   parameters. *)
and make_alias_param ty =
  match ty with
  | TInt | TBool | TParam _ -> ty
  | TAlias name -> TParam name
  | TArrow (ty_in, ty_out) -> TArrow (make_alias_param ty_in, make_alias_param ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, make_alias_param ty')) tss)

(** [param_constraints ty ty_sub] returns the type-parameter constraints
   needed to unify [ty] and [ty_sub]. All we know is that ty_sub has to be a
   subtype of ty. *)
and param_constraints ty ty_sub =
  if has_no_parameters ty && has_no_parameters ty_sub then []
  else
    match ty, ty_sub with
    | TParam p, TInt -> [(p, TInt)]
    | TParam p, TBool -> [(p, TBool)]
    | TParam p, TAlias _ -> type_error "alias should not get here"
    | TParam p, TParam p' when p = p' -> []
    | TParam p, TParam p' -> type_error ("cannot unify different type parameters " ^ p ^ " and " ^ p')
    | TParam p, TArrow (ty_in, ty_out) -> [(p, ty_sub)]
    | TParam p, TRecord xs -> [(p, ty_sub)]
    | _, TParam _ -> param_constraints ty_sub ty
    | TRecord xs, TRecord ys -> List.concat (List.map (fun (l1, ty1) -> param_constraints ty1 (lookup_type l1 ys)) xs)
    | TArrow (ty_in1, ty_out1), TArrow (ty_in2, ty_out2) -> (param_constraints ty_in2 ty_in1) @ (param_constraints ty_out1 ty_out2)
    | _, _ -> type_error ("cannot unify " ^ string_of_type ty ^ " and " ^ string_of_type ty_sub)

(** [unify_constraints xs] returns the substitutions necessary (including those in [acc]) to unify the constraints (p, ty); ... in [xs]. *)
and unify_constraints acc xs = match xs with
  | [] -> acc
  | (p, ty)::xs' ->
     let ty_final =
       (try let ty_prev = List.assoc p acc in
         if subtype ty_prev ty
         then ty
         else if subtype ty ty_prev then
           ty_prev
         else type_error ("cannot unify the constraints that type parameter " ^ p ^ " : " ^ string_of_type ty ^ " and " ^ p ^ " : " ^ string_of_type ty_prev)
       with Not_found -> ty) in
     unify_constraints ((p, ty_final)::acc) xs'

(** [substitute_params_maybe ctx ty] returns [ty] with type parameters replaced by
   their definitions from [ctx], if available. *)
and substitute_params_maybe ctx ty =
  match ty with
  | TInt | TBool | TAlias _ -> ty
  | TParam name -> (try List.assoc name ctx with Not_found -> ty)
  | TArrow (ty_in, ty_out) -> TArrow (substitute_params_maybe ctx ty_in, substitute_params_maybe ctx ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, substitute_params_maybe ctx ty')) tss)
