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
  | Closure _ -> assert false
  | Let (x, e1, e2) -> type_of ((x, type_of ctx e1)::ctx) e2
  | App (e1, e2) ->
     (match type_of ctx e1 with
	TArrow (ty1, ty2) -> check ctx e2 ty1; ty2
      | _ as ty -> type_error ("expected function but got " ^ string_of_type ty))
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
       | TForAll (n1, k1, ty1), TForAll (n2, k2, ty2) ->
          subtype (substitute_params_maybe [(n1, TParam n2)] ty1) ty2
       | _, _ -> false
    )

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
