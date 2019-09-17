(** Type checking with record subtyping *)

open Syntax

exception Type_error of string
exception Kind_error of string

(** [type_error msg] spro¾i izjemo [Type_error msg] *)
let type_error msg = raise (Type_error msg)

let kind_error msg = raise (Kind_error msg)

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

let true_type_operator = TAbstraction ("f", "T", KArrow (KStar, KArrow (KStar, KStar)), TAbstraction ("g", "F", KArrow (KStar, KStar), TParam "T"))
let false_type_operator = TAbstraction ("f", "T", KArrow (KStar, KArrow (KStar, KStar)), TAbstraction ("g", "F", KArrow (KStar, KStar), TParam "F"))

let rec kind_of kctx ty = match ty with
  | TParam x -> (try List.assoc x kctx with Not_found -> type_error ("unknown type parameter " ^ x))
  | TInt
  | TStringLiteral _
  | TBool -> KStar
  | TNever -> KStar
  | TArrow (ty1, ty2) -> kind_check kctx ty1 KStar;
                         kind_check kctx ty2 KStar;
                         KStar
  | TRecord tss -> List.iter (fun (l, ty') -> kind_check kctx ty' KStar) tss;
     KStar
  | TUnion (ty1, ty2) -> kind_check kctx ty1 KStar;
                         kind_check kctx ty2 KStar; KStar
  | TForAll (name, kind, ty') -> kind_of ((name, kind)::kctx) ty'
  | TLet (name, ty1, ty2) -> kind_of ((name, kind_of kctx ty1)::kctx) ty2
  | TAbstraction (f, x, kind, ty) ->
     (match kind with
      | KArrow (k1, k2) as karrow ->
         kind_check ((f, KArrow (k1, k2)) :: (x, k1) :: kctx) ty k2;
         karrow
      | _ -> kind_error (Printf.sprintf "expected arrow kind but got %s" (string_of_kind kind)))
  | TClosure (_, kind, _) -> kind_error "kind_of: should not reach here"
  | TApplication (ty1, ty2) ->
     (match kind_of kctx ty1 with
      | KArrow (k1, k2) -> kind_check kctx ty2 k1; k2
      | _ as kind -> kind_error (Printf.sprintf "application: expected arrow kind but got %s" (string_of_kind kind)))
  | TExtends (ty_sub, ty_super) -> kind_check kctx ty_sub KStar;
                                   kind_check kctx ty_super KStar;
                                   KArrow (KStar, KArrow (KStar, KStar))
  | TKeyof ty' -> kind_check kctx ty' KStar; KStar
  | TDistribute (ty1, ty_union) ->
     (match kind_of kctx ty1 with
      | KArrow (KStar, k2) -> kind_check kctx ty_union KStar; k2
      | _ as kind -> kind_error (Printf.sprintf "TDistribute: expected arrow kind but got %s" (string_of_kind kind)))

and kind_check kctx ty kind =
  let kty = kind_of kctx ty in
  if not (kty = kind) then kind_error (Printf.sprintf "incompatible kinds: %s is not the same as kind %s" (string_of_kind kty) (string_of_kind kind))

(** [type_of ctx e] returns the type of [e] in context [ctx]. *)
let rec type_of ctx = function
    Var x -> lookup_type x ctx
  | StringLiteral x -> TStringLiteral x
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
     let ty_eval = eval_type ctx ty
     in
     (match ty_eval with
      | TArrow (ty1, ty2) ->
         check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) e ty2 ;
         TArrow (ty1, ty2)
      | _ -> type_error ("expected function type but got " ^ string_of_type ty_eval))
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
     let ty_arg_eval = eval_type ctx ty_arg in
     (match type_of ctx e1 with
      | TForAll (name, _, ty) -> substitute_params_maybe ((name, ty_arg_eval)::ctx) ty
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
  | Left (ty1, ty2, e) ->
     let ty1_eval, ty2_eval = eval_type ctx ty1, eval_type ctx ty2 in
     check ctx e ty1_eval; TUnion (ty1_eval, ty2_eval)
  | Right (ty1, ty2, e) ->
     let ty1_eval, ty2_eval = eval_type ctx ty1, eval_type ctx ty2 in
     check ctx e ty2_eval; TUnion (ty1_eval, ty2_eval)
  | Match (e, ty1, n1, e1, ty2, n2, e2) ->
     let ty1_eval, ty2_eval = eval_type ctx ty1, eval_type ctx ty2 in
     check ctx e (TUnion (ty1_eval, ty2_eval));
     let ty_left = type_of ((n1, ty1_eval)::ctx) e1
     and ty_right = type_of ((n2, ty2_eval)::ctx) e2
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
  | TInt | TBool | TNever | TStringLiteral _ -> ty
  | TParam name -> (try List.assoc name ctx with Not_found -> ty)
  | TArrow (ty_in, ty_out) -> TArrow (substitute_params_maybe ctx ty_in, substitute_params_maybe ctx ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, substitute_params_maybe ctx ty')) tss)
  | TForAll (name, kind, ty) ->
     TForAll (name, kind, substitute_params_maybe ((name, TParam name)::ctx) ty)
  | TUnion (ty1, ty2) -> TUnion (substitute_params_maybe ctx ty1, substitute_params_maybe ctx ty2)
  | TLet (name, ty1, ty2) -> substitute_params_maybe ((name, ty1)::ctx) ty2
  | TAbstraction (f, x, kind, ty) -> type_error "substitute_params_maybe: TODO TAbstraction"
  | TClosure _ -> type_error "substitute_params_maybe: TODO TClosure"
  | TApplication (ty1, ty2) -> TApplication (substitute_params_maybe ctx ty1, substitute_params_maybe ctx ty2)
  | TExtends (ty1, ty2) -> TExtends (substitute_params_maybe ctx ty1, substitute_params_maybe ctx ty2)
  | TKeyof ty' -> TKeyof (substitute_params_maybe ctx ty')
  | TDistribute (ty1, ty2) -> TDistribute (substitute_params_maybe ctx ty1, substitute_params_maybe ctx ty2)

and get_stored_types env = List.concat (List.map (fun (n, e) -> match decode_type_application (n, e) with | Some p' -> [p'] | None -> []) env)

and encode_type_application (name, ty) = (name, Closure ([], "hack_to_store_type", Var "dummy", ty))
and decode_type_application (name, e_ty) = (match e_ty with
                                            | Closure ([], "hack_to_store_type", Var "dummy", ty) -> Some (name, ty)
                                            | _ -> None
                                           )

and eval_type tenv ty = match ty with
  | TInt | TBool | TNever | TStringLiteral _ -> ty
  | TParam name -> (try List.assoc name tenv with Not_found -> ty)
  | TArrow (ty_in, ty_out) -> TArrow (eval_type tenv ty_in, eval_type tenv ty_out)
  | TRecord tss -> TRecord (List.map (fun (l, ty') -> (l, eval_type tenv ty')) tss)
  | TForAll (name, kind, ty') ->
     TForAll (name, kind, eval_type ((name, TParam name)::tenv) ty')
  | TUnion (ty1, ty2) -> TUnion (eval_type tenv ty1, eval_type tenv ty2)
  | TLet (name, ty1, ty2) -> eval_type ((name, eval_type tenv ty1)::tenv) ty2
  | TAbstraction (f, x, kind, ty) ->
     let rec tc = TClosure ((f, tc)::tenv, x, ty) in tc
  | TClosure _ -> ty
  | TApplication (ty1, ty2) ->
     (match eval_type tenv ty1 with
      | TClosure (tenv', x, ty') ->
         eval_type ((x, eval_type tenv ty2)::tenv') ty'
      | _ -> type_error (Printf.sprintf "eval_type: invalid application of %s to %s" (string_of_type ty1) (string_of_type ty2)))
  | TExtends (ty_sub, ty_super) ->
     let ty_sub' = eval_type tenv ty_sub
     and ty_super' = eval_type tenv ty_super
     in
     eval_type tenv (if (subtype ty_sub' ty_super')
                     then true_type_operator
                     else false_type_operator)
  | TKeyof ty' ->
     let ty_eval = eval_type tenv ty' in
     (match ty_eval with
      | TRecord xss -> List.fold_right
                         (fun (l, e) xs -> let tsl = TStringLiteral ("\"" ^ l ^ "\"")
                                           in match xs with
                                              | TNever -> tsl
                                              | _ -> TUnion (tsl, xs))
                         xss TNever
      | _ -> type_error (Printf.sprintf "eval_type: Keyof expected a record type but got %s" (string_of_type ty_eval)))
  | TDistribute (ty1, ty_union) ->
     let ty_union' = eval_type tenv ty_union
     in
     (match ty_union' with
      | TUnion (ty_a, ty_b) ->
         (match eval_type tenv (TDistribute (ty1, ty_a)), eval_type tenv (TDistribute (ty1, ty_b)) with
          | TNever, TNever -> TNever
          | TNever, ty' -> ty'
          | ty', TNever -> ty'
          | ty1, ty2 -> TUnion (ty1, ty2))
      | _ -> eval_type tenv (TApplication (ty1, ty_union'))
     )
