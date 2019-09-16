(** Abstract syntax *)

(** Type type of variable names *)
type name = string

(** The type of field names *)
type label = string

(** Kinds. *)
type kind =
  | KStar (** Base kind [*]. *)

(** Types *)
type ty =
  | TInt (** integers [int] *)
  | TBool (** boolean values [bool] *)
  | TArrow of ty * ty (** function types [ty1 -> ty2] *)
  | TRecord of (label * ty) list (** records [{l1:ty1, ..., lN:tyN}] *)
  | TParam of name (** type parameter like in [Container A] *)
  | TForAll of name * kind * ty (** parametric polymorphism [forall A: * . A -> A] *)
  | TUnion of ty * ty (** union type [ty1 | ty2] *)
  | TLet of name * ty * ty (** type-let [type Foo = A | B | ... in Foo -> Foo] *)

(** Expressions *)
type expr =
  | Var of name (** variable *)
  | Int of int (** integer constant *)
  | Plus of expr * expr (** sum [e1 + e2] *)
  | Minus of expr * expr (** difference [e1 - e2] *)
  | Times of expr * expr (** products [e1 * e2] *)
  | Divide of expr * expr (** quotient [e1 / e2] *)
  | Bool of bool (** boolean constant [true] or [false] *)
  | Equal of expr * expr (** integer equality [e1 = e2] *)
  | Less of expr * expr (** integer comparison [e1 < e2] *)
  | And of expr * expr (** conjunction [e1 and e2] *)
  | Or of expr * expr (** disjunction [e1 or e2] *)
  | Not of expr (** negation [not e] *)
  | If of expr * expr * expr (** conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * expr (** recursive function [fun f(x : ty1):ty2 is e] *)
  | TFun of name * kind * expr (** type abstraction [\A:K. e] *)
  | Closure of environment * name * expr * ty (** closure (internal value) *)
  | Let of name * expr * expr (** local definition [let x = e1 in e2] *)
  | App of expr * expr (** application [e1 e2] *)
  | TApp of expr * ty (** type application [e [A]] *)
  | Record of (label * expr) list (** record [{l1=e1, ..., lN=eN}] *)
  | Project of expr * label (** field projection [e.l] *)
  | Left of ty * ty * expr (** Left part of a union type [Left [int] [bool] 3] *)
  | Right of ty * ty * expr (** Right part of a union type [Right [int] [bool] true] *)
  | Match of expr * ty * name * expr * ty * name * expr
(** Pattern-match on union type [match (f x) with | int as y -> y + 1 | bool
   as y -> not y] *)

(** An environment is an associative list [(x1,v1);...;(xN,vN)]. *)
and environment = (name * expr) list

(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr (** an expression to be evaluated *)
  | Def of name * expr (** Global definition [let x = e] *)

(** [string_of_kind k] converts a kind [k] to a string. *)
let rec string_of_kind = function
  | KStar -> "*"

(** [string_of_type ty] converts type [ty] to a string. *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty with
	| TInt -> (4, "int")
	| TBool -> (4, "bool")
	| TRecord ts ->
	    (4, "{" ^
	       String.concat ", "
	       (List.map (fun (l,t) -> l ^ " : " ^ (to_str (-1) t)) ts) ^
	       "}")
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
        | TParam name -> (4, name)
        | TForAll (name, kind, ty) -> (4, "(forall " ^ name ^ ": " ^ string_of_kind kind ^ ". " ^ to_str 0 ty ^ ")")
        | TUnion (ty1, ty2) -> (4, Printf.sprintf "(%s | %s)" (to_str 0 ty1) (to_str 0 ty2))
        | TLet (name, ty1, ty2) -> (4, Printf.sprintf "type %s = %s in %s" name (to_str 0 ty1) (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(** [string_of_value v] converts a value [v] to a string. *)
let rec string_of_value = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Record rs ->
      "{" ^ String.concat ", "
	(List.map (fun (l,e) -> l ^ " = " ^ (string_of_value e)) rs) ^
	"}"
  | Closure _ -> "<fun>"
  | Left (_, _, x) -> "Left " ^ (string_of_value x)
  | Right (_, _, x) -> "Right " ^ (string_of_value x)
  | TFun _ -> "<poly-fun>"
  | _ -> assert false

let rec is_value = function
  | Int _ -> true
  | Bool _ -> true
  | Record rs -> List.for_all (fun (_,e) -> is_value e) rs
  | Closure _ -> true
  | Left (_, _, x) -> is_value x
  | Right (_, _, x) -> is_value x
  | _ -> assert false

let rec string_of_expr = function
  | Var x -> x
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Fun (f, x, ty, e) ->
     Printf.sprintf "fun %s(%s): %s is %s" f x (string_of_type ty) (string_of_expr e)
  | TFun (name, kind, e) ->
     Printf.sprintf "\\%s: %s. %s" name (string_of_kind kind) (string_of_expr e)
  | Closure (_, x, e, ty) ->
     Printf.sprintf "Closure (%s, %s, %s, %s)" "<env>" x (string_of_expr e) (string_of_type ty)
  | App (e1, e2) ->
     Printf.sprintf "(%s %s)" (string_of_expr e1) (string_of_expr e2)
  | TApp (e, ty) ->
     Printf.sprintf "%s [%s]" (string_of_expr e) (string_of_type ty)
  | Record rs ->
      Printf.sprintf "{%s}" (String.concat ", " (List.map (fun (l,e) -> Printf.sprintf "%s: %s" l (string_of_expr e)) rs))
  | Plus (e1, e2) -> "(Plus TODO)"
  | Minus (e1, e2) -> "(Minus TODO)"
  | Times (e1, e2) -> "(Times TODO)"
  | Divide (e1, e2) -> "(Divide TODO)"
  | Equal (e1, e2) -> "(Equal TODO)"
  | Less (e1, e2) -> "(Less TODO)"
  | And (e1, e2) -> "(And TODO)"
  | Or (e1, e2) -> "(Or TODO)"
  | Not b -> "(Not TODO)"
  | If (e1, e2, e3) -> "(If TODO)"
  | Let (x, e1, e2) -> "(Let TODO)"
  | Project (e, l) -> "(Project TODO)"
  | Left (ty1, ty2, e) -> "(Left TODO)"
  | Right (ty1, ty2, e) -> "(Right TODO)"
  | Match (e, ty1, n1, e1, ty2, n2, e2) -> "(Match TODO)"
