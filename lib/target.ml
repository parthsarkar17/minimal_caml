module type ATTRIBUTE = sig
  type t

  val assoc : (t * string) list
end

(** [AttributeStrings(Attribute)] exposes functions to convert Attribute types to 
and from strings. *)
module AttributeStrings (Attribute : ATTRIBUTE) = struct
  (** [to_string attr] is the string version of the variant instance [attr]*)
  let to_string (attr : Attribute.t) : string = List.assoc attr Attribute.assoc

  (** [of_string s] is the variant-instance version of the string s, if the match is successful. *)
  let of_string (s : string) : Attribute.t =
    match
      Attribute.assoc
      |> List.fold_left (fun acc (x, y) -> (y, x) :: acc) []
      |> List.assoc_opt s
    with
    | None ->
        raise (Failure ("can't find attribute: '" ^ s ^ "' in assoc. list"))
    | Some s -> s
end

(** Lists the colors that a target could contain *)
module Color : ATTRIBUTE = struct
  type t =
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | Black
    | White
    | Brown
    | Gray

  let assoc =
    [
      (Red, "red");
      (Orange, "orange");
      (Yellow, "yellow");
      (Green, "green");
      (Blue, "blue");
      (Purple, "purple");
      (Black, "black");
      (White, "white");
      (Brown, "brown");
      (Gray, "gray");
    ]
end

(** Lists the shapes a target could take *)
module Shape : ATTRIBUTE = struct
  type t =
    | Circle
    | Semi
    | Quarter
    | Triangle
    | Square
    | Rectangle
    | Trapezoid
    | Pentagon
    | Heptagon
    | Hexagon
    | Octagon
    | Star
    | Cross

  let assoc =
    [
      (Circle, "circle");
      (Semi, "semicircle");
      (Quarter, "quarter_circle");
      (Triangle, "triangle");
      (Square, "square");
      (Rectangle, "rectangle");
      (Trapezoid, "trapezoid");
      (Pentagon, "pentagon");
      (Heptagon, "heptagon");
      (Hexagon, "hexagon");
      (Octagon, "octagon");
      (Star, "star");
      (Cross, "cross");
    ]
end

(** Lists the alphanumerics the target could contain *)
module Alpha : ATTRIBUTE = struct
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine

  let assoc =
    [
      (A, "A");
      (B, "B");
      (C, "C");
      (D, "D");
      (E, "E");
      (F, "F");
      (G, "G");
      (H, "H");
      (I, "I");
      (J, "J");
      (K, "K");
      (L, "L");
      (M, "M");
      (N, "N");
      (O, "O");
      (P, "P");
      (Q, "Q");
      (R, "R");
      (S, "S");
      (T, "T");
      (U, "U");
      (V, "V");
      (W, "W");
      (X, "X");
      (Y, "Y");
      (Z, "Z");
      (One, "1");
      (Two, "2");
      (Three, "3");
      (Four, "4");
      (Five, "5");
      (Six, "6");
      (Seven, "7");
      (Eight, "8");
      (Nine, "9");
    ]
end

(* Create string representation functions on target attributes. *)
module ShapeString = AttributeStrings (Shape)
module ColorString = AttributeStrings (Color)
module AlphaString = AttributeStrings (Alpha)

(* Define a target type *)
type t = {
  shape : Shape.t;
  shape_color : Color.t;
  alpha : Alpha.t;
  alpha_color : Color.t;
}

let string_of_target { shape; shape_color; alpha; alpha_color } =
  "{" ^ "shape: "
  ^ ShapeString.to_string shape
  ^ "; " ^ "shape_color: "
  ^ ColorString.to_string shape_color
  ^ "; " ^ "alpha: "
  ^ AlphaString.to_string alpha
  ^ "; " ^ "alpha_color: "
  ^ ColorString.to_string alpha_color
  ^ ";" ^ "}"
