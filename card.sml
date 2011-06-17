(* Cards are atomic actions *)
structure Card = struct
   datatype card =
      I
    | Zero
    | Succ
    | Dbl
    | Get
    | Put
    | S
    | K
    | Inc
    | Dec
    | Attack
    | Help
    | Copy
    | Revive
    | Zombie
end
