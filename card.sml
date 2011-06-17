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

  fun card2str I = "I"
    | card2str Zero = "Zero"
    | card2str Succ = "Succ"
    | card2str Dbl = "Dbl"
    | card2str Get = "Get"
    | card2str Put = "Put"
    | card2str S = "S"
    | card2str K = "K"
    | card2str Inc = "Inc"
    | card2str Dec = "Dec"
    | card2str Attack = "Attack"
    | card2str Help = "Help"
    | card2str Copy = "Copy"
    | card2str Revive = "Revive"
    | card2str Zombie = "Zombie"
end
