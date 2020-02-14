#use "AP1util.ml";;
type t_card_color = HEART | DIAMOND | SPADE | CLUB;;

type t_card_rank = int;;

type t_card = { color : t_card_color;
                rank : t_card_rank};;

let create_card(card_col, card_rank : t_card_color * t_card_rank ) : t_card =
  {color = card_col;
   rank = card_rank};;

let cardcol_of_int(i : int ) : t_card_color =
  if i <1 && i>4
  then failwith "erreur i is not valid"
  else
    if i = 1
    then HEART
    else
      if i = 2
      then DIAMOND
      else
        if i = 3
        then SPADE
        else CLUB;;

let cardRank52_of_int ( i : int) : t_card_rank =
  if i <= 1 && i >=13
  then failwith "i is not valid"
  else i ;;

let card_of_int52 (i : int) : t_card =
  if i < 1 || i > 52
  then failwith "Votre carte n'existe pas"
  else
    if i >= 1 && i <= 13
    then { color = HEART ; rank = ((i-1) mod 13) +1}
    else
      if i >= 14 && i <= 26
      then { color = DIAMOND ; rank = ((i-1) mod 13) +1}
      else
        if i >= 27 && i <= 39
        then { color = SPADE ; rank = ((i-1) mod 13) +1}
        else { color = CLUB ; rank = ((i-1) mod 13) +1};;


let rec  make_deck52_aux(i: int) : t_card list =
  if i = 0
  then []
  else add_lst(make_deck52_aux(i-1),card_of_int52(i));;


let make_deck52() : t_card list =
  make_deck52_aux(52);;

let cardrank32_of_int(i : int ) : t_card_rank =
  if i > 8 || i < 1
  then failwith "votre rang n'existe pas"
  else
    if i = 1
    then i
    else i + 5
;;

let card_of_int32(i : int ) : t_card =
  if i > 32 || i < 1
  then failwith "Votre carte n'est pas compris entre 1 et 32"
  else
    if i >= 1 && i <= 8
    then {color = HEART ; rank = (i mod 8)}
    else
      if i >= 9 && i <= 16
      then {color = DIAMOND ; rank = ( i mod 8) }
      else
        if i >= 17 && i <= 24
        then {color = SPADE ; rank = ( i mod 8)}
        else {color = CLUB; rank = ( i mod 8)};;

let rec  make_deck32_aux(i: int) : t_card list =
  if i = 0
  then []
  else add_lst(make_deck32_aux(i-1),card_of_int32(i));;


let make_deck32() : t_card list =
  make_deck32_aux(32);;

let rec shuffle(deck, deck_len : t_card list * int) : t_card list =
  if deck_len = 0
  then []
  else shuffle(rem_nth(deck,rand_int(0,deck_len)),deck_len-1)
;;

type t_param = {
    nbplayer : int;
    nbcarddeck : int;
    nbturn : int;
    nbcardstart :int;
    nbcardgiventurn : int}
;;
let valid_param(p : t_param) : bool =
  (p.nbcarddeck = 52 || p.nbcarddeck = 32) && (p.nbplayer = 4 || p.nbplayer = 3)
;;
type t_player = {
    number : int;
    hand : t_card list ref ;
    cemetery : t_card list ref}
;;

let create_player(number : int) : t_player =
  {
    number = number;
    hand = [];
    cemetery = []
  }
;;

let rec make_player_aux(number : int ) : t_player list =
  if number = 0
  then []
  else add_lst(make_player_aux(number - 1),create_player(number))
;;
  
let make_players(param : t_param) : t_player list =
  if not(valid_param(param))
  then failwith "Parametres invalides"
  else make_player_aux(param.nbplayer)
;;

(* PARTIE 2 *)

let distribute(players,deck,p : t_player array * t_card list ref * t_param) : unit =
  for i = 0 to p.nbplayer - 1
  do
    (players.(i).hand) := add_lst(!(players.(i).hand), fst(!deck));
    deck := rem_fst(!deck);
  done
;;

let distribute_4cards(players,deck,p: t_player array * t_card list ref * t_param) : unit =
  for i = 0 to p.nbcardgiventurn - 1
  do
    distribute(deck,players,p);
  done
;;


let rec fill_board(board, deck, p : t_card list ref * t_card list ref * t_param ) : unit =
  if(len(!board) = p.nbcardstart || len(!deck) = 0)
  then ()
  else (
    board := add_fst(!board, fst(!deck));
    deck := rem_fst(!deck);
    fill_board(board,deck,p);
  )
;;
