#use "CPinter.ml";;
#use "4 cartes.ml";;

let test_distribute_structural() : unit =
  let test_status : t_test_status = create_test_status("distribute") in
  (
    test_func_equals_value(test_status, "Distribuer une carte a chaque joueur", distribute(), (make_player({nbplayer = 4;nbcarddeck = 52;nbturn = 4;nbcardstart = 4;nbcardgiventurn = 3}),make_deck52(),{nbplayer = 4;nbcarddeck = 52;nbturn = 4;nbcardstart = 4;nbcardgiventurn = 3})
