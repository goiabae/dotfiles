(* Toggle of the completion hint-box *)
UTop.set_show_box false ;;

#require "utop" ;;
#require "react" ;;
#require "lambda-term" ;;

(* change prompt string *)
UTop.prompt := fst (React.S.create LTerm_text.(eval [
  S "utop";
  B_bold true;
  B_fg (LTerm_style.green);
  S "|> "
])) ;;
