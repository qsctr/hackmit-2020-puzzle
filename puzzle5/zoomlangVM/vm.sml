exception ZoomLangDebugExn
exception ZoomLangTooFastExn
exception ZoomLangNoPrevRegExn of int
exception ZoomLangInvalidKeyExn of char * int

(* helper functions *)
fun vector_increment (vec1, i, inc) = Vector.update (vec1, i, inc + Vector.sub (vec1, i))
fun vec2list vec = Vector.foldr (fn (a, l) => a :: l) [] vec
fun list2string xs = "[" ^ (String.concatWith ", " (map Int.toString xs)) ^ "]"

fun pre_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps = (
  if pc >= Vector.length program
  then (scores, "")
  else interpret program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps handle
      ZoomLangDebugExn => (scores, "pc = " ^ Int.toString pc ^ "\npc_dir = " ^ Int.toString pc_dir ^ "\nstep = " ^ Int.toString step ^ "\nunused_breaks = " ^ list2string unused_breaks ^ "\nused_breaks = " ^ list2string used_breaks ^ "\ncur_reg = " ^ Int.toString cur_reg ^ "\nprev_reg = " ^ getOpt (Option.map Int.toString prev_reg, "none") ^ "\nreg_dir = " ^ Int.toString reg_dir ^ "\nsteps_per_reg = " ^ (list2string o vec2list) steps_per_reg ^ "\ncur_reg_steps = " ^ Int.toString cur_reg_steps ^ "\n")
    | ZoomLangTooFastExn => (scores, "cannot execute yes because no previous register\n")
    | ZoomLangNoPrevRegExn reg_err => (scores, "register " ^ Int.toString reg_err ^ " cannot go any faster\n")
    | ZoomLangInvalidKeyExn (invalid, pc_err) => (scores, "Invalid symbol " ^ str invalid ^ " at position " ^ Int.toString pc_err ^ "\n"))
and interpret program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps = (case Vector.sub (program, pc) of
    #"y" => if isSome prev_reg then step_helper program students (vector_increment (scores, cur_reg, Vector.sub (scores, valOf prev_reg))) pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false else raise ZoomLangTooFastExn
  | #"f" => if Vector.sub (steps_per_reg, cur_reg) > 1 then step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir (vector_increment (steps_per_reg, cur_reg, ~1)) cur_reg_steps false else raise ZoomLangNoPrevRegExn cur_reg
  | #"s" => step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir (vector_increment (steps_per_reg, cur_reg, 1)) cur_reg_steps false
  | #"+" => step_helper program students (vector_increment (scores, cur_reg, 1)) pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false
  | #"-" => step_helper program students (vector_increment (scores, cur_reg, ~1)) pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false
  | #"c" => step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg (~reg_dir) steps_per_reg cur_reg_steps false
  | #"b" => if List.exists (fn x => x = pc) used_breaks then step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps true else step_helper program students scores pc pc_dir step (pc :: unused_breaks) used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false
  | #"a" => (case unused_breaks of
      [] => step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false
    | break :: used_breaks' => step_helper program students scores break pc_dir step used_breaks' (break :: used_breaks) cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps false)
  | #"n" => raise ZoomLangDebugExn
  | invalid => raise ZoomLangInvalidKeyExn (invalid, pc))
and step_helper program students scores pc pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps b_step = (
  if b_step
  then pre_helper program students scores (pc + pc_dir) pc_dir step unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg cur_reg_steps
  else if cur_reg_steps + 1 >= Vector.sub (steps_per_reg, cur_reg)
  then pre_helper program students scores (pc + pc_dir) pc_dir (step + 1) unused_breaks used_breaks ((cur_reg + reg_dir) mod students) (SOME cur_reg) reg_dir steps_per_reg 0
  else pre_helper program students scores (pc + pc_dir) pc_dir (step + 1) unused_breaks used_breaks cur_reg prev_reg reg_dir steps_per_reg (cur_reg_steps + 1))

fun run_interpreter program students = pre_helper (Vector.fromList (String.explode program)) students (Vector.tabulate (students, fn n => 0)) 0 1 0 [] [] 0 NONE 1 (Vector.tabulate (students, fn n => 1)) 0

fun trimLast s = String.substring(s, 0, size s - 1)
fun run_with_IO () =
  let
    val program = (print "Enter the program: "; trimLast (valOf (TextIO.inputLine TextIO.stdIn)))
    val SOME students = (print "How many students? "; Int.fromString (trimLast (valOf (TextIO.inputLine TextIO.stdIn))))
    val (scores, error) = run_interpreter program students
  in
    print ((list2string o vec2list) scores ^ "\n" ^ error)
  end