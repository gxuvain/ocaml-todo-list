let display_welcome_message =
  print_endline "Welcome to Todo Manager\n";
  print_endline "Here are all available commands:";
  print_endline "add: Add a todo";
  print_endline "remove: Remove a todo";
  print_endline "list: List all todos";
  print_endline "update: Update a todo"

let get_user_input message =
  let message = message ^ ": " in
  print_endline message;
  read_line ()

let add_command todos =
  let todo = get_user_input "\nEnter todo" in

  match List.find_opt (fun t -> t = todo) todos with
  | Some found ->
      print_endline "Todo already exists!";
      todos
  | None -> todo :: todos

let remove_command todos =
  let todo = get_user_input "\nEnter todo" in

  match List.find_opt (fun t -> t = todo) todos with
  | None ->
      print_endline "Todo not found!";
      todos
  | Some found -> List.filter (fun t -> t <> todo) todos

let list_command todos =
  print_endline "\nTodos:";
  List.iter print_endline todos;
  todos

let update_command todos =
  let todo = get_user_input "\nTodo to update" in

  match List.find_opt (fun t -> t = todo) todos with
  | None ->
      print_endline "Todo not found!";
      todos
  | Some found ->
      let new_todo = get_user_input "New todo" in
      List.map (fun t -> if t = found then new_todo else todo) todos

let parse_user_input input_value todos =
  match input_value with
  | "add" -> add_command todos
  | "remove" -> remove_command todos
  | "list" -> list_command todos
  | "update" -> update_command todos
  | _ ->
      print_endline "Unknown command!";
      todos

let rec main todos =
  display_welcome_message;

  let user_input = get_user_input "\nWhat do you want to do?" in

  if user_input = "quit" then print_endline "\nBye!"
  else
    let todos = parse_user_input user_input todos in
    main todos

let () = main []
