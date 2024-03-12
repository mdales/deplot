module StringMap = Map.Make (String)

let usage_msg = "deplot <project yml>"
let args = ref []
let anon_fun arg = args := !args @ [arg]
let speclist = []


exception Project_filename_missing
exception Ill_formatted_top_yml
exception Ill_formatted_process_yml
exception Ill_formatted_child_yml


type process_node = {
  style: string;
  title: string;
  inputs : string list;
  outputs: string list;
  context: string option;
}

type box_node = {
  title: string;
  name: string;
  children: process_node list;
}

type generative_box_node = {
  title: string;
  names: string list;
  children: process_node list;
}

type node =
| Process of process_node
| Box of box_node
| Generative of generative_box_node


let parse_args (arglist : string list) : string =
  let projname = ref "" in
    let () = match arglist with
      | [] -> raise Project_filename_missing
      | name :: _ -> projname := name
    in
      !projname


let process_style (process_type : string) : string =
  match process_type with
    | "littlejohn" -> "box3d"
    | _ -> "box"


let get_child (target : string) (items : (string * Yaml.value) list) : string list =
  try
    match (List.assoc target items) with
    | `A(l) -> List.filter_map ( fun (item : Yaml.value) : string option ->
        match item with
        | `String(s) -> Some s
        | _ -> None
      ) l
    | _ -> raise Ill_formatted_child_yml
  with
  | Not_found -> []

let get_value (key : string) (items : (string * Yaml.value) list) : string =
  try
    match (Yaml.to_string (List.assoc key items)) with
    | Ok x -> String.trim x
    | _ -> raise Ill_formatted_child_yml
  with
    | Not_found -> ""

let get_optional_value (key : string) (items : (string * Yaml.value) list) : string option =
  try
    match (Yaml.to_string (List.assoc key items)) with
    | Ok x -> Some (String.trim x)
    | _ -> None
  with
    | Not_found -> None

let get_child_nodes (key : string) (items : (string * Yaml.value) list) : (string * Yaml.value) list =
  try
    match (List.assoc key items) with
    | `O(d) -> d
    | _ -> raise Ill_formatted_child_yml
  with
  | Not_found -> []


let build_process (item : string * Yaml.value) : process_node =
  let title, value = item in
    match value with
      | `O(d) ->
        let name = get_optional_value "name" d in
        let display_title = match name with
        | Some x -> x
        | None -> title
      in
        {
          title = display_title;
          style = get_value "type" d;
          inputs = get_child "inputs" d;
          outputs = get_child "outputs" d;
          context = None
        }
      | _ -> raise Ill_formatted_process_yml


let build_group (item : string * Yaml.value) : box_node =
  let title, value = item in
  match value with
      | `O(d) ->
        let name = get_optional_value "name" d in
        let display_title = match name with
        | Some x -> x
        | None -> title
      in
        {
          title = title;
          name = display_title;
          children = List.map build_process (get_child_nodes "children" d)
        }
      | _ -> raise Ill_formatted_process_yml


let build_generatve_group (item : string * Yaml.value) : generative_box_node =
  let title, value = item in
  match value with
      | `O(d) ->
        {
          title = title;
          names = get_child "names" d;
          children = List.map build_process (get_child_nodes "children" d)
        }
      | _ -> raise Ill_formatted_process_yml


let parse_process (item : string * Yaml.value) : node =
  let _, value = item in
    match value with
      | `O(d) -> (
        match (get_value "type" d) with
        | "group" -> Box (build_group item)
        | "groups" -> Generative (build_generatve_group item)
        | _ -> Process (build_process item)
      )
      | _ -> raise Ill_formatted_process_yml


let parse_project (project : Yaml.value) : node list =
  match project with
    | `O(d) -> List.map parse_process d
    | _ -> raise Ill_formatted_top_yml


let _display_project (project : node list) : unit =
  List.iter (
    fun node ->
      match node with
      | Generative _node -> (

      )
      | Box node -> (
        Printf.fprintf Stdlib.stdout "Group %s\n" node.title;
        List.iter (fun (child : process_node) ->
          Printf.fprintf Stdlib.stdout "\t%s\n" child.title;
          Printf.fprintf Stdlib.stdout "\tInputs:\n";
          List.iter ( fun item ->
            Printf.fprintf Stdlib.stdout "\t\t%s\n" item
          ) child.inputs;
          Printf.fprintf Stdlib.stdout "\tOutputs:\n";
          List.iter ( fun item ->
            Printf.fprintf Stdlib.stdout "\t\t%s\n" item
          ) child.outputs;
          Printf.fprintf Stdlib.stdout "\n";
        ) node.children;
      )
      | Process node -> (
        Printf.fprintf Stdlib.stdout "%s\n" node.title;
        Printf.fprintf Stdlib.stdout "Inputs:\n";
        List.iter ( fun item ->
          Printf.fprintf Stdlib.stdout "\t%s\n" item
        ) node.inputs;
        Printf.fprintf Stdlib.stdout "Outputs:\n";
        List.iter ( fun item ->
          Printf.fprintf Stdlib.stdout "\t%s\n" item
        ) node.outputs;
        Printf.fprintf Stdlib.stdout "\n";
      )
  ) project


let process_to_dot (node : process_node) (index : int ref) (nodemap : int StringMap.t ref) =
  let node_style = process_style node.style in
  let process_index = !index in
  index := !index + 1;
  Printf.fprintf Stdlib.stdout "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index node_style node.title;
  List.iter ( fun item ->
    try
      let input_index = StringMap.find item !nodemap in
        Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" input_index process_index
    with
      | Not_found -> (
        try
          match node.context with
          | None -> raise Not_found
          | Some c ->
            let input_index = StringMap.find (Printf.sprintf "%s %s" item c) !nodemap in
              Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" input_index process_index
        with Not_found ->
          let input_index = !index in
            index := !index + 1;
            nodemap := StringMap.add item input_index !nodemap;
            Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" input_index item;
            Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" input_index process_index
      )
  ) node.inputs;
  List.iter ( fun output_name ->
    let multi = match String.index_from_opt output_name 0 '*' with
      | Some(_) -> true
      | None -> false
    in
      let output_name = if multi then (String.sub output_name 0 ((String.length output_name) - 1_)) else output_name in
      let context_output_name = match node.context with
      | None -> output_name
      | Some c -> Printf.sprintf "%s %s" output_name c
      in
        let arrow_style = if multi then "crow" else "normal" in
          try
            let output_index = StringMap.find context_output_name !nodemap in
              Printf.fprintf Stdlib.stdout "\tn%d->n%d[arrowhead=\"%s\",penwidth=\"1.0\"];\n" process_index output_index arrow_style
          with
            | Not_found ->
              let output_index = !index in
                index := !index + 1;
                nodemap := StringMap.add context_output_name output_index !nodemap;
                Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" output_index output_name;
                Printf.fprintf Stdlib.stdout "\tn%d->n%d[arrowhead=\"%s\",penwidth=\"1.0\"];\n" process_index output_index arrow_style
  ) node.outputs;
  Printf.fprintf Stdlib.stdout "\n"

let group_to_dot (node : box_node) (index : int ref) (nodemap : int StringMap.t ref) =

  Printf.fprintf Stdlib.stdout "subgraph \"cluster_%s\" {\n" node.title;
  Printf.fprintf Stdlib.stdout "\tlabel = \"%s\"\n" node.name;

  List.iter (fun n ->
    process_to_dot n index nodemap
  ) node.children;

  Printf.fprintf Stdlib.stdout "}\n"

let generative_group_to_dot (node : generative_box_node) (index : int ref) (nodemap : int StringMap.t ref) =
  List.iteri ( fun i name ->
    let box = {
      title = Printf.sprintf "%d_%s" i node.title;
      name = name;
      children = List.map (fun (n : process_node) ->
        { n with
          title = Printf.sprintf "%s_%i" n.title i ;
          context = Some name;
        }) node.children;
    } in
    group_to_dot box index nodemap
  ) node.names


let project_to_dot (project : node list) : unit =
  let nodemap = ref StringMap.empty in
    Printf.fprintf Stdlib.stdout "digraph{\n";
    let index = ref 0 in
      List.iter ( fun node ->
        match node with
        | Process (p) -> process_to_dot p index nodemap
        | Box (b) -> group_to_dot b index nodemap
        | Generative (g) -> generative_group_to_dot g index nodemap
      ) project;
    Printf.fprintf Stdlib.stdout "}\n"


let () =
  Arg.parse speclist anon_fun usage_msg;
  let ymlfilename = parse_args !args in
    match Yaml_unix.of_file Fpath.(v ymlfilename) with
      | Result.Error(_) -> Printf.fprintf Stdlib.stderr "Didn't work"
      | Result.Ok(project) ->
        let process_list = parse_project(project) in
          (* display_project process_list *)
          project_to_dot process_list
