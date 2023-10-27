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
}


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
    let value = List.assoc target items in
      match value with
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
    let value = List.assoc key items in
      let strval = Yaml.to_string value in
        match strval with
          | Ok x -> String.trim x
          | _ -> raise Ill_formatted_child_yml
  with
    | Not_found -> ""

let parse_process (item : string * Yaml.value) : process_node =
  let title, value = item in
    match value with
      | `O(d) -> {
          title = title;
          style = get_value "type" d;
          inputs = get_child "inputs" d;
          outputs = get_child "outputs" d
        }
      | _ -> raise Ill_formatted_process_yml


let parse_project (project : Yaml.value) : process_node list =
  match project with
    | `O(d) -> List.map parse_process d
    | _ -> raise Ill_formatted_top_yml


let _display_project (project : process_node list) : unit =
  List.iter (
    fun node ->
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
  ) project


let project_to_dot (project : process_node list) : unit =
  let nodemap = ref StringMap.empty in
    Printf.fprintf Stdlib.stdout "digraph{\n";
    let index = ref 0 in
      List.iter (
        fun node ->
          let node_style = process_style node.style in
            let process_index = !index in
              index := !index + 1;
              Printf.fprintf Stdlib.stdout "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index node_style node.title;
              List.iter ( fun item ->
                try
                  let input_index = StringMap.find item !nodemap in
                    Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" input_index process_index
                with
                  | Not_found ->
                    let input_index = !index in
                      index := !index + 1;
                      nodemap := StringMap.add item input_index !nodemap;
                      Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" input_index item;
                      Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" input_index process_index
              ) node.inputs;
              List.iter ( fun output_name ->
                let multi = match String.index_from_opt output_name 0 '*' with
                  | Some(_) -> true
                  | None -> false
                in
                  let output_name = if multi then (String.sub output_name 0 ((String.length output_name) - 1_)) else output_name in
                    let arrow_style = if multi then "crow" else "normal" in
                      try
                        let output_index = StringMap.find output_name !nodemap in
                          Printf.fprintf Stdlib.stdout "\tn%d->n%d[arrowhead=\"%s\",penwidth=\"1.0\"];\n" process_index output_index arrow_style
                      with
                        | Not_found ->
                          let output_index = !index in
                            index := !index + 1;
                            nodemap := StringMap.add output_name output_index !nodemap;
                            Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" output_index output_name;
                            Printf.fprintf Stdlib.stdout "\tn%d->n%d[arrowhead=\"%s\",penwidth=\"1.0\"];\n" process_index output_index arrow_style
              ) node.outputs;
              Printf.fprintf Stdlib.stdout "\n"
      ) project;
    Printf.fprintf Stdlib.stdout "}\n"


let () =
  Arg.parse speclist anon_fun usage_msg;
  let ymlfilename = parse_args !args in
    match Yaml_unix.of_file Fpath.(v ymlfilename) with
      | Result.Error(_) -> Printf.fprintf Stdlib.stderr "Didn't work"
      | Result.Ok(project) ->
        let process_list = parse_project(project) in
          (* display_project process_list; *)
          project_to_dot process_list
