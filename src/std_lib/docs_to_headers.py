UNSUPPORTED_TYPES = ["Sym", "ExeArr"]

def kebab_to_snake(name):
    return name.replace("-", "_").lower()

def main():
    with open("builtin_function_header_names.txt", mode="r", encoding="utf-8") as f:
        header_names = f.readlines()
    with open("builtin_function_headers.txt", mode="r", encoding="utf-8") as f: 
        header_types = f.readlines()
    
    headers = []
    replaced_names = []
    for (n, t) in zip(header_names, header_types):
        fn_name = n.strip()
        new_name = kebab_to_snake(fn_name.replace("->", "_to_"))
        if "!=" in new_name:
            new_name = new_name.replace("!=", "")
            new_name += "_neq"
        if "=" in new_name:
            new_name = new_name.replace("=", "")
            new_name += "_eq"
        if "!" in new_name:
            new_name = "not_" + new_name.replace("!", "")
        if "?" in new_name:
            new_name = "is_" + new_name.replace("?", "")
        if "~" in new_name:
            continue # Not supported

        if fn_name != new_name:
            replaced_names.append(f"{fn_name} {new_name}")
        fn_name = new_name

        types = t.strip()
        types_split = types.split("->")
        parameters = types_split[0].strip().split(",")
        return_type_str = ""
        if len(types_split) > 1:
            return_type = types.split("->")[-1].strip().replace(":", "")
            if return_type in UNSUPPORTED_TYPES:
                continue
            return_type_str = f" -> {return_type}"
        
        params_strs = []
        for param in parameters:
            if not param.strip():
                continue
            
            param_split = param.split(":")
            if len(param_split) == 1:
                parm_name = kebab_to_snake(param_split[0].strip())
                param_type = "T"
                continue
            else:
                (parm_name, param_type) = param.split(":")
                parm_name = kebab_to_snake(parm_name.strip())
                param_type = param_type.strip()
                if param_type in UNSUPPORTED_TYPES:
                    continue
            params_strs.append(f"{parm_name}: {param_type}")

        headers.append(f"{fn_name}({', '.join(params_strs)}){return_type_str}")
    with open("std_headers.txt", mode="w", encoding="utf-8") as f:
        f.write("\n".join(headers))
    with open("replaced_names.txt", mode="w", encoding="utf-8") as f:
        f.write("\n".join(replaced_names))


if __name__ == "__main__":
    main()