from anaconda_common import script_method


def process_definitions(script, definitions):
    cache = {script.path: script.source.splitlines()}

    def get_description(d):
        if d.module_path not in cache:
            with open(d.module_path, 'r') as file:
                cache[d.module_path] = file.read().splitlines()

        return cache[d.module_path][d.line - 1]

    return [{'line': d.line,
             'column': d.column,
             'name': d.name,
             'description': get_description(d),
             'module': d.module_name,
             'type': d.type,
             'path': d.module_path}
            for d in definitions if not d.in_builtin_module()]


@script_method
def goto_definitions(script):
    return (process_definitions(script, script.goto_definitions()) or
            process_definitions(script, script.goto_assignments()))


@script_method
def usages(script):
    return process_definitions(script, script.usages())
