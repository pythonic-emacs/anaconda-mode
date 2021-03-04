
from __future__ import print_function
import sys
import os
import site
from distutils.version import LooseVersion

# CLI arguments.

assert len(sys.argv) > 3, 'CLI arguments: %s' % sys.argv

server_directory = sys.argv[-3]
server_address = sys.argv[-2]
virtual_environment = sys.argv[-1]

# Ensure directory.

server_directory = os.path.expanduser(server_directory)
virtual_environment = os.path.expanduser(virtual_environment)

# Installation check.

IS_PY2 = sys.version_info[0] == 2

# jedi versions >= 0.18 don't support Python 2
if IS_PY2:
    jedi_dep = ('jedi', '0.17.2')
    server_directory += '-py2'
else:
    jedi_dep = ('jedi', '0.18.0')
    server_directory += '-py3'
service_factory_dep = ('service_factory', '0.1.6')

if not os.path.exists(server_directory):
    os.makedirs(server_directory)
site.addsitedir(server_directory)

missing_dependencies = []


def is_package_dir(path):
    if os.path.isdir(path):
        if IS_PY2:
            return path.endswith(".egg")
        else:
            return not (path.endswith(".dist-info") or path.endswith(".egg-info"))
    return False

def instrument_installation():
    for package in (jedi_dep, service_factory_dep):
        package_is_installed = False
        for path in os.listdir(server_directory):
            path = os.path.join(server_directory, path)
            if is_package_dir(path):
                if path not in sys.path:
                    sys.path.insert(0, path)
                if package[0] in path:
                    package_is_installed = True
        if not package_is_installed:
            missing_dependencies.append('=='.join(package))

instrument_installation()

# Installation.

def install_deps_setuptools():
    import setuptools.command.easy_install
    cmd = ['--install-dir', server_directory,
           '--site-dirs', server_directory,
           '--always-copy', '--always-unzip']
    cmd.extend(missing_dependencies)
    setuptools.command.easy_install.main(cmd)
    instrument_installation()

def install_deps_pip():
    import subprocess
    cmd = [sys.executable, '-m', 'pip', 'install', '--target', server_directory]
    cmd.extend(missing_dependencies)
    subprocess.check_call(cmd)
    instrument_installation()

if missing_dependencies:
    if IS_PY2:
        install_deps_setuptools()
    else:
        install_deps_pip()

del missing_dependencies[:]

try:
    import jedi
except ImportError:
    missing_dependencies.append('=='.join(jedi_dep))

try:
    import service_factory
except ImportError:
    missing_dependencies.append('>='.join(service_factory_dep))

# Try one more time in case if anaconda installation gets broken somehow
if missing_dependencies:
    if IS_PY2:
        install_deps_setuptools()
    else:
        install_deps_pip()
    import jedi
    import service_factory

# Setup server.

assert LooseVersion(jedi.__version__) >= LooseVersion(jedi_dep[1]), 'Jedi version should be >= %s, current version: %s' % (jedi_dep[1], jedi.__version__)

if virtual_environment:
    virtual_environment = jedi.create_environment(virtual_environment, safe=False)
else:
    virtual_environment = None

# Define JSON-RPC application.

import functools
import threading

def script_method(f):
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        timer = threading.Timer(30.0, sys.exit)
        timer.start()
        result = f(jedi.Script(source, path=path, environment=virtual_environment), line, column)
        timer.cancel()
        return result
    return wrapper

def process_definitions(f):
    @functools.wraps(f)
    def wrapper(script, line, column):
        definitions = f(script, line, column)
        if len(definitions) == 1 and not definitions[0].module_path:
            return '%s is defined in %s compiled module' % (
                definitions[0].name, definitions[0].module_name)
        return [[str(definition.module_path),
                 definition.line,
                 definition.column,
                 definition.get_line_code().strip()]
                for definition in definitions
                if definition.module_path] or None
    return wrapper

@script_method
def complete(script, line, column):
    return [[definition.name, definition.type]
            for definition in script.complete(line, column)]

@script_method
def company_complete(script, line, column):
    return [[definition.name,
             definition.type,
             definition.docstring(),
             str(definition.module_path),
             definition.line]
            for definition in script.complete(line, column)]

@script_method
def show_doc(script, line, column):
    return [[definition.module_name, definition.docstring()]
            for definition in script.infer(line, column)]

@script_method
@process_definitions
def infer(script, line, column):
    return script.infer(line, column)

@script_method
@process_definitions
def goto(script, line, column):
    return script.goto(line, column)

@script_method
@process_definitions
def get_references(script, line, column):
    return script.get_references(line, column)

@script_method
def eldoc(script, line, column):
    signatures = script.get_signatures(line, column)
    if len(signatures) == 1:
        signature = signatures[0]
        return [signature.name,
                signature.index,
                [param.description[6:] for param in signature.params]]

# Run.

app = [complete, company_complete, show_doc, infer, goto, get_references, eldoc]

service_factory.service_factory(app, server_address, 0, 'anaconda_mode port {port}')
