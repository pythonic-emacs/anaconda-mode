import jedi
import click
import logging
import functools
from jsonrpc import dispatcher, JSONRPCResponseManager

try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
except:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

logger = logging.getLogger(__name__)


class HTTPRequestHandler(BaseHTTPRequestHandler):

    protocol_version = 'HTTP/1.1'
    error_message_format = ''

    def do_POST(self):
        logger.info('Processing request...')
        content_len = self.headers.get('content-length')
        if content_len is not None:
            data = self.rfile.read(int(content_len))
            data = data.decode('utf-8')
            status, response = self.process_request(data)
        else:
            status, response = 400, 'Missing content-length header'

        response = response.encode('utf-8')
        self.send_response(status)
        self.send_header("Content-Length", len(response))
        self.end_headers()
        self.wfile.write(response)

    @staticmethod
    def process_request(request):
        response = JSONRPCResponseManager.handle(request, dispatcher)
        status = 500 if response.error else 200
        return status, response.json


def script_method(f):
    @dispatcher.add_method
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        try:
            return f(jedi.Script(source, line, column, path))
        except jedi.NotFoundError:
            return []

    return wrapper


@script_method
def complete(script):
    """Select auto-complete candidates for source position."""

    def first_line(text):
        """Return text first line."""
        return text.strip().split('\n', 1)[0]

    return [{'name': comp.name,
             'doc': comp.docstring() or None,
             'info': first_line(comp.docstring(raw=True)) or None,
             'type': comp.type,
             'path': comp.module_path or None,
             'line': comp.line}
            for comp in script.completions()]


@script_method
def doc(script):
    """Documentation for all definitions at point."""
    docs = ['\n'.join([d.module_name + ' - ' + d.description,
                       '=' * 40,
                       d.docstring() or "- No docstring -"]).strip()
            for d in script.goto_definitions()]

    return ('\n' + '-' * 40 + '\n').join(docs)


def process_definitions(f):
    @functools.wraps(f)
    def wrapper(script):
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
                for d in f(script) if not d.in_builtin_module()]

    return wrapper


@script_method
@process_definitions
def goto_definitions(script):
    return script.goto_definitions()


@script_method
@process_definitions
def goto_assignments(script):
    return script.goto_assignments()


@script_method
@process_definitions
def usages(script):
    return script.usages()


@script_method
def eldoc(script):
    """Return eldoc format documentation string or ''."""
    signatures = script.call_signatures()

    if len(signatures) == 1:
        sgn = signatures[0]
        return {
            'name': sgn.name,
            'index': sgn.index,
            'params': [p.description for p in sgn.params]
        }

    return {}


@click.command()
@click.option('--bind', default='', help='Interface address to bind.')
@click.option('--port', type=int, default=8000, help='Server port.')
@click.option('--debug', default=False, is_flag=True,
              help='Enable debug logging.')
def main(bind, port, debug):
    """Runs anaconda server."""
    if debug:
        logging.basicConfig(level=logging.DEBUG, filename='anaconda_mode.log')

    logger.info('Starting anaconda_mode server...')
    server = HTTPServer((bind, port), HTTPRequestHandler)

    click.echo('anaconda_mode server started')
    server.serve_forever()


if __name__ == '__main__':
    main()
