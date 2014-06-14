import logging
import os
import sys


def add_package_path(base):
    if base not in sys.path:
        sys.path.append(base)

    # Add every directory inside vendor/ to sys.path.
    vendor = os.path.join(base, 'vendor')
    for file in os.listdir(vendor):
        path = os.path.join(vendor, file)
        if os.path.isdir(path) and path not in sys.path:
            sys.path.append(path)

BASE_DIR = os.path.dirname(__file__)
add_package_path(BASE_DIR)

logger = logging.getLogger(__name__)

PY3 = sys.version_info[0] == 3

if PY3:
    from http.server import BaseHTTPRequestHandler, HTTPServer
    from importlib import import_module
else:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

    def import_module(name):
        __import__(name)
        return sys.modules[name]


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
        from jsonrpc import dispatcher, JSONRPCResponseManager
        response = JSONRPCResponseManager.handle(request, dispatcher)
        status = 500 if response.error else 200
        return status, response.json


import click  # noqa isort:skip


@click.command()
@click.option('--bind', default='', help='Interface address to bind.')
@click.option('--port', type=int, default=8000, help='Server port.')
@click.option('paths', '--path', multiple=True, type=click.Path())
@click.option('plugins', '--plugin', multiple=True)
@click.option('--debug', default=False, is_flag=True,
              help='Enable debug logging.')
def main(bind, port, paths, plugins, debug):
    """Runs anaconda server."""
    for path in paths:
        add_package_path(path)

    for plugin in plugins:
        import_module('anaconda_' + plugin)

    if debug:
        logfile = os.path.join(BASE_DIR, 'log', 'development.log')
        logging.basicConfig(level=logging.DEBUG, filename=logfile)

    logger.info('Starting anaconda_mode server...')
    server = HTTPServer((bind, port), HTTPRequestHandler)

    click.echo('anaconda_mode server started')
    server.serve_forever()

if __name__ == '__main__':
    main()
