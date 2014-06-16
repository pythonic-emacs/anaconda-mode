import logging
import os
import sys

PY3 = sys.version_info[0] == 3

if PY3:
    from http.server import BaseHTTPRequestHandler, HTTPServer
else:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

BASE_DIR = os.path.dirname(__file__)
logger = logging.getLogger(__name__)

# Add every directory inside vendor/ to sys.path.
for file in os.listdir('vendor'):
    path = os.path.join(BASE_DIR, 'vendor', file)
    if os.path.isdir(path) and path not in sys.path:
        sys.path.append(path)

import click  # isort:skip
from jsonrpc import dispatcher, JSONRPCResponseManager  # isort:skip

# FIXME: This must be an positional argument.  See #28.
import anaconda_jedi   # noqa isort:skip
import anaconda_eldoc  # noqa isort:skip


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
