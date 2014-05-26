import sys
import os

BASE_DIR = os.path.dirname(__file__)
LOG_DIR = os.path.join(BASE_DIR, 'log')

# Add every directory inside vendor/ to sys.path.
for file in os.listdir('vendor'):
    path = os.path.join(BASE_DIR, 'vendor', file)
    if os.path.isdir(path) and path not in sys.path:
        sys.path.append(path)

try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
except ImportError:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

from jsonrpc import JSONRPCResponseManager, dispatcher
import logging
import click

logger = logging.getLogger(__name__)

# FIXME: This must be an positional argument.
import anaconda_jedi
import anaconda_eldoc


def run_server(ip, port):
    """Run anaconda server."""

    logger.info('Starting Jedi server...')

    address = (ip, port)

    node = HTTPServer(address, RequestHandler)

    node.serve_forever()


class RequestHandler(BaseHTTPRequestHandler):

    error_message_format = ''

    def do_POST(self):
        """Process client POST request."""

        logger.info('Processing request...')

        content_len = self.headers.get('content-length')
        if content_len is not None:
            data = self.rfile.read(int(content_len))
            status, response = handle(data)
        else:
            status, response = 400, 'Missing content-length header'

        self.send_response(status)
        self.end_headers()
        self.wfile.write(response.encode())


def handle(request):
    """Perform json rpc call."""

    response = JSONRPCResponseManager.handle(request, dispatcher)
    status = 500 if response.error else 200
    return status, response.json


def setup_logger(logfile):
    """Set logger handler, formatter params.

    Create logging directory if necessary.

    :param logfile: log file name
    :type logfile: str
    """

    logger.setLevel(logging.DEBUG)

    if not os.path.isdir(LOG_DIR):
        os.mkdir(LOG_DIR)

    handler = logging.FileHandler(os.path.join(LOG_DIR, logfile))
    handler.setLevel(logging.DEBUG)

    formatter = logging.Formatter(logging.BASIC_FORMAT)
    handler.setFormatter(formatter)

    logger.addHandler(handler)


@click.command()
@click.option('--ip', default='127.0.0.1', help='Server IP.')
@click.option('--port', type=int, default=8000, help='Server port.')
@click.option('--debug', default=False, is_flag=True,
              help='Enable debug logging.')
def main(ip, port, debug):
    """Runs anaconda server."""
    if debug:
        setup_logger('development.log')

    run_server(ip, port)


if __name__ == '__main__':
    main()
