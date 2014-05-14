import sys
import os

BASE_DIR = os.path.dirname(__file__)
VENDOR_DIR = os.path.join(BASE_DIR, 'vendor')
sys.path.append(VENDOR_DIR)

try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
except ImportError:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

from jsonrpc import JSONRPCResponseManager, dispatcher
import argparse
import logging

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

        content_len = self.headers['content-length']
        logger.debug('Content length: %s', content_len)

        data = self.rfile.read(int(content_len))
        logger.debug('Accepted content: %s', data)

        response = handle(data)
        logger.debug('Jedi result: %s', response.data)

        self.send_response(500 if response.error else 200)
        self.end_headers()
        self.wfile.write(response.json.encode())


def handle(request):
    """Perform json rpc call."""

    return JSONRPCResponseManager.handle(request, dispatcher)


def parse_args():
    """Parse command line arguments."""

    parser = argparse.ArgumentParser(
        description='Jedi auto-completion server.')

    parser.add_argument('-d', '--debug', action='store_true',
                        help='enable debug logging')

    parser.add_argument('-i', '--ip', type=str, default='localhost',
                        help='server IP')

    parser.add_argument('-p', '--port', type=int, default=8000,
                        help='server port number')

    return parser.parse_args()


def main():
    """Program entry point."""

    args = parse_args()

    if args.debug:

        logger.setLevel(logging.DEBUG)

        if not os.path.isdir("log"):
            os.mkdir("log")

        handler = logging.FileHandler('log/development.log')
        handler.setLevel(logging.DEBUG)

        formatter = logging.Formatter(logging.BASIC_FORMAT)
        handler.setFormatter(formatter)

        logger.addHandler(handler)

    run_server(args.ip, args.port)


if __name__ == '__main__':
    main()
