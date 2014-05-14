from anaconda_mode import rpc

try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
except ImportError:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

import logging

logger = logging.getLogger(__name__)


class Handler(BaseHTTPRequestHandler):

    error_message_format = ''

    def do_POST(self):
        """Process client POST request."""

        logger.info('Processing request...')

        content_len = self.headers['content-length']
        logger.debug('Content length: %s', content_len)

        data = self.rfile.read(int(content_len))
        logger.debug('Accepted content: %s', data)

        response = rpc.handle(data)
        logger.debug('Jedi result: %s', response.data)

        if response is not None:

            self.send_response(200)
            self.end_headers()
            self.wfile.write(response.json)

        else:

            self.send_error(500)


class Server(HTTPServer):
    """Anaconda HTTP server."""

    def start(self):
        """Start Anaconda server."""

        logger.info('Starting Jedi server...')
        self.serve_forever()
