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

        try:

            content_len = self.headers['content-length']
            logger.debug('Content length: %s', content_len)

            data = self.rfile.read(int(content_len))
            logger.debug('Accepted content: %s', data)

            response = rpc.handle(data)
            logger.debug('Jedi result: %s', response)

        except TypeError:

            logger.exception('Request send wrong keywords combination')
            self.send_error(400)

        except ValueError:

            logger.exception('Request send broken json string')
            self.send_error(400)

        else:

            if response is not None:

                logger.debug('Send message to host: %s', response.json)

                self.send_response(200)
                self.end_headers()
                self.wfile.write(response.json)

            else:

                self.send_error(500)

        return


class Server(HTTPServer):
    """Anaconda HTTP server."""

    def start(self):
        """Start Anaconda server."""

        logger.info('Starting Jedi server...')
        self.serve_forever()
