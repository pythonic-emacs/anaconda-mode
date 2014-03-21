try:
    from http import server
except ImportError:
    import BaseHTTPServer as server

from anaconda_mode import anaconda
import json
import logging

logger = logging.getLogger(__name__)


class Handler(server.BaseHTTPRequestHandler):

    error_message_format = ''

    def do_POST(self):
        """Process client POST request."""

        logger.info('Processing request...')

        try:

            content_len = self.headers['content-length']
            logger.debug('Content length: %s', content_len)

            request = self.rfile.read(int(content_len)).decode()
            logger.debug('Accepted content: %s', request)

            data = json.loads(request)

            response = anaconda.process(**data)
            logger.debug('Jedi result: %s', response)

        except TypeError:

            logger.exception('Request send wrong keywords combination')
            self.send_error(400)

        except ValueError:

            logger.exception('Request send broken json string')
            self.send_error(400)

        else:

            if response is not None:

                message = json.dumps(response).encode()
                logger.debug('Send message to host: %s', message)

                self.send_response(200)
                self.end_headers()
                self.wfile.write(message)

            else:

                self.send_error(500)

        return


class Rpc(server.HTTPServer):
    """Jedi HTTP server."""

    def start(self):
        """Start Jedi server."""

        logger.info('Starting Jedi server...')
        self.serve_forever()
