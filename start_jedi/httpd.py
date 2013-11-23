from start_jedi import jedi
from http import server

import json
import logging

logger = logging.getLogger(__name__)


class JediHandler(server.BaseHTTPRequestHandler):

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
            result = jedi.process(**data)
            logger.debug('Jedi result: %s', result)

            message = json.dumps(result).encode()
            logger.debug('Send message to host: %s', message)

        except jedi.AdjectiveOperation:

            logger.exception('Request call unsupported API interface')
            self.send_error(400)

        except jedi.MissingSource:

            logger.exception('Request send incomplete source code.')
            self.send_error(400)

        except TypeError:

            logger.exception('Request send wrong keywords combination')
            self.send_error(400)

        except ValueError:

            logger.exception('Request send broken json string')
            self.send_error(400)

        else:

            self.send_response(200)
            self.end_headers()
            self.wfile.write(message)

        return


class JediServer(server.HTTPServer):
    """Jedi HTTP server."""

    def start(self):
        """Start Jedi server."""

        logger.info('Starting Jedi server...')
        self.serve_forever()
