import start_jedi.jedi as jedi

import http.server as server
import json
import logging

logger = logging.getLogger(__name__)


class JediHandler(server.BaseHTTPRequestHandler):

    def do_POST(self):
        """Process client POST request."""

        logger.info('Processing request...')

        try:

            content_len = int(self.headers['content-length'])
            logger.debug('Content length: %s', content_len)

            request = self.rfile.read(content_len).decode()
            logger.debug('Accepted content: %s', request)

            result = jedi.do_jedi(json.loads(request))
            logger.debug('Jedi result: %s', result)

            message = json.dumps(result).encode()
            logger.debug('Send message to host: %s', message)

        except (TypeError, ValueError):

            logger.exception('Request processing error', exc_info=True)

        else:

            self.send_response(200)
            self.end_headers()
            self.wfile.write(message)

        return


def start_jedi(port=8000):
    """Start Jedi node."""

    address = ('', port)
    httpd = server.HTTPServer(address, JediHandler)
    logger.info('Starting server...')
    httpd.serve_forever()
