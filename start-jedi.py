#!/usr/bin/env python3

import http.server as server
import json
import logging


logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)


def do_jedi(req):
    """Process Jedi operation correspond to request."""
    return req


class JediHandler(server.BaseHTTPRequestHandler):

    def do_POST(self):
        """Process client POST request."""

        logger.debug('Processing request...')

        content_len = int(self.headers['content-length'])
        logger.debug('Content length: %s', content_len)

        request = self.rfile.read(content_len).decode()
        logger.debug('Accepted content: %s', request)

        result = do_jedi(json.loads(request))
        logger.debug('Jedi result: %s', result)

        message = json.dumps(result).encode()
        logger.debug('Send message to host: %s', message)

        self.send_response(200)
        self.end_headers()
        self.wfile.write(message)

        return


def main(port=8000):
    """Start Jedi node."""

    address = ('', port)
    httpd = server.HTTPServer(address, JediHandler)
    httpd.serve_forever()


if __name__ == '__main__':
    main()
