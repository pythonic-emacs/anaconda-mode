#!/usr/bin/env python3

import http.server as server
import json


def do_jedi(req):
    """Process Jedi operation correspond to request."""
    return req


class JediHandler(server.BaseHTTPRequestHandler):

    def do_POST(self):
        """Process client POST request."""

        content_len = int(self.headers['content-length'])
        request = self.rfile.read(content_len).decode()
        result = do_jedi(json.loads(request))
        message = json.dumps(result).encode()

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
