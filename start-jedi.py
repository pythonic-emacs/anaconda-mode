#!/usr/bin/env python3

from http import server


class JediHandler(server.BaseHTTPRequestHandler):

    def do_POST(self):
        """Process client POST request."""

        self.send_response(200)
        self.end_headers()
        self.wfile.write('Hello\r\n'.encode())
        return


def main(port=8000):
    """Start Jedi node."""

    address = ('', port)
    httpd = server.HTTPServer(address, JediHandler)
    httpd.serve_forever()


if __name__ == '__main__':
    main()
