import start_jedi.httpd
import start_jedi.cli


def run(port):
    """Run jedi server."""

    address = ('', port)

    handler = start_jedi.httpd.JediHandler

    node = start_jedi.httpd.JediServer(address, handler)

    node.start()
