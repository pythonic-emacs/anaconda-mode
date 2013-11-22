from start_jedi import httpd


def run(port):
    """Run jedi server."""

    address = ('', port)

    node = httpd.JediServer(address, httpd.JediHandler)

    node.start()
