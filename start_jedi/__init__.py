from start_jedi import httpd
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)


def run():
    port = 8000
    address = ('', port)

    node = httpd.JediServer(address, httpd.JediHandler)

    node.start()
