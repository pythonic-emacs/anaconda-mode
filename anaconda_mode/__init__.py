import sys
import os

BASE_DIR = os.path.dirname(__file__)
VENDOR_DIR = os.path.join(BASE_DIR, 'vendor')
sys.path.append(VENDOR_DIR)

import anaconda_mode.transport
import anaconda_mode.cli
import logging

logger = logging.getLogger(__name__)


def run_server(ip, port):
    """Run anaconda server."""

    address = (ip, port)

    handler = anaconda_mode.rpc.Handler

    node = anaconda_mode.rpc.Rpc(address, handler)

    node.start()


def main():
    """Program entry point."""

    args = anaconda_mode.cli.args()

    if args.debug:

        logger.setLevel(logging.DEBUG)

        if not os.path.isdir("log"):
            os.mkdir("log")

        handler = logging.FileHandler('log/development.log')
        handler.setLevel(logging.DEBUG)

        formatter = logging.Formatter(logging.BASIC_FORMAT)
        handler.setFormatter(formatter)

        logger.addHandler(handler)

    run_server(args.ip, args.port)
