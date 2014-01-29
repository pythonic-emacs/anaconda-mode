import argparse


def args():
    """Parse command line arguments."""

    parser = argparse.ArgumentParser(
        description='Jedi auto-completion server.')

    parser.add_argument('-d', '--debug', action='store_true',
                        help='enable debug logging')

    parser.add_argument('-i', '--ip', type=str, default='localhost',
                        help='server IP')

    parser.add_argument('-p', '--port', type=int, default=8000,
                        help='server port number')

    return parser.parse_args()
