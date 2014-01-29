import start_jedi
import logging
import os

args = start_jedi.cli.args()

logger = logging.getLogger('start_jedi')

if args.debug:

    logger.setLevel(logging.DEBUG)

    if not os.path.isdir("log"):
        os.mkdir("log")

    handler = logging.FileHandler('log/development.log')
    handler.setLevel(logging.DEBUG)

    formatter = logging.Formatter(logging.BASIC_FORMAT)
    handler.setFormatter(formatter)

    logger.addHandler(handler)


start_jedi.run(args.ip, args.port)
