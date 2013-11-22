import start_jedi
import logging

args = start_jedi.cli.args()

if args.debug:
    logging.basicConfig(level=logging.DEBUG)

logger = logging.getLogger(__name__)

start_jedi.run(args.port)
