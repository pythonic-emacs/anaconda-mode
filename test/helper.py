import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger('start_jedi')

handler = logging.FileHandler('log/tests.log')
handler.setLevel(logging.DEBUG)

logger.addHandler(handler)
logger.propagate = False  # Not echo to console
