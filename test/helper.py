import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger('start_jedi')

handler = logging.FileHandler('log/tests.log')
handler.setLevel(logging.DEBUG)

formatter = logging.Formatter('%(levelname)s:%(name)s:%(message)s')
handler.setFormatter(formatter)

logger.addHandler(handler)
logger.propagate = False  # Not echo to console
