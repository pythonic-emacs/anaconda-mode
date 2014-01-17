import os
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger('start_jedi')

if not os.path.isdir("log"):
    os.mkdir("log")

handler = logging.FileHandler('log/test.log')
handler.setLevel(logging.DEBUG)

formatter = logging.Formatter(logging.BASIC_FORMAT)
handler.setFormatter(formatter)

logger.addHandler(handler)
logger.propagate = False  # Not echo to console
