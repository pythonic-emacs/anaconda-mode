from start_jedi.httpd import start_jedi
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

start_jedi()
