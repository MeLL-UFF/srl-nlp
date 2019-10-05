from __future__ import print_function

import logging
from configparser import ConfigParser
from os import path

config = ConfigParser()
_package_directory = path.dirname(__file__)
_config_path = path.join(_package_directory, 'external.conf')
config.read(_config_path)

logger = logging.getLogger(__name__)
logger.debug(f"Config path: {_config_path}")
