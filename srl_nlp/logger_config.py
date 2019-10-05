import logging
import sys
from time import time

logger = logging.getLogger(__name__)


def config_logger(args):
    """Logger settings"""
    log_format = "[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s"

    if args.verbosity == 0:
        level = logging.CRITICAL
    elif args.verbosity == 1:
        level = logging.INFO
    else:
        level = logging.DEBUG
    if args.log:
        logging.basicConfig(filename=args.log, level=level, format=log_format)
    else:
        logging.basicConfig(level=level, format=log_format)


def silence_logger():
    """Only show log for Critical messages"""
    log_format = "[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s"
    level = logging.CRITICAL
    logging.basicConfig(level=level, format=log_format)


def _timeit(foo, level):
    def timer(*args, **kwargs):
        foo_name = sys._getframe().f_code.co_name
        start_time = time()
        foo(*args, **kwargs)
        elapsed_time = time() - start_time

        logger.log(level, '{{{foo}}} Elapsed time: {time} seconds'.format(foo=foo_name, time=elapsed_time))

    return timer


def timeit(foo, level=logging.INFO):
    return _timeit(foo, level)


def timeit_debug(foo):
    return _timeit(foo, logging.DEBUG)


def add_logger_args(parser):
    parser.add_argument('-v', '--verbosity', action='count', default=0,
                        help='increase output verbosity')
    parser.add_argument('--log', help='saves log file')
