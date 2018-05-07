import logging


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
    logger = logging.getLogger(__name__)

    return logger


def add_logger_args(parser):
    parser.add_argument('-v', '--verbosity', action='count', default=0,
                        help='increase output verbosity')
    parser.add_argument('--log', help='saves log file')
