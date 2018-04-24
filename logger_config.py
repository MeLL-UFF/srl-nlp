import logging


def config_logger(args):
    """Logger settings"""
    FORMAT = "[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s"
    if args.verbosity == 0:
        logging.basicConfig(level=logging.CRITICAL, format=FORMAT)
    elif args.verbosity == 1:
        logging.basicConfig(level=logging.INFO, format=FORMAT)
    elif args.verbosity > 1:
        logging.basicConfig(level=logging.DEBUG, format=FORMAT)
    logger = logging.getLogger(__name__)
    if args.log:
        fh = logging.FileHandler(args.log)
        fh.setLevel(logging.INFO)
        logger.addHandler(fh)

    return logger


def add_logger_args(parser):
    parser.add_argument('-v', '--verbosity', action='count', default=0,
                        help='increase output verbosity')
    parser.add_argument('--log', help='saves log file')
