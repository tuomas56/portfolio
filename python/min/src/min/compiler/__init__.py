import min.parser as parser
from . import freemarker as freemarker
from . import makemodule as makemodule
from . import envtransformer as envtransformer
from . import anftransformer as anftransformer
from . import linearize as linearize
import logging

logger = logging.getLogger(__name__)

def compile(source):
    logger.info('Parsing source.')
    ast = parser.parse(source)
    marked_ast = freemarker.transform(ast)
    module = makemodule.transform(marked_ast)
    env_module = envtransformer.transform(module)
    anf_module = anftransformer.transform(env_module)
    linear_module = linearize.transform(anf_module)
    logger.debug('Got final module:')
    for name, func in linear_module.functions.items():
        _, argnames, body = func
        logger.debug('%s:\n    Function(\n        %s,%s)', name, argnames, body)
    return linear_module