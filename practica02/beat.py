from antlr4 import *
from ExprLexer import ExprLexer
from ExprParser import ExprParser
from BeatVisitor import BeatVisitor
import sys

input_stream = FileStream(sys.argv[1], encoding='utf-8')
lexer = ExprLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ExprParser(token_stream)
tree = parser.root()

visitor = BeatVisitor()

visitor.visit(tree)
