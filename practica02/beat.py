from antlr4 import *
from llullLexer import llullLexer
from llullParser import llullParser
from BeatVisitor import BeatVisitor
import sys

input_stream = FileStream(sys.argv[1], encoding='utf-8')
lexer = llullLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = llullParser(token_stream)
tree = parser.root()

visitor = BeatVisitor()

visitor.visit(tree)
