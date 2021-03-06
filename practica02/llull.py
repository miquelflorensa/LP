from antlr4 import *
from llullLexer import llullLexer
from llullParser import llullParser
from LlullVisitor import LlullVisitor
import sys

input_stream = FileStream(sys.argv[1], encoding='utf-8')
lexer = llullLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = llullParser(token_stream)
tree = parser.root()

# Depenent de la mida de l'entrada passem o no paràmetres a LlullVisitor
if len(sys.argv) == 3:
    visitor = LlullVisitor(sys.argv[2])
elif len(sys.argv) > 3:
    visitor = LlullVisitor(sys.argv[2], [param for param in sys.argv[3:]])
else:
    visitor = LlullVisitor()

visitor.visit(tree)
