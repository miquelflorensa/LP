from antlr4 import *
from ExprLexer import ExprLexer
from ExprParser import ExprParser
from TreeVisitor import TreeVisitor
import sys
#input_stream = InputStream(input('? '))
input_stream = FileStream(sys.argv[1],encoding='utf-8')
lexer = ExprLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ExprParser(token_stream)
tree = parser.root()

if len(sys.argv) == 3:
    visitor = TreeVisitor(sys.argv[2])
elif len(sys.argv) > 3:
    visitor = TreeVisitor(sys.argv[2], [param for param in sys.argv[3:]])
else:
    visitor = TreeVisitor()

visitor.visit(tree)
