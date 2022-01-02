# Generated from Expr.g by ANTLR 4.7.2
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
else:
    from ExprParser import ExprParser

# This class defines a complete generic visitor for a parse tree produced by ExprParser.

class ExprVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ExprParser#root.
    def visitRoot(self, ctx:ExprParser.RootContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#instancies.
    def visitInstancies(self, ctx:ExprParser.InstanciesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#instancia.
    def visitInstancia(self, ctx:ExprParser.InstanciaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#procediment.
    def visitProcediment(self, ctx:ExprParser.ProcedimentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#proc.
    def visitProc(self, ctx:ExprParser.ProcContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Div.
    def visitDiv(self, ctx:ExprParser.DivContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Res.
    def visitRes(self, ctx:ExprParser.ResContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Mod.
    def visitMod(self, ctx:ExprParser.ModContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Mul.
    def visitMul(self, ctx:ExprParser.MulContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Var.
    def visitVar(self, ctx:ExprParser.VarContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Num.
    def visitNum(self, ctx:ExprParser.NumContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Sum.
    def visitSum(self, ctx:ExprParser.SumContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#condicional.
    def visitCondicional(self, ctx:ExprParser.CondicionalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#iteracioWhile.
    def visitIteracioWhile(self, ctx:ExprParser.IteracioWhileContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#iteracioFor.
    def visitIteracioFor(self, ctx:ExprParser.IteracioForContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Eq.
    def visitEq(self, ctx:ExprParser.EqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Neq.
    def visitNeq(self, ctx:ExprParser.NeqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Gt.
    def visitGt(self, ctx:ExprParser.GtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Lt.
    def visitLt(self, ctx:ExprParser.LtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Geq.
    def visitGeq(self, ctx:ExprParser.GeqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#Leq.
    def visitLeq(self, ctx:ExprParser.LeqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#assignacio.
    def visitAssignacio(self, ctx:ExprParser.AssignacioContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#write.
    def visitWrite(self, ctx:ExprParser.WriteContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExprParser#read.
    def visitRead(self, ctx:ExprParser.ReadContext):
        return self.visitChildren(ctx)



del ExprParser