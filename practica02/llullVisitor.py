# Generated from llull.g4 by ANTLR 4.7.2
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .llullParser import llullParser
else:
    from llullParser import llullParser

# This class defines a complete generic visitor for a parse tree produced by llullParser.

class llullVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by llullParser#root.
    def visitRoot(self, ctx:llullParser.RootContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#procediments.
    def visitProcediments(self, ctx:llullParser.ProcedimentsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#instancies.
    def visitInstancies(self, ctx:llullParser.InstanciesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#instancia.
    def visitInstancia(self, ctx:llullParser.InstanciaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#proc.
    def visitProc(self, ctx:llullParser.ProcContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#procediment.
    def visitProcediment(self, ctx:llullParser.ProcedimentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#condicional.
    def visitCondicional(self, ctx:llullParser.CondicionalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#iteracioWhile.
    def visitIteracioWhile(self, ctx:llullParser.IteracioWhileContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#iteracioFor.
    def visitIteracioFor(self, ctx:llullParser.IteracioForContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#taulesArray.
    def visitTaulesArray(self, ctx:llullParser.TaulesArrayContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#taulesSet.
    def visitTaulesSet(self, ctx:llullParser.TaulesSetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#assignacio.
    def visitAssignacio(self, ctx:llullParser.AssignacioContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#write.
    def visitWrite(self, ctx:llullParser.WriteContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#read.
    def visitRead(self, ctx:llullParser.ReadContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Div.
    def visitDiv(self, ctx:llullParser.DivContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Res.
    def visitRes(self, ctx:llullParser.ResContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Mod.
    def visitMod(self, ctx:llullParser.ModContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Mul.
    def visitMul(self, ctx:llullParser.MulContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Var.
    def visitVar(self, ctx:llullParser.VarContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Num.
    def visitNum(self, ctx:llullParser.NumContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Coment.
    def visitComent(self, ctx:llullParser.ComentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Sum.
    def visitSum(self, ctx:llullParser.SumContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#TaulesGet.
    def visitTaulesGet(self, ctx:llullParser.TaulesGetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Eq.
    def visitEq(self, ctx:llullParser.EqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Neq.
    def visitNeq(self, ctx:llullParser.NeqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Gt.
    def visitGt(self, ctx:llullParser.GtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Lt.
    def visitLt(self, ctx:llullParser.LtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Geq.
    def visitGeq(self, ctx:llullParser.GeqContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by llullParser#Leq.
    def visitLeq(self, ctx:llullParser.LeqContext):
        return self.visitChildren(ctx)



del llullParser