if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor


class Proces:
    def __init__(self, nom, parametres, instancies):
        self.nom = nom
        self.parametres = parametres
        self.instancies = instancies



class TreeVisitor(ExprVisitor):

    def __init__(self, procesPrincipal='main', parametresPrincipals=[]):
        self.stackSimbols = []
        self.processos = {}
        self.procesPrincipal = procesPrincipal
        self.parametresPrincipals = parametresPrincipals

    def visitRoot(self, ctx):
        l = list(ctx.getChildren())
        for i in range(len(l)-1):
            self.visit(l[i])


    def visitProcediments(self, ctx):
        l = list(ctx.getChildren())
        for i in range(len(l)):
            self.visit(l[i])
        dictAux = {}
        paraPrinc = self.processos[self.procesPrincipal].parametres
        for i in range(len(paraPrinc)):
            dictAux[paraPrinc[i]] = int(self.parametresPrincipals[i])

        self.stackSimbols.append(dictAux)
        return self.visit(self.processos[self.procesPrincipal].instancies)

    def visitProcediment(self, ctx):
        l = list(ctx.getChildren())
        nom = l[1].getText()
        parametres = []
        param = l[3].getText()
        i = 3
        while param != ')':
            if param != ',':
                parametres.append(param)
            i += 1
            param = l[i].getText()
        self.processos[nom] = Proces(nom, parametres, ctx.instancies())

    def visitProc(self, ctx):
        l = list(ctx.getChildren())
        nom = l[0].getText()
        parametres = {}
        param = l[2].getText()
        i = 2
        j = 0
        while param != ')':
            if param != ',':
                parametres[self.processos[nom].parametres[j]] = self.visit(l[i])
                j += 1
            i += 1
            param = l[i].getText()

        self.stackSimbols.append(parametres)
        self.visit(self.processos[nom].instancies)
        self.stackSimbols.pop()

    def visitTaulesArray(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        n = int(self.visit(l[4]))
        array = []
        array = [0 for i in range(n)]
        self.stackSimbols[-1][nom] = array

    def visitTaulesSet(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        i = int(self.visit(l[4]))
        x = int(self.visit(l[6]))
        self.stackSimbols[-1][nom][i] = x

    def visitTaulesGet(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        i = int(self.visit(l[4]))
        return self.stackSimbols[-1][nom][i]

    def visitMod(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) % self.visit(l[2])

    def visitMul(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) * self.visit(l[2])

    def visitDiv(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) / self.visit(l[2])

    def visitSum(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])

    def visitRes(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) - self.visit(l[2])

    def visitNum(self, ctx):
        l = list(ctx.getChildren())
        return int(l[0].getText())

    def visitVar(self, ctx):
        l = list(ctx.getChildren())
        var = l[0].getText()
        return self.stackSimbols[-1][var]

    def visitComent(self, ctx):
        l = list(ctx.getChildren())
        string = l[0].getText()
        string = string[:-1]
        string = string[1:]
        return string


    # Visitador del condicional
    def visitCondicional(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[2]) == 1:
            return self.visit(l[5])
        elif len(l) > 7:
            return self.visit(l[9])

    # Visitador per l'iteració amb while
    def visitIteracioWhile(self, ctx):
        l = list(ctx.getChildren())
        while self.visit(l[2]) == 1:
            self.visit(l[5])


    # Visitador per l'iteració amb for
    def visitIteracioFor(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[2])

        while self.visit(l[4]) == 1:
            self.visit(l[9])
            self.visit(l[6])



    #==================== COMPARADORS ===========================

    # Visitador del comparador d'igualtat (==)
    def visitEq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) == self.visit(l[2]):
            return 1
        else: return 0

    # Visitador del comparador de desigualtat (<>)
    def visitNeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) != self.visit(l[2]):
            return 1
        else: return 0

    # Visitador del comparador de major que (>)
    def visitGt(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) > self.visit(l[2]):
            return 1
        else: return 0

    # Visitador del comparador de menor que (<)
    def visitLt(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) < self.visit(l[2]):
            return 1
        else: return 0

    # Visitador del comparador de major o igual que (>=)
    def visitGeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) >= self.visit(l[2]):
            return 1
        else: return 0

    # Visitador del comparador de menor o igual que (<=)
    def visitLeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) <= self.visit(l[2]):
            return 1
        else: return 0

    #============================================================

    def visitAssignacio(self, ctx):
        l = list(ctx.getChildren())
        var = l[0].getText()
        self.stackSimbols[-1][var] = self.visit(l[2])

    # Visitador que permet llegir del canal d'entrada estàndard
    def visitRead(self, ctx):
        l = list(ctx.getChildren())
        var = l[2].getText()
        value = int(input())
        self.stackSimbols[-1][var] = value

    def visitWrite(self, ctx):
        l = list(ctx.getChildren())
        for i in range(2, len(l)):
            if i%2 == 0:
                print(self.visit(l[i]), end =" ")
        print()
