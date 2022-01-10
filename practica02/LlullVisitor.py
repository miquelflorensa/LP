if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor


# Definció d'un procés
class Proces:
    def __init__(self, nom, parametres, instancies):
        self.nom = nom
        self.parametres = parametres
        self.instancies = instancies


# Definció d'una Excepció
class LlullExcepcio(Exception):
    def __init__(self, missatge):
        self.missatge = "Error: " + missatge


# Definció de la classe Visitor de Llull
class LlullVisitor(ExprVisitor):
    # Constructor de Visitor de Llull on per defecte el procés principal
    # és main i els parametres principals és una llista buida
    def __init__(self, procesPrincipal='main', parametresPrincipals=[]):
        self.stackSimbols = []
        self.processos = {}
        self.procesPrincipal = procesPrincipal
        self.parametresPrincipals = parametresPrincipals

    # Visitador produït per root
    def visitRoot(self, ctx):
        l = list(ctx.getChildren())
        for i in range(len(l)-1):
            self.visit(l[i])

    # Visitador produït per procediments
    def visitProcediments(self, ctx):
        l = list(ctx.getChildren())

        # Visitem tots el procediments del input
        for i in range(len(l)):
            self.visit(l[i])

        dictAux = {}
        paraPrinc = self.processos[self.procesPrincipal].parametres

        # Excepció en cas que el nombre de paràmetres no coincideixi
        if len(paraPrinc) != len(self.parametresPrincipals):
            raise LlullExcepcio("S'esperaven " + str(len(paraPrinc)) +
                " parametre(s), però s'han trobat " +
                str(len(self.parametresPrincipals)) + ".")

        for i in range(len(paraPrinc)):
            dictAux[paraPrinc[i]] = int(self.parametresPrincipals[i])

        # Anem al procés principal i apilem els seus paràmetres
        self.stackSimbols.append(dictAux)
        return self.visit(self.processos[self.procesPrincipal].instancies)

    # Visitador produït per procediment
    def visitProcediment(self, ctx):
        l = list(ctx.getChildren())
        nom = l[1].getText()

        # Excepció en cas que el procediment ja estigui definit
        if nom in self.processos:
            raise LlullExcepcio("Procediment " + nom + " ja està definit.")

        parametres = []
        param = l[3].getText()
        i = 3
        while param != ')':
            if param != ',':
                if param in parametres:
                    raise LlullExcepcio("Paràmetre duplicat en la funció " + nom)
                else:
                    parametres.append(param)
            i += 1
            param = l[i].getText()

        # Afegim al diccionari de processos el procés llegit
        self.processos[nom] = Proces(nom, parametres, ctx.instancies())

    # Visitador produït per proc (l'invocador de procediments)
    def visitProc(self, ctx):
        l = list(ctx.getChildren())
        nom = l[0].getText()

        # Excepció en cas que el procediment no hagi estat definit
        if nom not in self.processos:
            raise LlullExcepcio("Procediment " + nom + " no està definit.")

        parametres = []
        param = l[2].getText()
        i = 2
        j = 0
        while param != ')':
            if param != ',':
                parametres.append(self.visit(l[i]))
                j += 1
            i += 1
            param = l[i].getText()

        # Excepció en cas que el nombre de paràmetres no coincideixi
        if len(self.processos[nom].parametres) != len(parametres):
            raise LlullExcepcio("En la funció " + nom + " s'esperaven " +
                str(len(self.processos[nom].parametres)) + " parametre(s)" +
                ", però s'han trobat " + str(len(parametres)) + ".")

        # Creem i omplim un diccionari a partir d'una llista de paràmetres
        parametresProcediment = {}
        for parametre, valor in zip(self.processos[nom].parametres, parametres):
            parametresProcediment[parametre] = valor

        # Apilem els paràmetres al stack de simbols, visitem el procés
        # que es vol invocar i fem un pop dels paràmetres
        self.stackSimbols.append(parametresProcediment)
        self.visit(self.processos[nom].instancies)
        self.stackSimbols.pop()

    # Visitador produït per taulesArray (que crea un array)
    def visitTaulesArray(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        n = int(self.visit(l[4]))
        array = []
        array = [0 for i in range(n)]
        self.stackSimbols[-1][nom] = array

    # Visitador produït per taulesSet
    def visitTaulesSet(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        i = int(self.visit(l[4]))
        x = int(self.visit(l[6]))
        lengthArray = len(self.stackSimbols[-1][nom])

        # Excepció en cas que s'intenti accedir a un index inexistent
        if i >= lengthArray or i < 0:
            raise LlullExcepcio("Accés a un índex inexistent de la taula " +
                nom + ".")

        self.stackSimbols[-1][nom][i] = x

    # Visitador produït per taulesGet
    def visitTaulesGet(self, ctx):
        l = list(ctx.getChildren())
        nom = l[2].getText()
        i = int(self.visit(l[4]))
        lengthArray = len(self.stackSimbols[-1][nom])

        # Excepció en cas que s'intenti accedir a un index inexistent
        if i >= lengthArray or i < 0:
            raise LlullExcepcio("Accés a un índex inexistent de la taula " +
                nom + ".")

        return self.stackSimbols[-1][nom][i]

    # Visitador produït per Num
    def visitNum(self, ctx):
        l = list(ctx.getChildren())
        return int(l[0].getText())

    # Visitador produït per Var
    def visitVar(self, ctx):
        l = list(ctx.getChildren())
        var = l[0].getText()
        return self.stackSimbols[-1][var]

    # Visitador produït per Coment
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

    # Visitador produït per assignacio
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

    # Visitador que permet escriure pel canal d'entrada estàndard
    def visitWrite(self, ctx):
        l = list(ctx.getChildren())
        for i in range(2, len(l)):
            if i % 2 == 0:
                print(self.visit(l[i]), end=" ")
        print()

    # ==================== OPERADORS ========================

    # Visitador produït per Mod
    def visitMod(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) % self.visit(l[2])

    # Visitador produït per Mul
    def visitMul(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) * self.visit(l[2])

    # Visitador produït per Div
    def visitDiv(self, ctx):
        l = list(ctx.getChildren())
        denominador = self.visit(l[2])
        if denominador == 0:
            raise LlullExcepcio("Divisió per zero.")
        return self.visit(l[0]) / denominador

    # Visitador produït per Sum
    def visitSum(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])

    # Visitador produït per Res
    def visitRes(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) - self.visit(l[2])

    # Visitador del comparador d'igualtat (==)
    def visitEq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) == self.visit(l[2]):
            return 1
        else:
            return 0

    # Visitador del comparador de desigualtat (<>)
    def visitNeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) != self.visit(l[2]):
            return 1
        else:
            return 0

    # Visitador del comparador de major que (>)
    def visitGt(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) > self.visit(l[2]):
            return 1
        else:
            return 0

    # Visitador del comparador de menor que (<)
    def visitLt(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) < self.visit(l[2]):
            return 1
        else:
            return 0

    # Visitador del comparador de major o igual que (>=)
    def visitGeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) >= self.visit(l[2]):
            return 1
        else:
            return 0

    # Visitador del comparador de menor o igual que (<=)
    def visitLeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) <= self.visit(l[2]):
            return 1
        else:
            return 0

    # ============================================================
