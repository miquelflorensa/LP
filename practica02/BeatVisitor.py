from termcolor import colored
if __name__ is not None and "." in __name__:
    from .llullParser import llullParser
    from .llullVisitor import llullVisitor
else:
    from llullParser import llullParser
    from llullVisitor import llullVisitor


class BeatVisitor(llullVisitor):

    def __init__(self):
        self.forActive = 0
        self.level = 0

    def visitRoot(self, ctx):
        l = list(ctx.getChildren())
        for i in range(len(l)-1):
            self.visit(l[i])

    def espais(self, n):
        string = ""
        for i in range(n):
            string += "    "
        return string

    def visitProcediments(self, ctx):
        l = list(ctx.getChildren())
        for i in range(len(l)):
            self.visit(l[i])

    def visitProcediment(self, ctx):
        l = list(ctx.getChildren())
        string = colored(l[0].getText(), 'red') + " "
        string += colored(l[1].getText(), 'magenta')
        string += '('
        i = 3
        while l[i].getText() != ')':
            if l[i].getText() == ',':
                string += ', '
            else:
                string += colored(l[i].getText(), 'green')
            i += 1
        string += ') {'
        print(string)
        self.level += 1
        self.visit(l[i+2])
        self.level -= 1
        print("}")

    def visitProc(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        self.level += 1
        string += colored(l[0].getText(), 'cyan') + '('
        for i in range(2, len(l)-1):
            if l[i].getText() == ',':
                string += ', '
            else:
                string += self.visit(l[i])
        string += ')'
        self.level -= 1
        print(string)

    def visitTaulesArray(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        string += colored(l[0].getText(), 'yellow') + '('
        string += self.visit(l[2]) + ', '
        string += self.visit(l[4]) + ') '
        print(string)

    def visitTaulesSet(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        string += colored(l[0].getText(), 'yellow') + '('
        string += self.visit(l[2]) + ', '
        string += self.visit(l[4]) + ', '
        string += self.visit(l[6]) + ')'
        print(string)

    def visitTaulesGet(self, ctx):
        l = list(ctx.getChildren())
        string = colored(l[0].getText(), 'yellow') + '('
        string += self.visit(l[2]) + ', '
        string += self.visit(l[4]) + ')'
        return string

    def visitMod(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += ' % '
        string += self.visit(l[2])
        return string

    def visitMul(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += ' * '
        string += self.visit(l[2])
        return string

    def visitDiv(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += ' / '
        string += self.visit(l[2])
        return string

    def visitSum(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += ' + '
        string += self.visit(l[2])
        return string

    def visitRes(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += ' - '
        string += self.visit(l[2])
        return string

    def visitNum(self, ctx):
        l = list(ctx.getChildren())
        string = colored(l[0].getText(), 'cyan')
        return string

    def visitVar(self, ctx):
        l = list(ctx.getChildren())
        string = l[0].getText()
        return string

    def visitComent(self, ctx):
        l = list(ctx.getChildren())
        string = colored(l[0].getText(), 'green')
        return string

    # Visitador del condicional
    def visitCondicional(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        self.level += 1
        string += colored('if', 'cyan') + ' ('
        string += self.visit(l[2]) + ') {'
        print(string)
        self.visit(l[5])
        if len(l) > 7:
            string = '} ' + colored('else', 'cyan') + ' {'
            print(string)
            self.visit(l[9])
        self.level -= 1
        print(self.espais(self.level) + "}")

    # Visitador per l'iteració amb while
    def visitIteracioWhile(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        self.level += 1
        string += colored('while', 'cyan') + ' ('
        string += self.visit(l[2]) + ') {'
        print(string)
        self.visit(l[5])
        self.level -= 1
        print(self.espais(self.level) + "}")

    # Visitador per l'iteració amb for
    def visitIteracioFor(self, ctx):
        l = list(ctx.getChildren())
        self.forActive = 1
        string = self.espais(self.level)
        self.level += 1
        string += colored('for', 'cyan') + " ("
        string += self.visit(l[2]) + '; '
        string += self.visit(l[4]) + '; '
        string += self.visit(l[6]) + ') {'
        self.forActive = 0
        print(string)
        self.visit(l[9])
        self.level -= 1
        print(self.espais(self.level) + "}")


#     ==================== COMPARADORS ===========================

    # Visitador del comparador d'igualtat (==)
    def visitEq(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' == ', 'magenta')
        string += self.visit(l[2])
        return string

    # Visitador del comparador de desigualtat (<>)
    def visitNeq(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' <> ', 'magenta')
        string += self.visit(l[2])
        return string

    # Visitador del comparador de major que (>)
    def visitGt(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' > ', 'magenta')
        string += self.visit(l[2])
        return string

    # Visitador del comparador de menor que (<)
    def visitLt(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' < ', 'magenta')
        string += self.visit(l[2])
        return string

    # Visitador del comparador de major o igual que (>=)
    def visitGeq(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' >= ', 'magenta')
        string += self.visit(l[2])
        return string

    # Visitador del comparador de menor o igual que (<=)
    def visitLeq(self, ctx):
        l = list(ctx.getChildren())
        string = self.visit(l[0])
        string += colored(' <= ', 'magenta')
        string += self.visit(l[2])
        return string

#   ============================================================

    def visitAssignacio(self, ctx):
        l = list(ctx.getChildren())
        string = l[0].getText() + ' = '
        string += self.visit(l[2])
        if self.forActive == 1:
            return string
        else:
            string = self.espais(self.level) + string
            print(string)

    # Visitador que permet llegir del canal d'entrada estàndard
    def visitRead(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        string += colored(l[0].getText(), 'red') + '('
        string += l[2].getText()
        string += ')'
        print(string)

    def visitWrite(self, ctx):
        l = list(ctx.getChildren())
        string = self.espais(self.level)
        string += colored(l[0].getText(), 'red') + '('
        for i in range(2, len(l)-1):
            if l[i].getText() == ',':
                string += ', '
            else:
                string += self.visit(l[i])
        string += ')'
        print(string)
