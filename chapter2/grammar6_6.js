class Parser {
    static parse(input) {
        const p = new Parser(input)
        p.parse()
    }

    constructor(input) {
        this.input = input
        this.tp = 0
        this.plp = 0
        this.rules = []
        this.parsedOrder = 0

        ;['a', 'b', 'c'].forEach((ch) => {
            this[ch] = (tail) => {
                if (this.input[this.tp] === ch) {
                    this.tp += 1
                    tail()
                    this.tp -= 1
                }
            }
        })
    }

    pushRule(rule) {
        this.rules.push(rule)
    }

    popRule() {
        this.rules.pop()
    }

    parse() {
        const tail = () => {
            if (this.input[this.tp] === undefined) {
                console.log(`Found ${this.parsedOrder++}:\n`)
                console.log(this.rules.join('\n'))
            }
        }
        
        this.S(tail)
    }

    A(tail) {
        const _A = () => {
            this.A(tail)
        }
        this.pushRule("A -> a")
        this.a(tail)
        this.popRule()
        this.pushRule("A -> aA")
        this.a(_A)
        this.popRule()
    }

    B(tail) {
        const _c = () => {
            this.c(tail)
        }
        const _Bc = () => {
            this.B(_c)
        }
        this.pushRule("B -> bc")
        this.b(_c)
        this.popRule()
        this.pushRule("B -> bBc")
        this.b(_Bc)
        this.popRule()
    }

    C(tail) {
        const _C = () => {
            this.C(tail)
        }
        this.pushRule("C -> c")
        this.c(tail)
        this.popRule()
        this.pushRule("C -> cC")
        this.c(_C)
        this.popRule()
    }

    D(tail) {
        const _b = () => {
            this.b(tail)
        }
        const _Db = () => {
            this.D(_b)
        }
        this.pushRule("D -> ab")
        this.a(_b)
        this.popRule()
        this.pushRule("D -> aDb")
        this.a(_Db)
        this.popRule()
    }

    S(tail) {
        const _C = () => {
            this.C(tail)
        }
        const _B = () => {
            this.B(tail)
        }
        this.pushRule("S -> DC")
        this.D(_C)
        this.popRule()
        this.pushRule("S -> AB")
        this.A(_B)
        this.popRule()
    }
}
