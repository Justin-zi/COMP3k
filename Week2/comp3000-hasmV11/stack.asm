// Reverse Polish Notation
//goto -> runs all the code
//call -> remembers where the line was called and goes back

main:   r3 = alloc(80)

        call sub
        r1 = pop(r3)
        out r1
        return



sub :   r2 = pop(r3)
        r1 = pop(r3)
        r1 = r1 - r2
        push(r3, r1)
        return 

add:    r2 = pop(r3)
        r1 = pop(r3)
        r1 = r1 + r2
        push(r3, r1)
        return 

multiply:   r2 = pop(r3)
            r1 = pop(r3)
            r1 = r1 * r2
            push(r3, r1)
            return 
            
divide: r2 = pop(r3)
        r1 = pop(r3)
        r2 = r2 / r1
        push(r3, r2)
        return 
