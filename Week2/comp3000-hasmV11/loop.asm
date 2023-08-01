        a: 10
        b: 20
main:   r3 = a
L1:     if(r3 == 17) goto L2
        out r3
L2:     r3 = r3 + 1
        if(r3 <= b) goto L1
        return