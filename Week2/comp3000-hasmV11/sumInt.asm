main:   r1 = 1;
        r2 = 0;
loop:   out r2
        r2 = r2 + r1
        r1 = r1 + 1
        if(r1 <= 100) goto loop
        out r2 
        return 