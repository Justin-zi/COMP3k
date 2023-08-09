main:   r1 = 1
        r2 = 1
loop:   if(r1 == 10) goto skip
        if(r1 == 14) goto skip
        r2 = r2 * r1
skip:   r1 = r1+ 1
        if(r1 <= 6) goto loop
        out r2
        return 

// Week 1 Quizz factorial 


