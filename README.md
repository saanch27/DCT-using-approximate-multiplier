# DCT-using-approximate-multiplier
In this project, we have implemented an 8 point 1-D DCT using approximate carry reverse adder based multiplier.
We have first made 8 bit multiplier using two approaches- pipelined and nonpipelined.
In both the multipliers, approximate reverse adders were used.
Pipelined approach uses 7,5,3 as approximation bits in each stage.
Pipelined approach gave us lesser error than non-pipelined so we implemented DCT through pipelined approach.

Following formulae were used for DCT calculation:

Y0 = [(x0+x7)+(x1+x6)+(x2+x5)+(x3+x4)]*C0 = x0*C0+x1*C0+x2*C0+x3*C0+x4*C0+x5*C0+x6*C0+x7*C0

Y1=[x0-x7]*C1+[x1-x6]*C3+[x2-x5]*C5+[x3-x4]*C7=[s0_7]*C1+[s1_6]*C3+[s2_5]*C5+[s3_4]*C7

Y2=x0*C2+x7*C2-x3*C2-x4*C2+x1*C6+x6*C6-x2*C5-x5*C6

Y3=[x0-x7]*C3-[x1-x6]*C7-[x2-x5]*C1-[x3-x4]*C5=[s0_7]*C3-[s1_6]*C7-[s2_5]*C1-[s3_4]*C5

Y4=x0*C4+x7*C4+x3*C4+x4*C4-x1*C4-x6*C4-x2*C4-x5*C4

Y5 = [x0-x7]*C5+[x1-x6]*(-C1)+[x2-x5]*C7+[x3-x4]*C3 = [s0_7]*C5-[s1_6]*C1+[s2_5]*C7+[s3_4]*C3

Y6 = [(x0+x7)-(x3+x4)]*C6-[(x1+x6)-(x2+x5)]*C2 =x0*C6+x7*C6-x3*C6-x4*C6-x1*C2-x6*C2-x2*C2-x5*C2

Y7 = [x0-x7]*C7+[x1-x6]*(-C5)+[x2-x5]*C3+[x3-x4]*(-C1) = [s0_7]*C7-[s1_6]*C5+[s2_5]*C3-[s3_4]*C1


As seen from Y0,Y2,Y4,Y6,Y7 , we have opened the brackets and multiplied individually, instead of adding all inputs at once and then multiply by coefficient. 
This was done to increase range, as our inputs of our multiplier were of 8 bits. So adding 2 8 bit number together first, might have resulted in a value greater than 8 bits. To resolve this issue, each input was first multiplied by coefficient resulting in 16 bit product and then two 16 bit resultants were added or subtracted according to the equation.

C0 = 1/2 * cos(π/4),      C1 = 1/2 * cos(π/16),      C2 = 1/2 * cos(2π/16),      C3 = 1/2 * cos(3π/16),

C4 = 1/2 * cos(4π/16),    C5 = 1/2 * cos(5π/16),     C6 = 1/2 * cos(6π/16),      C7 = 1/2 * cos(7π/16).

All the above coefficients were represented in 8 bit binary in Q0.7 format i.e. 8th bit for integer and last 7 bits for fractional parts.
Hence:

     C0 = 8'b01011010; //  cos(4pi/16) => 0.70710678118
     
     C1 = 8'b01111100; //  cos(1pi/16) => 0.9807852804
     
     C2 = 8'b01110110; //  cos(2pi/16) => 0.92387953251 
     
     C3 = 8'b01101010; //  cos(3pi/16) => 0.8314696123
     
     C4 = 8'b01011010; //  cos(4pi/16) => 0.70710678118
     
     C5 = 8'b01000110; //  cos(5pi/16) => 0.55557023302
     
     C6 = 8'b00110000; //  cos(6pi/16) => 0.38268343236
     
     C7 = 8'b00011000; //  cos(7pi/16) => 0.19509032201
