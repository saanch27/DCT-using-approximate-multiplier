/*
 ****************************************************************************
 ***********************4 bit mutliplier*************************************
 ****************************************************************************
 *
 *                         in13 in12 in11 in10
 *                         in23 in22 in21 in20
 *  -------------------------------------------
 *                         p03  p02  p01  p00  --------------|
 *                    p13  p12  p11  p10       --------------|stage-1-pair-1 => h= 4-1
 *               p23  p22  p21  p20            --------------|
 *          p33  p32  p31  p30                 --------------|stage-1-pair-2 => h= 4-3
 *  -------------------------------------------
 *              c1   x13   x12  x11  x10  p00  --------------|
 *      c2 x23  x22  x21   x20  p20            --------------|stage-2-pair-1 => h= 4-(1+3)/2
 *  -------------------------------------------
 *     s7   s6   s5   s4   s3   s2   s1   s0
 *  -------------------------------------------
 *     <------(N..-H)----> <--------H------>
 *  -------------------------------------------
 *
 */



/* h is approximation=9
 ****************************************************************************
 ***********************8 bit mutliplier*************************************
 ****************************************************************************
 *                    
 *                                             in17 in16 in15 in14 in13 in12 in11 in10
 *                                             in27 in26 in25 in24 in23 in22 in21 in20
 *-------------------------------------------------------------------------------------
 *                                             p07  p06  p05  p04  p03  p02  p01  p00  --------------| n bit adder, n+1bit output
 *                                        p17  p16  p15  p14  p13  p12  p11  p10       --------------|stage-1-pair-1 => h= 8
 *                                   p27  p26  p25  p24  p23  p22  p21  p20            --------------|
 *                              p37  p36  p35  p34  p33  p32  p31  p30                 --------------|stage-1-pair-2 => h= 6
 *                         p47  p46  p45  p44  p43  p42  p41  p40                      --------------|
 *                    p57  p56  p55  p54  p53  p52  p51  p50                           --------------|stage-1-pair-3 => h= 4
 *               p67  p66  p65  p64  p63  p62  p61  p60                                --------------|
 *          p77  p76  p75  p74  p73  p72  p71  p70                                     --------------|stage-1-pair-4 => h= 2
 *-------------------------------------------------------------------------------------
 *                                    c01  x17  x16  x15  x14  x13  x12  x11  x10  p00 --------------| n+2 bit output,
 *                          c02  x27  x26  x25  x24  x23  x22  x21  x20  p20           --------------|stage-2-pair-1 => h= 7
 *                c03  x37  x36  x35  x34  x33  x32  x31  x30  p40                     --------------|
 *      c04  x47  x46  x45  x44  x43  x42  x41  x40  p60                               --------------|stage-2-pair-2 => h= 3
 *-------------------------------------------------------------------------------------
 *                     c11  y20  y19  y18  y17  y16  y15  y14  y13  y12  y11  x10  p00 --------------|  
 *  c12 y30  y29  y28  y27  y26  y25  y24  y23  y22  y21  x30  p40                     --------------|stage-3-pair-1 => h= 5
 *-------------------------------------------------------------------------------------
 *      s15  s14  s13  s12  s11  s10  s9   s8   s7   s6   s5   s4   s3   s2   s1   s0
 *---------------------------------------------------------------------------------
 *     <---------(N-H=6)---------->   <----------------(H=9)-------------------->
 *---------------------------------------------------------------------------------
 *
 */

/*
 *                    
 *                                             in17 in16 in15 in14 in13 in12 in11 in10
 *                                             in27 in26 in25 in24 in23 in22 in21 in20
 *-------------------------------------------------------------------------------------
 *                                             p07  p06  p05  p04  p03  p02  p01  p00  --------------|
 *                                        p17  p16  p15  p14  p13  p12  p11  p10       --------------|stage-1 => h= 8
 *-------------------------------------------------------------------------------------
 *                                   c18  x17  x16  x15  x14  x13  x12  x11  x10  p00
 *                                   p27  p26  p25  p24  p23  p22  p21  p20            --------------|stage-2 => h=7
 *-------------------------------------------------------------------------------------
 *                              c29  x28  x27  x26  x25  x24  x23  x22  x21  x10  p00
 *                              p37  p36  p35  p34  p33  p32  p31  p30                 --------------|stage-3 => h= 6
 *-------------------------------------------------------------------------------------
 * ...
 * ...                                                                                 --------------|stage-i => h= (H-i) if +ve else 0
 * ...
 *-------------------------------------------------------------------------------------
 *  s15  s14  s13  s12  s11  s10  s9   s8  s7   s6   s5   s4   s3   s2   s1   s0
 *---------------------------------------------------------------------------------
 *     <---------(N-H=6)---------->   <----------------(H=9)-------------------->
 *---------------------------------------------------------------------------------
 *
 */






module controller (clk,rst,enable,cntrl,inp1,inp2,prod);
    parameter N = 8;
    parameter integer h = 0;
    input clk;
    input rst;
    input enable;
    input cntrl;
    input [N:1] inp1;
    input [N:1] inp2;
    reg [N:1] in1;
    reg [N:1]in2;
    reg [8:1]temp;
    output reg [N+N:1] prod;

  reg [1:0]state;
  parameter IDLE=0;
  parameter GENERATE_PARTIAL_PRODUCTS=1;
  parameter ACCUMULATION=2;
  parameter RESULT=3;
  integer counter;

  reg [1:3]en;
  reg [N:1] p[1:N];
  reg done;
  reg [7:0]r=0;

  reg [N:1] p1, p2;
  wire [N+2:2] o1;
  wire [(N)  +1+1:1] stage2in;
  
  reg [N+2:1] st2_1, st2_2;
  wire [N+5:3] o2;
  wire [(N+2)+1+2:1] stage3in;
  reg [N+5:1] st3_1, st3_2;
  wire [N+10:5] o3;
  wire [(N+5)+1+4:1] stage4in;
  
 reg  bit0;
 reg [10:1]stored;
 reg [13:1]stored2;


  reg bufTracker1 = 1, bufTracker2 = 1, bufTracker3 = 1, bufTracker4 = 1, bufTracker5 = 1;
  integer s, i, loop=1;



  always @(posedge clk) begin
  in1=inp1;
  in2=inp2;
  if(cntrl==1)
  in1=~in1+1;
  if(in1>85 || in2>85 )begin
	  if(in1 < in2 )begin
		  temp=in1;
		  in1=in2;
		  in2=temp;
              end
  end
  else if(in1<=85 && in2<=85)begin
	  if(in1>in2)begin
                  temp=in1;
		  in1=in2;
		  in2=temp;
          end
  end
  if (rst) begin
      state <= IDLE;
      counter <= 1;
  end 
  else begin
     
      case (state)
        IDLE: begin
          if (enable) state <= GENERATE_PARTIAL_PRODUCTS;
          else state <= IDLE;
          prod<=32'd0;
          en[1:3]<={1'b0,1'b0,1'b0};
		  loop<=1;
        end
        GENERATE_PARTIAL_PRODUCTS: begin
        
          for (i = 1; i <= N; i = i + 1) p[i] <= in2[i] ? in1 : 'd0;
          counter <= 1;
          state <= ACCUMULATION;
          prod<=32'd0;
        end
        
      
        ACCUMULATION: begin
        
        case(loop)
        
        1:begin 
        
          p1<=p[1];
          p2<=p[2];
         
         
         st2_1<={1'b0,p[3],1'b0};
        
         st2_2<={2'b00,p[4]};
         
         st3_1<={2'b00,p[5],3'b000};
        
         st3_2<={5'b00000,p[6]};
         
         en[1]<=1;
         en[2]<=1;
         en[3]<=1;
          
          end
        
        2: begin
        
        stored<=stage4in[13:4]; //o3 10:1
        en[1]<=0;
        en[2]<=1;
        st2_1 <=stage2in;
   
        st2_2<=(stage3in[11:2]);//10:1
      
        en[3]<=1;
        st3_1<={2'b00,p[7],3'b000};
      
        st3_2<={5'b00000,p[8]};
     
        end
        
        
        3: begin
         stored2<=stage3in[13:1]; //o2 ///////////
        en[1]<=0;
        en[2]<=1;
        en[3]<=0;
         
    
        st2_1<=stored[10:1];
        st2_2<=stage4in[13:4];
        
        end
        
        
        4:begin
        en[1]<=0;
        en[2]<=0;
        en[3]<=1;
        st3_1<=stored2;
        st3_2<=stage3in;
        end
        default:;
      endcase  
       if(loop==4)
       state<=RESULT;
       else begin
       state<=ACCUMULATION; 
       loop<=loop+1;
       end 
end
        RESULT: begin
          begin
           if(cntrl==1'b1)
              prod <= ~stage4in[(N+10):1]+1;
           else
               prod <= stage4in[(N+10):1];
            
           done<=1;
           state<=IDLE;          
           loop<=0;
          end
        end
      endcase
    end
  end
  

    

  assign stage2in[1]              = en[1]  ? p1[1] : stage2in[1];
  assign stage2in[N+2:2]          = en[1]  ? o1 : stage2in[N+2:2];


  assign stage3in[2:1]            = en[2]  ? st2_1[2:1] : stage3in[2:1];
  assign stage3in[(N+2)+1+2:3]    = en[2]  ? o2 : stage3in[(N+2)+1+2:3];


  assign stage4in[4:1]            = en[3] ? st3_1[4:1] : stage4in[4:1];
  assign stage4in[(N+5)+1+4:5]    = en[3] ? o3 : stage4in[(N+5)+1+4:5];


 genericAdder #(
      .N(N),.h(7)) uut (en[1],clk,p1 >> 1,p2,o1[N+2:2]);
  genericAdder #(
      .N(N + 2),
      .h(5)
  ) dut (
      en[2],
      clk,
      st2_1 >> 2,
      st2_2,
      o2[(N+2)+1+2:3]
  );
  genericAdder #(
      .N(N + 5),
      .h(3)
  ) put (
      en[3],
      clk,
      st3_1 >> 4,
      st3_2,
      o3[(N+5)+1+4:5]
  );





endmodule

module genericAdder (en,clk,in1,in2,s);
parameter N = 8;
parameter h=2;
//initial
//begin
//reg f[];
//f=new[pair_h1];
//end

input en;
input clk;
input [N-1:0]in1,in2;
reg cin=1'b0;
output [N:0] s;

reg [N-1:0] a,b;
reg c;
wire [N:0] cy;
wire [h:0] f;
always @ (*)

begin
    a = in1;
    b = in2;
    c = cin;
end

genvar aux;
generate
    for(aux=0;aux<=h;aux=aux+1)
    begin
        if(aux==0)
         assign f[aux]=en?c:'bz;
        else
         auxRA1 a1 (en,a[aux-1],b[aux-1],f[aux]);
    end
endgenerate


assign cy[h] = en? h>0 ? f[h] : c : 'bz;

genvar i;
generate
    for(i=h-1;i>=0;i=i-1)
    begin
        RA1 u1 (en,a[i],b[i],cy[i+1],f[i],s[i],cy[i]);//negated carry is sent, to clear doubt see circuit of rcpfa1
    end
endgenerate
genvar j;
generate
    for(j=h;j<N;j=j+1)
    begin
        FA u2 (en,a[j],b[j],cy[j],s[j],cy[j+1]);
    end
endgenerate

assign s[N]=cy[N];
endmodule

module FA(en,a,b,cin,sum,carry);
    input en,a,b,cin;
    output sum,carry;
    wire sum,carry;

    assign sum=en?a^b^cin:'bz; // sum bit
    assign carry=en?((a&b) | (b&cin) | (a&cin)):'bz; //carry bit

endmodule
  module RA1(en,ai,bi,ci1n,fi,si,cin);
    input en,ai,bi,ci1n,fi;
    output si,cin;
    wire xi,yi;
    wire ci1n,fin;
    wire sint,cint;
    wire g1,g2,g3,g4;
    and n1(g1,ai,bi);
    nor n2(xi,g1,ci1n);
    or n6(g2,ai,bi);
    nand n7(yi,g2,ci1n);
    not n3(fin,fi);
    or n4(g3,fin,xi);
    and n8(g4,fi,yi);
    nand n5(sint,yi,g3);
    nor n9(cint,g4,xi);
    assign si=en?sint:'bz;
    assign cin=en?cint:'bz;
endmodule

module auxRA1(input en, ai, bi, output fi1);
    assign fi1=en?ai:'bz;
endmodule
module precalculate(
    input [8:0] y0,y1,y2,y3,y4,y5,y6,y7,
    output [8:0]s0_7,s1_6,s2_5,s3_4
    );
    //ADDITION s07,s16,s25,s34
  //  Adder_Block S07(.A(y0[8:0]),.B(y7[8:0]),.operation(1'b0),.R(s07[8:0]));
  //  Adder_Block S16(.A(y1[8:0]),.B(y6[8:0]),.operation(1'b0),.R(s16[8:0]));
  //  Adder_Block S25(.A(y2[8:0]),.B(y5[8:0]),.operation(1'b0),.R(s25[8:0]));
  //  Adder_Block S34(.A(y3[8:0]),.B(y4[8:0]),.operation(1'b0),.R(s34[8:0]));
    //
    //SUBTRACTION s0_7,s1_6,s2_5,s3_4
    Adder_Block S0_7(.A(y0[8:0]),.B(y7[8:0]),.operation(1'b1),.R(s0_7[8:0]));
    Adder_Block S1_6(.A(y1[8:0]),.B(y6[8:0]),.operation(1'b1),.R(s1_6[8:0]));
    Adder_Block S2_5(.A(y2[8:0]),.B(y5[8:0]),.operation(1'b1),.R(s2_5[8:0]));
    Adder_Block S3_4(.A(y3[8:0]),.B(y4[8:0]),.operation(1'b1),.R(s3_4[8:0]));
    //
    //ADDITION s0734,s1625
  //  Adder_Block S0734(.A(s07[8:0]),.B(s34[8:0]),.operation(1'b0),.R(s0734[8:0]));
  //  Adder_Block S1625(.A(s16[8:0]),.B(s25[8:0]),.operation(1'b0),.R(s1625[8:0]));
    //
    //SUBTRACTION s07_34,s16_25
  //  Adder_Block S07_34(.A(s07[8:0]),.B(s34[8:0]),.operation(1'b1),.R(s07_34[8:0]));
   // Adder_Block S16_25(.A(s16[8:0]),.B(s25[8:0]),.operation(1'b1),.R(s16_25[8:0]));
    //
    //ADDITION s07341625
   // Adder_Block S07341625(.A(s0734[8:0]),.B(s1625[8:0]),.operation(1'b0),.R(s07341625[8:0]));
    //
    //SUBTRACTION s0734_1625
  //  Adder_Block S0734_1625(.A(s0734[8:0]),.B(s1625[8:0]),.operation(1'b1),.R(s0734_1625[8:0]));
endmodule

module Adder_Block ( //16-Bit Adder Block
    input [8:0]A, // MSB is sign bit
    input [8:0]B, // MSB is sign bit
    input operation, // Addition or Subtraction
    output reg [8:0]R // MSB is sign bit
    );
always@(*)
    if(operation)
        R = A - B;
    else
        R = A + B;
endmodule

module Adder_Block2 ( //16-Bit Adder Block
    input [17:0]A, // MSB is sign bit
    input [17:0]B, // MSB is sign bit
    input operation, // Addition or Subtraction
    output reg [17:0]R // MSB is sign bit
    );
always@(*)
    if(operation)
        R = A - B;
    else
        R = A + B;
endmodule

module DCT_1D(
    input [8:0] y0,y1,y2,y3,y4,y5,y6,y7,
    input clk,rst,enable,
    //input [15:0]Data_in, // Coefficient
    //input [7:0]mem_select, // decoder_out
   // output reg[8:0] x07,x16,x25,x34,x0_7,x1_6,x2_5,x3_4,x0734,x1625,x07_34,x16_25,x07341625,x0734_1625,x0,x1,x2,x3,x4,x5,x6,x7,
	//output [8:0]s07,s16,s25,s34,s0_7,s1_6,s2_5,s3_4,s0734,s1625,s07_34,s16_25,s07341625,s0734_1625;
	output [8:0]s0_7,s1_6,s2_5,s3_4,
	output [17:0]Y0,Y1,Y1_0,Y1_1,Y1_3,Y2,Y3,Y4,Y5,Y6,Y7
	//output [15:0]t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12
    );
    //wire [8:0]s0_7,s1_6,s2_5,s3_4;//final values
  //  reg [15:0]temp1,temp2,temp3,temp4;
 //   reg [15:0]t5,t6,t7,t8,t9,t10,t11,t12;
    wire [7:0]C0,C1,C2,C3,C4,C5,C6,C7;// coefficient matrix values
    //wire [21:0]done_mul;
    // Taking only fractional part with positive sign bit
    //     Sign Bit      Fractional Part
    //      1-Bit     .   15-Bits
   //reg [15:0]sign_array;
 wire[15:0] t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12; 

   
   assign C0 = 8'b01011010; //  cos(4pi/16) => 0.70710678118
   assign C1 = 8'b01111100; //  cos(1pi/16) => 0.9807852804
   assign C2 = 8'b01110110; //  cos(2pi/16) => 0.92387953251  
   assign C3 = 8'b01101010; //  cos(3pi/16) => 0.8314696123
   assign C4 = 8'b01011010; //  cos(4pi/16) => 0.70710678118
   assign C5 = 8'b01000110; //  cos(5pi/16) => 0.55557023302
   assign C6 = 8'b00110000; //  cos(6pi/16) => 0.38268343236
   assign C7 = 8'b00011000; //  cos(7pi/16) => 0.19509032201
    //Precalculations Started
    precalculate Step1(.y0(y0),.y1(y1),.y2(y2),.y3(y3),.y4(y4),.y5(y5),.y6(y6),.y7(y7),
                       .s0_7(s0_7),.s1_6(s1_6),
                       .s2_5(s2_5),.s3_4(s3_4));
					   
  
	
    wire [17:0]r1,r2,r3,r4,r5,r6,r0_7,r2_7;
     reg [17:0]rY0,rY1;
     reg flag=0;
 
	controller c1_1(clk,rst,enable,y0[8],y0[7:0],C0,t5);
	controller c1_2(clk,rst,enable,y7[8],y7[7:0],C0,t6);
	controller c1_3(clk,rst,enable,y3[8],y3[7:0],C0,t7);
	controller c1_4(clk,rst,enable,y4[8],y4[7:0],C0,t8);
	controller c1_5(clk,rst,enable,y1[8],y1[7:0],C0,t9);
	controller c1_6(clk,rst,enable,y6[8],y6[7:0],C0,t10);
	controller c1_7(clk,rst,enable,y2[8],y2[7:0],C0,t11);
	controller c1_8(clk,rst,enable,y5[8],y5[7:0],C0,t12);
	
	//always @(t5,t6,t7,t8,t9,t10,t11,t12)begin
	// Adder_Block2 a0(.A(19712),.B(20096),.operation(1'b0),.R(r1[17:0])); 
	 Adder_Block2 a0(.A({t5[15],t5[15],t5[15:0]}),.B({t6[15],t6[15],t6[15:0]}),.operation(1'b0),.R(r1[17:0])); 
	 Adder_Block2 a1(.A({t7[15],t7[15],t7[15:0]}),.B({t8[15],t8[15],t8[15:0]}),.operation(1'b0),.R(r2[17:0])); 
	 Adder_Block2 a2(.A({t9[15],t9[15],t9[15:0]}),.B({t10[15],t10[15],t10[15:0]}),.operation(1'b0),.R(r3[17:0])); 
	 Adder_Block2 a3(.A({t11[15],t11[15],t11[15:0]}),.B({t12[15],t12[15],t12[15:0]}),.operation(1'b0),.R(r4[17:0])); 
	 Adder_Block2 a4(.A(r1[17:0]),.B(r2[17:0]),.operation(1'b0),.R(r5[17:0])); 
	 Adder_Block2 a5(.A(r3[17:0]),.B(r4[17:0]),.operation(1'b0),.R(r6[17:0])); 
	 Adder_Block2 a6(.A(r5[17:0]),.B(r6[17:0]),.operation(1'b0),.R(r0_7[17:0])); 


	//reg flag=0;
	always @(r0_7)begin
	rY0=r0_7;
	if(rY0[17]==1'b1)
	begin
	//flag=1;
	rY0=~rY0+1;
	rY0=rY0/2;
	rY0=~rY0+1;
	end
	else
	rY0=rY0/2;
	end
	assign Y0=rY0;
	
	//p5<=t5;
	
	// Y0=(t5+t6+t7+t8+t9+t10+t11+t12)/2;
	
	//Y1 CALCULATION
	controller c2_1(clk,rst,enable,s0_7[8],s0_7[7:0],C1,t1);
	controller c2_2(clk,rst,enable,s1_6[8],s1_6[7:0],C3,t2);
	controller c2_3(clk,rst,enable,s2_5[8],s2_5[7:0],C5,t3);
	controller c2_4(clk,rst,enable,s3_4[8],s3_4[7:0],C7,t4); 
	//assign Y1=(t1+t2+t3+t4)/2; //giving wromg result 2's complement
	
	 Adder_Block2 y1_0(.A({t1[15],t1[15],t1[15:0]}),.B({t2[15],t2[15],t2[15:0]}),.operation(1'b0),.R(Y1_0[17:0])); 
	 Adder_Block2 y1_1(.A({t3[15],t3[15],t3[15:0]}),.B({t4[15],t4[15],t4[15:0]}),.operation(1'b0),.R(Y1_1[17:0])); 
	 Adder_Block2 y1_3(.A(Y1_0[17:0]),.B(Y1_1[17:0]),.operation(1'b0),.R(Y1_3[17:0])); 
	//assign Y1=Y1_3;
	
	
	always @(Y1_3)begin
	rY1=Y1_3;
	if(rY1[17]==1'b1)
	begin
	flag=1;
	rY1=~rY1+1;
	rY1=rY1/2;
	rY1=~rY1+1;
	end
	else
	rY1=rY1/2;
	end
	assign Y1=rY1;
	
	wire[15:0]b1,b2,b3,b4,b5,b6,b7,b8;
	wire[17:0]c1,c2,c3,c4,c5,c6,c7;	
	controller c3_1(clk,rst,enable,y0[8],y0[7:0],C2,b1);
	controller c3_2(clk,rst,enable,y7[8],y7[7:0],C2,b2);
	controller c3_3(clk,rst,enable,y3[8],y3[7:0],C2,b3);
	controller c3_4(clk,rst,enable,y4[8],y4[7:0],C2,b4); 
	//assign Y1=(t1+t2+t3+t4)/2; //giving wromg result 2's complement
	controller c3_5(clk,rst,enable,y1[8],y1[7:0],C6,b5);
	controller c3_6(clk,rst,enable,y6[8],y6[7:0],C6,b6);
	controller c3_7(clk,rst,enable,y2[8],y2[7:0],C6,b7);
	controller c3_8(clk,rst,enable,y5[8],y5[7:0],C6,b8);

	Adder_Block2 y2_0(.A({b1[15],b1[15],b1[15:0]}),.B({b2[15],b2[15],b2[15:0]}),.operation(1'b0),.R(c1[17:0])); 
	Adder_Block2 y2_1(.A({b3[15],b3[15],b3[15:0]}),.B({b4[15],b4[15],b4[15:0]}),.operation(1'b0),.R(c2[17:0])); 
	Adder_Block2 y2_2(.A({b5[15],b5[15],b5[15:0]}),.B({b6[15],b6[15],b6[15:0]}),.operation(1'b0),.R(c3[17:0])); 
	Adder_Block2 y2_3(.A({b7[15],b7[15],b7[15:0]}),.B({b8[15],b8[15],b8[15:0]}),.operation(1'b0),.R(c4[17:0])); 
	Adder_Block2 y2_4(.A(c1[17:0]),.B(c3[17:0]),.operation(1'b0),.R(c5[17:0])); 
	Adder_Block2 y2_5(.A(c2[17:0]),.B(c4[17:0]),.operation(1'b0),.R(c6[17:0])); 
	Adder_Block2 y2_6(.A(c5[17:0]),.B(c6[17:0]),.operation(1'b1),.R(r2_7[17:0])); 
	//assign Y1=Y1_3;
	reg [17:0]rY2;	
	
	always @(r2_7)begin
	rY2=r2_7;
	if(rY2[17]==1'b1)
	begin
	flag=1;
	rY2=~rY2+1;
	rY2=rY2/2;
	rY2=~rY2+1;
	end
	else
	rY2=rY2/2;
	end
	assign Y2=rY2;

	wire [15:0]d1,d2,d3,d4;
	wire [17:0]e1,e2;
	wire [17:0]r3_7;	
        controller c4_1(clk,rst,enable,s0_7[8],s0_7[7:0],C3,d1);
	controller c4_2(clk,rst,enable,s1_6[8],s1_6[7:0],C7,d2);
	controller c4_3(clk,rst,enable,s2_5[8],s2_5[7:0],C1,d3);
	controller c4_4(clk,rst,enable,s3_4[8],s3_4[7:0],C5,d4); 

	Adder_Block2 y3_0(.A({d1[15],d1[15],d1[15:0]}),.B({d2[15],d2[15],d2[15:0]}),.operation(1'd1),.R(e1[17:0])); 
	Adder_Block2 y3_1(.A({d3[15],d3[15],d3[15:0]}),.B({d4[15],d4[15],d4[15:0]}),.operation(1'd0),.R(e2[17:0])); 
	Adder_Block2 y3_2(.A(e1[17:0]),.B(e2[17:0]),.operation(1'd0),.R(r3_7[17:0])); 

        reg [17:0]rY3;	
	
	always @(r3_7)begin
	rY3=r3_7;
	if(rY3[17]==1'b1)
	begin
	flag=1;
	rY3=~rY3+1;
	rY3=rY3/2;
	rY3=~rY3+1;
	end
	else
	rY3=rY3/2;
	end
	assign Y3=rY3;

        wire [15:0]f1,f2,f3,f4;
	wire [17:0]g1,g2;
	wire [17:0]r5_7;	
        controller c5_1(clk,rst,enable,s0_7[8],s0_7[7:0],C5,f1);
	controller c5_2(clk,rst,enable,s1_6[8],s1_6[7:0],C1,f2);
	controller c5_3(clk,rst,enable,s2_5[8],s2_5[7:0],C7,f3);
	controller c5_4(clk,rst,enable,s3_4[8],s3_4[7:0],C3,f4); 

	Adder_Block2 y4_0(.A({f1[15],f1[15],f1[15:0]}),.B({f2[15],f2[15],f2[15:0]}),.operation(1'd1),.R(g1[17:0])); 
	Adder_Block2 y4_1(.A({f3[15],f3[15],f3[15:0]}),.B({f4[15],f4[15],f4[15:0]}),.operation(1'd0),.R(g2[17:0])); 
	Adder_Block2 y4_2(.A(g1[17:0]),.B(g2[17:0]),.operation(1'd0),.R(r5_7[17:0])); 

        reg [17:0]rY5;	
	
	always @(r5_7)begin
	rY5=r5_7;
	if(rY5[17]==1'b1)
	begin
	flag=1;
	rY5=~rY5+1;
	rY5=rY5/2;
	rY5=~rY5+1;
	end
	else
	rY5=rY5/2;
	end
	assign Y5=rY5;

	wire [15:0]h1,h2,h3,h4;
	wire [17:0]i1,i2;
	wire [17:0]r7_7;	
        controller c7_1(clk,rst,enable,s0_7[8],s0_7[7:0],C7,h1);
	controller c7_2(clk,rst,enable,s1_6[8],s1_6[7:0],C5,h2);
	controller c7_3(clk,rst,enable,s2_5[8],s2_5[7:0],C3,h3);
	controller c7_4(clk,rst,enable,s3_4[8],s3_4[7:0],C1,h4); 

	Adder_Block2 y5_0(.A({h1[15],h1[15],h1[15:0]}),.B({h2[15],h2[15],h2[15:0]}),.operation(1'd1),.R(i1[17:0])); 
	Adder_Block2 y5_1(.A({h3[15],h3[15],h3[15:0]}),.B({h4[15],h4[15],h4[15:0]}),.operation(1'd1),.R(i2[17:0])); 
	Adder_Block2 y5_2(.A(i1[17:0]),.B(i2[17:0]),.operation(1'd0),.R(r7_7[17:0])); 

        reg [17:0]rY7;	
	
	always @(r7_7)begin
	rY7=r7_7;
	if(rY7[17]==1'b1)
	begin
	flag=1;
	rY7=~rY7+1;
	rY7=rY7/2;
	rY7=~rY7+1;
	end
	else
	rY7=rY7/2;
	end
	assign Y7=rY7;

	wire [15:0]j1,j2,j3,j4,j5,j6,j7,j8;
	wire [17:0]k1,k2,k3,k4,k5,k6,r4_7;
        controller c8_1(clk,rst,enable,y0[8],y0[7:0],C4,j1);
	controller c8_2(clk,rst,enable,y7[8],y7[7:0],C4,j2);
	controller c8_3(clk,rst,enable,y3[8],y3[7:0],C4,j3);
	controller c8_4(clk,rst,enable,y4[8],y4[7:0],C4,j4);
	controller c8_5(clk,rst,enable,y1[8],y1[7:0],C4,j5);
	controller c8_6(clk,rst,enable,y6[8],y6[7:0],C4,j6);
	controller c8_7(clk,rst,enable,y2[8],y2[7:0],C4,j7);
	controller c8_8(clk,rst,enable,y5[8],y5[7:0],C4,j8);
	
	//always @(t5,t6,t7,t8,t9,t10,t11,t12)begin
	// Adder_Block2 a0(.A(19712),.B(20096),.operation(1'b0),.R(r1[17:0])); 
	 Adder_Block2 x0(.A({j1[15],j1[15],j1[15:0]}),.B({j2[15],j2[15],j2[15:0]}),.operation(1'b0),.R(k1[17:0])); 
	 Adder_Block2 x1(.A({j3[15],j3[15],j3[15:0]}),.B({j4[15],j4[15],j4[15:0]}),.operation(1'b0),.R(k2[17:0])); 
	 Adder_Block2 x2(.A({j5[15],j5[15],j5[15:0]}),.B({j6[15],j6[15],j6[15:0]}),.operation(1'b0),.R(k3[17:0])); 
	 Adder_Block2 x3(.A({j7[15],j7[15],j7[15:0]}),.B({j8[15],j8[15],j8[15:0]}),.operation(1'b0),.R(k4[17:0])); 
	 Adder_Block2 x4(.A(k1[17:0]),.B(k2[17:0]),.operation(1'b0),.R(k5[17:0])); 
	 Adder_Block2 x5(.A(k3[17:0]),.B(k4[17:0]),.operation(1'b0),.R(k6[17:0])); 
	 Adder_Block2 x6(.A(k5[17:0]),.B(k6[17:0]),.operation(1'b1),.R(r4_7[17:0])); 

	reg [17:0]rY4;
	//reg flag=0;
	always @(r4_7)begin
	rY4=r4_7;
	if(rY4[17]==1'b1)
	begin
	//flag=1;
	rY4=~rY4+1;
	rY4=rY4/2;
	rY4=~rY4+1;
	end
	else
	rY4=rY4/2;
	end
	assign Y4=rY4;
	
        wire[15:0]m1,m2,m3,m4,m5,m6,m7,m8;
	wire[17:0]n1,n2,n3,n4,n5,n6,n7,r6_7;	
	controller c6_1(clk,rst,enable,y0[8],y0[7:0],C6,m1);
	controller c6_2(clk,rst,enable,y7[8],y7[7:0],C6,m2);
	controller c6_3(clk,rst,enable,y3[8],y3[7:0],C6,m3);
	controller c6_4(clk,rst,enable,y4[8],y4[7:0],C6,m4); 
	//assign Y1=6t1+t2+t3+t4)/2; //giving wromg resumt 2's complement
	controller c6_5(clk,rst,enable,y1[8],y1[7:0],C2,m5);
	controller c6_6(clk,rst,enable,y6[8],y6[7:0],C2,m6);
	controller c6_7(clk,rst,enable,y2[8],y2[7:0],C2,m7);
	controller c6_8(clk,rst,enable,y5[8],y5[7:0],C2,m8);

	Adder_Block2 z1(.A({m1[15],m1[15],m1[15:0]}),.B({m2[15],m2[15],m2[15:0]}),.operation(1'b0),.R(n1[17:0])); 
	Adder_Block2 z2(.A({m3[15],m3[15],m3[15:0]}),.B({m4[15],m4[15],m4[15:0]}),.operation(1'b0),.R(n2[17:0])); 
	Adder_Block2 z3(.A({m5[15],m5[15],m5[15:0]}),.B({m6[15],m6[15],m6[15:0]}),.operation(1'b0),.R(n3[17:0])); 
	Adder_Block2 z4(.A({m7[15],m7[15],m7[15:0]}),.B({m8[15],m8[15],m8[15:0]}),.operation(1'b0),.R(n4[17:0])); 
	Adder_Block2 z5(.A(n1[17:0]),.B(n2[17:0]),.operation(1'b1),.R(n5[17:0])); 
	Adder_Block2 z6(.A(n3[17:0]),.B(n4[17:0]),.operation(1'b1),.R(n6[17:0])); 
	Adder_Block2 z7(.A(n5[17:0]),.B(n6[17:0]),.operation(1'b1),.R(r6_7[17:0])); 
	//assign Y1=Y1_3;
	reg [17:0]rY6;	
	
	always @(r6_7)begin
	rY6=r6_7;
	if(rY6[17]==1'b1)
	begin
	flag=1;
	rY6=~rY6+1;
	rY6=rY6/2;
	rY6=~rY6+1;
	end
	else
	rY6=rY6/2;
	end
	assign Y6=rY6;

endmodule

module mul(input[7:0] a,b,output[16:0]c );
 
assign c=a*b;

endmodule

module tb();

   reg [8:0] y0,y1,y2,y3,y4,y5,y6,y7;
   reg clk,rst,enable;
   wire [8:0]s0_7,s1_6,s2_5,s3_4;
   wire [17:0]Y0,Y1,Y1_0,Y1_1,Y1_3,Y2,Y3,Y4,Y5,Y6,Y7;
  // wire [15:0]t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12;
  
DCT_1D dut(y0,y1,y2,y3,y4,y5,y6,y7,clk,rst,enable,s0_7,s1_6,s2_5,s3_4,Y0,Y1,Y1_0,Y1_1,Y1_3,Y2,Y3,Y4,Y5,Y6,Y7);
initial begin
clk=1'b0;
rst=1'b1;
enable=1'b0;
end
always #5 clk=~clk;
initial begin
#10 rst=1'b0; enable=1'b1;
#10  y0=9'd215;y1=9'd210;y2=9'b110100110;y3=9'd145;y4=9'd148;y5=9'd159;y6=9'd142;y7=9'd200;
#2000 $finish;

end  
endmodule
