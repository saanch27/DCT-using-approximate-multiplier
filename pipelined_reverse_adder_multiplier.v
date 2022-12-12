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






module controller (clk,rst,enable,in1,in2,prod);
    parameter N = 8;
    parameter integer h = 0;
    input clk;
    input rst;
    input enable;
    input  [N:1] in1;
    input  [N:1] in2;
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
 
    if (rst) begin
      state <= IDLE;
      counter <= 1;
    end else begin
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


module tb();

reg clk;
reg rst;
reg enable;
reg [8:1] in1;
reg [8:1] in2;
wire [16:1] prod;


controller 
    uut(
     clk,
    rst,
    enable,
    in1,
    in2,
    prod
);

initial begin
clk=1'b0;
rst=1'b1;
enable=1'b0;
in1=8'd16;
in2=8'd17;
end


always #5 clk=~clk;


initial begin
#10 rst=1'b0; enable=1'b1;

#100 in1=8'd16; in2=8'd17;

  
#100 in1=8'd15; in2=8'd22;
#100 in1=8'd212; in2=8'd156;
#100 in1=8'd255; in2=8'd255;

#2000 $finish;

end

initial begin
$dumpfile("test.vcd");
$dumpvars(0,tb);
end

endmodule