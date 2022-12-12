//`timescale 1ns/1ps
module controller(in1,in2,prod);
	parameter N = 8;
    parameter integer h = 0;
    /*parameter ST = $clog2(N);
    parameter WIDTH = $clog2(ST);
    parameter integer pair_h1[7]={8,6,4,7,2,3,5};*/


    input [N:1]in1,in2;
	wire [N+2:1]o1;
	wire [N+3:1]o2;
	wire [N+4:1]o3;
	wire [N+5:1]o4;
	wire [N+6:1]o5;
	wire [N+7:1]o6;
	wire [N+8:1]o7;
	output [N+N:1]prod;
	
   //reg en[1:ST];
   logic [N:1] p[1:N];
   integer i;
	
	
	assign o1[1]=p[1][1];
	assign o2[2:1]=o1[2:1];
	assign o3[3:1]=o2[3:1];
	assign o4[4:1]=o3[4:1];
	assign o5[5:1]=o4[5:1];
	assign o6[6:1]=o5[6:1];
	assign o7[7:1]=o6[7:1];
    assign prod=o7;
	always @(*)begin
    for (i = 1; i <= N; i = i + 1) p[i] <= in2[i] ? in1 : 'd0;	  
    end
	
	genericAdder  #(.N(N),.h(8)) x1(1'b1,p[1]>>1,p[2],o1[N+2:2]);
	genericAdder  #(.N(N),.h(6))x2(1'b1,o1[N+2:3],p[3],o2[N+3:3]);
	genericAdder  #(.N(N),.h(4))x3(1'b1,o2[N+3:4],p[4],o3[N+4:4]);
	genericAdder  #(.N(N),.h(7))x4(1'b1,o3[N+4:5],p[5],o4[N+5:5]);
	genericAdder  #(.N(N),.h(2))x5(1'b1,o4[N+5:6],p[6],o5[N+6:6]);
	genericAdder  #(.N(N),.h(3))x6(1'b1,o5[N+6:7],p[7],o6[N+7:7]);
	genericAdder  #(.N(N),.h(5))x7(1'b1,o6[N+7:8],p[8],o7[N+8:8]);
	


endmodule
module genericAdder (en,in1,in2,s);
parameter N = 8;
parameter h=0;
input en;
//input reg clk;
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


module tb_nonpipelined();
reg [8:1] in1;
reg [8:1] in2;
wire [16:1] prod;
integer i,j;
integer fd;

controller uut(
    in1,
    in2,
    prod
);


initial begin
for(i=1;i<256;i=i+1)begin
	for(j=1;j<256;j=j+1)begin
		in1=i; in2=j; 
		#19 $display("Product=%0d In1=%0d In2=%0d ",prod,in1,in2);
		$fwrite(fd,prod,"\t",in1,"\t",in2,"\n"); 
		//#70 $display("Product=%0d",prod);
	end
end
#2 $finish;
end

initial begin
$dumpfile("test.vcd");
$dumpvars(0,tb_nonpipelined);
fd = $fopen("error_detect_nonpipelined.txt","w");  
end

endmodule