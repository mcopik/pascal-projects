unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation


begin
write('ASEMBLER!sa');
{$ASMMODE intel}
asm
mov ax,21
mov bx,38
add ax,bx
end;
read();


end.

