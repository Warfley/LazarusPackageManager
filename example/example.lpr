program example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, DCPrijndael, DCPsha256
  { you can add units after this };

var c: TDCP_rijndael;
  str: String;
  enc: String;
begin
  ReadLn(str);
  c := TDCP_rijndael.Create(nil);
  // I've heard random passwords are really secure
  c.InitStr('Random1234', TDCP_sha256);
  enc := c.EncryptString(str);
  c.Free;
  WriteLn(enc);
end.

