program lpm;

{$mode objfpc}{$H+}
{$ScopedEnums On}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  Terminal,
  AsyncProgress
  {$IfDef WINDOWS}
  , windows, TerminalApplication
  {$EndIf};

type
  { TLPMApplication }

  TLPMApplication = class(TTerminalApplication)
  private

  protected
    function CreateParser: TArgumentParser; override;

    procedure Execute(const Args: TParsedArguments); override;
  public

  end;

{ TMyApplication }

const LPMVersion = '2.0.0';

var
  Application: TLPMApplication;

function TLPMApplication.CreateParser: TArgumentParser;
begin
  Result := TArgumentParser.Create('lpm ' + LPMVersion + ' ('
    + {$include %date%} + '): '#13#10
    + 'Lazarus Package Manager is a command line tool for managing the local lazarus installation. '
    + 'Allowing to install and remove packages both locally and using the OPM.');
  Result.AddValueOption('l', 'lazarus', 'Path to local Lazarus installation. '
    + 'Alternatively just execute from within the Lazarus directory, '
    + 'copy the lpm executable into the lazarus directory or '
    + 'set the LAZDIR environment variable');
end;

procedure TLPMApplication.Execute(const Args: TParsedArguments);
begin
  WriteHelp;
  ReadLn;
end;

begin
  SetConsoleOutputCP(DefaultSystemCodePage);
  SetTextCodePage(Output, DefaultSystemCodePage);
  SetTextCodePage(ErrOutput, DefaultSystemCodePage);
  Application:=TLPMApplication.Create;
  try
    Application.Run;
  finally    
    Application.Free;
  end;
end.

