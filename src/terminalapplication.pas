unit TerminalApplication;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch arrayoperators}
{$ScopedEnums On}
{$TypedAddress On}

interface

uses
  Classes, SysUtils, Terminal, TerminalModifier, Generics.Collections;

type
  EInvalidArgument = class(Exception);
  ENoValuePresent = class(Exception);
  EAlreadyRegistered = class(Exception);
  EInvalidParameter = class(Exception);

  TArgumentParser = class;

  TArgumentType = (Option, Parameter);
  TParsedArgument = record
    Name: String;
    Value: String;
    Parser: TArgumentParser;
    ArgumentType: TArgumentType;
  end;

  TParsedArgumentArray = array of TParsedArgument;

  { TParsedArguments }

  TParsedArguments = record
  private
    FArguments: TParsedArgumentArray;

    function GetListOption(const AName: String): TStringArray;
    function GetListParameter(const AName: String): TStringArray;
    function GetOption(const AName: String): String;
    function GetParameter(const AName: String): String;
  public
    function HasOption(const AName: String): Boolean;
    function HasParameter(const AName: String): Boolean;

    property Options[const AName: String]: String read GetOption;
    property ListOptions[const AName: String]: TStringArray read GetListOption;
    property Parameter[const AName: String]: String read GetParameter;
    property ListParameter[const AName: String]: TStringArray read GetListParameter;
  end;

  { TArgumentParser }

  TArgumentParser = class
  private type
    TOptionType = (Switch, MaybeValue, Value, List);
    TOptionInfo = record
      ShortName: Char;
      LongName: String;
      ArgumentType: TOptionType;
      Required: Boolean;

      Description: String;
    end;

    TOptionArguments = array of TOptionInfo;

    TParameterType = (Simple, List, Command);
    TParameterInfo = record
      Name: String;
      Description: String;
      Required: Boolean;
      ArgumentType: TParameterType;
    end;
    TParameterArguments = array of TParameterInfo;

    TCommandMap = specialize THashMap<String, TArgumentParser>;
  private
    FOptions: TOptionArguments;
    FCommand: String;
    FDescription: String;
    FParameters: TParameterArguments;
    FSubCommands: TCommandMap;

    function DescriptionPadding: Integer;
    function WriteHelpCallLine: String;
    function WriteHelpOptions(MaxWidth: Integer): String;
    function WriteHelpParams(MaxWidth: Integer): String;

    function ParseLongOption(const Args: TStringArray; var AIndex: SizeInt): TParsedArgument;
    function ParseShortOptions(const Args: TStringArray; var AIndex: SizeInt): TParsedArgumentArray;
    function ParseOption(const Args: TStringArray; var AIndex: SizeInt): TParsedArgumentArray; inline;
    function ParseParam(const Args: TStringArray; var AIndex: SizeInt; AParamIndex: Integer): TParsedArgumentArray;

    function DoParse(const Args: TStringArray; var AIndex: SizeInt): TParsedArgumentArray;
  public
    constructor Create(const ADescription: String = '');
    destructor Destroy; override;

    procedure AddOption(ShortName: Char; const LongName: String;
                        const Description: String = '';         
                        Required: Boolean = False;
                        OptionType: TOptionType = TOptionType.Switch);
    procedure AddOption(const LongName: String;
                        const Description: String = ''; 
                        Required: Boolean = False;
                        OptionType: TOptionType = TOptionType.Switch); inline;
    procedure AddValueOption(ShortName: Char; const LongName: String;
                             const Description: String = '';
                             Required: Boolean = False); inline;
    procedure AddValueOption(const LongName: String;
                             const Description: String = '';
                             Required: Boolean = False); inline;
    procedure AddListOption(ShortName: Char; const LongName: String;
                            const Description: String = '';
                            Required: Boolean = False); inline;
    procedure AddListOption(const LongName: String;
                            const Description: String = '';
                            Required: Boolean = False); inline;

    procedure AddParameter(const AName: String; const Description: String = '';
                           Required: Boolean = True; IsList: Boolean = False);

    procedure CommandParam(const AName: String; const ADescription: String = '';
                           Required: Boolean = False);

    function AddCommand(const ACommand: String; const ADescription: String = ''): TArgumentParser;

    function HasOption(ShortName: Char; LongName: String): Boolean;
    function HasOption(LongName: String): Boolean;

    function HasParameter(const ParamName: String): Boolean;

    function WriteHelp(MaxWidth: Integer): String;

    function Parse: TParsedArguments; inline;
    function Parse(const Args: TStringArray): TParsedArguments; inline;
  end;

  { TTerminalApplication }

  TTerminalApplication = class
  private
    FTerminal: TTerminal;
    FColors: Boolean;
    FVerbose: Boolean;
    FParser: TArgumentParser;
  protected
    procedure Execute(const Args: TParsedArguments); virtual; abstract;
    function CreateParser: TArgumentParser; virtual;

    procedure WriteHelp(AParser: TArgumentParser = nil); virtual;

    procedure WriteError(const ACause: String; const AMessage: String; const MsgEnd: String = #13#10);
    procedure WriteWarning(const ACause: String; const AMessage: String; const MsgEnd: String = #13#10);
    procedure WriteInfo(const AMessage: String; const MsgEnd: String = #13#10);
    procedure WriteHint(const AMessage: String; const MsgEnd: String = #13#10);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;

    property Terminal: TTerminal read FTerminal;
    property Colors: Boolean read FColors;
    property Verbose: Boolean read FVerbose;
    property Parser: TArgumentParser read FParser;
  end;

implementation

function PaddedText(AText: String; Padding: Integer; MaxWidth: Integer; InitialPad: Integer = Integer.MaxValue): String;
var
  PadString: String;
begin
  Result := '';
  PadString := '';

  if AText.Length > MaxWidth - Padding then
  begin
    SetLength(PadString, Padding);
    FillChar(PadString[1], Padding, ' ');
    PadString := #13#10 + PadString;
  end;

  if InitialPad < Padding then
  begin
    SetLength(Result, Padding - InitialPad);
    FillChar(Result[1], Result.Length, ' ');
  end;

  repeat
    Result += AText.Substring(0, MaxWidth - Padding);
    AText := AText.Substring(MaxWidth - Padding);
    Result += PadString;
  until AText.IsEmpty;
end;

{ TParsedArguments }

function TParsedArguments.GetListOption(const AName: String): TStringArray;
var
  Arg: TParsedArgument;
begin
  Result := [];
  for Arg in FArguments do
   if (Arg.ArgumentType = TArgumentType.Option) and (Arg.Name = AName) then
     Result += [Arg.Value];
end;

function TParsedArguments.GetListParameter(const AName: String): TStringArray;
var
  Arg: TParsedArgument;
begin
  Result := [];
  for Arg in FArguments do
   if (Arg.ArgumentType = TArgumentType.Parameter) and (Arg.Name = AName) then
     Result += [Arg.Value];
end;

function TParsedArguments.GetOption(const AName: String): String;
var
  i: Integer;
begin
  Result := '';
  for i:=High(FArguments) downto Low(FArguments) do
   if (FArguments[i].ArgumentType = TArgumentType.Option) and (FArguments[i].Name = AName) then
     Exit(FArguments[i].Value);
end;

function TParsedArguments.GetParameter(const AName: String): String;
var
  i: Integer;
begin
  Result := '';
  for i:=High(FArguments) downto Low(FArguments) do
   if (FArguments[i].ArgumentType = TArgumentType.Parameter) and (FArguments[i].Name = AName) then
     Exit(FArguments[i].Value);
end;

function TParsedArguments.HasOption(const AName: String): Boolean;
var
  Arg: TParsedArgument;
begin
  Result := False;
  for Arg in FArguments do
   if (Arg.ArgumentType = TArgumentType.Option) and (Arg.Name = AName) then
     Exit(True);
end;

function TParsedArguments.HasParameter(const AName: String): Boolean;
var
  Arg: TParsedArgument;
begin
  Result := False;
  for Arg in FArguments do
   if (Arg.ArgumentType = TArgumentType.Parameter) and (Arg.Name = AName) then
     Exit(True);
end;

{ TArgumentParser }

function TArgumentParser.DescriptionPadding: Integer;
var
  Opt: TOptionInfo;
  RequiredWidth: Integer;
  Param: TParameterInfo;
  SubCommand: String;
begin
  Result := 0;
  // Options
  // Format:
  //   -X, --XXX[=VALUE]    DESC'
  // I.e. 2 + ShortSize(2) + 2 + LongSize + 4
  //    = 10 + LongSize
  //    = 12 + LongName + Opt(6)
  for Opt in FOptions do
  begin
    RequiredWidth := 12 + Opt.LongName.Length;
    if Opt.ArgumentType > TOptionType.Switch then
      RequiredWidth += 6;
    if Opt.ArgumentType = TOptionType.MaybeValue then
      RequiredWidth += 2;
    if Result < RequiredWidth then
      Result := RequiredWidth;
  end;
  // Parameter
  // Format:
  //   XXX    DESC
  // I.e. 2 + Name + 4
  //    = 6 + Name
  for Param in FParameters do
  begin
    RequiredWidth := 6 + Length(Param.Name);
    if Result < RequiredWidth then
      Result := RequiredWidth;
  end;

  // Subcommands
  // Format:
  //     XXX    DESC
  // I.e. 4 + Name + 4
  //    = 8 + Name
  for SubCommand in FSubCommands.Keys do
  begin
    RequiredWidth := 8 + Length(SubCommand);
    if Result < RequiredWidth then
      Result := RequiredWidth;
  end;
end;

function TArgumentParser.WriteHelpCallLine: String;

function WriteCallLineOpts: String;
var
  OptionalSwitchShort: String = '';
  RequiredSwitchShort: String = '';
  SwitchLong: String = '';
  ValueOptions: String = '';

  OptString: String;  
  Opt: TOptionInfo;
begin
  Result := '';

  for Opt in FOptions do
    if (Opt.ArgumentType = TOptionType.Switch) then
    begin
      if Opt.Required and (Opt.ShortName = #00) then
        SwitchLong += ' --' + Opt.LongName
      else if Opt.Required and (Opt.ShortName <> #00) then
        RequiredSwitchShort += Opt.ShortName
      else if not Opt.Required and (Opt.ShortName = #00) then
        SwitchLong += ' [--%s]'.Format([Opt.LongName])
      else if not Opt.Required and (Opt.ShortName <> #00) then
        OptionalSwitchShort += Opt.ShortName;
    end
    else
    begin
      if Opt.ArgumentType = TOptionType.MaybeValue then
      begin
        if Opt.ShortName = #00 then
          OptString := '--%s[=VALUE]'.Format([Opt.LongName])
        else
          OptString := '-%s[ VALUE]'.Format([Opt.ShortName]);
      end
      else
      begin
        if Opt.ShortName = #00 then
          OptString := '--%s=VALUE'.Format([Opt.LongName])
        else
          OptString := '-%s VALUE'.Format([Opt.ShortName]);
      end;

      if Opt.ArgumentType = TOptionType.List then
        OptString += ' [%s [...]]'.Format([OptString]);

      if Opt.Required then
        ValueOptions += ' ' + OptString
      else
        ValueOptions += ' [%s]'.Format([OptString]);
    end;

  Result := FCommand;
  if not OptionalSwitchShort.IsEmpty then
    Result += ' [-%s]'.Format([OptionalSwitchShort]);
  if not SwitchLong.IsEmpty then
    Result += SwitchLong;
  if not ValueOptions.IsEmpty then
    Result += ValueOptions;
end;

function WriteCallLineParams: string;
var
  Param: TParameterInfo;
  ParamString: String;
begin
  Result := '';
  for Param in FParameters do
  begin
    ParamString := '<%s>'.Format([Param.Name]);
    if Param.ArgumentType = TParameterType.List then
      ParamString += ' [%s [...]]'.Format([ParamString])
    else if Param.ArgumentType = TParameterType.Command then
      ParamString += ' [<Args>]';
    if Param.Required then
      Result += ' ' + ParamString
    else
      Result += ' [%s]'.Format([ParamString]);
  end;
end;

begin
  Result := FCommand;

  Result += WriteCallLineOpts;
  Result += WriteCallLineParams;
end;

function TArgumentParser.WriteHelpOptions(MaxWidth: Integer): String;

function WriteOptLine(Opt: TOptionInfo; Padding: Integer): String;
var
  Desc: String;
begin
  if Opt.ShortName <> #00 then
    Result := '  -%s, '.Format([Opt.ShortName])
  else
    Result := '      ';
  Result += '--%s'.Format([Opt.LongName]);
  if Opt.ArgumentType = TOptionType.MaybeValue then
    Result += '[=VALUE]'
  else if Opt.ArgumentType > TOptionType.MaybeValue then
    Result += '=VALUE';

  Desc := Opt.Description;
  if Desc.IsEmpty then
    Exit;

  Result += PaddedText(Desc, Padding, MaxWidth, Result.Length);
end;

var
  Opt: TOptionInfo;
  Padding: Integer;
begin
  Padding := DescriptionPadding;
  if MaxWidth < Padding + 5 then // If it doesn't fit, we ignore it
    MaxWidth := Integer.MaxValue;

  Result := '';
  for Opt in FOptions do
    Result += WriteOptLine(Opt, Padding) + #13#10;
end;

function TArgumentParser.WriteHelpParams(MaxWidth: Integer): String;

function WriteParamLine(Param: TParameterInfo; Padding: Integer): String;
var
  Desc: String;
begin
  if Param.ArgumentType = TParameterType.Command then
    Result := #13#10'Subcommands available for <' + Param.Name + '>:'
  else
    Result := '  ' + Param.Name;

  Desc := Param.Description;
  if Desc.IsEmpty then
    Exit;

  Result += PaddedText(Desc, Padding, MaxWidth, Result.Length);
end;

function WriteCommandLine(Parser: TArgumentParser; Padding: Integer): String;
var
  Desc: String;
begin
  Result := '    ' + Parser.FCommand;

  Desc := Parser.FDescription;
  if Desc.IsEmpty then
    Exit;

  Result += PaddedText(Desc, Padding, MaxWidth, Result.Length);
end;

var
  Param: TParameterInfo;
  Padding: Integer;
  Parser: TArgumentParser;
begin
  Padding := DescriptionPadding;
  if MaxWidth > Padding + 5 then // If it doesn't fit, we ignore it
    MaxWidth := Integer.MaxValue;

  Result := '';
  for Param in FParameters do
    Result += WriteParamLine(Param, Padding) + #13#10;

  for Parser in FSubCommands.Values do
    Result += WriteCommandLine(Parser, Padding) + #13#10;
end;

function TArgumentParser.ParseLongOption(const Args: TStringArray;
  var AIndex: SizeInt): TParsedArgument;
var
  OptArg, OptName, OptValue: String;
  NameEnd: SizeInt;
  i: Integer;
begin
  OptArg := Args[AIndex];
  NameEnd := OptArg.IndexOf('=');
  if NameEnd < 0 then
    NameEnd := OptArg.Length;

  OptName := OptArg.Substring(2, NameEnd);
  OptValue := OptArg.Substring(NameEnd + 1);

  for i:=0 to Length(FOptions) - 1 do
    if FOptions[i].LongName = OptName then
    begin
      if (FOptions[i].ArgumentType >= TOptionType.Value) and (NameEnd < OptArg.Length) then
        raise ENoValuePresent.Create('Expected a Value for --' + OptName);
      if FOptions[i].ArgumentType = TOptionType.Switch then
        OptValue := 'True';
      Result.Name := OptName;
      Result.Parser := Self;
      Result.Value := OptValue;
      Result.ArgumentType := TArgumentType.Option;
      Inc(AIndex);
      Exit;
    end;

  raise EInvalidArgument.Create('Unknown option: --' + OptName);
end;

function TArgumentParser.ParseShortOptions(const Args: TStringArray;
  var AIndex: SizeInt): TParsedArgumentArray;

function ParseShortOpt(ShortName: Char; const LookAhead: String): TParsedArgument;
var
  Opt: TOptionInfo;
  OptValue: String = 'True';
begin
  for Opt in FOptions do
    if Opt.ShortName = ShortName then
    begin
      if (Opt.ArgumentType >= TOptionType.MaybeValue) then
        if (Opt.ArgumentType > TOptionType.MaybeValue) and (
           LookAhead.IsEmpty or
           (LookAhead.StartsWith('-') and
             (LookAhead <> '-') and
             (LookAhead <> '--')
           )
        ) then
          raise ENoValuePresent.Create('Expected a Value for -' + ShortName)
        else if Opt.ArgumentType > TOptionType.MaybeValue then
        begin
          OptValue := LookAhead;
          Inc(AIndex);
        end;
      Result.Name := Opt.LongName;
      Result.Value := OptValue;
      Result.Parser := Self;   
      Result.ArgumentType := TArgumentType.Option;
      Exit;
    end;

  raise EInvalidArgument.Create('Unknown option: -' + ShortName);
end;

var
  OptArg: String;
  LookAhead: String;
  i: Integer;
begin        
  OptArg := Args[AIndex];
  Result := [];
  SetLength(Result, Length(OptArg) - 1);
  for i:=2 to OptArg.Length do
  begin
    LookAhead := '';
    if (i = OptArg.Length) and (AIndex < Length(Args) - 1) then
      LookAhead := Args[AIndex + 1];
    Result[i - 2] := ParseShortOpt(OptArg[i], LookAhead); // this may change AIndex, don't use it afterwards
  end;
  Inc(AIndex);
end;

function TArgumentParser.ParseOption(const Args: TStringArray;
  var AIndex: SizeInt): TParsedArgumentArray;
begin
  if Args[AIndex][2] = '-' then
    Result := [ParseLongOption(Args, AIndex)]
  else
    Result := ParseShortOptions(Args, AIndex);
end;

function TArgumentParser.ParseParam(const Args: TStringArray;
  var AIndex: SizeInt; AParamIndex: Integer): TParsedArgumentArray;
var
  ParamValue: String;
  Param: TParameterInfo;
  ParsedParam: TParsedArgument;
  NextParser: TArgumentParser;
begin
  ParamValue := Args[AIndex];
  if (Length(FParameters) = 0) or (
       (AParamIndex > High(FParameters)) and
       (FParameters[High(FParameters)].ArgumentType <> TParameterType.List)
     ) then
    raise EInvalidArgument.Create('Unexpected parameter ' + ParamValue);
  if AParamIndex > High(FParameters) then
    AParamIndex := High(FParameters);

  Param := FParameters[AParamIndex];

  ParsedParam.Name := Param.Name;
  ParsedParam.Value := ParamValue;
  ParsedParam.Parser := Self;   
  ParsedParam.ArgumentType := TArgumentType.Parameter;
  Result := [ParsedParam];

  Inc(AIndex);

  if Param.ArgumentType = TParameterType.Command then
  begin
    if not FSubCommands.TryGetValue(ParamValue, NextParser) then
      raise EInvalidArgument.Create('Unknown subcommand ' +ParamValue);
    Result += NextParser.DoParse(Args, AIndex);
  end;
end;

function TArgumentParser.DoParse(const Args: TStringArray;
  var AIndex: SizeInt): TParsedArgumentArray;
var
  ParamIndex: Integer = 0;
begin
  Result := [];
  while AIndex < Length(Args) do
    if Args[AIndex].StartsWith('-') and (Args[AIndex] <> '-') and (Args[AIndex] <> '--') then
      Result += ParseOption(Args, AIndex)
    else
    begin
      Result += ParseParam(Args, AIndex, ParamIndex);
      Inc(ParamIndex);
    end;
end;

constructor TArgumentParser.Create(const ADescription: String);
begin
  inherited Create;
  FCommand := ExtractFileName(argv[0]);
  FDescription := ADescription;
  FOptions := [];
  FParameters := [];
  FSubCommands := TCommandMap.Create;
end;

destructor TArgumentParser.Destroy;
var
  SubParser: TArgumentParser;
begin
  for SubParser in FSubCommands.Values do
    SubParser.Free;
  FSubCommands.Free;
  inherited Destroy;
end;

procedure TArgumentParser.AddOption(ShortName: Char;
  const LongName: String; const Description: String; Required: Boolean;
  OptionType: TOptionType);
var
  Opt: TOptionInfo;
begin
  if (ShortName <> #00) and HasOption(ShortName, '') then
    raise EAlreadyRegistered.Create('Duplicate option -' + ShortName);
  if HasOption(LongName) then
    raise EAlreadyRegistered.Create('Duplicate option --' + LongName);

  Opt := Default(TOptionInfo);
  Opt.ShortName := ShortName;
  Opt.LongName := LongName;
  Opt.Required := Required;
  Opt.Description := Description;
  Opt.ArgumentType := OptionType;
  FOptions += [Opt];
end;

procedure TArgumentParser.AddOption(const LongName: String;
  const Description: String; Required: Boolean; OptionType: TOptionType);
begin
  AddOption(#00, LongName, Description, Required, OptionType);
end;

procedure TArgumentParser.AddValueOption(ShortName: Char;
  const LongName: String; const Description: String; Required: Boolean);
begin
  AddOption(ShortName, LongName, Description, Required, TOptionType.Value);
end;

procedure TArgumentParser.AddValueOption(const LongName: String;
  const Description: String; Required: Boolean);
begin
  AddOption(#00, LongName, Description, Required, TOptionType.Value);
end;

procedure TArgumentParser.AddListOption(ShortName: Char;
  const LongName: String; const Description: String; Required: Boolean);
begin
  AddOption(ShortName, LongName, Description, Required, TOptionType.List);
end;

procedure TArgumentParser.AddListOption(const LongName: String;
  const Description: String; Required: Boolean);
begin
  AddOption(#00, LongName, Description, Required, TOptionType.List);
end;

procedure TArgumentParser.AddParameter(const AName: String;
  const Description: String; Required: Boolean; IsList: Boolean);
var
  Param: TParameterInfo;
begin
  if HasParameter(AName) then
    raise EAlreadyRegistered.Create('Duplicate parameter ' + AName);
  if (Length(FParameters) > 0) and (FParameters[High(FParameters)].ArgumentType > TParameterType.Simple) then
    raise EInvalidParameter.Create('Cannot declare a parameter after a list or command parameter');
  if (Length(FParameters) > 0) and Required and not FParameters[High(FParameters)].Required then
    raise EInvalidParameter.Create('Cannot add a required parameter after an optional one');

  Param := Default(TParameterInfo);
  Param.Description := Description;
  Param.Name := AName;
  Param.Required := Required;
  if IsList then
    Param.ArgumentType := TParameterType.List
  else
    Param.ArgumentType := TParameterType.Simple;
  FParameters += [Param];
end;

procedure TArgumentParser.CommandParam(const AName: String;
  const ADescription: String; Required: Boolean);
var
  Param: TParameterInfo;
begin
  if HasParameter(AName) then
    raise EAlreadyRegistered.Create('Duplicate parameter ' + AName);
  if (Length(FParameters) > 0) and (FParameters[High(FParameters)].ArgumentType > TParameterType.Simple) then
    raise EInvalidParameter.Create('Cannot declare a parameter after a list or command parameter'); 
  if (Length(FParameters) > 0) and Required and not FParameters[High(FParameters)].Required then
    raise EInvalidParameter.Create('Cannot add a required parameter after an optional one');

  Param := Default(TParameterInfo);
  Param.Description := ADescription;
  Param.Name := AName;
  Param.Required := Required;
  Param.ArgumentType := TParameterType.Command;
  FParameters += [Param];
end;

function TArgumentParser.AddCommand(const ACommand: String;
  const ADescription: String): TArgumentParser;
begin
  if FSubCommands.ContainsKey(ACommand) then
    raise EAlreadyRegistered.Create('Command already registered ' + ACommand);
  if (Length(FParameters) = 0) or (FParameters[High(FParameters)].ArgumentType <> TParameterType.Command) then
    CommandParam('action');
  Result := TArgumentParser.Create(ADescription);
  Result.FCommand := ACommand;
  FSubCommands.Add(ACommand, Result);
end;

function TArgumentParser.HasOption(ShortName: Char; LongName: String
  ): Boolean;
var
  Opt: TOptionInfo;
begin
  Result := False;
  for Opt in FOptions do
    if ((ShortName <> #00) and (Opt.ShortName = ShortName))
    or (not Opt.LongName.IsEmpty and (Opt.LongName = LongName)) then
      Exit(True);
end;

function TArgumentParser.HasOption(LongName: String): Boolean;
begin
  Result := HasOption(#00, LongName);
end;

function TArgumentParser.HasParameter(const ParamName: String): Boolean;
var
  Param: TParameterInfo;
begin
  Result := False;
  for Param in FParameters do
    if Param.Name = ParamName then
      Exit(True);
end;

function TArgumentParser.WriteHelp(MaxWidth: Integer): String;
begin
  Result := 'Usage: ' + PaddedText(WriteHelpCallLine, FCommand.Length + 8, MaxWidth);
  Result += #13#10#13#10;
  if not FDescription.IsEmpty then
    Result += FDescription + #13#10#13#10;
  if Length(FOptions) > 0 then
  begin
    Result += 'Options:'#13#10;
    Result += WriteHelpOptions(MaxWidth);
  end;
  if Length(FParameters) > 0 then
  begin
    Result += #13#10'Positional Parameters:'#13#10;
    Result += WriteHelpParams(MaxWidth);
  end;
end;

function TArgumentParser.Parse: TParsedArguments;
var
  Args: TStringArray;
  i: Integer;
begin
  Args := Default(TStringArray);
  SetLength(Args, argc - 1);
  for i:=1 to argc - 1 do
    Args[i - 1] := argv[i];
  Result := Parse(Args);
end;

function TArgumentParser.Parse(const Args: TStringArray): TParsedArguments;
var
  Index: SizeInt;
begin
  Index := 0;
  Result.FArguments := DoParse(Args, Index);
end;

{ TTerminalApplication }

function TTerminalApplication.CreateParser: TArgumentParser;
begin
  Result := TArgumentParser.Create;
end;

procedure TTerminalApplication.WriteHelp(AParser: TArgumentParser);
var
  MaxWidth: Integer;
begin
  if not Assigned(AParser) then
    AParser := Parser;
  MaxWidth := Integer.MaxValue;
  if Terminal.IsATTY then
    MaxWidth := Terminal.GetWindowSize.Columns;
  WriteLn(AParser.WriteHelp(MaxWidth));
end;

procedure TTerminalApplication.WriteError(const ACause: String;
  const AMessage: String; const MsgEnd: String);
var
  MaxWidth: Integer;
begin
  if Colors then
  begin
    MaxWidth := Terminal.GetWindowSize.Columns;
    if not ACause.IsEmpty then
      Terminal.Output.WriteModified('[ERROR] ' + PaddedText(ACause, 8, MaxWidth) + #13#10,
                                    [BoldWeight, ForegroundColor(255, 0, 0)]);
    Terminal.Output.WriteColored(PaddedText(AMessage, 8, MaxWidth, 0) + MsgEnd, $FF0000);
    Exit;
  end;
  if not ACause.IsEmpty then
    Write('[ERROR] ', ACause, #13#10);
  WriteLn(AMessage + MsgEnd);
end;

procedure TTerminalApplication.WriteWarning(const ACause: String;
  const AMessage: String; const MsgEnd: String);
var
  MaxWidth: Integer;
begin
  if Colors then
  begin
    MaxWidth := Terminal.GetWindowSize.Columns;
    if not ACause.IsEmpty then
      Terminal.Output.WriteModified('[WARNING] ' + PaddedText(ACause, 10, MaxWidth) + #13#10,
                                    [BoldWeight, ForegroundColor(255, 128, 0)]);
    Terminal.Output.WriteColored(PaddedText(AMessage, 10, MaxWidth, 0) + MsgEnd, $FF8000);
    Exit;
  end;
  if not ACause.IsEmpty then
    Write('[WARNING] ', ACause, #13#10);
  WriteLn(AMessage + MsgEnd);
end;

procedure TTerminalApplication.WriteInfo(const AMessage: String;
  const MsgEnd: String);
begin
  if Colors then
  begin
    Terminal.Output.WriteColored(AMessage + MsgEnd, $660066);
    Exit;
  end;
  WriteLn(AMessage + MsgEnd);
end;

procedure TTerminalApplication.WriteHint(const AMessage: String;
  const MsgEnd: String);
begin
  if not Verbose then
    Exit;
  if Colors then
  begin
    Terminal.Output.WriteColored(AMessage + MsgEnd, 666699);
    Exit;
  end;
  WriteLn(AMessage + MsgEnd);
end;

constructor TTerminalApplication.Create;
begin
  FTerminal := TTerminal.Create;
  FColors := Terminal.IsATTY;
  FParser := CreateParser;
  FVerbose := False;
end;

destructor TTerminalApplication.Destroy;
begin
  Terminal.Free;
  FParser.Free;
  inherited Destroy;
end;

procedure TTerminalApplication.Run;
var
  Args: TParsedArguments;
  i: Integer;
begin
  // Adding default options:

  if not Parser.HasOption('help') then
    Parser.AddOption('help', 'Displays this help');

  if not Parser.HasOption('verbose') then
    Parser.AddOption('verbose', 'Also display hints and debug messages');

  if not Parser.HasOption('colors') then
    Parser.AddOption('colors', 'Always print colors, even when not connected to a terminal');

  try
    Args := Parser.Parse;
  except
  on E: EInvalidArgument do
  begin
    WriteError('Invalid Argument', E.Message);   
    WriteLn;
    WriteHelp;
    Exit;
  end;
  on E: ENoValuePresent do
  begin
    WriteError('Missing Parameter Value', E.Message);   
    WriteLn;
    WriteHelp;
    Exit;
  end;
  end;

  for i:=High(Args.FArguments) downto Low(Args.FArguments) do
    if (Args.FArguments[i].ArgumentType = TArgumentType.Option) and
       (Args.FArguments[i].Name = 'help') then
    begin
      WriteHelp(Args.FArguments[i].Parser);
      Exit;
    end;
  FVerbose := Args.HasOption('verbose');
  FColors := Parser.HasOption('colors');

  Execute(Args);
end;

end.

