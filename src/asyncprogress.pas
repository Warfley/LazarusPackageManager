unit AsyncProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, process, Zipper, lpmutils;

type
  EThreadTerminated = class(Exception);

  TProgressEvent = procedure(Sender: TObject; Progress: Double) of object;
  TStringMessageEvent = procedure(Sender: TObject; const Output: String) of object;
  TOutputHandler = function(const Output: String; PreviousProgress: Double): Double;
  TOutputHandlerMethod = function(const Output: String; PreviousProgress: Double): Double of object;

  { TAsyncProgress }

  TAsyncProgress = class
  private
    FOnProgress: TProgressEvent;
    FOnOutput: TStringMessageEvent;

    FTaskName: String;
    FTerminated: Boolean;
    FResult: Boolean;
    FLastError: String;

  protected
    procedure Success; inline;
    procedure Error(const AMessage: String); inline;
    procedure Progress(Progress: Double); inline;
    procedure Output(const AOutput: String); inline;

    procedure Execute; virtual; abstract;
  public
    constructor Create(const ATaskName: String);

    function Run: Boolean; inline;
    procedure Terminate; inline;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnOutput: TStringMessageEvent read FOnOutput write FOnOutput;

    property TaskName: String read FTaskName;
    property Terminated: Boolean read FTerminated;

    property Successful: Boolean read FResult;
    property LastError: String read FLastError;
  end;

  { TAsyncDownload }

  TAsyncDownload = class(TAsyncProgress)
  private
    FURL: String;
    FTarget: String;
    FGuessedSize: SizeInt;
    procedure DataReceived(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AURL: String;
      const ATarget: String; AGuessedSize: SizeInt);
  end;

  { TAsyncProcessExecution }

  TAsyncProcessExecution = class(TAsyncProgress)
  private
    FExecutable: String;
    FArguments: array of String;
    FOutputParser: TOutputHandler;
    FOutputParserMethod: TOutputHandlerMethod;
    FEnvironment: TStringList;

    function ParseOutput(const OutputData: String; LastProgress: Double): Double;
  protected
    procedure Execute; override;
  public                      
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String);
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String;
                       const AOutputParser: TOutputHandler);
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String;
                       const AOutputParser: TOutputHandlerMethod);

    destructor Destroy; override;

    procedure AddToPath(const NewPath: String);

    property Environment: TStringList read FEnvironment;
  end;

  { TAsyncUnzip }

  TAsyncUnzip = class(TAsyncProgress)
  private
    FZipArchive: String;
    FTargetDir: String;
    procedure ZipProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AZipArchive: String;
                       const ATargetDir: String);
  end;

  { TAsyncZipCleanup }

  TAsyncZipCleanup = class(TAsyncProgress)
  private
    FZipArchive: String;
    FTargetDir: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AZipArchive: String;
                       const ATargetDir: String);
  end;

function PercentParser(const AOutput: String; LastProgress: Double): Double;
implementation

function PercentParser(const AOutput: String; LastProgress: Double): Double;
var
  LastPercent, PercentStart: SizeInt;
  Progress: LongInt;
begin
  LastPercent := AOutput.LastIndexOf('%') + 1;
  if LastPercent = 0 then
    Exit(LastProgress);
  PercentStart := LastPercent - 1;
  while (PercentStart > 0) and (AOutput[PercentStart] in ['0'..'9']) do
    Dec(PercentStart);
  Progress := StrToInt(AOutput.Substring(PercentStart, LastPercent-PercentStart-1));
  Result := Progress / 100;
end;

{ TAsyncProgress }

procedure TAsyncProgress.Success;
begin
  FResult := True;
  FLastError := String.Empty;
end;

procedure TAsyncProgress.Error(const AMessage: String);
begin
  FResult := False;
  FLastError := AMessage;
end;

procedure TAsyncProgress.Progress(Progress: Double);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress);
end;

procedure TAsyncProgress.Output(const AOutput: String);
begin
  if Assigned(FOnOutput) then
    FOnOutput(Self, AOutput);
end;

constructor TAsyncProgress.Create(const ATaskName: String);
begin
  inherited Create;
  FTaskName := ATaskName;
  FTerminated := False;
end;

function TAsyncProgress.Run: Boolean;
begin
  Execute;
  Result := FResult;
end;

procedure TAsyncProgress.Terminate;
begin
  FTerminated := True;
end;

{ TAsyncDownload }

procedure TAsyncDownload.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  if Terminated then
    TFPHTTPClient(Sender).Terminate;
  if ContentLength >= CurrentPos then
    Progress(CurrentPos/ContentLength)
  else if FGuessedSize >= CurrentPos then
    Progress(CurrentPos/FGuessedSize)
  else
    Progress(-1);
end;

procedure TAsyncDownload.Execute;
var
  http: TFPHTTPClient;
begin
  if Terminated then
    Exit;
  http := TFPHTTPClient.Create(nil);
  try
    http.OnDataReceived :=@DataReceived;
    http.AllowRedirect := True;
    try
      ForceDirectories(ExtractFileDir(FTarget));
      Output('Start downloading ' + FURL);
      http.Get(FURL, FTarget);
    except on E: Exception do
      Error(E.Message);
    end;
  finally
    http.Free;
  end;
  Output('Download successful to ' + FTarget);
  Success;
end;

constructor TAsyncDownload.Create(const ATaskName: String; const AURL: String; const ATarget: String;
  AGuessedSize: SizeInt);
begin
  inherited Create(ATaskName);
  FURL := AURL;
  UniqueString(FURL);
  FTarget := ATarget;
  UniqueString(FTarget);
  FGuessedSize := AGuessedSize;
end;

{ TAsyncProcessExecution }

function TAsyncProcessExecution.ParseOutput(const OutputData: String;
  LastProgress: Double): Double;
begin
  Result := LastProgress;
  if Assigned(FOutputParser) then
    Result := FOutputParser(OutputData, LastProgress)
  else if Assigned(FOutputParserMethod) then
    Result := FOutputParserMethod(OutputData, LastProgress);
end;

procedure TAsyncProcessExecution.Execute;
var
  Proc: TProcess;
  buff: String = '';
  GitOut: String = '';
  Arg: String;
  LastProgress: Double;
begin  
  if Terminated then
    Exit;
  LastProgress := -1;
  Proc := TProcess.Create(nil);
  try
    proc.Executable := FExecutable;
    Proc.Environment.Assign(FEnvironment);
    for Arg in FArguments do
      Proc.Parameters.Add(Arg);
    Proc.Parameters.Delimiter := ' ';
    Output('Executing %s %s'.Format([Proc.Executable, Proc.Parameters.DelimitedText]));
    Proc.Options := Proc.Options + [poUsePipes, poNoConsole];

    Proc.Execute;
    while Proc.Running do
    begin
      if Terminated then
      begin
        Proc.Terminate(1);
        Exit;
      end;
      if Proc.Output.NumBytesAvailable > 0 then
      begin
        SetLength(buff, Proc.Output.NumBytesAvailable);
        Proc.Output.Read(buff[1], buff.Length);
        LastProgress := ParseOutput(buff, LastProgress);
        Progress(LastProgress);
        Output(buff);
        GitOut += buff;
      end
      else if Proc.Stderr.NumBytesAvailable > 0 then
      begin
        SetLength(buff, Proc.Stderr.NumBytesAvailable);
        Proc.Stderr.Read(buff[1], buff.Length);
        LastProgress := ParseOutput(buff, LastProgress);
        Progress(LastProgress);
        Output(buff); 
        GitOut += buff;
      end
      else
        Sleep(10);
    end;
    // Fetch last output from pipes
    if Proc.Output.NumBytesAvailable > 0 then
    begin
      SetLength(buff, Proc.Output.NumBytesAvailable);
      Proc.Output.Read(buff[1], buff.Length);
      LastProgress := ParseOutput(buff, LastProgress);
      Progress(LastProgress);
      Output(buff);
      GitOut += buff;
    end;
    if Proc.Stderr.NumBytesAvailable > 0 then
    begin
      SetLength(buff, Proc.Stderr.NumBytesAvailable);
      Proc.Stderr.Read(buff[1], buff.Length);
      LastProgress := ParseOutput(buff, LastProgress);
      Progress(LastProgress);
      Output(buff);
      GitOut += buff;
    end;
    if Proc.ExitCode = 0 then
    begin
      Progress(1);
      Success;
    end
    else
      Error(GitOut);
  finally
    Proc.Free;
  end;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String);
var
  i: Integer;
  Env: String;
begin
  Inherited Create(ATaskName);
  FEnvironment := TStringList.Create;
  for i := 1 to GetEnvironmentVariableCount do
  begin
    Env := GetEnvironmentString(i);
    // Not sure if GetEnvironmentString may be shared, better to unique it
    UniqueString(Env);
    FEnvironment.Add(Env);
  end;
  FExecutable := AExecutable;
  UniqueString(FExecutable);
  SetLength(FArguments, Length(AArguments));
  for i:=0 to Length(AArguments) - 1 do
  begin
    FArguments[i] := AArguments[i];
    UniqueString(FArguments[i]);
  end;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String;
  const AOutputParser: TOutputHandler);
begin
  Create(ATaskName, AExecutable, AArguments);
  FOutputParser := AOutputParser;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String;
  const AOutputParser: TOutputHandlerMethod);
begin
  Create(ATaskName, AExecutable, AArguments);
  FOutputParserMethod := AOutputParser;
end;

destructor TAsyncProcessExecution.Destroy;
begin
  FEnvironment.Free;
  inherited Destroy;
end;

procedure TAsyncProcessExecution.AddToPath(const NewPath: String);
begin
  // Add to front of PATH so it has highest priority
  {$IfDef WINDOWS}
  FEnvironment.Values['Path'] := NewPath + ';' + FEnvironment.Values['Path'];
  {$Else}
  FEnvironment.Values['PATH'] := NewPath + ':' + FEnvironment.Values['PATH'];
  {$EndIf}
end;

{ TAsyncUnzip }

procedure TAsyncUnzip.ZipProgress(Sender: TObject; const ATotPos,
  ATotSize: Int64);
begin
  if Terminated then
    TUnZipper(Sender).Terminate;
  Progress(ATotPos / ATotSize);
end;

procedure TAsyncUnzip.Execute;
var
  ZipFile: TUnZipper;
begin    
  if Terminated then
    Exit;
  try
    ForceDirectories(FTargetDir);
    ZipFile := TUnZipper.Create;
    try
      Output('Unzipping ' + FZipArchive + ' to ' + FTargetDir);
      ZipFile.OutputPath := FTargetDir;
      ZipFile.FileName := FZipArchive;
      ZipFile.OnProgressEx :=@ZipProgress;
      ZipFile.UnZipAllFiles;
    finally
      ZipFile.Free;
    end;
  except on E: Exception do
    Error(E.Message);
  end;
  Output('All files unzipped');
  Success;
end;

constructor TAsyncUnzip.Create(const ATaskName: String;
  const AZipArchive: String; const ATargetDir: String);
begin
  inherited Create(ATaskName);
  FZipArchive := AZipArchive;
  UniqueString(FTargetDir);
  FTargetDir := ATargetDir;
  UniqueString(FTargetDir);
end;  

{ TAsyncZipCleanup }

procedure TAsyncZipCleanup.Execute;
var
  ZipFile: TUnZipper;
  SubDirName, RelFileName, NewFileName: String;
  fl: TSearchRec;
begin
  if Terminated then
    Exit;
  try
    // gitlab zip files have a root directory that has the name of the commit
    // and a checksum. First we need to move all the contents from that directoy
    // to the target directory
    ZipFile := TUnZipper.Create;
    try
      ZipFile.FileName := FZipArchive;
      ZipFile.Examine;
      SubDirName := ZipFile.Entries[0].ArchiveFileName;
      SetLength(SubDirName, SubDirName.Length - 1);
    finally
      ZipFile.Free;
    end;
    Output('Moving extracted files to target directory');
    for fl in IterateContents(IncludeTrailingPathDelimiter(FTargetDir) + SubDirName) do
    begin
      RelFileName := ExtractRelativePath(IncludeTrailingPathDelimiter(FTargetDir) + SubDirName, fl.Name);
      NewFileName := IncludeTrailingPathDelimiter(FTargetDir) + RelFileName.Substring(SubDirName.Length + 1);
      RenameFile(fl.Name, NewFileName);
    end;
    Output('Removing archive and extraction artifacts');
    DeleteFile(FZipArchive);
    RmDir(IncludeTrailingPathDelimiter(FTargetDir) + SubDirName);
    Progress(1);
  except on E: Exception do
    Error(E.Message);
  end;
  Success;
end;

constructor TAsyncZipCleanup.Create(const ATaskName: String;
  const AZipArchive: String; const ATargetDir: String);
begin
  inherited Create(ATaskName);
  FZipArchive := AZipArchive;
  UniqueString(FTargetDir);
  FTargetDir := ATargetDir;
  UniqueString(FTargetDir);
end;

end.

