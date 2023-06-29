unit LazarusPackageManager;

{$mode ObjFPC}{$H+}
{$ModeSwitch arrayoperators}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, XmlReader, GitlabFetch, FileUtil,
  LazFileUtils, process, AsyncProgress, Generics.Collections;

type
  TPackageInfo = record
    PackageName: String;
    PackageFile: String;
    GlobalPackage: Boolean;
    PackageVersion: TBuildVersion;
    Registered: Boolean;
    Installable: Boolean;
    Installed: Boolean;
  end;

  TPackageMap = specialize THashMap<String, TPackageInfo>;

  { TLazarusPackageManager }

  TLazarusPackageManager = class
  private
    FConfigPath: String;
    FLazarusPath: String;

    FPackages: TPackageMap;

    function GetFPCDir: String;
    function GetMakePath: String;
    procedure LoadGlobalPackages;
    procedure LoadRegisteredPackages;
    procedure LoadInstalledPackages;
  public
    constructor Create(const LazarusPath: String);
    destructor Destroy; override;

    procedure UninstallPackage(const PackageName: String);
    procedure InstallPackage(const PackageFile: String);
    procedure RegisterPackage(const PackageFile: String);

    function RebuildLazarusJob: TAsyncProgress; inline;
    function RebuildLazbuildJob: TAsyncProgress; inline;
    function CompilePackageJob(const Package: TPackageInfo): TAsyncProgress; inline;

    procedure Reload;

    property Packages: TPackageMap read FPackages;
  end;

  { TLazarusProgressTracker }

  TLazarusProgressTracker = class
  private
    FPackagesToCompile: TStringList;
    FTotalProgress: Integer;
    FCurrentProgress: Integer;

    procedure LoadPackages(const ConfigDir: String);
  public
    constructor Create(const ConfigDir: String);
    destructor Destroy; override;

    function ParseOutput(const Output: String; LastProgress: Double): Double;
    procedure ProgressTerminated(Sender: TObject);
  end;

function GetConfigPath(const LazarusDir: String): String;
function ReadPackageFile(const PackageFile: String; GlobalPackage: Boolean): TPackageInfo;

function LazbuildPath(const LazarusDir: String): String; inline;
function FindInPath(const ExecName: String; out ExecPath: String): Boolean; inline;
implementation

function LazbuildPath(const LazarusDir: String): String;
begin
  Result := IncludeTrailingPathDelimiter(LazarusDir) + 'lazbuild' {$IfDef WINDOWS}+ '.exe'{$EndIf};
end;

function FindInPath(const ExecName: String; out ExecPath: String): Boolean;
begin
  {$IfDef WINDOWS}
  Result := RunCommand('C:\Windows\System32\where.exe', [ExecName], ExecPath, [poNoConsole]);
  {$Else}
  Result := RunCommand('which', [ExecName], ExecPath, [poNoConsole]);
  {$EndIf}
  ExecPath := ExecPath.Trim;
end;

function GetConfigPath(const LazarusDir: String): String;
var
  cfg, pcp: String;
  sl: TStringList;
begin
  {$IfDef WINDOWS}
  Result := ExtractFilePath(GetEnvironmentVariable('APPDATA')) + 'Local' + PathDelim + 'lazarus';
  {$Else}
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.lazarus';
  {$EndIf}
  cfg := IncludeTrailingPathDelimiter(LazarusDir) + 'lazarus.cfg';
  if not FileExists(cfg) then
    Exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(cfg);
    pcp := sl.Values['--primary-config-path'];
    if pcp.IsEmpty then
      pcp := sl.Values['--pcp'];
  finally
    sl.Free;
  end;
  if not pcp.IsEmpty then
    Result := pcp;
end;

function ParseVersionNode(VersionNode: TDOMNode): TBuildVersion;
var
  MajorNode, MinorNode, BuildNode: TDOMNode;
begin
  Result := BuildVersion(0, 0, 0);
  MajorNode := VersionNode.Attributes.GetNamedItem('Major');
  if Assigned(MajorNode) then
    Result.Major := AnsiString(MajorNode.NodeValue).ToInteger;
  MinorNode := VersionNode.Attributes.GetNamedItem('Minor');
  if Assigned(MinorNode) then
    Result.Minor := AnsiString(MinorNode.NodeValue).ToInteger;
  BuildNode := VersionNode.Attributes.GetNamedItem('Build');
  if Assigned(BuildNode) then
    Result.Patch := AnsiString(BuildNode.NodeValue).ToInteger;
end;

{ TLazarusProgressTracker }

procedure TLazarusProgressTracker.LoadPackages(const ConfigDir: String);
var
  sl: TStringList;
  StaticPackagesFile, pkg: String;
begin
  sl := TStringList.Create;
  try
    StaticPackagesFile := IncludeTrailingPathDelimiter(ConfigDir) + 'staticpackages.inc';
    if not FileExists(StaticPackagesFile) then
      Exit;
    sl.LoadFromFile(StaticPackagesFile);
    for pkg in sl do
      if not pkg.IsEmpty then
        FPackagesToCompile.Add(pkg.TrimRight([',']));
  finally
    sl.Free;
  end;
  FTotalProgress := FPackagesToCompile.Count + 5;
end;

constructor TLazarusProgressTracker.Create(const ConfigDir: String);
begin
  inherited Create;
  FCurrentProgress:=0;
  FPackagesToCompile := TStringList.Create;
  LoadPackages(ConfigDir);
end;

destructor TLazarusProgressTracker.Destroy;
begin
  FPackagesToCompile.Free;
  inherited Destroy;
end;

function TLazarusProgressTracker.ParseOutput(const Output: String;
  LastProgress: Double): Double;
var
  Found: Boolean;
  i: Integer;
begin
  if FTotalProgress = 0 then
    Exit(-1);
  repeat
    Found := False;
    for i:=0 to FPackagesToCompile.Count - 1 do
      if Output.Contains('Compiling ' + FPackagesToCompile[i]) then
      begin
        Found := True;
        FPackagesToCompile.Delete(i);
        inc(FCurrentProgress);
        Break;
      end;
  until not Found;
  if Output.Contains('Compiling lazarus.pp') then
    FCurrentProgress := FTotalProgress - 4;
  if Output.Contains('Linking') then
   FCurrentProgress := FTotalProgress - 2;
  Result := FCurrentProgress / FTotalProgress;
end;

procedure TLazarusProgressTracker.ProgressTerminated(Sender: TObject);
begin
  Free;
end;

{ TLazarusPackageManager }

procedure TLazarusPackageManager.LoadGlobalPackages;
var
  LinkList: TStringList;
  Link: String;
  i: Integer;
  Package: TPackageInfo;
begin
  LinkList := TStringList.Create;
  try
    FindAllFiles(LinkList, IncludeTrailingPathDelimiter(FLazarusPath) +
                 'packager' + PathDelim + 'globallinks', '*.lpl', False);
    for i := 0 to LinkList.Count - 1 do
    begin
      Link := ReadFileToString(LinkList[i])
             .Trim
             .Replace('$(LazarusDir)', FLazarusPath)
             .Replace('/', PathDelim);
      Package := ReadPackageFile(Link, True);
      FPackages.AddOrSetValue(Package.PackageName.ToLower, Package);
    end;

  finally
    LinkList.Free;
  end;
end;



function ReadPackageFile(const PackageFile: String; GlobalPackage: Boolean): TPackageInfo;
var
  PackageXML: TXMLDocument;
  PackageNode, NameNode, VersionNode, FilesNode, HasRegisteredNode: TDOMNode;
  i: Integer;
begin
  Result := Default(TPackageInfo);
  ReadXMLFile(PackageXML, PackageFile);
  try
    Result.PackageName := ExtractFileNameWithoutExt(PackageFile);
    Result.PackageFile := PackageFile;
    Result.GlobalPackage := GlobalPackage;
    PackageNode := PackageXML.DocumentElement.ChildNodes[0];
    NameNode := PackageNode.FindNode('Name');
    if Assigned(NameNode) then
      Result.PackageName := AnsiString(NameNode.Attributes.GetNamedItem('Value').NodeValue);
    VersionNode := PackageNode.FindNode('Version');
    if Assigned(VersionNode) then
      Result.PackageVersion := ParseVersionNode(VersionNode);

    FilesNode := PackageNode.FindNode('Files');
    if not Assigned(FilesNode) then
      Exit;
    for i:=0 to FilesNode.ChildNodes.Count - 1 do
    begin
      HasRegisteredNode := FilesNode.ChildNodes[i].FindNode('HasRegisterProc');
      if Assigned(HasRegisteredNode) and
         (TDOMElement(HasRegisteredNode).GetAttribute('Value') = 'True') then
      begin
        Result.Installable := True;
        Break;
      end;
    end;
  finally
    PackageXML.Free;
  end;
end;

function TLazarusPackageManager.GetFPCDir: String;
var
  EnvironmentFile: String;
  EnvironmentXML: TXMLDocument;
  ConfigsNode, CompilerNode: TDOMNode;
begin
  Result := '';
  EnvironmentFile := IncludeTrailingPathDelimiter(FConfigPath) + 'environmentoptions.xml';
  if not FileExists(EnvironmentFile) then
  begin
    if FindInPath('fpc', Result) then
      Result := ExtractFilePath(Result)
    else
      Result := '';
    Exit;
  end;
  ReadXMLFile(EnvironmentXML, EnvironmentFile);
  try
    ConfigsNode := EnvironmentXML.DocumentElement.FirstChild;
    if not Assigned(ConfigsNode) or (ConfigsNode.ChildNodes.Count = 0) then
      Exit;
    CompilerNode := ConfigsNode.FindNode('CompilerFilename');
    if Assigned(CompilerNode) then
      Result := ExtractFileDir(TDOMElement(CompilerNode).GetAttribute('Value'));
  finally
    EnvironmentXML.Free;
  end;
  Result := Result.Replace('$(Lazarusdir)', FLazarusPath);
  if not Result.Contains(DirectorySeparator) then
    if FindInPath('fpc', Result) then
      Result := ExtractFilePath(Result)
    else
      Result := '';   ;
end;

function TLazarusPackageManager.GetMakePath: String;
var
  MakePath: String;
  Found: Boolean;
begin
  Result := '';
  {$IfDef WINDOWS}
  Result := GetFPCDir;
  if Result.IsEmpty then
    Exit('');
  Result := IncludeTrailingPathDelimiter(Result) + 'make.exe';
  Exit;
  {$EndIf}
  Found := FindInPath('make', MakePath);
  if Found then
    Result := MakePath;
end;

procedure TLazarusPackageManager.LoadRegisteredPackages;

procedure AddPackagesFromNode(RootNode: TDOMNode);
var
  i: Integer;
  ChildNode, FileNameNode, VersionNode: TDOMNode;
  PackageName, PackageFile: String;
  Package: TPackageInfo;
begin
  for i:=0 to RootNode.ChildNodes.Count - 1 do
  begin
    ChildNode := RootNode.ChildNodes[i];
    PackageName := AnsiString(ChildNode.FindNode('Name').Attributes.GetNamedItem('Value').NodeValue);

    if FPackages.TryGetValue(PackageName.ToLower, Package) then
    begin
      Package.Registered := True;
      FPackages[PackageName.ToLower] := Package;
      Exit;
    end;

    // Not a global package => read from file
    FileNameNode := ChildNode.FindNode('Filename');
    if not Assigned(FileNameNode) then
      Continue; // Because Anchordockpkg is weird

    PackageFile := AnsiString(FileNameNode.Attributes.GetNamedItem('Value').NodeValue);
    if not FilenameIsAbsolute(PackageFile) then
      PackageFile := CreateAbsolutePath(PackageFile, FLazarusPath);
    if not FileExists(PackageFile) then
      Continue;
    Package := ReadPackageFile(PackageFile, False);
    Package.Registered := True;
    FPackages.Add(PackageName.ToLower, Package);
  end;
end;

var
  PackagesConfig: String;
  PackagesXML: TXMLDocument;
  i: Integer;
begin
  PackagesConfig := IncludeTrailingPathDelimiter(FConfigPath) + 'packagefiles.xml';
  if not FileExists(PackagesConfig) then
    Exit;
  ReadXMLFile(PackagesXML, PackagesConfig);
  try
    for i:=0 to PackagesXML.DocumentElement.ChildNodes.Count - 1 do
      AddPackagesFromNode(PackagesXML.DocumentElement.ChildNodes[i]);
  finally
    PackagesXML.Free;
  end;
end;

procedure TLazarusPackageManager.LoadInstalledPackages;
var
  sl: TStringList;
  StaticPackagesFile, PackageName: String;
  i: Integer;
  Package: TPackageInfo;
begin
  StaticPackagesFile := IncludeTrailingPathDelimiter(FConfigPath) + 'staticpackages.inc';
  if not FileExists(StaticPackagesFile) then
    Exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(StaticPackagesFile);
    for i:=0 to sl.Count - 1 do
    begin
      PackageName := sl[i].TrimRight([',']);
      if not FPackages.TryGetValue(PackageName.ToLower, Package) then
        Continue;
      Package.Installed := True;
      FPackages[PackageName.ToLower] := Package;
    end;
  finally
    sl.Free;
  end;
end;

constructor TLazarusPackageManager.Create(const LazarusPath: String);
begin
  inherited Create;
  FLazarusPath := LazarusPath;
  FConfigPath := GetConfigPath(FLazarusPath);

  FPackages := TPackageMap.Create;
end;

destructor TLazarusPackageManager.Destroy;
begin
  FPackages.Free;
  inherited Destroy;
end;

procedure TLazarusPackageManager.Reload;
begin
  FPackages.Clear;
  LoadGlobalPackages;
  LoadRegisteredPackages;
  LoadInstalledPackages;
end;

procedure TLazarusPackageManager.UninstallPackage(const PackageName: String);

procedure DeleteFromStaticPackagesFile;
var
  sl: TStringList;
  StaticPackagesFile, PkgEntry: String;
  i: Integer;
begin
  StaticPackagesFile := IncludeTrailingPathDelimiter(FConfigPath) + 'staticpackages.inc';
  if not FileExists(StaticPackagesFile) then
    Exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(StaticPackagesFile);
    i := 0;
    while i < sl.Count do
    begin
      PkgEntry := sl[i].TrimRight([',']);
      if PkgEntry.ToLower = PackageName.ToLower then
        sl.Delete(i)
      else
        Inc(i);
    end;
    sl.SaveToFile(StaticPackagesFile);
  finally
    sl.Free;
  end;
end;

procedure DeleteFromMiscellaneousOptions;
var
  OptionsFile, NodePackageName: String;
  MOptionsXML: TXMLDocument;
  OptionsNode, AutoInstallsNode, BuildOptionsNode: TDOMNode;
  i: Integer;
  NewInstalls, NewInstallNode: TDOMElement;
begin
  OptionsFile := IncludeTrailingPathDelimiter(FConfigPath) + 'miscellaneousoptions.xml';
  ReadXMLFile(MOptionsXML, OptionsFile);
  try
    OptionsNode := MOptionsXML.DocumentElement.ChildNodes[0];
    BuildOptionsNode := OptionsNode.FindNode('BuildLazarusOptions');
    if not Assigned(BuildOptionsNode) then
      Exit;
    AutoInstallsNode := BuildOptionsNode.FindNode('StaticAutoInstallPackages');
    if not Assigned(AutoInstallsNode) then
      Exit;
    NewInstalls := MOptionsXML.CreateElement('StaticAutoInstallPackages');
    for i:=0 to AutoInstallsNode.ChildNodes.Count - 1 do
    begin
      NodePackageName := AnsiString(AutoInstallsNode.ChildNodes[i].Attributes.GetNamedItem('Value').NodeValue);
      if NodePackageName.ToLower = PackageName.ToLower then
        Continue;
      NewInstallNode := MOptionsXML.CreateElement(UnicodeString('Item' + IntToStr(NewInstalls.ChildNodes.Count + 1)));
      NewInstallNode.SetAttribute('Value', UnicodeString(NodePackageName));
      NewInstalls.AppendChild(NewInstallNode);
    end;
    NewInstalls.SetAttribute('Count', UnicodeString(NewInstalls.ChildNodes.Count.ToString));
    BuildOptionsNode.ReplaceChild(NewInstalls, AutoInstallsNode);

    WriteXMLFile(MOptionsXML, OptionsFile);
  finally
    MOptionsXML.Free;
  end;
end;

begin
  DeleteFromStaticPackagesFile;
  DeleteFromMiscellaneousOptions;
end;

procedure TLazarusPackageManager.InstallPackage(const PackageFile: String);
var
  dummy: String;
begin
  RunCommand(LazbuildPath(FLazarusPath), ['--add-package', PackageFile], dummy, [poNoConsole])
end;

procedure TLazarusPackageManager.RegisterPackage(const PackageFile: String);
var
  dummy: String;
begin
  RunCommand(LazbuildPath(FLazarusPath), ['--add-package-link=' + PackageFile], dummy, [poNoConsole]);
end;

function TLazarusPackageManager.RebuildLazarusJob: TAsyncProgress;
var
  Tracker: TLazarusProgressTracker;
begin
  Tracker := TLazarusProgressTracker.Create(FConfigPath);
  Result := TAsyncProcessExecution.Create('Building Lazarus', LazbuildPath(FLazarusPath),
                                          ['--build-ide='], @Tracker.ParseOutput);
  Result.OnTerminate := @Tracker.ProgressTerminated;
end;

function TLazarusPackageManager.RebuildLazbuildJob: TAsyncProgress;
var
  FPCDir, MakePath: String;
  Tracker: TLazarusProgressTracker;
begin
  Tracker := TLazarusProgressTracker.Create(FConfigPath);
  FPCDir := GetFPCDir;
  if FPCDir.IsEmpty then
    raise EFileNotFoundException.Create('No FPC installation found');
  MakePath := GetMakePath;
  if MakePath.IsEmpty then
    raise EFileNotFoundException.Create('No make found');
  Result := TAsyncProcessExecution.Create('Building Lazbuild', MakePath,
                                          ['-C', FLazarusPath, 'lazbuild'],
                                          @Tracker.ParseOutput);
  TAsyncProcessExecution(Result).AddToPath(FPCDir); 
  Result.OnTerminate := @Tracker.ProgressTerminated;
end;

function TLazarusPackageManager.CompilePackageJob(const Package: TPackageInfo
  ): TAsyncProgress;
begin
  Result := TAsyncProcessExecution.Create('Building Package: ' + Package.PackageName,
                                          LazbuildPath(FLazarusPath), [Package.PackageFile]);
end;

end.

