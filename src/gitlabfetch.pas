unit GitlabFetch;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch arrayoperators}

interface

uses
  Classes, SysUtils, opensslsockets, fphttpclient, Regexpr, fpjson, jsonparser;

type
  EGitException = class(Exception);

  { TBuildVersion }

  TBuildVersion = record
    Major, Minor, Patch: Integer;

    class operator <(const lhs, rhs: TBuildVersion): Boolean; inline;
    function ToString: String;
  end;

  // Maybe at date
  TBranchVersion = record
    Version: TBuildVersion;
    BranchName: String;
    CommitID: String;
    isTag: Boolean;
  end;

  TBranchVersions = array of TBranchVersion;

const
  FPCProjectID = '28644964';
  FPCTagPrefix = 'release';
  FPCCloneUrl = 'https://gitlab.com/freepascal.org/fpc/source.git';
  LazarusProjectID = '28419588';
  LazarusTagPrefix = 'lazarus';
  LazarusCloneUrl = 'https://gitlab.com/freepascal.org/lazarus/lazarus.git';

  APIUrl = 'https://gitlab.com/api/v4/projects/';
  TagsUrl = '/repository/tags';
  ArchiveURL = '/repository/archive.zip';

function BuildVersion(Major, Minor, Patch: Integer): TBuildVersion; inline;


function MainBranch: TBranchVersion;
function GetTagVersions(const ProjectID: String; const TagPrefix: String): TBranchVersions;
function DownloadURL(const ProjectID: String; const BranchVersion: TBranchVersion): String; inline;
function GitArguments(const ProjectURL: String; const BranchVersion: TBranchVersion;
                      const OutputDir: String): specialize TArray<String>; inline;
procedure SortVersions(var Versions: TBranchVersions);
function FindOrInsertSorted(NewVersion: TBranchVersion; var Versions: TBranchVersions): SizeInt;
implementation

function BuildVersion(Major, Minor, Patch: Integer): TBuildVersion;
begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Patch := Patch;
end;


class operator TBuildVersion.<(const lhs, rhs: TBuildVersion): Boolean;
begin
  Result := (lhs.Major < rhs.Major)
         or (lhs.Major = rhs.Major) and (lhs.Major < rhs.Minor)
         or (lhs.Major = rhs.Major) and (lhs.Major = rhs.Minor) and (lhs.Patch < rhs.Patch);
end;

function TBuildVersion.ToString: String;
begin
  if (Major = 999) and (Minor = 999) and (Patch = 999) then
    Result := 'Experimental'
  else
    Result := '%d.%d.%d'.Format([Major, Minor, Patch]);
end;

function ParseTagVersion(const TagPrefix: String; TagJson: TJSONObject;
                         out BranchVersion: TBranchVersion): Boolean;
var
  BranchRegex: TRegExpr;
begin
  Result := True;
  BranchRegex := TRegExpr.Create(TagPrefix + '_(\d+)_(\d+)_(\d+)');
  try
    BranchVersion.BranchName := TagJson.Get('name', '');
    if (not BranchRegex.Exec(BranchVersion.BranchName)) or
       (BranchRegex.MatchLen[0] <> BranchVersion.BranchName.Length) then
      Exit(False);
    BranchVersion.Version := BuildVersion(StrToInt(BranchRegex.Match[1]),
                                          StrToInt(BranchRegex.Match[2]),
                                          StrToInt(BranchRegex.Match[3]));
    BranchVersion.isTag := True;
    BranchVersion.CommitID := TagJson.Get('target', '');
    if BranchVersion.CommitID.IsEmpty then
      Exit(False);
  finally
    BranchRegex.Free;
  end;
end;

function MainBranch: TBranchVersion;
begin
  Result.Version := BuildVersion(999, 999, 999); 
  Result.BranchName := 'main';
  Result.CommitID := 'main';
  Result.isTag := False;
end;

function GetTagVersions(const ProjectID: String; const TagPrefix: String
  ): TBranchVersions;
var
  FetchURL, TagJson: String;
  JsonData: TJSONData;
  TagList: TJSONArray;
  TagObject: TJSONEnum;
  ParsedBranch: TBranchVersion;
begin
  Result := [];
  FetchURL := APIUrl + ProjectID + TagsUrl;
  TagJson := TFPHTTPClient.SimpleGet(FetchURL);
  JsonData := GetJSON(TagJson);
  try
    TagList := JsonData as TJSONArray;
    for TagObject in TagList do
      if ParseTagVersion(TagPrefix, TagObject.Value as TJSONObject, ParsedBranch) then
        Result += [ParsedBranch];
  finally
    JsonData.Free;
  end;
end;

function DownloadURL(const ProjectID: String;
                     const BranchVersion: TBranchVersion): String;
begin
  Result := APIUrl + ProjectID + ArchiveURL;
  if BranchVersion.BranchName <> 'main' then
    Result += '?sha=' + BranchVersion.CommitID;
end;

function GitArguments(const ProjectURL: String; const BranchVersion: TBranchVersion;
                      const OutputDir: String): specialize TArray<String>;
begin
  Result := ['clone', '--depth=1', ProjectURL, OutputDir];
end;

procedure SortVersions(var Versions: TBranchVersions);

procedure SwapElements(A, B: SizeInt);
var
  tmp: TBranchVersion;
begin
  tmp := Versions[A];
  Versions[A] := Versions[B];
  Versions[B] := tmp;
end;

var
  i, j: SizeInt;
begin
  // Version list is so small, bubble sort is fast than enough
  for i:=0 to Length(Versions) - 1 do
    for j:=i + 1 to Length(Versions) - 1 do
      if Versions[i].Version < Versions[j].Version then
        SwapElements(i, j);
end;

function FindOrInsertSorted(NewVersion: TBranchVersion; var Versions: TBranchVersions): SizeInt;
begin
  // So small that linear search is fast enough
  Result := 0;
  while (Result < Length(Versions)) and (NewVersion.Version < Versions[Result].Version) do
    Inc(Result);
  // If we found, the element at index result will not be smaller than the new element
  if (Result = Length(Versions)) or (Versions[Result].Version < NewVersion.Version) then
    Insert(NewVersion, Versions, Result);
end;

end.

