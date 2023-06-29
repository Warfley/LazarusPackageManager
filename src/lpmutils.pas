unit lpmutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFileIterator }

  TFileIterator = class
  private
    FFileRec: TSearchRec;
    FFirstMove: Boolean;
    FPattern: String;
    FDirectory: String;
    FAttributes: LongInt;
  public
    constructor Create(const ADirectory: String; const APattern: String = '*';
                       AAttributes: LongInt = faAnyFile);
    destructor Destroy; override;

    function GetEnumerator: TFileIterator;

    function MoveNext: Boolean;

    property Current: TSearchRec read FFileRec;
  end;

function IterateContents(const ADirectory: String; const APattern: String = '*';
                         AAttributes: LongInt = faAnyFile): TFileIterator; inline;
implementation

function IterateContents(const ADirectory: String; const APattern: String;
  AAttributes: LongInt): TFileIterator;
begin
  Result := TFileIterator.Create(ADirectory, APattern, AAttributes);
end;

{ TFileIterator }

constructor TFileIterator.Create(const ADirectory: String;
  const APattern: String; AAttributes: LongInt);
begin
  inherited Create;
  FDirectory := ADirectory;
  FPattern := APattern;
  FAttributes := AAttributes;
  FFirstMove := True;
end;

destructor TFileIterator.Destroy;
begin
  if not FFirstMove then
    FindClose(FFileRec);
  inherited Destroy;
end;

function TFileIterator.GetEnumerator: TFileIterator;
begin
  Result := Self;
end;

function TFileIterator.MoveNext: Boolean;
begin
  if FFirstMove then
  begin
    FFirstMove := False;
    Result := FindFirst(IncludeTrailingPathDelimiter(FDirectory) + FPattern,
                        FAttributes, FFileRec) = 0;
    Exit;
  end;
  Result := FindNext(FFileRec) = 0;
end;

end.

