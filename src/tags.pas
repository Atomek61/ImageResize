unit tags;

{$mode Delphi}
{$modeswitch typehelpers}
{$modeSwitch advancedRecords}

// An TFilesTags dictionary contains a dictionary of tags for a list of files.
//
// First of all, add a list of full qualified filenames. For each of them,
// a tags dictionary will be created.
//
// Now you can load .tags files with LoadTagsFiles.This will search for .tags-
// files in each directory where files are originated.
//
// .tags file format
//
//   Filename, Title, Copyright, MyTag
//   DSC02405.jpg, Die Hinfahrt im Stau, , myfavorite
//   DSC02407.jpg, "Ich verliere sie, die Hose", (c) 2023 Mogli Maisenkaiser
//   DSC02410.jpg, Mona Lisa,*
//
// - The file is UTF-8 encoded, with BOM
// - The first column is always "Filename"
// - Only the file title is to be stored
// - The order of rows has no meaning
// - The headers tagIds are free, only Filename is mandatory
// - The column order is free, only Filename is always first
// - There may be less fields in a row than in the header, but not more
// - Fields with a comma, must be enquoted with double quote "
// - A field may contain *, this repeats the last field
// - To use an empty Asterisk, enquote it "*"
//
// To add tags from EXIF information, handle them later.
//

interface

uses
  Classes, SysUtils, generics.collections;

const
  TAGS_FILETITLE      = '.tags';

  TAGID_FILENAME     =  'Filename';
  TAGID_FILETITLE    =  'Filetitle';

  TAGID_TITLE        =  'Title';
  TAGID_TIMESTAMP    =  'Timestamp';
  TAGID_COPYRIGHT    =  'Copyright';

type
  TTagIDs = TStringArray;

  { TTagIDsHelper }

  TTagIDsHelper = type helper for TTagIDs
    procedure Add(const ID :string);
    function Contains(const ID :string) :boolean;
  end;

  TTags = TDictionary<string, string>; // Tags of a source file
  TFilesTagsDictionary = TObjectDictionary<string, TTags>; // Directory/.tags
  TTagsFileDictionary = TDictionary<string, string>;
  TFilenameTags = TPair<string, TTags>;
  TUniqueStrings = THashSet<string>;

  { TFilesTags }

  TFilesTags = class(TFilesTagsDictionary)
  public type
    TSaveOption = (soCompact, soRelative);
    TSaveOptions = set of TSaveOption;
  private
    FFilenames :TStringList; // Contains the Filenames in the order they where added
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const Filename :string); overload;
    procedure Add(Filenames :TStrings); overload;
    procedure LoadTagsFiles;
    procedure SaveToFile(const LstFilename :string; const TagIDs :TTagIDs; Options :TSaveOptions = []);
    function TagIDs :TTagIDs;
  end;

resourcestring
  SErrInvalidTagsFileFmt  = 'Invalid .tags file ''%s'' - %s.';
  SErrInvalidLineFmt      = 'Invalid .tags file ''%s'' line %d - %s.';
  SErrEmpty               = 'file is empty';
  SErrFilenameNotFound    = 'tag ''Filename'' not in first column';
  SErrMoreValuesThanTags  = 'more values than tags';
  SErrMissingFilename     = 'missing filename';
  SErrAsterisk            = 'first row cannot contain a *';

implementation

function RemoveQuotes(const Item :string) :string; overload;
var
  n :integer;
  s :string;
begin
  s := Trim(Item);
  n := Length(s);
  if (n<2) or (s[1]<>'"') or (s[n]<>'"') then Exit(s);
  result := Copy(s, 2, n-2);
end;

procedure RemoveQuotes(var Items :TStringArray); overload;
var i :integer;
begin
  for i:=0 to High(Items) do
    Items[i] := RemoveQuotes(Items[i]);
end;

function Quoted(const Value :string) :string;
begin
  result := '"' + Value + '"';
end;

function QuotedIfComma(const Value :string) :string;
var
  i :integer;
begin
  for i:=1 to Length(Value) do begin
    if Value[i]=',' then Exit(Quoted(Value));
  end;
  result := Value;
end;

{ TTagIDsHelper }

procedure TTagIDsHelper.Add(const ID: string);
begin
  if Contains(ID) then Exit;
  SetLength(self, Length(self)+1);
  self[High(self)] := ID;
end;

function TTagIDsHelper.Contains(const ID: string): boolean;
var
  i :integer;
begin
  for i:=0 to High(self) do
    if SameText(ID, self[i]) then Exit(true);
  result := false;
end;

{ TFilesTags }

constructor TFilesTags.Create;
begin
  inherited;
  FFilenames := TStringList.Create;
end;

destructor TFilesTags.Destroy;
begin
  FFilenames.Free;
  inherited;
end;

procedure TFilesTags.Add(const Filename: string);
begin
  inherited Add(Filename, TTags.Create);
  FFilenames.Add(Filename);
end;

procedure TFilesTags.Add(Filenames: TStrings);
var
  Filename :string;
begin
  for Filename in Filenames do
    Add(Filename);
end;

procedure TFilesTags.LoadTagsFiles;
var
  TagsFileSet :TUniqueStrings;
  TagIDSet :TUniqueStrings;
  FilenameTags :TFilenameTags;
  Path, Filetitle, Filename :string;
  TagsFilename :string;
  TagsFile :TStringList;
  ColKeys :TStringArray;
  Row :TStringArray;
  Tags :TTags;
  LastTags :TTags;
  FilenameTag :string;
  i, j :integer;
  Value :string;
  UnusedRow :boolean;
  UnusedTags :TTags;

  procedure RaiseInvalid(const Reason :string; Line :integer=-1);
  begin
    if Line=-1 then
      raise Exception.CreateFmt(SErrInvalidTagsFileFmt, [TagsFilename, Reason])
    else
      raise Exception.CreateFmt(SErrInvalidLineFmt, [TagsFilename, Line+1, Reason]);
  end;

  procedure AddTagID(const TagID :string);
  begin
    if TagIDSet.Contains(TagId) then Exit;
    TagIdSet.Add(TagID);
  end;

begin
  TagsFileSet := TUniqueStrings.Create;
  TagIDSet := TUniqueStrings.Create;
  UnusedTags := TTags.Create;
  try
    for FilenameTags in self do begin
      Path := IncludeTrailingPathDelimiter(ExtractFilePath(FilenameTags.Key));
      Filetitle := ExtractFilename(FilenameTags.Key);
      TagsFilename := Path + TAGS_FILETITLE;
      if not TagsFileSet.Contains(TagsFilename) and FileExists(TagsFilename) then begin
        TagsFile := TStringList.Create;
        try
          LastTags := nil;
          TagsFileSet.Add(TagsFilename);
          TagsFile.LoadFromFile(TagsFilename, TEncoding.UTF8);
          if TagsFile.Count=0 then RaiseInvalid(SErrEmpty);
          // Handle header line: Filename, Tag1, Tag2, ...
          ColKeys := TagsFile[0].Split(',');
          if (Length(ColKeys)=0) then RaiseInvalid(SErrEmpty);
          RemoveQuotes(ColKeys);
          FilenameTag := Trim(ColKeys[0]);
          if not SameText(FilenameTag, TAGID_FILENAME) then RaiseInvalid(SErrFilenameNotFound, 0);
          for i:=1 to TagsFile.Count-1 do begin
            Row := TagsFile[i].Split(',', '"', '"');
            if (Length(Row)<1) then
              RaiseInvalid(SErrMissingFilename, i);
            if Length(Row)>Length(ColKeys) then
              RaiseInvalid(SErrMoreValuesThanTags, i);
            Filetitle := RemoveQuotes(Row[0]);
            Filename := Path + Filetitle;
            // All rows are beeing evaluated
            UnusedRow := not TryGetValue(Filename, Tags);
            if UnusedRow then Tags := UnusedTags;
//              WriteLn(Format('Loading for %s', [Filename]));
            for j:=1 to High(ColKeys) do begin
              if j>=Length(Row) then break;
              if Trim(Row[j])='*' then begin
                if not Assigned(LastTags) then RaiseInvalid(SErrAsterisk);
                Value := LastTags[ColKeys[j]];
              end else
                Value := Row[j];
              Tags.AddOrSetValue(ColKeys[j], RemoveQuotes(Value));
            end;
            LastTags := Tags;
          end;
        finally
          TagsFile.Free;
        end;
      end;
    end;
  finally
    TagsFileSet.Free;
    TagIDSet.Free;
    UnusedTags.Free;
  end;
end;

procedure TFilesTags.SaveToFile(const LstFilename: string; const TagIDs :TTagIDs; Options :TSaveOptions);
var
  s :TStringList;
  Line :string;
  Filename :string;
  Tags :TTags;
  Pair :TPair<string, string>;
  i :integer;
  BasePath :string;

  function GetField(const TagID :string) :string;
  begin
    if Tags.TryGetValue(TagID, result) then
      result := QuotedIfComma(result)
    else
      result := '';
  end;

begin
  if soRelative in Options then
    BasePath := ExtractFilePath(LstFilename)
  else
    BasePath := '';
  s := TStringList.Create;
  try
    s.WriteBOM := true;
    if soCompact in Options then begin
      for Filename in FFilenames do begin
        Tags := self[Filename];
        Line := ExtractFilename(Filename)+': ';
        for Pair in Tags do
          Line := Line + Pair.Key + '=' + Pair.Value+', ';
        s.Add(Line);
      end;
    end else begin
      begin
        Line := TAGID_FILENAME;
        for i:=0 to High(TagIDs) do
          Line := Line + ', ' + TagIDs[i];
        s.Add(Line);
        for Filename in FFilenames do begin
          Tags := self[Filename];
          if soRelative in Options then
            Line := ExtractRelativePath(BasePath, Filename)
          else
            Line := Filename;
          Line := QuotedIfComma(Line);
          for i:=0 to High(TagIDs) do
            Line := Line + ',' + GetField(TagIDs[i]);
          s.Add(Line);
        end;
      end;
    end;
    s.SaveToFile(LstFilename, TEncoding.UTF8);
  finally
    s.Free;
  end;
end;

function TFilesTags.TagIDs: TTagIDs;
var
  i :TFilenameTags;
  j :TPair<string, string>;
begin
  result := nil;
  for i in self do begin
    for j in i.value do
      result.Add(j.Key);
  end;
end;

//procedure test;
//var
//  ft :TFilesTags;
//begin
//  ft := TFilesTags.Create;
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src1\DSC04236.jpg');
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src1\DSC04242.jpg');
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src2\DSC04288.jpg');
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src2\DSC04262.jpg');
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src2\src3\DSC04293.jpg');
//  ft.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\src2\src3\DSC04314.jpg');
//  try
//    ft.LoadTagsFiles;
//    ft.SaveToFile('D:\Mf\Dev\Lazarus\ImageResize\tst\srcB\.filestags', [soRelative]);
//  except on E :Exception do
//    begin
//      WriteLn(E.Message);
//    end;
//  end;
//end;
//
//initialization
//  test;
end.

