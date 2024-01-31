unit tags;

{$mode Delphi}
{$modeswitch typehelpers}
{$modeSwitch advancedRecords}

// A TFilesTags dictionary contains a dictionary of tags for a list of files.
//
// First of all, add a list of full qualified filenames. For each of them,
// a tags dictionary will be created.
//
// Now you can load .tags files with LoadFromTagsFiles.This will search for .tags-
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
// - The files name has no path
// - The other columns are free and optional
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
  Classes, SysUtils, generics.collections, StringArrays;

const
  TAGS_FILETITLE      = '.tags';
  TAGID_FILENAME     =  'Filename';

type
  TStringDictionary = TDictionary<string, string>;
  TTags = TStringDictionary; // Tags of a source file
  TFilesTagsDictionary = TObjectDictionary<string, TTags>; // Directory/.tags
  TFileTags = TPair<string, TTags>;
  TUniqueStrings = THashSet<string>;

  { TFilesTags }

  TFilesTags = class(TFilesTagsDictionary)
  public type
    TSaveOption = (soCompact, soRelative);
    TSaveOptions = set of TSaveOption;
  private
    FFilenames :TStringArray; // Contains the Filenames in the order they where added
    FTagKeys :TStringArray;
    // One call for two sources: .tags files or .images file.
    // 1. If .tags, then load with LoadFromTagsFile with a given image file list
    // 2. If .images, then load with LoadFromImagesFile with a given .image file (Filenames will be created from this file)
    procedure LoadFromFile(const Filename :string; ImplicitFilenames :boolean);
  public
    procedure Clear; override;
    procedure Prepare(const Filenames :TStringArray);
    procedure LoadFromTags;
    procedure SaveToFile(const LstFilename :string; const TagKeys :TStringArray; Options :TSaveOptions = []);
    procedure SaveToImagesFile(const ImagesFilename :string; const TagKeys :TStringArray; Size :integer);
    procedure LoadFromImagesFile(const ImagesFilename :string);
    property TagKeys :TStringArray read FTagKeys;
    property Filenames :TStringArray read FFilenames;
  end;

resourcestring
  SErrInvalidTagsFileFmt  = 'Invalid .tags file ''%s'' - %s.';
  SErrInvalidLineFmt      = 'Invalid .tags file ''%s'' line %d - %s.';
  SErrEmpty               = 'file is empty';
  SErrFilenameNotFound    = 'tag ''Filename'' not in first column';
  SErrFilenameNotAllowed  = 'multiple ''Filename'' tags';
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

{ TFilesTags }

procedure TFilesTags.Clear;
begin
  inherited;
  FTagKeys.Clear;
  FFilenames.Clear;
end;

//procedure TFilesTags.Add(const Filename: string);
//begin
//  inherited Add(Filename, TTags.Create);
//  FFilenames.Add(Filename);
//end;
//
//procedure TFilesTags.Add(Filenames: TStrings);
//var
//  Filename :string;
//begin
//  for Filename in Filenames do
//    Add(Filename);
//end;
//
procedure TFilesTags.LoadFromFile(const Filename :string; ImplicitFilenames :boolean);
var
  Path, Filetitle :string;
  Lines :TStringList;
  ColKeys :TStringArray;
  Row :TStringArray;
  Tags :TTags;
  LastTags :TTags;
  FilenameTag :string;
  i, j :integer;
  Value :string;
  UnusedRow :boolean;
  UnusedTags :TTags;
  ItemFilename :string;

  procedure RaiseInvalid(const Reason :string; Line :integer=-1);
  begin
    if Line=-1 then
      raise Exception.CreateFmt(SErrInvalidTagsFileFmt, [Filename, Reason])
    else
      raise Exception.CreateFmt(SErrInvalidLineFmt, [Filename, Line+1, Reason]);
  end;

begin
  UnusedTags := TTags.Create;
  try
    Lines := TStringList.Create;
    try
      LastTags := nil;
      Lines.LoadFromFile(Filename, TEncoding.UTF8);
      if Lines.Count=0 then RaiseInvalid(SErrEmpty);
      // Handle header line: Filename, Tag1, Tag2, ...
      Path := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
      ColKeys := Lines[0].Split(',');
      if (Length(ColKeys)=0) then RaiseInvalid(SErrEmpty);
      RemoveQuotes(ColKeys);
      FilenameTag := Trim(ColKeys[0]);
      if not SameText(FilenameTag, TAGID_FILENAME) then RaiseInvalid(SErrFilenameNotFound, 0);
      // Update global list of all keys...
      for j:=1 to High(ColKeys) do begin
        // 'Filename' is only allowed for the first column
        if SameText(ColKeys[j], TAGID_FILENAME) then
          RaiseInvalid(SErrFilenameNotAllowed, -1);
        if not FTagKeys.Contains(ColKeys[j]) then
          FTagKeys.Add(ColKeys[j]);
      end;
      // Parse lines...
      for i:=1 to Lines.Count-1 do begin
        Row := Lines[i].Split(',', '"', '"');
        if (Length(Row)<1) then
          RaiseInvalid(SErrMissingFilename, i);
        if Length(Row)>Length(ColKeys) then
          RaiseInvalid(SErrMoreValuesThanTags, i);
        Filetitle := RemoveQuotes(Row[0]);
        ItemFilename := Path + Filetitle;
        if not ImplicitFilenames then begin
          // All rows are beeing evaluated
          UnusedRow := not TryGetValue(ItemFilename, Tags);
          if UnusedRow then Tags := UnusedTags;
        end else begin
          Tags := TTags.Create;
          FFilenames.Add(ItemFilename);
          Add(ItemFilename, Tags);
        end;
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
      Lines.Free;
    end;
  finally
    UnusedTags.Free;
  end;
end;

procedure TFilesTags.Prepare(const Filenames :TStringArray);
var
  Filename :string;
begin
  Clear;

  // Create a tags dictionary for all the files
  for Filename in Filenames do begin
    FFilenames.Add(Filename);
    Add(Filename, TTags.Create);
  end;
end;

procedure TFilesTags.LoadFromTags;
var
  Filename :string;
  Path :string;
  TagsFilename :string;
  UniqueTagsFiles :TUniqueStrings;
begin
  UniqueTagsFiles := TUniqueStrings.Create;
  try
    // Look for .tags files in every folder where a file is, but take care not to
    // load tags twice
    for Filename in Filenames do begin
      Path := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
      TagsFilename := Path + TAGS_FILETITLE;
      if not UniqueTagsFiles.Contains(TagsFilename) and FileExists(TagsFilename) then begin
        UniqueTagsFiles.Add(TagsFilename);
        LoadFromFile(TagsFilename, false);
      end;
    end;
  finally
    UniqueTagsFiles.Free;
  end;
end;

procedure TFilesTags.SaveToFile(const LstFilename: string; const TagKeys :TStringArray; Options :TSaveOptions);
var
  s :TStringList;
  Line :string;
  Filename, Key :string;
  Tags :TTags;
  Pair :TPair<string, string>;
  BasePath :string;

  function GetField(const TagKey :string) :string;
  begin
    if Tags.TryGetValue(TagKey, result) then
      result := ', '+QuotedIfComma(result)
    else
      result := ',';
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
        for Key in TagKeys do
          Line := Line + ', ' + Key;
        s.Add(Line);
        for Filename in FFilenames do begin
          Tags := self[Filename];
          if soRelative in Options then
            Line := ExtractRelativePath(BasePath, Filename)
          else
            Line := Filename;
          for Key in TagKeys do
            Line := Line + GetField(Key);
          s.Add(Line);
        end;
      end;
    end;
    s.SaveToFile(LstFilename, TEncoding.UTF8);
  finally
    s.Free;
  end;
end;

procedure TFilesTags.SaveToImagesFile(const ImagesFilename: string; const TagKeys :TStringArray; Size: integer);
var
  i :integer;
  Tags :TTags;
  s :TStringList;
  Line :string;
  BasePath :string;
  TargetFilename :string;
  Key :string;

  function GetField(const TagKey :string) :string;
  begin
    if Tags.TryGetValue(TagKey, result) then
      result := ', '+QuotedIfComma(result)
    else
      result := ',';
  end;

begin
  BasePath := ExpandFilename(ExtractFilePath(ImagesFilename));
  s := TStringList.Create;
  try
    s.WriteBOM := true;
    Line := TAGID_FILENAME;
    for Key in TagKeys do
      Line := Line + ', ' + Key;
    s.Add(Line);
    for i:=0 to FFilenames.Count-1 do if TryGetValue(FFilenames[i], Tags) and Tags.TryGetValue(IntToStr(Size), TargetFilename) then begin
      Line := ExtractRelativePath(BasePath, TargetFilename);
      for Key in TagKeys do
        Line := Line + GetField(Key);
      s.Add(Line);
    end;
    s.SaveToFile(ImagesFilename, TEncoding.UTF8);
  finally
    s.Free;
  end;
end;

procedure TFilesTags.LoadFromImagesFile(const ImagesFilename: string);
begin
  Clear;
  LoadFromFile(ImagesFilename, true);
end;

//procedure test;
//var
//  ft :TFilesTags;
//  fns :TStringArray;
//begin
//  ft := TFilesTags.Create;
//  fns.Clear;
//  fns.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcF\DSC04752.jpg');
//  fns.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcF\DSC04757.jpg');
//  fns.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcF\DSC04770.jpg');
//  try
//    ft.LoadFromTagsFiles(fns);
//    ft.SaveToFile('D:\Mf\Dev\Lazarus\ImageResize\tst\tst9\.images', ft.TagKeys, [soRelative]);
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

