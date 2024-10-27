unit Tags;

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
  Classes, SysUtils, Generics.Collections, StringArrays;

const
  TAGS_FILETITLE      = '.tags';
  FILENAME_KEY     =  'Filename';

type
  TStringDictionary = TDictionary<string, string>;

  { TTags }

  TTags = class(TStringDictionary)
  public
    procedure SaveToFile(const Filename :string);
  end; // Tags of a source file

  TFilesTagsDictionary = TObjectDictionary<string, TTags>; // Directory/.tags
  TUniqueStrings = THashSet<string>;

  { TFilesTags }

  TFilesTags = class(TFilesTagsDictionary)
  public type
    TSaveOption = (soCompact, soRelative);
    TSaveOptions = set of TSaveOption;
  private
    FFilenames :TStringArray; // Contains the Filenames in the order they where added
    FTagKeys :TStringArray;
    // One call for two purposes: .tags files or .imgtags file.
    // 1. If .tags, then load with LoadFromTagsFile with a given image file list
    // 2. If .imgtags, then load with LoadFromImgTagsFile with a given .imgtags file (Filenames will be created from this file)
    procedure LoadFromFile(const Filename :string; const ExpectedFilenameKey :string; ImplicitFilenames :boolean);
  public
    procedure Clear; override;
    procedure Prepare(const Filenames :TStringArray);
    procedure LoadFromTags;
    procedure SaveToFile(const LstFilename :string; const TagKeys :TStringArray; Options :TSaveOptions = []);
    procedure SaveToImgTagsFile(const ImgTagsFilename :string; const Keys :TStringArray; Size :integer);
    procedure SaveAllToImgTagsFile(const ImgTagsFilename :string; Size :integer);
    procedure LoadFromImgTagsFile(const ImgTagsFilename :string);
    property TagKeys :TStringArray read FTagKeys;
    property Filenames :TStringArray read FFilenames;
  end;

resourcestring
  SErrInvalidTagsFileFmt      = 'Invalid .tags file ''%s'' - %s.';
  SErrInvalidLineFmt          = 'Invalid .tags file ''%s'' line %d - %s.';
  SErrEmpty                   = 'file is empty';
  SErrFilenameKeyNotFoundFmt  = 'first Column ''%s'' not found';
  SErrFilenameNotAllowedFmt   = 'multiple ''%s'' tags not allowed';
  SErrMoreValuesThanTags      = 'more values than tags';
  SErrMissingFilename         = 'missing filename';
  SErrAsterisk                = 'first row cannot contain a *';

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

function IsAnySizePrefix(const Key :string) :boolean;
var
  i :integer;
  c :char;
begin
  for i:=1 to Length(Key) do begin
    c := Key[i];
    if (c<'0') or (c>'9') then
      Exit((i>1) and (c='.'));
  end;
  result := false;
end;

function RemoveAnySizePrefix(const Key :string) :string;
var
  i :integer;
  c :char;
begin
  for i:=1 to Length(Key) do begin
    c := Key[i];
    if (c<'0') or (c>'9') then begin
      if (i>1) and (c='.') then
        Exit(Copy(Key, i+1, Length(Key)-i))
      else
        Exit(Key);
    end;
  end;
  result := Key;
end;

{ TTags }

procedure TTags.SaveToFile(const Filename: string);
var
  s :TStringList;
  Line :string;
  KeyArray :TStringArray;
  i :integer;
begin
  s := TStringList.Create;
  try
    s.WriteBOM := true;
    KeyArray := Keys.ToArray;
    s.Add(KeyArray.Join(', '));
    Line := '';
    for i:=0 to KeyArray.Count-1 do begin
      Line := Line + self[KeyArray[i]];
      if i<KeyArray.Count-1 then
        Line := Line + ', ';
    end;
    s.Add(Line);
    s.SaveToFile(Filename, TEncoding.UTF8);
  finally
    s.Free;
  end;

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
procedure TFilesTags.LoadFromFile(const Filename :string; const ExpectedFilenameKey :string; ImplicitFilenames :boolean);
var
  Path, Filetitle :string;
  Lines :TStringList;
  ColKeys :TStringArray;
  Row :TStringArray;
  Tags :TTags;
  LastTags :TTags;
  FilenameKey :string;
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
      FilenameKey := Trim(ColKeys[0]);
      if not SameText(FilenameKey, ExpectedFilenameKey) then
        RaiseInvalid(Format(SErrFilenameKeyNotFoundFmt, [ExpectedFilenameKey]), 0);
      // Update global list of all keys...
      for j:=1 to High(ColKeys) do begin
        // 'Filename' is only allowed for the first column
        if SameText(ColKeys[j], ExpectedFilenameKey) then
          RaiseInvalid(Format(SErrFilenameNotAllowedFmt, [ExpectedFilenameKey]), -1);
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
        LoadFromFile(TagsFilename, 'Filename', false);
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
        Line := FILENAME_KEY;
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

procedure TFilesTags.SaveToImgTagsFile(const ImgTagsFilename: string; const Keys :TStringArray; Size: integer);
var
  i, j :integer;
  Tags :TTags;
  s :TStringList;
  Line :string;

  function GetField(const TagKey :string) :string;
  begin
    if Tags.TryGetValue(TagKey, result) then
      result := ', '+QuotedIfComma(result)
    else
      result := ',';
  end;

begin
  if Length(Keys)=0 then Exit;
  //BasePath := ExpandFilename(ExtractFilePath(ImgTagsFilename));
  s := TStringList.Create;
  try
    s.WriteBOM := true;
    Line := RemoveAnySizePrefix(Keys[0]);
    for i:=1 to High(Keys) do
      Line := Line + ', '+RemoveAnySizePrefix(Keys[i]);
    s.Add(Line);

    // Create a line for each Filename
    for i:=0 to FFilenames.Count-1 do if TryGetValue(FFilenames[i], Tags) then begin
      Line := Tags[Keys[0]];
      for j:=1 to High(Keys) do
        Line := Line + GetField(Keys[j]);
      s.Add(Line);
    end;
    s.SaveToFile(ImgTagsFilename, TEncoding.UTF8);
  finally
    s.Free;
  end;
end;

procedure TFilesTags.SaveAllToImgTagsFile(const ImgTagsFilename: string; Size: integer);
var
  UniqueKeys :TUniqueStrings;
  Keys :TStringArray;
  Key :string;
  i :integer;
  Tags :TTags;
  SizePrefix :string;

begin
  UniqueKeys := TUniqueStrings.Create;
  // Merge all Keys, which has not SIZE. prefix or the prefix belongs to Size
  SizePrefix := IntToStr(Size)+'.';
  for i:=0 to FFilenames.Count-1 do
    if TryGetValue(FFilenames[i], Tags) then
      for Key in Tags.Keys do
        if Key.StartsWith(SizePrefix) or not IsAnySizePrefix(Key) then
          UniqueKeys.Add(Key);
  Keys := UniqueKeys.ToArray;
  // Put Filename as first Key
  for i:=1 to High(Keys) do
    if Keys[i].EndsWith('.ImgFilename') then begin
      Key := Keys[0];
      Keys[0] := Keys[i];
      Keys[i] := Key;
      break;
    end;
  SaveToImgTagsFile(ImgTagsFilename, Keys, Size);
end;

procedure TFilesTags.LoadFromImgTagsFile(const ImgTagsFilename: string);
begin
  Clear;
  LoadFromFile(ImgTagsFilename, 'ImgFilename', true);
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

//procedure test;
//begin
//  if IsAnySizePrefix('abc') then Beep;
//  if IsAnySizePrefix('480.abc') then Beep;
//  if IsAnySizePrefix('12xa.abc') then Beep;
//  if IsAnySizePrefix('.345') then Beep;
//  if IsAnySizePrefix('48.') then Beep;
//end;
//
//procedure test;
//var
//  a :string;
//begin
//  a := RemoveAnySizePrefix('abc');
//  a := RemoveAnySizePrefix('480.abc');
//  a := RemoveAnySizePrefix('12xa.abc');
//  a := RemoveAnySizePrefix('.345');
//  a := RemoveAnySizePrefix('48.');
//end;
//
//initialization
//  test;
end.

