unit tags;

{$mode Delphi}
{$modeswitch typehelpers}
{$modeSwitch advancedRecords}

// An TFilesTags dictionary contains a dictionary of tags for a list of files.
//
// With "Add" not only a file is added, it is checked, if an .tags file exists
// in its directory, and if exists, loads it with its tags for all listed files.
//
// To add tags from EXIF information, handle them later.
//
// An .tags file is an CSV file containing tags for files in that directory.
// The first line is a header defining the tags. The first tag is always
// 'Filename'. The other tags are optional. The Filename is without the path.
// Values may be enquoted by double quotes. The existance and the order of the
// tag-keys is free.
// The value for the filename is mandatory, the other values may be ommited.
// I.e. you can have 4 tag-keys in the header but less in the line with the
// values.
//
// Example:
//
//   Filename, Description, Copyright
//   DSC02405.jpg, Die Hinfahrt im Stau
//   DSC02407.jpg, "Ich verliere sie, die Hose"
//   DSC02410.jpg, Mona Lisa, (c) 2023 Mogli Maisenkaiser

interface

uses
  Classes, SysUtils, generics.collections;

const
  TAGS_FILETITLE = '.tags';

  TAGKEY_FILENAME     :string =  'Filename';
  TAGKEY_FILETITLE    :string =  'Filetitle';
  TAGKEY_DESCRIPTION  :string =  'Description';
  TAGKEY_TIMESTAMP    :string =  'Timestamp';
  TAGKEY_COPYRIGHT    :string =  'Copyright';

//  COMMON_TAGKEYS      :array[0..2] of string = ('Description', 'Timestamp', 'Copyright');

type
  TIDArray = TStringArray;

  { TIDArrayHelper }

  TIDArrayHelper = type helper for TIDArray
    procedure Add(const ID :string);
    function Contains(const ID :string) :boolean;
  end;

  TTags = TDictionary<string, string>;
  TFilesTagsDictionary = TObjectDictionary<string, TTags>; // Directory/.tags
  TTagsFileDictionary = TDictionary<string, string>;

  { TFilesTags }

  TFilesTags = class(TFilesTagsDictionary)
  public type
    TFileFormat = (ffCompact, ffCSV);
  private
    FFilenames :TStringList;
    FTagIDs :TStringList;
    FTagsFiles :TTagsFileDictionary;
    function GetFilename(Index : integer): string;
    function GetFilenameCount: integer;
    function GetTagID(Index : integer): string;
    function GetTagIDCount: integer;
    function GetTag(const Filename, Key : string): string;
  public
    class var FormatSettings :TFormatSettings;
    constructor Create;
    destructor Destroy; override;
    function Add(const Filename :string) :TTags; overload;
    procedure Add(Filenames :TStrings); overload;
    procedure AddOrSetValue(const Filename, Key, Value :string); overload;
    procedure Add(const Filename :string; TagsDict :TTags); overload;
    procedure Merge(const Filename :string; TagsDict :TTags); // Adds Tags, if not exist
    function TryTagsOf(const Filename :string; out Tags :TTags) :boolean;
    procedure SaveToFile(const LstFilename :string; FileFormat :TFileFormat);
    property Filenames[Index :integer] :string read GetFilename;
    property FilenameCount :integer read GetFilenameCount;
    property TagIDs[Index :integer] :string read GetTagID;
    property TagIDCount :integer read GetTagIDCount;
    property Tag[const Filename, Key :string] :string read GetTag;
  end;

resourcestring
  SErrFilenameTagNotFoundFmt    = 'First column must be named ''Filename'' in ''%s''.';
  SErrFmtMoreValuesThanTagsFmt  = 'More values than tags in ''%s''.';
  SErrFmtMissingFilenameFmt     = 'Missing filename in ''%0:s'' line %1:d';
  SErrNoDictForFileFoundFmt     = 'No tags defined for ''%s''.';
  SErrNoTagForKeyFmt            = 'No tag ''%1:s'' found for ''%0:s'',';

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

{ TIDArrayHelper }

procedure TIDArrayHelper.Add(const ID: string);
begin
  if Contains(ID) then Exit;
  SetLength(self, Length(self)+1);
  self[High(self)] := ID;
end;

function TIDArrayHelper.Contains(const ID: string): boolean;
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
  FFilenames.Sorted := true;
  FFilenames.Duplicates := dupError;
  FTagIDs := TStringList.Create;
  FTagIDs.Sorted := true;
  FTagIDs.Duplicates := dupIgnore;
  FTagsFiles := TTagsFileDictionary.Create;
end;

destructor TFilesTags.Destroy;
begin
  FFilenames.Free;
  FTagsFiles.Free;
  FTagIDs.Free;
  inherited Destroy;
end;

function TFilesTags.GetTag(const Filename, Key : string): string;
var
  TagsDict :TTags;
begin
  if not TryGetValue(Filename, TagsDict) then
    raise Exception.CreateFmt(SErrNoDictForFileFoundFmt, [Filename]);
  if not TagsDict.TryGetValue(Key, result) then
    raise Exception.CreateFmt(SErrNoTagForKeyFmt, [Filename, Key]);
end;

function TFilesTags.GetFilename(Index : integer): string;
begin
  result := FFilenames[Index];
end;

//function TFilesTags.GetCommonMetaData(const Filename: string): TCommonTags;
//var
//  TagsDict :TTags;
//  Key :string;
//  Value :string;
//begin
//  result.Clear;
//  if TryTagsOf(Filename, TagsDict) then begin
//    for Key in COMMON_TAGKEYS do
//      if TagsDict.TryGetValue(Key, Value) then result.Description := Value;
//  end;
//end;
//
function TFilesTags.GetFilenameCount: integer;
begin
  result := FFilenames.Count;
end;

function TFilesTags.GetTagID(Index: integer): string;
begin
  result := FTagIDs[Index];
end;

function TFilesTags.GetTagIDCount: integer;
begin
  result := FTagIDs.Count;
end;

// Checks for a file, if a Tag-Dictionary exists, if not one is created and
// checked, if a file named .tmgtags exists in its directory. If so, all the
// tags are loaded for all files, mentioned in the imgtags file.
function TFilesTags.Add(const Filename: string) :TTags;
var
  Path :string;
  Filetitle :string;
  FilenameTag :string;
  TagsDict :TTags;
  ImgtagsFilename :string;
  Table :TStringlist;
  ColKeys :TStringArray;
  Row :TStringArray;
  i, j :integer;

  procedure AddNew(const Filename :string);
  begin
    FFilenames.Add(Filename);
    Add(Filename, TTags.Create);
  end;

begin
  result := nil;
  Table := nil;

  Path := ExtractFilepath(Filename);
  ImgtagsFilename := Path + TAGS_FILETITLE;

  // Check, if .tags file is to be evaluated...
  if not ContainsKey(Filename) then begin
    if FTagsFiles.ContainsKey(ImgtagsFilename) then begin
      // .tags file already evaluated but File was not described there
      AddNew(Filename);
    end else if FileExists(ImgtagsFilename) then begin
      // Parse .tags file and load all TagsDicts for the files described in it
      FTagsFiles.Add(ImgtagsFilename, '');
      Table := TStringList.Create;
      try
        Table.LoadFromFile(ImgtagsFilename);
        // Die erste Zeile enthält die Spaltennamen. Die erste Spalte muss
        // 'Filename' sein und die Dateititel enthalten
        ColKeys := Table[0].Split(',');
        RemoveQuotes(ColKeys);
        FilenameTag := Trim(ColKeys[0]);
        if not SameText(FilenameTag, TAGKEY_FILENAME) then
          raise Exception.CreateFmt(SErrFilenameTagNotFoundFmt, [ImgtagsFilename]);
        for i:=1 to table.Count-1 do begin
          // Für jede Zeile wird ein Dict angelegt
          TagsDict := TTags.Create;
          try
            Row := Table[i].Split(',', '"', '"');
            RemoveQuotes(Row);
            if Length(Row)>Length(ColKeys) then
              raise Exception.CreateFmt(SErrFmtMoreValuesThanTagsFmt, [ImgtagsFilename]);
            if (Length(Row)<1) then
              raise Exception.CreateFmt(SErrFmtMissingFilenameFmt, [ImgtagsFilename, i+1]);
            Filetitle := Trim(Row[0]);
            if Length(Filetitle)=0 then
              raise Exception.CreateFmt(SErrFmtMissingFilenameFmt, [ImgtagsFilename, i+1]);
            for j:=1 to High(ColKeys) do begin
              if j>=Length(Row) then break;
              TagsDict.Add(ColKeys[j], Row[j]);
              FTagIDs.Add(ColKeys[j]);
            end;
          except
            TagsDict.Free;
            raise;
          end;
          Add(Path+Filetitle, TagsDict);
          FFilenames.Add(Path+Filetitle);
        end;
      finally
        Table.Free
      end;
      // Now, check again if the file has TagsDict - if not, add an empty TagsDict
      if not ContainsKey(Filename) then begin
        AddNew(Filename);
      end;
    end;
  end;
end;

procedure TFilesTags.Add(Filenames: TStrings);
var
  i :integer;
begin
  for i:=0 to Filenames.Count-1 do
    Add(Filenames[i]);
end;

procedure TFilesTags.AddOrSetValue(const Filename, Key, Value: string);
var
  Dict :TTags;
begin
  Dict := self[Filename];
  Dict.AddOrSetValue(Key, Value);
end;

procedure TFilesTags.Add(const Filename: string; TagsDict: TTags);
begin
  inherited Add(Filename, TagsDict);
end;

procedure TFilesTags.Merge(const Filename: string; TagsDict: TTags);
var
  Tags :TTags;
  Pair :TPair<string, string>;
begin
  if not TryGetValue(Filename, Tags) then
    Add(Filename, TagsDict)
  else
    for Pair in TagsDict do
      if not Tags.ContainsKey(Pair.Key) then
        Tags.Add(Pair);
end;

function TFilesTags.TryTagsOf(const Filename: string; out Tags: TTags): boolean;
begin
  result := TryGetValue(Filename, Tags);
end;

procedure TFilesTags.SaveToFile(const LstFilename: string; FileFormat :TFileFormat);
var
  s :TStringList;
  Filename :string;
  Line :string;
  TagsDict :TTags;
  FilePair :TPair<string, TTags>;
  Pair :TPair<string, string>;
  i,j :integer;
  Value :string;
begin
  s := TStringList.Create;
  case FileFormat of
  ffCompact:
    for FilePair in self do begin
      Line := ExtractFilename(FilePair.Key)+': ';
      for Pair in FilePair.Value do
        Line := Line + Pair.Key + '=' + Pair.Value+', ';
      s.Add(Line);
    end;
  ffCSV:
    begin
      Line := 'Filename';
      for i:=0 to FTagIDs.Count-1 do
        Line := Line + ',' + FTagIDs[i];
      s.Add(Line);
      for i:=0 to FFilenames.Count-1 do begin
        Line := QuotedIfComma(FFilenames[i]);
        TagsDict := self[Filenames[i]];
        for j:=0 to FTagIDs.Count-1 do begin
          if TagsDict.TryGetValue(FTagIDs[j], Value) then
            Line := Line + ',' + QuotedIfComma(Value)
          else
            Line := Line + ','
        end;
        s.Add(Line);
      end;
    end;
  end;
  s.SaveToFile(LstFilename);
  s.Free;
end;

initialization
begin
  GetLocaleFormatSettings($0409, TFilesTags.FormatSettings);
end;

end.

