unit EXIFUtils;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, tags, datetimeutils;

function ReadExifTags(const Filename :string; Tags :TTags; out TagIDs :TTagIDs) :boolean;
procedure WriteExifTags(const Filename :string; Tags :TTags; const TagIDs :TTagIDs);
function allSupported(const TagIDs :TTagIDs) :boolean;

implementation

uses
  dMetadata;

const
  TAGIDS_SUPPORTED   :array[0..2] of string = (TAGID_TITLE, TAGID_TIMESTAMP, TAGID_COPYRIGHT);

function allSupported(const TagIDs :TTagIDs) :boolean;
var
  i, j :integer;
begin
  result := true;
  for i:=0 to High(TagIDs) do begin
    for j:=0 to High(TAGIDS_SUPPORTED) do begin
      result := TAGIDS_SUPPORTED[j] = TagIDs[i];
      if result then break;
    end;
    if not result then Exit(false);
  end;
end;

function ReadExifTags(const Filename :string; Tags :TTags; out TagIDs :TTagIDs) :boolean;
var
  ImgData :TImgData;
  Timestamp :TDateTime;
  Value :string;
begin
  result := false;
  ImgData := TImgData.Create;
  TagIDs := nil;
  try
    ImgData.ProcessFile(Filename);
    if ImgData.HasMetaData then begin
      // Title
      Value := ImgData.ExifObj.ImageDescription;
      if Value<>'' then begin
        TagIDs.Add(TAGID_TITLE);
        Tags.AddOrSetValue(TAGID_TITLE, Value);
      end;

      // Timestamp
      Timestamp := ImgData.ExifObj.DateTimeOriginal;
      if Timestamp<>0.0 then begin
        TagIDs.Add(TAGID_TIMESTAMP);
        Tags.AddOrSetValue(TAGID_TIMESTAMP, datetimeutils.DateTimeToStr(Timestamp));
      end;

      // Copyright
      Value := ImgData.ExifObj.Copyright;
      if Value<>'' then begin
        TagIDs.Add(TAGID_COPYRIGHT);
        Tags.AddOrSetValue(TAGID_COPYRIGHT, Value);
      end;

      result := Length(TagIDs)>0;
    end;
  finally
    ImgData.Free;
  end;
end;

procedure WriteExifTags(const Filename :string; Tags :TTags; const TagIDs :TTagIDs);
var
  ImgData :TImgData;
  Value :string;
begin
  //if (Length(TagIDs)=0) or not TagsDict.TryGetValue(Filename, TagsDict) then Exit;
  ImgData := TImgData.Create;
  try
    ImgData.CreateExifObj;
    if TagIDs.Contains(TAGID_TITLE) and Tags.TryGetValue(TAGID_TITLE, Value) then
      ImgData.ExifObj.ImageDescription := Value;
    if TagIDs.Contains(TAGID_TIMESTAMP) and Tags.TryGetValue(TAGID_TIMESTAMP, Value) then
      ImgData.ExifObj.DateTimeOriginal := datetimeutils.StrToDateTime(Value);
    if TagIDs.Contains(TAGID_COPYRIGHT) and Tags.TryGetValue(TAGID_COPYRIGHT, Value) then
      ImgData.ExifObj.Copyright := Value;
    ImgData.WriteEXIFJpeg(Filename);
  finally
    ImgData.Free;
  end;
end;

//procedure Test;
//var
//  MetaData :TMetaData;
//begin
//  MetaData.Description := 'Holladiewaldfee';
//  MetaData.Timestamp := Now;
//  MetaData.Copyright := '@ 2023 Jan-Erich Rotbart';
//  WriteMetaData('D:\TEMP\BOGA23\img2560\DSC04205.jpg', MetaData);
//end;
//
//begin
//  Test;
end.

