unit EXIFUtils;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, tags, Generics.Collections;

function ReadExifTags(const Filename :string; Tags :TTags; out TagIDs :TIDArray) :boolean;
procedure WriteExifTags(const Filename :string; Tags :TTags; const TagIDs :TIDArray);

implementation

uses
  dMetadata;

function ReadExifTags(const Filename :string; Tags :TTags; out TagIDs :TIDArray) :boolean;
var
  ImgData :TImgData;
  Timestamp :TDateTime;
  Value :string;
begin
  result := false;
  ImgData := TImgData.Create;
  SetLength(TagIDs, 0);
  try
    ImgData.ProcessFile(Filename);
    if ImgData.HasMetaData then begin
      // Description
      Value := ImgData.ExifObj.ImageDescription;
      if Value<>'' then begin
        TagIDs.Add(TAGKEY_DESCRIPTION);
        Tags.AddOrSetValue(TAGKEY_DESCRIPTION, Value);
      end;

      // Timestamp
      Timestamp := ImgData.ExifObj.DateTimeOriginal;
      if Timestamp<>0.0 then begin
        TagIDs.Add(TAGKEY_TIMESTAMP);
        Tags.AddOrSetValue(TAGKEY_TIMESTAMP, FormatDateTime('YYYY-MM-DD HH:NN:SS', Timestamp, TFilesTags.FormatSettings));
      end;

      // Copyright
      Value := ImgData.ExifObj.Copyright;
      if Value<>'' then begin
        TagIDs.Add(TAGKEY_COPYRIGHT);
        Tags.AddOrSetValue(TAGKEY_COPYRIGHT, Value);
      end;

      result := Length(TagIDs)>0;
    end;
  finally
    ImgData.Free;
  end;
end;

procedure WriteExifTags(const Filename :string; Tags :TTags; const TagIDs :TIDArray);
var
  ImgData :TImgData;
  Value :string;
begin
  //if (Length(TagIDs)=0) or not TagsDict.TryGetValue(Filename, TagsDict) then Exit;
  ImgData := TImgData.Create;
  try
    ImgData.CreateExifObj;
    if TagIDs.Contains(TAGKEY_DESCRIPTION) and Tags.TryGetValue(TAGKEY_DESCRIPTION, Value) then
      ImgData.ExifObj.ImageDescription := Value;
    if TagIDs.Contains(TAGKEY_TIMESTAMP) and Tags.TryGetValue(TAGKEY_TIMESTAMP, Value) then
      ImgData.ExifObj.DateTimeOriginal := StrToDateTime(Value);
    if TagIDs.Contains(TAGKEY_COPYRIGHT) and Tags.TryGetValue(TAGKEY_COPYRIGHT, Value) then
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

