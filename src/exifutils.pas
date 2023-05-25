unit EXIFUtils;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils;

type

  TTag = (ttDescription, ttTimestamp, ttCopyright, ttFilename);
  TTags = set of TTag;

  { TMetaData }

  TMetaData = record
    Description :string;  // Flag $01
    Timestamp :TDateTime; // Flag $02
    Copyright :string;    // Flag $04
    Filename :string;
    procedure Clear;
  end;

function ReadMetaData(const Filename :string; out MetaData :TMetaData) :boolean;
procedure WriteMetaData(const Filename :string; const MetaData :TMetaData; Tags :TTags = [ttDescription]);

implementation

uses
  math, dMetadata;

function ReadMetaData(const Filename :string; out MetaData :TMetaData) :boolean;
var
  ImgData :TImgData;
begin
  result := false;
  ImgData := TImgData.Create;
  try
    ImgData.ProcessFile(Filename);
    if ImgData.HasMetaData then begin
      MetaData.Description  := ImgData.ExifObj.ImageDescription;
      MetaData.Timestamp    := ImgData.ExifObj.DateTimeOriginal;
      MetaData.Copyright    := ImgData.ExifObj.Copyright;
      MetaData.Filename     := ExtractFilename(Filename);
      result := true;
    end;
  finally
    ImgData.Free;
  end;
end;

procedure WriteMetaData(const Filename :string; const MetaData :TMetaData; Tags :TTags);
var
  ImgData :TImgData;
begin
  if Tags=[] then Exit;
  ImgData := TImgData.Create;
  try
    ImgData.CreateExifObj;
    if ttDescription in Tags then
      ImgData.ExifObj.ImageDescription := MetaData.Description;
    if ttTimestamp in Tags then
      ImgData.ExifObj.DateTimeOriginal := MetaData.Timestamp;
    if ttCopyright in Tags then
      ImgData.ExifObj.Copyright := MetaData.Copyright;
    ImgData.WriteEXIFJpeg(Filename);
  finally
    ImgData.Free;
  end;
end;

{ TMetaData }

procedure TMetaData.Clear;
begin
  Description := '';
  Timestamp   := NaN;
  Copyright   := '';
  Filename    := '';
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

