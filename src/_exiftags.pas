unit exiftags;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, filestags;

type
  TAddExifOption = (aeOverride); // Override means, that EXIF will override .tags-based
  TAddExifOptions = set of TAddExifOption;

procedure AddExifTags(FileTags :TFileTags; Options :TAddExifOptions = []);

resourcestring
  SErrNoTagsDictFoundFmt = 'No tags found in FileTags - FileTags.Add called before?';

implementation

uses
  dMetadata;

procedure AddExifTags(FileTags :TFileTags; Options :TAddExifOptions);
var
  i :integer;
  ImgData :TImgData;
  TagsDict :TTagsDictionary;

  procedure AddTag(const Key, Value :string);
  begin
    if aeOverride in Options then begin
       TagsDict.AddOrSetValue(Key, Value);
    end else begin
      if not TagsDict.ContainsKey(Key) then
        TagsDict.Add(Key, Value);
    end;
  end;

begin
  for i:=0 to FileTags.FilenameCount-1 do begin
    ImgData := TImgData.Create;
    try
      ImgData.ProcessFile(FileTags.Filenames[i]);
      if ImgData.HasMetaData then begin
        if not FileTags.TryTagsOf(FileTags.Filenames[i], TagsDict) then
          raise Exception.CreateFmt(SErrNoTagsDictFoundFmt, [FileTags.Filenames[i]]);
        AddTag(TAGKEY_DESCRIPTION, ImgData.ExifObj.TagValueAsString['ImageDescription']);
        AddTag(TAGKEY_TIMESTAMP,   ImgData.ExifObj.TagValueAsString['DateTimeOriginal']);
        AddTag(TAGKEY_COPYRIGHT,   ImgData.ExifObj.TagValueAsString['Copyright']);
      end;
    finally
      ImgData.Free;
    end;

  end;
end;

procedure Test;
var
  d :TFileTags;
begin
  d := TFileTags.Create;
  d.Add('D:\TEMP\tst\DSC04205.jpg');
  d.Add('D:\TEMP\tst\DSC04207.jpg');

  AddExifTags(d, []);

  d.Tag['D:\TEMP\tst\DSC04207.jpg', 'Description'];




  d.Free;
end;

initialization
begin
//  Test;
end;

end.

