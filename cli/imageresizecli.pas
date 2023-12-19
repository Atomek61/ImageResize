program imageresizecli;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Types, SysUtils, CustApp, interfaces, exifutils, tags,
  { you can add units after this } fileutil, utils, imgres, generics.collections;

const
  IMGRESCLIVER = '3.2';
  IMGRESCLICPR = 'imgres CLI '+IMGRESCLIVER+' for engine '+IMGRESVER+' (c) 2023 Jan Schirrmacher, www.atomek.de';

  INTROSTR = 'Free tool for JPEG and PNG quality resampling.';
  USAGESTR = '  Usage: imgres filename folder size {-option [param]}';

  HINTSSTR =
    '  filename            Is a single JPEG or PNG file, a text filename preceeded by @ with a list of filenames'#10+
    '                      or a wildcard with path/mask. Multiple masks are to be separated by semicolon.'#10+
    '  folder              Where to store the resulting resampled and resized files. The folder must contain'#10+
    '                      the placeholder ''%SIZE%'' when there are multiple sizes. Non-existing folders'#10+
    '                      will be created.'#10+
    '  size                Is a size in pixels or a comma-separated list of sizes, where the size refers to'#10+
    '                      the longer side of the image.'#10+
    '  {-option [param]}   Is a list of pairs of option/parameter. Some options dont have a parameter.'#10#10+
    'Options:'#10#10+
    'short long            parameter   comment'#10+
    '-----------------------------------------'#10+
    '   -j -jpgquality     1..100     A quality from 1 to 100 percent (default is 75)'#10+
    '   -p -pngcompression degree     none|fastest|default|max'#10+
    '   -f -filter         filter     Box, Linear, HalfCosine, Cosine, Bicubic, Mitchell, Spline,'#10+
    '                                 Lanczos2, Lanczos3, Lanczos4, BestQuality'#10+
    '                                 Default is Lanczos2.'#10+
    '   -r -rename         template   Rename files by a template with placeholders.'#10+
    '                                 SIZE% and %INDEX[:n[,d]]% with n=startindex, d=number of digits/auto'#10+
    '   -w -watermark      file.png   A PNG watermark file and optional a position and opacity, see example.'#10+
    '   -s -shake          0..n       shakes the image list, makes sense together with -r.'#10+
    '                                 A random seed of 0 (assumed if ommited) means an unpredictable sequence.'#10+
    '                                 A fix value will shake a list always in the same manner, '#10+
    '                                 unless the number of files doesnt change.'#10+
    '   -m -meta           sources    EXIF | TAGS | EXIF,TAGS'#10+
    '                                 EXIF loads tags from origin files, TAGS from .tags files'#10+
    '   -e -exif           taglist    writes certain metadata into the resized files.'#10+
    '                                 taglist is a list of special tagnames: "Description,Timestamp,Copyright"'#10+
    '                                 if no meta sources is defined, EXIF is assumed.'#10+
    '   -c -copyright      "text"     Writes (overrides) the EXIF or .tags copyright tag.'+#10+
    '   -l -listing        file.csv   Exports a CSV list of filenames and tags.'+#10+
    '   -t -threadcount    0..n       Number of threads to use, 0 means maximum.'#10+
    '   -x -stoponerror               Stop on error. flag: 0-false, 1-true'#10+
    '   -h -help                      Outputs this text.'#10+
    '   -q -quit                      Suppresses any message output.'#10;

  EXAMPLESTR =
    'Examples:'#10#10+
    '  imgres myimage.png \Images\res640 640'#10+
    '    resamples a single png image with the default quality.'#10#10+

    '  imgres ..\theimage.jpg C:\TEMP\res%SIZE% 480,640,800 -j 50'#10+
    '    resamples a single jpg with 3 resolutions and stores them to different folders at 50% quality'#10#10+

    '  imgres @mylist.txt img%SIZE% 640,1920 -j 1 -p max -q'#10+
    '    resamples a list of files which path/name is stored in a file, with smallest file sizes in quiet mode.'#10#10+

    '  imgres myimages\*.jpg;*.png \MyImages 640 -w "mywatermark.png?10,1,98?50"'#10+
    '    adds a watermark of 10% width, 1% from the left, 2% from the bottom, with 50% opacity.'#10#10+

    '  imgres DSC4205.jpg C:\TEMP 640 -c "(c) 1941 ACME"'#10+
    '    inserts an EXIF copyright note'#10#10+

    '  More info at www.atomek.de/imageresize/cli/index.html';

  ERRINVALIDNUMBEROFPARAMS = 'Invalid number of parameters.';
  ERRINVALIDSRCFILENAME = 'Invalid parameter srcfilename.';
  ERRINVALIDDSTFOLDER = 'Invalid parameter dstfolder.';
//  ERRMISSINGSIZE = 'For multiple sizes dstfolder must contain placeholder ''%SIZE%''.';
  ERRINVALIDSHAKESEED = 'Invalid shake seed value, 0..n expected.';

type

  { TImageResizeCli }

  TImageResizeCli = class(TCustomApplication)
  private
    procedure DoException(Sender :TObject; E :Exception);
  protected
    procedure DoRun; override;
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnProgress(Sender :TObject; Progress :single);
  public
    constructor Create(AComponent :TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TImageResizeCli }

procedure TImageResizeCli.DoException(Sender: TObject; E: Exception);
begin
  Writeln('Error: '+E.Message);
end;

procedure TImageResizeCli.DoRun;
var
  i :integer;
  OptionCount :integer;
  Param :string;
  Sizes :TSizes;
  Quiet :boolean;
  JpgQuality :integer;
  PngCompression :integer;
  Filter :string;
  Processor :TProcessor;
  DstFolder :string;
  DstFileTemplate :string;
  SrcFolder :string;
  SrcFilename :string;
  SrcFilenames :TStringList;
  Items :TStringDynArray;
  FloatParams :TSingleDynArray;
  MrkFilename :string;
  MrkSize, MrkX, MrkY :single;
  MrkAlpha :single;
  ThreadCount :integer;
  StopOnError :boolean;
  Path, Mask :string;
  Masks :TStringArray;
  Item :string;
  Shake :boolean;
  ShakeSeed :integer;
  TagsSources :TTagsSources;
  TagIDs :TTagIDs;
  Copyright :string;
  TagsReportFilename :string;

  function IncludeTrailingPathDelimiterEx(const Path :string) :string;
  begin
    if Length(Path)=0 then
      result := Path
    else
      result := IncludeTrailingPathDelimiter(Path);
  end;

begin
  OptionCount := 0;

  // parse parameters
  if (ParamCount=0) or HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // Quiet
  Quiet := HasOption('q', 'quiet');
  if Quiet then
    inc(OptionCount);

  SrcFilenames := nil;
  Processor := TProcessor.Create;
  try

    // JpgQuality
    Param := GetOptionValue('j', 'jpgquality');
    if Param<>'' then begin
      if not TryStrToInt(Param, JpgQuality) or (JpgQuality<1) or (JpgQuality>100) then
        raise Exception.CreateFmt('Invalid jpgquality ''%s'', 1..100 expected.', [Param]);
      inc(OptionCount, 2);
    end else
      JpgQuality := Processor.JpgQuality;

    // PngCompression
    Param := GetOptionValue('p', 'pngcompression');
    if Param<>'' then begin
      if not TProcessor.TryStrToPngCompression(Param, PngCompression) then
        raise Exception.CreateFmt('Invalid pngcompression ''%s'', none, fastes, default or max expected.', [Param]);
      inc(OptionCount, 2);
    end else
      PngCompression := Processor.PngCompression;

    // Filter
    Param := GetOptionValue('f', 'filter');
    if Param<>'' then begin
      Filter := Param;
      inc(OptionCount, 2);
    end else
      Filter := Processor.Filter;

    // Watermark "C:\Folder\mark%SIZE%.png:-1.0,-1.0:50.0"
    MrkFilename := '';
    MrkSize := Processor.MrkSize;
    MrkX := Processor.MrkX;
    MrkY := Processor.MrkY;
    MrkAlpha := Processor.MrkAlpha;
    Param := GetOptionValue('w', 'watermark');
    if Param<>'' then begin
      inc(OptionCount, 2);
      Items := StrToStringArray(Param, '?');
      if (Length(Items)<1) or (Length(Items)>3) then
        raise Exception.CreateFmt('Invalid number of watermark parameters ''%s''.', [Param]);
      MrkFilename := Items[0];
      if Length(Items)>1 then begin
        if not StrToSingleArray(Items[1], ',', FloatParams, FormatSettings) or (Length(FloatParams)<>3) then
          raise Exception.CreateFmt('Invalid number of watermark position parameters ''%s''.', [Items[1]]);
        if (FloatParams[0]<0.0) or (FloatParams[0]>100.0) or (FloatParams[1]<0.0) or (FloatParams[1]>100.0) or (FloatParams[2]<0.0) or (FloatParams[2]>100.0) then
          raise Exception.CreateFmt('Invalid watermark size/position ''%s''.', [Items[1]]);
        MrkSize := FloatParams[0];
        MrkX := FloatParams[1];
        MrkY := FloatParams[2];
      end;
      if Length(Items)>2 then begin
        if not TryStrToFloat(Items[2], MrkAlpha, FormatSettings) or (MrkAlpha<0.0) or (MrkAlpha>100.0) then
          raise Exception.CreateFmt('Invalid watermark alpha value ''%s''.', [Items[2]]);
      end;
    end;

    // Tagging
    TagsSources := [];
    Param := GetOptionValue('m', 'meta');
    if Param<>'' then begin
      if not TProcessor.TryStrToTagsSources(Param, TagsSources) or (TagsSources=[]) then
        raise Exception.CreateFmt('Invalid tags sources ''%s'', keyword list of "EXIF" and/or "TAGS" expected.', [Param]);
      inc(OptionCount, 2);
    end;

    // EXIF Tags
    TagIDs := nil;
    Param := GetOptionValue('e', 'exif');
    if Param<>'' then begin
      TagIDs := StrToStringArray(Param, ',');
      if (Length(TagIDs)=0) or not allSupported(TagIDs) then
        raise Exception.CreateFmt('Invalid tags ''%s'', list of special tags expected - Title, Timestamp and Copyright.', [Param]);
      inc(OptionCount, 2);
    end;

    // Copyright overwriting
    Param := GetOptionValue('c', 'copyright');
    if Param<>'' then begin
      Copyright := Param;
      inc(OptionCount, 2);
    end else
      Copyright := '';

    // TagsReport for slideshow
    Param := GetOptionValue('l', 'listing');
    if Param<>'' then begin
      TagsReportFilename := Param;
      inc(OptionCount, 2);
    end else
      TagsReportFilename := '';

    // Threading control
    Param := GetOptionValue('t', 'threadcount');
    if Param<>'' then begin
      if not TryStrToInt(Param, ThreadCount) or (ThreadCount<-1) then
        raise Exception.CreateFmt('Invalid threadcount ''%s'', -1..n expected.', [Param]);
      inc(OptionCount, 2);
    end else
      ThreadCount := 0;

    // StopOnError-Flag
    StopOnError := HasOption('x', 'stoponerror');
    if StopOnError then
      inc(OptionCount);

    // Filerenaming
    DstFileTemplate := GetOptionValue('r', 'rename');
    if DstFileTemplate<>'' then begin
      inc(OptionCount, 2);
    end else
      DstFileTemplate := '';

    // File Shaking
    Shake := HasOption('s', 'shake');
    if Shake then begin
      inc(OptionCount, 1);
      Param := GetOptionValue('s', 'shake');
      if Param<>'' then begin
        inc(OptionCount, 1);
        if not TryStrToInt(Param, ShakeSeed) or (ShakeSeed<0) then
          raise Exception.Create(ERRINVALIDSHAKESEED);
      end else
        ShakeSeed := 0;
    end else begin
      Shake := false;
      ShakeSeed := 0;
    end;

    // Check number of parameters
    if ParamCount<>3+OptionCount then
      raise Exception.Create(ERRINVALIDNUMBEROFPARAMS);

    // Required Parameters: SrcFilename, Folder, Size.
    SrcFilename := ParamStr(1);
    DstFolder := ParamStr(2);

    // Sizes
    Param := ParamStr(3);
    if not TrySizesStrToSizes(Param, Sizes) or (Length(Sizes)=0) then
      raise Exception.CreateFmt('Invalid sizes ''%s''.', [Param]);

    // Dont allow empty parameters
    if Length(SrcFilename)=0 then
      raise Exception.Create(ERRINVALIDSRCFILENAME);
    if Length(DstFolder)=0 then
      raise Exception.Create(ERRINVALIDDSTFOLDER);

    // source file list
    SrcFilenames := TStringList.Create;
    if SrcFilename[1]='@' then begin
      // Build srcfile list with
      SrcFilename := Copy(SrcFilename, 2, Length(SrcFilename)-1);
      SrcFolder := IncludeTrailingPathDelimiterEx(ExpandFilename(ExtractFilePath(SrcFilename)));
      SrcFilenames.LoadFromFile(SrcFilename);
      for i:=0 to SrcFilenames.Count-1 do
        if not IsPathAbsolute(SrcFilenames[i]) then
          SrcFilenames[i] := SrcFolder + SrcFilenames[i];
    end else if IsWildcard(SrcFilename) then begin
      Path := ExtractFilePath(SrcFilename);
      Mask := ExtractFilename(SrcFilename);
      Masks := Mask.Split(';', '"', '"');
      for Item in Masks do
        FindAllFiles(SrcFilenames, Path, Item, false);
    end else begin
      SrcFilenames.Add(SrcFilename);
    end;

    // Prepare the processor
    Processor.SrcFilenames := SrcFilenames;
    Processor.DstFolder := DstFolder;
    Processor.Sizes := SizesToSizesStr(Sizes);
    Processor.JpgQuality := JpgQuality;
    Processor.PngCompression := PngCompression;
    Processor.Filter := Filter;
    Processor.MrkFilename := MrkFilename;
    Processor.MrkSize := MrkSize;
    Processor.MrkX := MrkX;
    Processor.MrkY := MrkY;
    Processor.MrkAlpha := MrkAlpha;
    Processor.ThreadCount := ThreadCount;
    Processor.StopOnError := StopOnError;
    Processor.DstFiletemplate := DstFileTemplate;
    Processor.Shake := Shake;
    Processor.ShakeSeed := ShakeSeed;
    Processor.TagsSources := TagsSources;
    Processor.TagIDs := TagIDs;
    Processor.Copyright := Copyright;
    Processor.TagsReportFilename := TagsReportFilename;

    if not Quiet then
      Processor.OnPrint := @OnPrint;

    ///////////////////////////////////////////////////
    // Finally call the processor...
    Processor.Execute;
    ///////////////////////////////////////////////////

  finally
    Processor.Free;
    SrcFilenames.Free;
  end;

  Terminate;
end;

procedure TImageResizeCli.OnPrint(Sender: TObject; const Line: string);
begin
  WriteLn(Line);
end;

procedure TImageResizeCli.OnProgress(Sender: TObject; Progress: single);
begin
  Writeln(Format('Progress %.1f%%', [Progress*100.0]));
end;

constructor TImageResizeCli.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  OnException := @DoException;
  StopOnException := true;
end;

procedure TImageResizeCli.WriteHelp;
begin
  WriteLn(IMGRESCLICPR);
  WriteLn(INTROSTR);
  WriteLn(#10+USAGESTR+#10);
  WriteLn(HINTSSTR);
  WriteLn(EXAMPLESTR);
end;

var
  Application: TImageResizeCli;

{$R *.res}

begin
  DefaultFormatSettings.DecimalSeparator := '.';
  Application:=TImageResizeCli.Create(nil);
  Application.Title:='Image Resize';
  Application.Run;
  Application.Free;
end.

