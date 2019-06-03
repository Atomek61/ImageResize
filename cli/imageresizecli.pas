program imageresizecli;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Types, SysUtils, CustApp, imgres,
  { you can add units after this } fileutil, utils;

const
  IMGRESCLIVER = '1.2';
  IMGRESCLICPR = 'imgres CLI V'+IMGRESCLIVER+' (c) 2019 Jan Schirrmacher, www.atomek.de';

  INTROSTR = 'Free tool for jpg and png quality file resizing.';
  USAGESTR = '  Usage: imgres (srcfilename|@srcfilelist) dstfolder [-s size[,size,..] [-j jpgquality]  [-p pngcompression] [-w watermark] [-h] [-q]';
  HINTSSTR =
    '  s size           - refers to the longer side of the image, size can be a single value or a list of comma-separated values'#10+
    '  j jpgquality     - is a quality from 1 to 100 percent (default is 75)'#10+
    '  p pngcompression - is one of none,fastest,default and max'#10+
    '  w watermark      - a watermark file and optional a position and opacity, see example'#10+
    '  h help           - outputs this text'#10+
    '  q quiet          - suppresses any message output'#10#10+
    '  dstfile must contain the placeholder ''%SIZE%'' when size is a list of sizes.'#10+
    '  A srcfilelist preceeded by ''@'' refers to a file containing a list of source files.'#10+
    '  The srcfilelist may contain relative pathes - they refer to the srcfilelist location.'#10+
    '  Non-existing folders will be created.'#10;
  EXAMPLESTR =
    'Examples:'#10+'  imgres myimage.png \Images\res640 -s 640'#10+
    '  resamples a single png image with the default quality.'#10#10+

    '  imgres ..\theimage.jpg C:\TEMP\res%SIZE% -s 480,640,800 -j 50'#10+
    '  resamples a single jpg with 3 different resolutions and stores them to different folders with 50% quality'#10#10+

    '  imgres @mylist.txt img%SIZE% -s 640,1920 -j 1 -p max -q'#10+
    '  resamples a list of files which path/name is stored in a file with smallest file sizes in quiet mode.'#10#10+

    '  imgres myfile.jpg /MyImages -s 640 -w "mywatermark.png?10,1,98?50"'#10+
    '  adds a watermark of 10% width, 1% from the left, 2% from the bottom, with 50% opacity.'#10;
  ERRINVALIDNUMBEROFPARAMS = 'Invalid number of parameters.';
  ERRINVALIDSRCFILENAME = 'Invalid parameter srcfilename.';
  ERRINVALIDDSTFOLDER = 'Invalid parameter dstfolder.';
  ERRMISSINGSIZE = 'For multiple sizes dstfolder must contain placeholder ''%SIZE%''.';

type

  { TImageResizeCli }

  TImageResizeCli = class(TCustomApplication)
  private
    procedure DoException(Sender :TObject; E :Exception);
  protected
    procedure DoRun; override;
    procedure OnPrint(Sender :TObject; const Line :string);
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
  OptionCount :integer;
  Param :string;
  Sizes :TIntegerDynArray;
  Quiet :boolean;
  JpgQuality :integer;
  PngCompression :integer;
  Processor :TImgRes;
  DstFolder :string;
  SrcFolder :string;
  SrcFilename :string;
  SrcFilenames :TStringList;
  i :integer;
  Items :TStringDynArray;
  FloatParams :TSingleDynArray;
  MrkFilename :string;
  MrkSize, MrkX, MrkY :single;
  MrkAlpha :single;

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
  Processor := TImgRes.Create;
  try

    // Size
    Param := GetOptionValue('s', 'size');
    if Param<>'' then begin
      if not SizesStrToSizes(Param, Sizes) or (Length(Sizes)=0) then
        raise Exception.CreateFmt('Invalid sizes ''%s''.', [Param]);
      inc(OptionCount, 2);
    end else begin
      SetLength(Sizes, 1);
      Sizes[0] := Processor.Size;
    end;

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
      if not TImgRes.TryStrToPngCompression(Param, PngCompression) then
        raise Exception.CreateFmt('Invalid pngcompression ''%s'', none, fastes, default or max expected.', [Param]);
      inc(OptionCount, 2);
    end else
      PngCompression := Processor.PngCompression;

    // Watermark "C:\Folder\mark%SIZE%.png:-1.0,-1.0:50.0"
    Param := GetOptionValue('w', 'watermark');
    if Param<>'' then begin
      inc(OptionCount, 2);
      if not StrToStringArray(Param, '?', Items) or (Length(Items)>3) then
        raise Exception.CreateFmt('Invalid number of watermark parameters ''%s''.', [Param]);
      MrkFilename := Items[0];
      MrkSize := Processor.WatermarkSize;
      MrkX := Processor.WatermarkX;
      MrkY := Processor.WatermarkY;
      MrkAlpha := Processor.WatermarkAlpha;
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

    // Check number of parameters
    if ParamCount<>2+OptionCount then
      raise Exception.Create(ERRINVALIDNUMBEROFPARAMS);

    // Required Parameters: SrcFilename and Folder.
    SrcFilename := ParamStr(1);
    DstFolder := ParamStr(2);

    // Check if multiple sizes are given and placeholder %SIZE% is not set in DstFolder
    if (Length(Sizes)>1) and not (Pos('%SIZE%', DstFolder)>0) then
      raise Exception.Create(ERRMISSINGSIZE);

    // Dont allow empty parameters
    if Length(SrcFilename)=0 then
      raise Exception.Create(ERRINVALIDSRCFILENAME);
    if Length(DstFolder)=0 then
      raise Exception.Create(ERRINVALIDDSTFOLDER);

    // srcfilelist
    SrcFilenames := TStringList.Create;
    if SrcFilename[1]='@' then begin
      // Build srcfile list with
      SrcFilename := Copy(SrcFilename, 2, Length(SrcFilename)-1);
      SrcFolder := IncludeTrailingPathDelimiterEx(ExtractFilePath(SrcFilename));
      SrcFilenames.LoadFromFile(SrcFilename);
      for i:=0 to SrcFilenames.Count-1 do
        if not IsPathAbsolute(SrcFilenames[i]) then
          SrcFilenames[i] := SrcFolder + SrcFilenames[i];
    end else
      SrcFilenames.Add(SrcFilename);

    Processor.JpgQuality := JpgQuality;
    Processor.PngCompression := PngCompression;
    Processor.WatermarkFilename := MrkFilename;
    Processor.WatermarkSize := MrkSize;
    Processor.WatermarkX := MrkX;
    Processor.WatermarkY := MrkY;
    Processor.WatermarkAlpha := MrkAlpha;
    if not Quiet then
      Processor.OnPrint := @OnPrint;

    ///////////////////////////////////////////////////
    // Finally call the processor...
    Processor.Execute(SrcFilenames, DstFolder, Sizes);
    ///////////////////////////////////////////////////
  finally
    Processor.Free;
    SrcFilenames.Free;
  end;

  // stop program loop
  Terminate;
end;

procedure TImageResizeCli.OnPrint(Sender: TObject; const Line: string);
begin
  WriteLn(Line);
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

