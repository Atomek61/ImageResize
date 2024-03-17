program imageresizecli;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Types, SysUtils, CustApp, interfaces, exifutils, tags, logging,
  { you can add units after this } fileutil, utils, imgres, generics.collections,
  Windows;

const
  IMGRESCLIVER = '4.1';
  IMGRESCLICPR = 'imgres CLI '+IMGRESCLIVER+' for engine '+IMGRESVER+' (c) 2024 Jan Schirrmacher, www.atomek.de';

  ERRINVALIDNUMBEROFPARAMS = 'Invalid number of parameters.';
  ERRINVALIDSRCFILENAME = 'Invalid parameter srcfilename.';
  ERRINVALIDDSTFOLDER = 'Invalid parameter dstfolder.';
  ERRINVALIDSHAKESEED = 'Invalid shuffle seed value, 0..n expected.';
  ERRNOSRCFILES = 'No source files found.';

type

  { TImageResizeCli }

  TImageResizeCli = class(TCustomApplication)
  private
    procedure DoException(Sender :TObject; E :Exception);
  protected
    procedure DoRun; override;
    procedure OnPrint(Sender :TObject; const Line :string; Level :TLogLevel);
    procedure OnProgress(Sender :TObject; Progress :single);
  public
    constructor Create(AComponent :TComponent); override;
  end;

function LoadStringFromResource(const ResName :string) :string;
var
  s :TResourceStream;
begin
  result := '';
  s := TResourceStream.Create(HInstance, ResName, Windows.RT_RCDATA);
  try
    SetLength(result, s.Size);
    s.Read(PChar(result)^, s.Size);
  finally
    s.Free;
  end;
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
  JPEGQuality :integer;
  PNGCompression :integer;
  Interpolation :TInterpolation;
  TargetFolder :string;
  TargetFileTemplate :string;
  SizeNames :TStringArray;
  SourceFolder :string;
  SourceFilename :string;
  SourceFilenames :TStringList;
  MrkParams :TProcessor.TWatermarkParams;
  ThreadCount :integer;
  StopOnError :boolean;
  Path, MaskStr :string;
  Masks :TStringArray;
  Mask :string;
  Shuffle :boolean;
  ShuffleSeed :integer;
  TagsSources :TTagsSources;
  TagKeys :TStringArray;
  Copyright :string;
  TagsReports :TTagsReports;
  DryRun :boolean;
  Processor :TProcessor;

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
    WriteLn(IMGRESCLICPR);
    WriteLn(LoadStringFromResource('CLIHELP'));
    Terminate;
    Exit;
  end;

  // Quiet
  Quiet := HasOption('q', 'quiet');
  if Quiet then
    inc(OptionCount);

  SourceFilenames := nil;
  Processor := TProcessor.Create;
  try

    // JPEGQuality
    Param := GetOptionValue('j', 'JPEG');
    if Param<>'' then begin
      if not TryStrToInt(Param, JPEGQuality) or (JPEGQuality<1) or (JPEGQuality>100) then
        raise Exception.CreateFmt(SErrInvalidJPEGQualityFmt, [Param]);
      inc(OptionCount, 2);
    end else
      JPEGQuality := Processor.JPEGQuality;

    // PNGCompression
    Param := GetOptionValue('p', 'PNG');
    if Param<>'' then begin
      if not TProcessor.TryNameToPNGCompression(Param, PNGCompression) then
        raise Exception.CreateFmt(SErrInvalidPNGCompressionNameFmt, [Param]);
      inc(OptionCount, 2);
    end else
      PNGCompression := Processor.PNGCompression;

    // Filter
    Param := GetOptionValue('i', 'interpolation');
    if Param<>'' then begin
      Interpolation := TProcessor.NameToInterpolation(Param);
      inc(OptionCount, 2);
    end else
      Interpolation := Processor.Interpolation;

    // Watermark "C:\Folder\mark{SIZE}.png:-1.0,-1.0:50.0"
    MrkParams.Filename := '';
    with Processor.WatermarkParams do begin
      MrkParams.Size := Size;
      MrkParams.X := X;
      MrkParams.Y := Y;
      MrkParams.Opacity := Opacity;
    end;
    Param := GetOptionValue('w', 'watermark');
    if Param<>'' then begin
      inc(OptionCount, 2);
      TProcessor.StrToWatermarkParams(Param, MrkParams);
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
    TagKeys := nil;
    Param := GetOptionValue('e', 'exif');
    if Param<>'' then begin
      TagKeys := StrToStringArray(Param, ',');
      if (Length(TagKeys)=0) or not allSupported(TagKeys) then
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

    // TagsReporting
    Param := GetOptionValue('l', 'listing');
    if Param<>'' then begin
      if not TProcessor.TryStrToTagsReports(Param, TagsReports) or (TagsReports=[]) then
        raise Exception.CreateFmt('Invalid tags reports ''%s'', keyword list of "TagsReport" and/or "Images" expected.', [Param]);
      inc(OptionCount, 2);
    end else
      TagsReports := [];

    // Threading control
    Param := GetOptionValue('t', 'threads');
    if Param<>'' then begin
      if not TryStrToInt(Param, ThreadCount) or (ThreadCount<-1) then
        raise Exception.CreateFmt('Invalid threads number ''%s'', -1..n expected.', [Param]);
      inc(OptionCount, 2);
    end else
      ThreadCount := 0;

    // StopOnError-Flag
    StopOnError := HasOption('x', 'stoponerror');
    if StopOnError then
      inc(OptionCount);

    // Filerenaming
    TargetFileTemplate := GetOptionValue('r', 'rename');
    if TargetFileTemplate<>'' then begin
      inc(OptionCount, 2);
    end else
      TargetFileTemplate := '';

    // SizeNames
    Param := GetOptionValue('n', 'sizenames');
    if Param<>'' then begin
      SizeNames := StrToStringArray(Param);
      inc(OptionCount, 2);
    end else
      SizeNames := nil;

    // File Shaking
    Shuffle := HasOption('s', 'shuffle');
    if Shuffle then begin
      inc(OptionCount, 1);
      Param := GetOptionValue('s', 'shuffle');
      if Param<>'' then begin
        inc(OptionCount, 1);
        if not TryStrToInt(Param, ShuffleSeed) or (ShuffleSeed<0) then
          raise Exception.Create(ERRINVALIDSHAKESEED);
      end else
        ShuffleSeed := 0;
    end else begin
      Shuffle := false;
      ShuffleSeed := 0;
    end;

    // DryRun flag
    DryRun := HasOption('d', 'dryrun');
    if DryRun then
      inc(OptionCount, 1);

    // Check number of parameters
    if ParamCount<>3+OptionCount then
      raise Exception.Create(ERRINVALIDNUMBEROFPARAMS);

    // Required Parameters: SourceFilename, Folder, Size.
    SourceFilename := ParamStr(1);
    TargetFolder := ParamStr(2);

    // Sizes
    Param := ParamStr(3);
    if not TryStrToSizes(Param, Sizes) or (Length(Sizes)=0) then
      raise Exception.CreateFmt('Invalid sizes ''%s''.', [Param]);

    // Dont allow empty parameters
    if Length(SourceFilename)=0 then
      raise Exception.Create(ERRINVALIDSRCFILENAME);
    if Length(TargetFolder)=0 then
      raise Exception.Create(ERRINVALIDDSTFOLDER);

    // source file list
    SourceFilenames := TStringList.Create;
    if SourceFilename[1]='@' then begin
      // Build srcfile list with
      SourceFilename := Copy(SourceFilename, 2, Length(SourceFilename)-1);
      SourceFolder := IncludeTrailingPathDelimiterEx(ExpandFilename(ExtractFilePath(SourceFilename)));
      SourceFilenames.LoadFromFile(SourceFilename);
      for i:=0 to SourceFilenames.Count-1 do
        if not IsPathAbsolute(SourceFilenames[i]) then
          SourceFilenames[i] := SourceFolder + SourceFilenames[i];
    end else if IsWildcard(SourceFilename) then begin
      Path := ExtractFilePath(SourceFilename);
      MaskStr := ExtractFilename(SourceFilename);
      Masks := MaskStr.Split(';', '"', '"');
      for Mask in Masks do
        FindAllFiles(SourceFilenames, Path, Mask, false);
    end else begin
      SourceFilenames.Add(SourceFilename);
    end;
    if SourceFilenames.Count=0 then
      raise Exception.Create(ERRNOSRCFILES);

    // Prepare the processor
    Processor.SourceFilenames := SourceFilenames;
    Processor.TargetFolder := TargetFolder;
    Processor.Sizes := Sizes;
    Processor.SizeNames := SizeNames;
    Processor.JPEGQuality := JPEGQuality;
    Processor.PNGCompression := PNGCompression;
    Processor.Interpolation := Interpolation;
    Processor.WatermarkParams := MrkParams;
    Processor.ThreadCount := ThreadCount;
    Processor.StopOnError := StopOnError;
    Processor.TargetFiletemplate := TargetFileTemplate;
    Processor.Sizenames := SizeNames;
    Processor.Shuffle := Shuffle;
    Processor.ShuffleSeed := ShuffleSeed;
    Processor.TagsSources := TagsSources;
    Processor.TagKeys := TagKeys;
    Processor.Copyright := Copyright;
    Processor.TagsReports := TagsReports;
    Processor.DryRun := DryRun;

    if not Quiet then
      Processor.OnPrint := OnPrint;

    ///////////////////////////////////////////////////
    // Finally call the processor...
    Processor.Execute;
    ///////////////////////////////////////////////////

  finally
    Processor.Free;
    SourceFilenames.Free;
  end;

  Terminate;
end;

procedure TImageResizeCli.OnPrint(Sender: TObject; const Line: string; Level :TLogLevel);
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
  OnException := DoException;
  StopOnException := true;
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

