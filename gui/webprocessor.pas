unit webprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, webprocessorparamsfrm;

type
  TWebProcessor = class;
  TWebProcessorClass = class of TWebProcessor;
  TWebProcessors = class;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;

  { TWebProcessor }

  TWebProcessor = class
  private
    FId :string;              // "slideshow200"
    FTitle :string;           // "Slideshow 2.0"
    FDescription :string;     // "Full-Screen Slideshow"
    FLongDescription :string;         // "Displays a single image and navigates through a list.
    FDate :TDateTime;
    FIconFile :string;
    FIcon :TPicture;
    FPreviewFile :string;
    FPreview :TPicture;
    FProcessorFolder :string;
    FTargetFolder :string;
    FParamsFrame :TWebProcessorParamsFrame;
    function GetIcon: TGraphic;
    function GetPreview: TGraphic;
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    destructor Destroy; override;
    class procedure Register(ProcessorClass :TWebProcessorClass);
    class function Create(const Filename :string) :TWebProcessor; overload;
    class function Scan(const Folder :string) :TWebProcessors;
    procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute;
    function GetParamsFrame :TWebProcessorParamsFrame;
    property Id :string read FId;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property LongDescription :string read FLongDescription;
    property Date :TDateTime read FDate;
    property Icon :TGraphic read GetIcon;
    property Preview :TGraphic read GetPreview;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
  end;

  { TWebProcessors }

  TWebProcessors = class(TObjectList<TWebProcessor>)
  private
    FDictionary :TDictionary<string, TWebProcessor>;
    function GetById(Id : string): TWebProcessor;
  protected
    procedure Notify(constref AValue: TWebProcessor; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortByDate;
    property ById[Id :string] :TWebProcessor read GetById;
  end;

implementation

uses
  FileUtil;

var
  WebProcessorClasses :TDictionary<string, TWebProcessorClass>;

const
  COMMON_SECTION    = 'Common';
  PROCESSOR_SECTION = 'Processor';
  WPREXT = 'wpr';

resourcestring
  SErrUnregisterWebProcessorFmt = 'Unregistered WebProcessor class ''%s''.';

{ TWebProcessor }

constructor TWebProcessor.Create(IniFile :TCustomIniFile);
var
  Lang, FallbackLang :string;

  function IniRead(const Key :string) :string;
  begin
    result := IniFile.ReadString(PROCESSOR_SECTION, Format('%s.%s', [Key, FallbackLang]), '');
    if result = '' then
      result := IniFile.ReadString(PROCESSOR_SECTION, Key, '');
  end;

begin
  inherited Create;
  GetLanguageIDs(Lang, FallBackLang);
  FId := ChangeFileExt(ExtractFilename(IniFile.Filename), '');
  FTitle            := IniRead('Title');
  FDescription      := IniRead('Description');
  FLongDescription  := IniRead('LongDescription');
  FProcessorFolder  := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate := IniFile.ReadDateTime(PROCESSOR_SECTION, 'Date', 0.0);
  FIconFile := IniFile.ReadString(PROCESSOR_SECTION, 'Icon', '');
  FPreviewFile := IniFile.ReadString(PROCESSOR_SECTION, 'Preview', '');
end;

destructor TWebProcessor.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
  inherited Destroy;
end;

function TWebProcessor.GetIcon: TGraphic;
begin
  if not Assigned(FIcon) and (FIconFile<>'') then begin
    FIcon := TPicture.Create;
    FIcon.LoadFromFile(FProcessorFolder+FIconFile);
  end;
  result := FIcon.Graphic;
end;

function TWebProcessor.GetPreview: TGraphic;
begin
  if not Assigned(FPreview) and (FPreviewFile<>'') then begin
    FPreview := TPicture.Create;
    FPreview.LoadFromFile(FProcessorFolder+FPreviewFile);
  end;
  result := FPreview.Graphic;
end;

class procedure TWebProcessor.Register(ProcessorClass: TWebProcessorClass);
begin
  // Assumes TIdWebProcessor
  WebProcessorClasses.Add(ProcessorClass.Classname, ProcessorClass);
end;

class function TWebProcessor.Create(const Filename :string): TWebProcessor;
var
  IniFile :TCustomIniFile;
  WebProcessorClass :TWebProcessorClass;
  ClassName :string;
begin
  IniFile := TIniFile.Create(Filename, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]);
  try
    ClassName := Format('T%sWebProcessor', [IniFile.ReadString(PROCESSOR_SECTION, 'Class', '')]);
    if not WebProcessorClasses.TryGetValue(ClassName, WebProcessorClass) then
      raise Exception.CreateFmt(SErrUnregisterWebProcessorFmt, [ClassName]);
    result := WebProcessorClass.Create(IniFile);
  except
    IniFile.Free;
    raise;
  end;
end;

class function TWebProcessor.Scan(const Folder: string): TWebProcessors;
var
  WprFilenames :TStringList;
  Filename :string;
  WebProcessor :TWebProcessor;
begin
  result := TWebProcessors.Create;
  try
    WprFilenames := FindAllFiles(Folder, '*.'+WPREXT, true);
    try
      for Filename in WprFilenames do begin
        WebProcessor := TWebProcessor.Create(Filename);
        result.Add(WebProcessor);
      end;
      result.SortByDate;
    finally
      WprFilenames.Free;
    end;
  except
    result.Free;
    raise;
  end;
end;

procedure TWebProcessor.Execute;
begin

end;

function TWebProcessor.GetParamsFrame: TWebProcessorParamsFrame;
begin
  if not Assigned(FParamsFrame) then begin
    FParamsFrame := TWebProcessorParamsFrame.Create(nil);
    FParamsFrame.Name := 'ParamsFrame'+Id;
  end;
  result := FParamsFrame;
end;

{ TWebProcessors }

function TWebProcessors.GetById(Id : string): TWebProcessor;
begin
  result := FDictionary[Id];
end;

procedure TWebProcessors.Notify(constref AValue: TWebProcessor;
  ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FDictionary) then begin
    case ACollectionNotification of
    cnAdded:
      FDictionary.Add(AValue.Id, AValue);
    cnRemoved, cnExtracted:
      FDictionary.Remove(AValue.Id);
    end;
  end;
  inherited Notify(AValue, ACollectionNotification);
end;

constructor TWebProcessors.Create;
begin
  inherited Create(false);
  FDictionary := TDictionary<string, TWebProcessor>.Create;
end;

destructor TWebProcessors.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

//function CompareDateTime(constref Left, Right :TDateTime) :integer;
//begin
//  result := CompareDateTime(Left
//end;
//

function CompareDate(constref Left, Right :TWebProcessor) :integer;
begin
  result := CompareDateTime(Left.FDate, Right.FDate);
end;

procedure TWebProcessors.SortByDate;
begin
  Sort(TComparer<TWebProcessor>.Construct(@CompareDate));
end;

initialization
begin
  WebProcessorClasses := TDictionary<string, TWebProcessorClass>.Create;
  TWebProcessor.Register(TWebProcessor);
end;

finalization
begin
  WebProcessorClasses.Free;
end;

end.

