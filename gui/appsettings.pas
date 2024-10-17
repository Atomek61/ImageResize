unit appsettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, Settings, ImgRes;

type

  { TProjectSettings }  // Not yet in charge

  TProjectSettings = class(TSettings)
  public
    Description         :TStringSetting;
    SourceMode          :TPicklistSetting;
    SourceFolder        :TStringSetting;
    SourceMasks         :TStringSetting;
    SourceFilenames     :TStringsSetting;
    Sizes               :TStringSetting;
    SizeNames           :TStringSetting;
    TargetFolder        :TStringSetting;
    JPEGQuality         :TPicklistSetting;
    PNGCompression      :TPicklistSetting;
    ResamplingFilter    :TPickListSetting;
    MrkEnabled          :TBooleanSetting;
    MrkFilename         :TStringSetting;
    MrkSize             :TUInt32Setting;
    MrkX                :TUInt32Setting;
    MrkY                :TUInt32Setting;
    MrkOpacity          :TUInt32Setting;
    RenEnabled          :TBooleanSetting;
    RenSimple           :TBooleanSetting;
    RenMode             :TPicklistSetting;
    RenTemplate         :TStringSetting;
    Shuffle             :TBooleanSetting;
    ShuffleSeed         :TUInt32Setting;
    TagsSourceEXIF      :TBooleanSetting;
    TagsSourceTagsFiles :TBooleanSetting;
    TagTitle            :TBooleanSetting;
    TagTimestamp        :TBooleanSetting;
    TagCopyright        :TBooleanSetting;
    Copyright           :TStringSetting;
    TagsReportEnabled   :TBooleanSetting;
    ImageInfosEnabled   :TBooleanSetting;
    NoCreate            :TBooleanSetting;
    PresentationId      :TStringSetting;
    constructor Create(const ASection :string); override;
  end;


  { TProcessingSettings }

  TProcessingSettings = class(TSettings)
  public
    ThreadsUsed :TUInt32Setting;
    StopOnError :TBooleanSetting;
    constructor Create(const ASection :string); override;
  end;

  { TDialogSettings }

  TDialogSettings = class(TSettings)
  public
    AutoSave :TBooleanSetting;
    WarnDirty :TBooleanSetting;
    RelPathes :TBooleanSetting;
    constructor Create(const ASection :string); override;
  end;

  { TPresentationSettings }

  TPresentationSettings = class(TSettings)
  public
    Id :TStringSetting;
    ImgTagsFilename :TStringSetting;
    constructor Create(const ASection :string); override;
  end;

  { TSizeInfo }

  { TSizeInfos }

  TSizeInfos = class
  public type
    TChange = (lcChanged, lcInsert, lcDelete, lcInit);
    TChangedEvent = procedure(Sender :TSizeInfos; Change :TChange; Index :integer) of object;

    TSizeInfo = class
    private
      FEnabled :boolean;
      FSize :integer;
      FName :string;
    public
      property Enabled :boolean read FEnabled;
      property Size :integer read FSize;
      property Name :string read FName;
      constructor Create(Enabled :boolean; Size :integer; const Name :string);
    end;

  public const
    DEFAULT_SIZES     :array[0..14] of integer = (32, 48, 64, 120, 240, 360, 480, 640, 800, 960, 1280, 1600, 1920, 2560, 4096);
    DEFAULT_SIZENAMES :array[0..2] of string = ('_THUMBNAIL', '_DOCUMENT', '');
    SIZENAMECOLORS    :array[0..2] of longint = ($dcf5f7, $dcf7dd, $f7f0dc);
    SCREENSIZECOLOR   :longint = $ff9933;
    THUMBNAILMAX      = 240;
    DOCUMENTMAX       = 960;
    ILLGCHARS         = '{}<>[]"'',|';
  private
    FItems :TObjectList<TSizeInfo>;
    FOnChanged :TChangedEvent;
    procedure Changed(Change :TChange; Index :integer);
    function GetCount: integer;
    function GetEnabled(Index : integer): boolean;
    function GetItem(Index : integer): TSizeInfo;
    procedure SetEnabled(Index : integer; AValue: boolean);
    function ProcessSizeName(const SizeName :string; Size :integer) :string;
  public
    class function SizeToClassIndex(Size :integer) :integer;
    constructor Create;
    destructor Destroy; override;
    function TrySetSizeString(const Str :string) :boolean;
    function TrySetString(const Str :string) :boolean;
    function GetString :string;
    function GetEnabledSizes :TSizes;
    function GetEnabledSizeNames :TStringArray;
    procedure Add(Enabled: boolean; Size: integer; const SizeName: string);
    procedure Replace(Index :integer; Enabled: boolean; Size: integer; const SizeName: string);
    procedure Delete(Index :integer);
    procedure Clear;
    function Required :boolean;
    property OnChanged :TChangedEvent read FOnChanged write FOnChanged;
    property Items[Index :integer] :TSizeInfo read GetItem; default;
    property Count :integer read GetCount;
    property Enabled[Index :integer] :boolean read GetEnabled write SetEnabled;
  end;

implementation

function ReplaceIllegalChars(const Str, Illg :string; Replacement :Char) :string;
var
  i, p :integer;
begin
  result := Str;
  for i:=1 to Length(Illg) do begin
    p := 1;
    repeat
      p := Pos(Illg[i], Str, p);
      if p>0 then begin
        result[p] := Replacement;
        inc(p);
      end;
    until p=0;
  end;
end;

{ TDialogSettings }

constructor TDialogSettings.Create(const ASection :string);
begin
  inherited;
  AutoSave := TBooleanSetting.Create(self, 'AutoSave');
  AutoSave.Default := True;
  WarnDirty := TBooleanSetting.Create(self, 'WarnDirty');
  RelPathes := TBooleanSetting.Create(self, 'RelPathes');
  RelPathes.Value := true;
end;

{ TPresentationSettings }

constructor TPresentationSettings.Create(const ASection: string);
begin
  inherited Create(ASection);
  Id := TStringSetting.Create(self, 'Id');
  ImgTagsFilename := TStringSetting.Create(self, 'ImgTagsFilename');
end;

{ TSizeInfo }

constructor TSizeInfos.TSizeInfo.Create(Enabled: boolean; Size: integer; const Name: string);
begin
  FEnabled := Enabled;
  if Size>0 then FSize := Size else FSize := 1000;
  FName := ReplaceIllegalChars(Name, ILLGCHARS, '_');
end;

{ TSizeInfos }

constructor TSizeInfos.Create;
begin
  FItems := TObjectList<TSizeInfo>.Create;
end;

destructor TSizeInfos.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TSizeInfos.Changed(Change: TChange; Index: integer);
begin
  if Assigned(FOnChanged) then
    FOnChanged(self, Change, Index);
end;

function TSizeInfos.GetCount: integer;
begin
  result := FItems.Count;
end;

function TSizeInfos.GetEnabled(Index : integer): boolean;
begin
  result := FItems[Index].Enabled;
end;

function TSizeInfos.GetItem(Index : integer): TSizeInfo;
begin
  result := FItems[Index];
end;

procedure TSizeInfos.SetEnabled(Index : integer; AValue: boolean);
begin
  if FItems[Index].FEnabled<>AValue then begin
    FItems[Index].FEnabled := AValue;
    Changed(lcChanged, Index);
  end;
end;

class function TSizeInfos.SizeToClassIndex(Size: integer): integer;
begin
  if Size<=THUMBNAILMAX then Exit(0);
  if Size<DOCUMENTMAX then Exit(1);
  result := 2;
end;

function TSizeInfos.GetString: string;
var
  i :integer;
begin
  result := '';
  for i:=0 to FItems.Count-1 do with FItems[i] do
    result := result + IfThen(i>0, '|', '') + Format('%s,%d,%s', [IfThen(Enabled, '1', '0'), Size, Name]);
end;

function TSizeInfos.GetEnabledSizes: TSizes;
var
  i :integer;
begin
  result := nil;
  for i:=0 to Count-1 do if FItems[i].Enabled then begin
    SetLength(result, Length(result)+1);
    result[High(result)] := FItems[i].Size;
  end;
end;

function TSizeInfos.GetEnabledSizeNames: TStringArray;
var
  i :integer;
begin
  result := nil;
  for i:=0 to Count-1 do if FItems[i].Enabled then begin
    SetLength(result, Length(result)+1);
    result[High(result)] := FItems[i].Name;
  end;
end;

// '64, 400, 1920'
function TSizeInfos.TrySetSizeString(const Str: string): boolean;
var
  l :TStringArray;
  s :string;
  Size :integer;
begin
  l := Str.Split(',');
  if Length(l)>0 then for s in l do
    if not TryStrToInt(s, Size) then Exit(false);
  FItems.Clear;
  if Length(l)>0 then for s in l do
    FItems.Add(TSizeInfo.Create(True, StrToInt(s), ''));
  Changed(lcInit, 0);
end;

// '1,120,THUMBNAIL|1,400,|0,1920,LARGE'
function TSizeInfos.TrySetString(const Str: string): boolean;
var
  l, e :TStringArray;
  t :string;
  EnVal, Size :integer;
begin
  if Length(Str)=0 then begin
    FItems.Clear;
    Changed(lcInit, 0);
    Exit(True);
  end;
  l := Str.Split('|');
  FItems.Clear;
  for t in l do begin
    e := t.Split(',');
    if (Length(e)<>3) or not (TryStrToInt(e[0], EnVal) and TryStrToInt(e[1], Size)) then Exit(false);
    FItems.Add(TSizeInfo.Create(EnVal<>0, Size, e[2]));
  end;
  Changed(lcInit, 0);
  result := true;
end;

function TSizeInfos.ProcessSizeName(const SizeName: string; Size: integer): string;
begin
  if SameText(SizeName, '<AUTO>') then
    result := DEFAULT_SIZENAMES[SizeToClassIndex(Size)]
  else if SameText(SizeName, '<SIZE>') then
    result := IntToStr(Size)
  else if SameText(SizeName, '<EMPTY>') then
    result := ''
  else
    result := SizeName;
  result := ReplaceIllegalChars(result, ILLGCHARS, '_');
end;

procedure TSizeInfos.Add(Enabled: boolean; Size: integer; const SizeName: string);
var
  i, n :integer;
  _SizeName :string;

  procedure Insert(Index: integer; Enabled: boolean; Size: integer; const SizeName: string);
  begin
    FItems.Insert(Index, TSizeInfo.Create(Enabled, Size, SizeName));
    Changed(lcInsert, Index);
  end;

begin
  if Size<=0 then Exit;
  _SizeName := ProcessSizeName(SizeName, Size);
  n := FItems.Count;
  if (n=0) or (Size<FItems[0].Size) then begin
    Insert(0, Enabled, Size, _SizeName);
    Exit;
  end;
  for i:=0 to n-1 do begin
    if Size=FItems[i].Size then begin
      FItems[i].FEnabled := Enabled;
      FItems[i].FName := _SizeName;
      Changed(lcChanged, i);
      Exit;
    end else if Size<FItems[i].Size then begin
      if (i>0) and (Size>FItems[i-1].Size) then begin
        Insert(i, Enabled, Size, _SizeName);
        Exit;
      end;
    end;
  end;
  Insert(n, Enabled, Size, _SizeName);
end;

procedure TSizeInfos.Replace(Index: integer; Enabled: boolean; Size: integer; const SizeName: string);
var
  _SizeName :string;
begin
  _SizeName := ProcessSizeName(SizeName, Size);
  if FItems[Index].Size = Size then begin
    FItems[Index].FName := _SizeName;
    FItems[Index].FEnabled := Enabled;
    Changed(lcChanged, Index);
  end else begin
    Delete(Index);
    Add(Enabled, Size, _SizeName);
  end;
end;

procedure TSizeInfos.Delete(Index: integer);
begin
  FItems.Delete(Index);
  Changed(lcDelete, Index);
end;

procedure TSizeInfos.Clear;
begin
  if Count=0 then Exit;
  FItems.Clear;
  Changed(lcInit, 0);
end;

function TSizeInfos.Required: boolean;
var
  Info :TSizeInfo;
begin
  if Count=0 then Exit(True);
  for Info in FItems do
    if Info.Enabled then Exit(false);
  result := true;
end;

{ TProjectSettings }

constructor TProjectSettings.Create(const ASection: string);
begin
  inherited Create(ASection);
  Description         := TStringSetting.Create(self,    'Description');
  SourceMode          := TPicklistSetting.Create(self,  'SourceMode');
  SourceFolder        := TStringSetting.Create(self,    'SourceFolder');
  SourceMasks         := TStringSetting.Create(self,    'SourceMasks');
  SourceFilenames     := TStringsSetting.Create(self,   'SourceFilenames');
  Sizes               := TStringSetting.Create(self,    'Sizes');
  SizeNames           := TStringSetting.Create(self,    'SizeNames');
  TargetFolder        := TStringSetting.Create(self,    'TargetFolder');
  JPEGQuality         := TPicklistSetting.Create(self,  'JPEGQuality');
  PNGCompression      := TPicklistSetting.Create(self,  'PNGCompression');
  ResamplingFilter    := TPickListSetting.Create(self,  'ResamplingFilter');
  MrkEnabled          := TBooleanSetting.Create(self,   'MrkEnabled');
  MrkFilename         := TStringSetting.Create(self,    'MrkFilename');
  MrkSize             := TUInt32Setting.Create(self,    'MrkSize');
  MrkX                := TUInt32Setting.Create(self,    'MrkX');
  MrkY                := TUInt32Setting.Create(self,    'MrkY');
  MrkOpacity            := TUInt32Setting.Create(self,    'MrkAlpha');
  RenEnabled          := TBooleanSetting.Create(self,   'RenEnabled');
  RenSimple           := TBooleanSetting.Create(self,   'RenSimple');
  RenMode             := TPicklistSetting.Create(self,  'RenMode');
  RenTemplate         := TStringSetting.Create(self,    'RenTemplate');
  Shuffle             := TBooleanSetting.Create(self,   'Shuffle');
  ShuffleSeed         := TUInt32Setting.Create(self,    'ShuffleSeed');
  TagsSourceEXIF      := TBooleanSetting.Create(self,   'TagsSourceEXIF');
  TagsSourceTagsFiles := TBooleanSetting.Create(self,   'TagsSourceTagsFiles');
  TagTitle            := TBooleanSetting.Create(self,   'TagTitle');
  TagTimestamp        := TBooleanSetting.Create(self,   'TagTimestamp');
  TagCopyright        := TBooleanSetting.Create(self,   'TagCopyright');
  Copyright           := TStringSetting.Create(self,    'Copyright');
  TagsReportEnabled   := TBooleanSetting.Create(self,   'TagsReportEnabled');
  ImageInfosEnabled   := TBooleanSetting.Create(self,   'ImageInfosEnabled');
  NoCreate            := TBooleanSetting.Create(self,   'NoCreate');
  PresentationId      := TStringSetting.Create(self,    'PresentationId');
end;

{ TProcessingSettings }

constructor TProcessingSettings.Create(const ASection :string);
begin
  inherited;
  ThreadsUsed := TUInt32Setting.Create(self, 'ThreadsUsed');
  StopOnError := TBooleanSetting.Create(self, 'StopOnError');
end;

end.

