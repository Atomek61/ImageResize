unit appsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

  { TProjectSettings }  // Not yet in charge

  TProjectSettings = class(TSettings)
  public
    Description         :TStringSetting;
    SourceMode          :TPicklistSetting;
    SourceFolder        :TStringSetting;
    SourceMasks         :TStringSetting;
    SourceFilenames     :TStringSetting;
    Sizes               :TStringSetting;
    TargetFolder        :TStringSetting;
    JPEGQuality         :TPicklistSetting;
    PNGCompression      :TPicklistSetting;
    ResamplingFilter    :TPickListSetting;
    MrkEnabled          :TBooleanSetting;
    MrkFilename         :TStringSetting;
    MrkSize             :TUInt32Setting;
    MrkX                :TUInt32Setting;
    MrkY                :TUInt32Setting;
    MrkAlpha            :TUInt32Setting;
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

implementation

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

{ TProjectSettings }

constructor TProjectSettings.Create(const ASection: string);
begin
  inherited Create(ASection);
  Description         := TStringSetting.Create(self,    'Description');
  SourceMode          := TPicklistSetting.Create(self,  'SourceMode');
  SourceFolder        := TStringSetting.Create(self,    'SourceFolder');
  SourceMasks         := TStringSetting.Create(self,    'SourceMasks');
  SourceFilenames     := TStringSetting.Create(self,    'SourceFilenames');
  Sizes               := TStringSetting.Create(self,    'Sizes');
  TargetFolder        := TStringSetting.Create(self,    'TargetFolder');
  JPEGQuality         := TPicklistSetting.Create(self,  'JPEGQuality');
  PNGCompression      := TPicklistSetting.Create(self,  'PNGCompression');
  ResamplingFilter    := TPickListSetting.Create(self,  'ResamplingFilter');
  MrkEnabled          := TBooleanSetting.Create(self,   'MrkEnabled');
  MrkFilename         := TStringSetting.Create(self,    'MrkFilename');
  MrkSize             := TUInt32Setting.Create(self,    'MrkSize');
  MrkX                := TUInt32Setting.Create(self,    'MrkX');
  MrkY                := TUInt32Setting.Create(self,    'MrkY');
  MrkAlpha            := TUInt32Setting.Create(self,    'MrkAlpha');
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

