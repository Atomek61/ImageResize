unit appsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

  { TProjectSettings }

  TProjectSettings = class(TSettings)
  public
    constructor Create(const ASection :string); override;
  end;


  { TProcessingSettings }

  TProcessingSettings = class(TSettings)
  public
    ThreadsUsed :TIntegerSetting;
    StopOnError :TBooleanSetting;
    constructor Create(const ASection :string); override;
  end;

  { TDialogSettings }

  TDialogSettings = class(TSettings)
  public
    AutoSave :TBooleanSetting;
    WarnDirty :TBooleanSetting;
    constructor Create(const ASection :string); override;
  end;

  { TPresentationSettings }

  TPresentationSettings = class(TSettings)
  public
    Id :TStringSetting;
    TargetFolder :TStringSetting;
    constructor Create(const ASection :string); override;
  end;

implementation

{ TDialogSettings }

constructor TDialogSettings.Create(const ASection :string);
begin
  inherited;
  AutoSave := TBooleanSetting.Create(self, 'AutoSave');
  AutoSave.DefaultText := 'True';
  WarnDirty := TBooleanSetting.Create(self, 'WarnDirty');
end;

{ TPresentationSettings }

constructor TPresentationSettings.Create(const ASection: string);
begin
  inherited Create(ASection);
  Id := TStringSetting.Create(self, 'Id');
  TargetFolder := TStringSetting.Create(self, 'TargetFolder');
end;

{ TProjectSettings }

constructor TProjectSettings.Create(const ASection: string);
begin
  inherited Create(ASection);
end;

{ TProcessingSettings }

constructor TProcessingSettings.Create(const ASection :string);
begin
  inherited;
  ThreadsUsed := TIntegerSetting.Create(self, 'ThreadsUsed');
  ThreadsUsed.Min := 0;
  StopOnError := TBooleanSetting.Create(self, 'StopOnError');
end;

end.

