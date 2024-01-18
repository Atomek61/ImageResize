unit appsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

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

implementation

{ TDialogSettings }

constructor TDialogSettings.Create(const ASection :string);
begin
  inherited;
  AutoSave := TBooleanSetting.Create(self, 'AutoSave');
  AutoSave.TextDefault := 'True';
  WarnDirty := TBooleanSetting.Create(self, 'WarnDirty');
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

