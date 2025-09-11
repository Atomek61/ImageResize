unit galleryprocessor;

{$mode delphi}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, StrUtils, Templates, Tags, StringArrays, Generics.Collections,
  FileUtil, Logging, IntTypes, ImgUtils, RegExpr, Math;

type

  { TProcessor }

  TProcessor = class

  private const

    IFMT_PARAMS_REGEXPR = '^(\d+)(?:,(\d+|auto))?$';
    FRAGMENTKEY_REGEXPR = '^(\w+)\.(\w+)\.(\w+)$';

  private type
    TFragment = record
      Name :string;
      Template :string;
      Extension :string;
    end;

    { TFragmentDictionary }

    TFragmentDictionary = class(TDictionary<string, TFragment>)
      FRegExpr :TRegExpr;
      constructor Create; override;
      destructor Destroy; override;
      procedure AddFromString(const Key, Value :string);
    end;
  private

    FDelimiters :TTypeDelimiters;   // Delimiters depending on extension
    FImgTagsFilename :string;       // Folder with images and meta info in .images file
    FTemplateFiles :TStringArray;   // Template files - .html, .js, .css or whatever
    FCopyFiles :TStringArray;       // Tobecopied files - .html, .js, .css or whatever
    FFilesTags :TFilesTags;         // Table with tags for each image file, usually from .images file in folder
    FListFragments :TFragmentDictionary;  // List fragments - each List, built by the FListEngine uses one
    FSysVars :TEngine;              // Global Vars like CR, LF
    FDocVars :TEngine;              // Global Vars like TITLE, DATE, OWNER, IMG-LIST, NAV-LIST

  public type

    TStats = record
      FilesToCopy :integer;         // Number of files to be copied
      FilesCopied :integer;         // Number of files copied
      TemplatesToProcess :integer;  // Num,ber of templates to be processed
      TemplatesProcessed :integer;  // Number of template files processed
      Lists :integer;               // Number of lists to build
      ItemsPerList :integer;        // Number of items per list (same for all lists)
      Dependencies :integer;        // Total number of dependencies
      Solved :integer;              // Number of solved dependencies
      Replacements :integer;        // Number of total replacements in the templates (without var solving)
      Elapsed :integer;             // ms
    end;

  private

    function ifmt(const VarName, Value, Params: string): string;

  public

    constructor Create(const Delimiters :TTypeDelimiters); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Execute(out Stats :TStats) :boolean;
    property ImgTagsFilename :string read FImgTagsFilename write FImgTagsFilename;
    property SysVars :TEngine read FSysVars;
    property DocVars :TEngine read FDocVars;
    property ListFragments :TFragmentDictionary read FListFragments;
    property CopyFiles :TStringArray read FCopyFiles write FCopyFiles;
    property TemplateFiles :TStringArray read FTemplateFiles write FTemplateFiles;
    property Delimiters :TTypeDelimiters read FDelimiters;

  end;

implementation

uses
  Utils;

resourcestring
  SErrImgTagsNotFoundFmt    = 'Tags file ''%s'' not found.';
  SMsgLoadingDotImagesFmt   = 'Loading image tags ''%s''...';
  SMsgBuildingLists         = 'Building lists...';
  SMsgProcessingTemplateFmt = 'Processing ''%s''...';
  SMsgCopyingFmt            = 'Copying ''%s''...';
  SMsgStatisticsFmt         = 'Copied: %d/%d (%.0f%%), processed: %d/%d (%.0f%%), solved: %d/%d (%.0f%%), lists: %d, rows: %d, replaced: %d, elapsed %.1fs.';
  SMsgFinalOk               = 'Ok';
  SErrIfmtParamsFmt         = 'invalid parameters (offset[,digits] expected)';
  SErrInvalidFragmentFmt    = 'Invalid list fragment "%s" (Fragment.NAME.ext expected).';

{ TProcessor }

constructor TProcessor.Create(const Delimiters :TTypeDelimiters);
begin
  FDelimiters := Delimiters;
  FFilesTags := TFilesTags.Create;
  FListFragments := TFragmentDictionary.Create;
  FDocVars := TEngine.Create;
  FSysVars := TEngine.Create;
  with FSysVars do begin
    Load('CR', #13);
    Load('LF', #10);
    Load('CRLF', #13#10);
  end;
end;

destructor TProcessor.Destroy;
begin
  FFilesTags.Free;
  FListFragments.Free;
  FDocVars.Free;
  FSysVars.Free;
  inherited Destroy;
end;

procedure TProcessor.Clear;
begin
  FImgTagsFilename := '';
  FTemplateFiles.Clear;
  FCopyFiles.Clear;
  FListFragments.Clear;
  FDocVars.Clear;
end;

function TProcessor.Execute(out Stats :TStats) :boolean;
var
  Lists :TStringDictionary;
  TemplateFilename :string;
  SourceFilename :string;
  TargetFilename :string;
  TargetFolder :string; // Same as ImgTagsFilename
  i, j :integer;
  Tags :TTags;
  Key, Value :string;
  Fragment :TFragment;
  List :TPair<string, string>;
  Engine :TEngine;
  FileSource :TStringList;
  Filename, FileText :string;
  Replacements :integer;
  t0 :Int64;
  Delimiters :TDelimiters;

  function Percent(f1, f2 :integer) :single;
  begin
    if f1=0 then
      result := 0.0
    else
      result := f2/f1*100.0;
  end;

  function LargerSize(const Width, Height :string) :string;
  begin
    result := IfThen(StrToInt(Engine[Width])>StrToInt(Engine[Height]), Width, Height);
  end;

  function PrettySize(const Size :string) :string;
  begin
    result := Int64ToPrettySize(StrToInt64(Engine[Size]));
  end;

  function PrettyRatio(const Width, Height :string) :string;
  var
    Ratio :TImageRatio;
    Orientation :TImageOrientation;
  begin
    if WidthHeightToRatioAndOrientation(StrToInt(Engine[Width]), StrToInt(Engine[Height]), Ratio, Orientation) then
      result := RatioToStr(Ratio)
    else
      result := '';
  end;

  function PrettyOrientation(const Width, Height :string) :string;
  var
    Ratio :TImageRatio;
    Orientation :TImageOrientation;
  begin
    if WidthHeightToRatioAndOrientation(StrToInt(Engine[Width]), StrToInt(Engine[Height]), Ratio, Orientation) then
      result := OrientationToStr(Orientation)
    else
      result := '';
  end;

begin
  Lists := TStringDictionary.Create;
  Engine := TEngine.Create;
  Engine.UnknownVarHandling := uvhClear;
  FileSource := TStringList.Create;
  Stats := Default(TStats);
  t0 := TThread.GetTickCount64;

  try
    try
      // Prepare Stats
      Stats.FilesToCopy := CopyFiles.Count;
      Stats.TemplatesToProcess := TemplateFiles.Count;
      Stats.Lists := ListFragments.Count;
      Stats.ItemsPerList := ListFragments.Count;

      // 1. Check the parameters
      if not FileExists(FImgTagsFilename) then
        raise Exception.CreateFmt(SErrImgTagsNotFoundFmt, [FImgTagsFilename]);
      TargetFolder := ExtractFilePath(FImgTagsFilename);

      // 2. Load the tags of the .imgtags file
      Log(SMsgLoadingDotImagesFmt, [FImgTagsFilename], llInfo);
      FFilesTags.LoadFromImgTagsFile(FImgTagsFilename);
      Stats.ItemsPerList := FFilesTags.Filenames.Count;

      // 3. Iterate over the images and build the lists
      Log(SMsgBuildingLists, llInfo);

      // Prepare the array of lists
      for Fragment in FListFragments.Values do
        Lists.Add(Fragment.Name, '');

      // Image- and Fragments-loop
      for i:=0 to FFilesTags.Filenames.Count-1 do begin
        // Build the Engine for each image
        Engine.Clear;
        Engine.Add('IMGINDEX', IntToStr(i));
        Engine.Add('IMGNUMBER', IntToStr(i+1));
        Engine.Add('IMGCOUNT', IntToStr(FFilesTags.Filenames.Count));
        Engine.Add('IMGFILENAME', ExtractFilename(FFilesTags.Filenames[i]));
        Engine.Add('IMGFILETITLE', ChangeFileExt(ExtractFilename(FFilesTags.Filenames[i]), ''));
        Engine.Add('IMGFILEEXT', ExtractExt(FFilesTags.Filenames[i]));
        Engine.Add('IMGFILEFORMAT', FileFormat(FFilesTags.Filenames[i]));

        // Add the Engine from the row of the .imagetags file
        Tags := FFilesTags[FFilesTags.Filenames[i]];
        for Key in FFilesTags.TagKeys do begin
          if not Tags.TryGetValue(Key, Value) then Value := '';
          Engine.Load(UpperCase(Key), Value);
        end;

        // Add some calculated vars
        Engine.Add('IMGSIZE', LargerSize('IMGWIDTH', 'IMGHEIGHT'));
        Engine.Add('IMGPRETTYFILESIZE', PrettySize('IMGFILESIZE'));
        Engine.Add('ORGRATIO', PrettyRatio('ORGWIDTH', 'ORGHEIGHT'));
        Engine.Add('ORGORIENTATION', PrettyOrientation('ORGWIDTH', 'ORGHEIGHT'));
        Engine.Add('ORGPRETTYFILESIZE', PrettySize('ORGFILESIZE'));
        Engine.Add('ORGSIZE', LargerSize('ORGWIDTH', 'ORGHEIGHT'));

        // Add the global vars
        for j:=0 to FSysVars.Count-1 do
          Engine.Load(FSysVars.Keys[j], FSysVars.Values[j]);

        // Solve the vars (dependencies between the vars)
        Engine.Delimiters := DOSDELIMITERS;
        Engine.Solve;
        // Todo: check errors of solving

        // Compile the Fragments
        // Add the fragments
        for Fragment in FListFragments.Values do begin
          Engine.Delimiters := FDelimiters[LowerCase(Fragment.Extension)];
          Engine.Load(Fragment.Name, Engine.Compile(Fragment.Template));
        end;

        // Update processing statistics
        inc(Stats.Dependencies, Engine.Stats.DepsTotal + Engine.Stats.Solved);
        inc(Stats.Solved, Engine.Stats.Solved);

        // Build the lists of fragments
        for Fragment in FListFragments.Values do
          Lists[Fragment.Name] := Lists[Fragment.Name] + Engine[Fragment.Name];
      end;

      // 4. Add the builded lists as global variables
      for List in Lists do
        FDocVars[List.Key] := List.Value;

      // 5. Load the template files and replace the global vars
      for TemplateFilename in FTemplateFiles do begin
        TargetFilename := TargetFolder+ExtractFilename(TemplateFilename);
        Log(SMsgProcessingTemplateFmt, [ExtractFilename(TemplateFilename)], llInfo);
        FileSource.LoadFromFile(TemplateFilename, TEncoding.UTF8);
        if FDelimiters.TryGetValue(LowerCase(ExtractExt(TemplateFilename)), Delimiters) then
          FDocVars.Delimiters := Delimiters
        else
          FDocVars.Delimiters := DOSDELIMITERS;
        FileText := FDocVars.Compile(FileSource.Text);
        inc(Stats.Replacements, FDocVars.ReplacementCount);
        FileSource.Text := FileText;
        FileSource.SaveToFile(TargetFilename, TEncoding.UTF8);
        inc(Stats.TemplatesProcessed);
      end;

      // 6. Copy some files
      for SourceFilename in FCopyFiles do begin
        Filename := ExtractFilename(SourceFilename);
        TargetFilename := TargetFolder+Filename;
        Log(SMsgCopyingFmt, [Filename], llInfo);
        CopyFile(SourceFilename, TargetFilename);
        inc(Stats.FilesCopied);
      end;

    finally
      Lists.Free;
      Engine.Free;
      FileSource.Free;
    end;
    result := true;
    Log(SMsgFinalOk)

  except on E: Exception do
    begin
      Log(E.Message, llError);
      result := false;
    end;
  end;
  with Stats do begin
    Elapsed := TThread.GetTickCount64-t0;
    Log(SMsgStatisticsFmt, [
      FilesCopied, FilesToCopy, Percent(FilesToCopy, FilesCopied),
      TemplatesProcessed, TemplatesToProcess, Percent(TemplatesToProcess, TemplatesProcessed),
      Solved, Dependencies, Percent(Dependencies, Solved),
      Lists, ItemsPerList,
      Replacements,
      Elapsed/1000.0
    ], llNews);
  end;
end;

function TProcessor.ifmt(const VarName, Value, Params: string): string;
var
  r :TRegExpr;
  Offset :integer;
  Digits :integer;
begin
  r := TRegExpr.Create(IFMT_PARAMS_REGEXPR);
  r.ModifierI := true;
  try
    if not r.Exec(Params) then
      raise Exception.CreateFmt(SErrIfmtParamsFmt, [Params]);
    Offset := StrToInt(r.Match[1]);
    if (r.Match[2]='') or SameText(r.Match[2], 'AUTO') then
      Digits := round(log10(self.FFilesTags.Filenames.Count))+1
    else
      Digits := StrToInt(r.Match[2]);
    result := Format('%*.*d', [Digits, Digits, StrToInt(Value)+Offset])
  finally
    r.Free;
  end;
end;

{ TProcessor.TFragmentDictionary }

constructor TProcessor.TFragmentDictionary.Create;
begin
  inherited;
  FRegExpr := TRegExpr.Create(FRAGMENTKEY_REGEXPR);
end;

destructor TProcessor.TFragmentDictionary.Destroy;
begin
  FRegExpr.Free;
  inherited Destroy;
end;

procedure TProcessor.TFragmentDictionary.AddFromString(const Key, Value: string);
var
  Fragment :TFragment;
begin
  // Fragment.NAME.ext
  if not FRegExpr.Exec(Key) then
    raise Exception.CreateFmt(SErrInvalidFragmentFmt, [Key]);
  Fragment.Name := FRegExpr.Match[2];
  Fragment.Extension := FRegExpr.Match[3];
  Fragment.Template := Value;
  inherited Add(Fragment.Name, Fragment);
end;

//procedure Test;
//var
//  Vars :TEngine;
//  d :string;
//begin
//  Vars := TEngine.Create;
//  Vars.Delimiters := DOSDELIMITERS;
//  Vars.Add('THUMBNAILS', 'mein kleiner Daumennagel');
//
//  d := Vars.Compile('XXXX {THUMBNAILS} YYYY');
//
//
//end;
//
//initialization
//begin
//  Test;
//end;

end.

