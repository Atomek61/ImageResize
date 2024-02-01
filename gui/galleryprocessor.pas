unit galleryprocessor;

{$mode delphi}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, TemplateEngine, Tags, StringArrays, Generics.Collections,
  FileUtil, Logging;

type

  { TProcessor }

  TProcessor = class
  private
    FDelimiters :TDelimiters;
    FTargetFolder :string;          // Folder with images and meta info in .images file
    FTemplateFiles :TStringArray;   // Template files - .html, .js, .css or whatever
    FCopyFiles :TStringArray;       // Template files - .html, .js, .css or whatever
    FFilesTags :TFilesTags;         // Table with tags for each image file, usually from .images file in folder
    FListFragments :TStringDictionary;  // List fragments - each List, built by the FListSolver uses one
    FSysVars :TSolver;           // Global Vars like CR, LF
    FDocVars :TSolver;              // Global Vars like TITLE, DATE, OWNER, IMG-LIST, NAV-LIST
  public type
    TStats = record
      FilesToCopy :integer;           // Number of files to be copied
      FilesCopied :integer;           // Number of files copied
      TemplatesToProcess :integer;    // Num,ber of templates to be processed
      TemplatesProcessed :integer;    // Number of template files processed
      Lists :integer;                 // Number of lists to build
      ItemsPerList :integer;          // Number of items per list (same for all lists)
      Dependencies :integer;          // Total number of dependencies
      Solved :integer;                // Number of solved dependencies
      Replacements :integer;          // Number of total replacements in the templates (without var solving)
      Elapsed :integer;               // ms
    end;
  public
    constructor Create(const Delimiters :TDelimiters); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Execute(out Stats :TStats) :boolean;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
    property SysVars :TSolver read FSysVars;
    property DocVars :TSolver read FDocVars;
    property ListFragments :TStringDictionary read FListFragments;
    property CopyFiles :TStringArray read FCopyFiles write FCopyFiles;
    property TemplateFiles :TStringArray read FTemplateFiles write FTemplateFiles;
    property Delimiters :TDelimiters read FDelimiters;

  end;

const
  DOTIMAGESFILETITLE = '.imgtags';

implementation

resourcestring
  SErrDirNotFoundFmt = 'Directory ''%s'' not found.';
  SErrDotImagesNotFoundFmt = 'Tags file ''%s'' not found.';
  SMsgLoadingDotImagesFmt = 'Loading image infos ''%s''...';
  SMsgBuildingLists = 'Building lists...';
  SMsgProcessingTemplateFmt = 'Processing ''%s''...';
  SMsgCopyingFmt = 'Copying ''%s''...';
  SMsgStatisticsFmt = 'Copied: %d/%d (%.0f%%), processed: %d/%d (%.0f%%), solved: %d/%d (%.0f%%), lists: %d, rows: %d, replaced: %d, elapsed %.1fs.';
  SMsgFinalOk = 'Ok';

{ TProcessor }

constructor TProcessor.Create(const Delimiters :TDelimiters);
begin
  FDelimiters := Delimiters;
  FFilesTags := TFilesTags.Create;
  FListFragments := TStringDictionary.Create;
  FDocVars := TSolver.Create(FDelimiters);
  FSysVars := TSolver.Create(FDelimiters);
  with FSysVars do begin
    Load('SYSCR', #13);
    Load('SYSLF', #10);
    Load('SYSCRLF', #13#10);
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
  FTargetFolder := '';
  FTemplateFiles.Clear;
  FCopyFiles.Clear;
  FListFragments.Clear;
  FDocVars.Clear;
end;

function TProcessor.Execute(out Stats :TStats) :boolean;
var
  ImagesFilename :string;
  Lists :TStringDictionary;
  TemplateFilename :string;
  SourceFilename :string;
  TargetFilename :string;
  i, j :integer;
  Tags :TTags;
  Key, Value :string;
  Fragment :TPair<string, string>;
  List :TPair<string, string>;
  SolverStats :TSolver.TStats;
  ImgVars :TSolver;
  FileSource :TStringList;
  Filename, FileText :string;
  Replacements :integer;
  t0 :Int64;

  function Percent(f1, f2 :integer) :single;
  begin
    if f1=0 then
      result := 0.0
    else
      result := f2/f1*100.0;
  end;

begin
  Lists := TStringDictionary.Create;
  ImgVars := TSolver.Create(FDelimiters);
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
      FTargetFolder := IncludeTrailingPathDelimiter(FTargetFolder);
      if not DirectoryExists(FTargetFolder) then
        raise Exception.CreateFmt(SErrDirNotFoundFmt, [FTargetFolder]);

      // 2. Load the tags of the .images file
      ImagesFilename := FTargetFolder + DOTIMAGESFILETITLE;
      if not FileExists(ImagesFilename) then
        raise Exception.CreateFmt(SErrDotImagesNotFoundFmt, [ImagesFilename]);
      Log(SMsgLoadingDotImagesFmt, [ImagesFilename], llInfo);
      FFilesTags.LoadFromImagesFile(ImagesFilename);
      Stats.ItemsPerList := FFilesTags.Filenames.Count;

      // 3. Iterate over the images and build the lists
      Log(SMsgBuildingLists, llInfo);
      // Prepare the array of lists
      for Fragment in FListFragments do
        Lists.Add(Fragment.Key, '');
      // Iterate over the images
      for i:=0 to FFilesTags.Filenames.Count-1 do begin
        // Build the ImgVars for each image
        ImgVars.Clear;
        ImgVars.Add('IMGINDEX', IntToStr(i));
        ImgVars.Add('IMGNUMBER', IntToStr(i+1));
        ImgVars.Add('IMGCOUNT', IntToStr(FFilesTags.Filenames.Count));
        ImgVars.Add('IMGFILENAME', ExtractFilename(FFilesTags.Filenames[i]));
        Tags := FFilesTags[FFilesTags.Filenames[i]];
        for Key in FFilesTags.TagKeys do begin
          if not Tags.TryGetValue(Key, Value) then Value := '';
          ImgVars.Load('IMG'+UpperCase(Key), Value);
        end;
        // Add the global vars
        for j:=0 to FSysVars.Count-1 do
          ImgVars.Load(FSysVars.Keys[j], FSysVars.Values[j]);
        for Fragment in FListFragments do
          ImgVars.Load(Fragment.Key, Fragment.Value);
        ImgVars.Solve(SolverStats);
        inc(Stats.Dependencies, SolverStats.LeftDependencies + SolverStats.Solved);
        inc(Stats.Solved, SolverStats.Solved);
        for Fragment in FListFragments do
          Lists[Fragment.Key] := Lists[Fragment.Key] + ImgVars[Fragment.Key];
      end;

      // 4. Add the builded lists as global variables
      for List in Lists do
        FDocVars['DOC'+List.Key] := List.Value;

      // 5. Load the template files and replace the global var
      for TemplateFilename in FTemplateFiles do begin
        TargetFilename := TargetFolder+ExtractFilename(TemplateFilename);
        Log(SMsgProcessingTemplateFmt, [ExtractFilename(TemplateFilename)], llInfo);
        FileSource.LoadFromFile(TemplateFilename, TEncoding.UTF8);
        FileText := FDocVars.Replace(FileSource.Text, Replacements);
        inc(Stats.Replacements, Replacements);
        FileSource.Text := FileText;
        FileSource.SaveToFile(TargetFilename, TEncoding.UTF8);
        inc(Stats.TemplatesProcessed);
      end;

      // 6. Copy some files
      for SourceFilename in FCopyFiles do begin
        Filename := ExtractFilename(SourceFilename);
        TargetFilename := FTargetFolder+Filename;
        Log(SMsgCopyingFmt, [Filename], llInfo);
        CopyFile(SourceFilename, TargetFilename);
        inc(Stats.FilesCopied);
      end;

    finally
      Lists.Free;
      ImgVars.Free;
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

//procedure Test;
//var
//  Processor :TProcessor;
//begin
//  Processor := TProcessor.Create;
//  try
//    Processor.Folder := 'D:\Mf\Dev\Lazarus\ImageResize\tst\tst9\img1920';
//    Processor.ListFragments.Add('LIST-IMG', '<img src="«IMG-URL»" alt="«IMG-TITLE»"/>'+#13+#10);
//    Processor.ListFragments.Add('LIST-JSON', '{url: "«IMG-URL»", title="«IMG-TITLE»"},'+#13+#10);
//    Processor.TemplateFiles.Add('D:\Mf\Dev\Lazarus\ImageResize\tst\srcF\index.html.template');
//    Processor.DocumentVars.Add('TITLE', 'Beispiel');
//    Processor.Execute;
//  finally
//    Processor.Free;
//  end;
//end;
//
//initialization
//begin
//  Test;
//end;
//
end.

