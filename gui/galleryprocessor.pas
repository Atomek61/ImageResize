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
    FDocumentVars :TSolver;           // Global Vars like TITLE, DATE, OWNER, IMG-LIST, NAV-LIST
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Execute;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
    property DocumentVars :TSolver read FDocumentVars;
    property ListFragments :TStringDictionary read FListFragments;
    property CopyFiles :TStringArray read FCopyFiles write FCopyFiles;
    property TemplateFiles :TStringArray read FTemplateFiles write FTemplateFiles;
    property Delimiters :TDelimiters read FDelimiters write FDelimiters;
  end;

const
  DOTIMAGESFILETITLE = '.images';

implementation

resourcestring
  SErrDirNotFoundFmt = 'Directory ''%s'' not found.';
  SErrDotImagesNotFoundFmt = 'Tags file ''%s'' not found.';
  SMsgLoadingDotImagesFmt = 'Loading image infos ''%s''...';
  SMsgBuildingLists = 'Building lists...';
  SMsgProcessingTemplateFmt = 'Processing ''%s''...';
  SMsgCopyingFmt = 'Copying ''%s''...';

{ TProcessor }

constructor TProcessor.Create;
begin
  FDelimiters := PERCENTDELIMITERS;
  FFilesTags := TFilesTags.Create;
  FListFragments := TStringDictionary.Create;
  FDocumentVars := TSolver.Create(FDelimiters);
end;

destructor TProcessor.Destroy;
begin
  FFilesTags.Free;
  FListFragments.Free;
  FDocumentVars.Free;
  inherited Destroy;
end;

procedure TProcessor.Clear;
begin
  FTargetFolder := '';
  FTemplateFiles.Clear;
  FCopyFiles.Clear;
  FListFragments.Clear;
  FDocumentVars.Clear;
end;

procedure TProcessor.Execute;
var
  ImagesFilename :string;
  Lists :TStringDictionary;
  TemplateFilename :string;
  SourceFilename :string;
  TargetFilename :string;
  i :integer;
  Tags :TTags;
  Key, Value :string;
  Fragment :TPair<string, string>;
  List :TPair<string, string>;
  Stats :TSolver.TStats;
  ListVars :TSolver;
  FileSource :TStringList;
  Filename, FileText :string;
begin
  Lists := TStringDictionary.Create;
  ListVars := TSolver.Create(FDelimiters);
  FileSource := TStringList.Create;
  try

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

    // 3. Iterate over the images and build the lists
    Log(SMsgBuildingLists, llInfo);
    for Fragment in FListFragments do
      Lists.Add(Fragment.Key, '');
    for i:=0 to FFilesTags.Filenames.Count-1 do begin
      ListVars.Clear;
      ListVars.Add('INDEX', IntToStr(i));
      ListVars.Add('URL', ExtractFilename(FFilesTags.Filenames[i]));
      Tags := FFilesTags[FFilesTags.Filenames[i]];
      for Key in FFilesTags.TagKeys do begin
        if not Tags.TryGetValue(Key, Value) then Value := '';
        ListVars.Add(UpperCase(Key), Value);
      end;
      for Fragment in FListFragments do
        ListVars.Add(Fragment.Key, Fragment.Value);
      ListVars.Solve(Stats);
      for Fragment in FListFragments do
        Lists[Fragment.Key] := Lists[Fragment.Key] + ListVars[Fragment.Key];
    end;

    // 4. Add the builded lists as global variables
    for List in Lists do
      FDocumentVars[List.Key] := List.Value;

    // 5. Load the template files and replace the global var
    for TemplateFilename in FTemplateFiles do begin
      TargetFilename := TargetFolder+ExtractFilename(TemplateFilename);
      Log(SMsgProcessingTemplateFmt, [ExtractFilename(TemplateFilename)], llInfo);
      FileSource.LoadFromFile(TemplateFilename, TEncoding.UTF8);
      FileText := FDocumentVars.Replace(FileSource.Text);
      FileSource.Text := FileText;
      FileSource.SaveToFile(TargetFilename, TEncoding.UTF8);
    end;

    // 6. Copy some files
    for SourceFilename in FCopyFiles do begin
      Filename := ExtractFilename(SourceFilename);
      TargetFilename := FTargetFolder+Filename;
      Log(SMsgCopyingFmt, [Filename], llInfo);
      CopyFile(SourceFilename, TargetFilename);
    end;

  finally
    Lists.Free;
    ListVars.Free;
    FileSource.Free;
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

