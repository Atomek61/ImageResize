////////////////////////////////////////////////////////////////////////////////
//
//  The VarResolver helps you maintaining Variables like %MYVAR% to contain
//  other Variables. The Resolver analyzes the dependencies and solves them
//  in the right order.
//
//  Rekursively usage is not allowed and a resolvable dependency tree must
//  exist.
//
//  Usage:
//
//  - create a TVarResolver object and add all of your Variable names and
//    Values
//  - call Resolve
//  - Get the resulting Values
//
//
//  09.2009 JS Initial Revision
//
////////////////////////////////////////////////////////////////////////////////


unit VarResolverFns;

interface

type
  TDepsArray = array of integer;
  TVarDef = record
    Name :string;
    Value :string;
    Deps :TDepsArray;
  end;
  TVarDefs =array of TVarDef;

  TResolver = class
    function GetCount :integer; virtual; abstract;
    function GetName(Index :integer) :string; virtual; abstract;
    function GetValue(Index :integer) :string; virtual; abstract;
    function FindValue(const Name :string; var Index :integer) :boolean; virtual; abstract;
    procedure SetValue(Index :integer; const Value :string); virtual; abstract;
    function Execute(const Del1, Del2 :string) :boolean;
  end;


implementation

uses
  TextFns, SysUtils;

function TResolver.Execute(const Del1, Del2 :string) :boolean;
var
  Deps :array of array of integer;
  i, j, k :integer;
  i0, it, i1 :integer;
  Ref :string;
  TotalDeps :integer;
  Resolved :integer;
  IsDep :boolean;
  Value :string;
  Count :integer;

  procedure RemoveDep(i, j :integer);
  begin
    if i<Count then
      Move(Deps[i,j+1], Deps[i,j], (High(Deps[i])-j)*sizeof(integer));
    SetLength(Deps[i], High(Deps[i]));
  end;

begin
  result := true;
  Count := GetCount;

  SetLength(Deps, Count);

  // Build dependency graph
  TotalDeps := 0;
  for i:=0 to Count-1 do begin
    i0 := 1;
    Value := GetValue(i);
    while FindEnclosed(Value, Del1, Del2, i0, it, i1) do begin
      Ref := Copy(Value, it, i1-it);
      if FindValue(Ref, j) then begin
        // check, if dependency isnt already there
        IsDep := false;
        for k:=0 to High(Deps[i]) do begin
          if Deps[i, k]=j then begin
            IsDep := true;
            break;
          end;
        end;
        if not IsDep then begin
          SetLength(Deps[i], Length(Deps[i])+1);
          Deps[i, High(Deps[i])] := j;
          inc(TotalDeps);
        end;
      end;
      i0 := i1 + Length(Del1);
    end;
  end;

  // Reduce dependencies by resolving that ones, which are to independend nodes
  while TotalDeps>0 do begin
    Resolved := 0;
    for i:=0 to Count-1 do begin // over all vars
      for j:=High(Deps[i]) downto 0 do begin // resolve all dependencies to independend vars
        k := Deps[i,j];
        if Length(Deps[k]) = 0 then begin
          SetValue(i, StringReplace(GetValue(i), Del1+GetName(k)+Del2, GetValue(k), [rfReplaceAll]));
          RemoveDep(i, j);
          inc(Resolved);
        end;
      end;
    end;
    if (Resolved=0) and (TotalDeps>0) then raise Exception.Create('Cant resolve Vars.');
    dec(TotalDeps, Resolved);
  end;
end;


end.
