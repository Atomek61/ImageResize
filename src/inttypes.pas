unit IntTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function Int64ToPrettySize(Value :Int64) :string;

implementation

// '980 Bytes', '1.2 kB', '10 kB', '9.4 MB', '220 MB', '1.7 TB'
// <                 1.000   %.0f Bytes
// <                10.000   %.1f kB
// <             1.000.000   %.0f kB     10 kB  ... 999 kB
// <            10.000.000   %.1f MB     1.0 MB ... 9.9 MB
// <         1.000.000.000   %.0f MB     10 MB  ... 999 MB
// <        10.000.000.000   %.1f GB     1.0 GB ... 9.9 GB
// <     1.000.000.000.000   %.0f GB     10 GB  ... 999 GB
// <    10.000.000.000.000   %.1f TB     1.0 TB ... 9.9 TB
// >=   10.000.000.000.000   %.0f TB     10 TB  ...

type
  TLimitInfo = record
    Limit :Int64;
    Factor :double;
    Fmt :string;
  end;

function Int64ToPrettySize(Value :Int64) :string;
const
  LIMITINFOS :array[0..7] of TLimitInfo = (
    (Limit: 10000; Factor: 1000.0; Fmt: '%.1f kB'),
    (Limit: 1000000; Factor: 1000.0; Fmt: '%.0f kB'),
    (Limit: 10000000; Factor: 1000000.0; Fmt: '%.1f MB'),
    (Limit: 1000000000; Factor: 1000000.0; Fmt: '%.0f MB'),
    (Limit: 10000000000; Factor: 1000000000.0; Fmt: '%.1f GB'),
    (Limit: 1000000000000; Factor: 1000000000.0; Fmt: '%.0f GB'),
    (Limit: 10000000000000; Factor: 1000000000000.0; Fmt: '%.1f TB'),
    (Limit: 1000000000000000; Factor: 1000000000000.0; Fmt: '%.0f TB')
  );
var
  Info :TLimitInfo;
begin
  if Value<1000 then
    result := Format('%d Bytes', [Value])
  else
    for Info in LIMITINFOS do
      if Value<Info.Limit then
        Exit(Format(Info.Fmt, [Value/Info.Factor]));
  //else if Value<10000 then
  //  result := Format('%.1f kB', [Value/1000.0])
  //else if Value<1000000 then
  //  result := Format('%.0f kB', [Value/1000.0])
  //else if Value<10000000 then
  //  result := Format('%.1f MB', [Value/1000000.0])
  //else if Value<100000000 then
  //  result := Format('%.0f MB', [Value/1000000.0])
  //else if Value<1000000000 then
  //  result := Format('%.1f GB', [Value/1000000000.0])
  //else if Value<100000000 then
  //  result := Format('%.0f MB', [Value/1000000.0])
end;

end.

