unit maxBeep;

interface

type
  TMaxSoundKind = (
    msInfo,
    msWarning,
    msError,
    msQuestion,
    msOk
    );

procedure BeepOk;
procedure BeepError;
procedure BeepQuestion;
procedure BeepInfo;
procedure BeepWarning;

procedure PlaySoundFile(const aFileName: string);

implementation

{$IF DEFINED(ANDROID) or DEFINED(IOS) or DEFINED(MACOS)}
{$DEFINE USE_FMX_MEDIA}
{$IFEND}

uses
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.mmSystem,
  {$ELSE}
  {$IFDEF USE_FMX_MEDIA}
  FMX.Media,
  {$ENDIF}
  {$ENDIF}
  System.IOUtils;

{$IFDEF USE_FMX_MEDIA}
var
  gPlayer: TMediaPlayer;

procedure EnsurePlayer;
begin
  if gPlayer = nil then
  begin
    gPlayer := TMediaPlayer.Create(nil);
    gPlayer.Loop := False;
  end;
end;

function GetDefaultSoundFile(const aKind: TMaxSoundKind): string;
var
  lFileName: string;
begin
  case aKind of
    msInfo:
      lFileName := 'info.wav';
    msWarning:
      lFileName := 'warning.wav';
    msError:
      lFileName := 'error.wav';
    msQuestion:
      lFileName := 'question.wav';
    msOk:
      lFileName := 'ok.wav';
  end;

  // @Pawel todo: adjust to your actual asset path (e.g. resources, documents, etc.)
  Result := TPath.Combine(TPath.GetDocumentsPath, lFileName);
end;
{$ENDIF}

procedure InternalBeep(const aKind: TMaxSoundKind);
{$IFDEF MSWINDOWS}
const
  C_MAP: array[TMaxSoundKind] of UINT = (
    MB_ICONASTERISK, // msInfo
    MB_ICONEXCLAMATION, // msWarning
    MB_ICONHAND, // msError
    MB_ICONQUESTION, // msQuestion
    MB_OK // msOk
    );
begin
  MessageBeep(C_MAP[aKind]);
end;
{$ELSE}
{$IFDEF USE_FMX_MEDIA}
var
  lFile: string;
begin
  lFile := GetDefaultSoundFile(aKind);
  PlaySoundFile(lFile);
end;
{$ELSE}
begin
  // no-op: sound not implemented on this platform
end;
{$ENDIF}
{$ENDIF}

procedure BeepOk;
begin
  InternalBeep(msOk);
end;

procedure BeepError;
begin
  InternalBeep(msError);
end;

procedure BeepQuestion;
begin
  InternalBeep(msQuestion);
end;

procedure BeepInfo;
begin
  InternalBeep(msInfo);
end;

procedure BeepWarning;
begin
  InternalBeep(msWarning);
end;

{$IFDEF MSWINDOWS}
procedure PlaySoundFile(const aFileName: string);
begin
  if aFileName = '' then
    exit;

  sndPlaySound(PChar(aFileName),
    SND_ASYNC or SND_NODEFAULT or SND_FILENAME);
end;
{$ENDIF}

{$IFDEF USE_FMX_MEDIA}
procedure PlaySoundFile(const aFileName: string);
begin
  if aFileName = '' then
    exit;

  if not TFile.Exists(aFileName) then
    exit;

  EnsurePlayer;

  gPlayer.stop;
  gPlayer.Clear;
  gPlayer.FileName := aFileName;
  gPlayer.Play;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
{$IFNDEF USE_FMX_MEDIA}
procedure PlaySoundFile(const aFileName: string);
begin
  // no-op: no backend for this platform
end;
{$ENDIF}
{$ENDIF}

{$IFDEF USE_FMX_MEDIA}
initialization

finalization
  gPlayer.Free;
  {$ENDIF}

end.

