unit maxBeep;
{just some small functions to play some sounds}
interface
uses
windows, classes, sysUtils;

{Info about windows.MesageBeep params:

This parameter can be one of the following values.
Value	Meaning

0xFFFFFFFF
A simple beep. If the sound card is not available, the sound is generated using the speaker.

MB_ICONASTERISK
0x00000040L
             See MB_ICONINFORMATION.

MB_ICONEXCLAMATION
0x00000030L
             See MB_ICONWARNING.

MB_ICONERROR
0x00000010L
The sound specified as the Windows Critical Stop sound.

MB_ICONHAND
0x00000010L
See MB_ICONERROR.

MB_ICONINFORMATION
0x00000040L
The sound specified as the Windows Asterisk sound.

MB_ICONQUESTION
0x00000020L
The sound specified as the Windows Question sound.

MB_ICONSTOP
0x00000010L
See MB_ICONERROR.

MB_ICONWARNING
0x00000030L
The sound specified as the Windows Exclamation sound.

MB_OK
0x00000000L
The sound specified as the Windows Default Beep sound.
}

procedure BeepOk;
procedure BeepError;
Procedure BeepQuestion;
Procedure BeepInfo;
procedure BeepWarning;
Procedure PlaySound(const FileName: string);
implementation
uses
mmsystem, messages, controls;

procedure BeepOk;
begin

        Windows.MessageBeep(mb_ok);
end;
procedure BeepError;
begin
        Windows.MessageBeep(MB_ICONERROR);
end;

Procedure BeepQuestion;
begin
        Windows.MessageBeep(MB_ICONQUESTION);
end;
Procedure BeepInfo;
begin
        Windows.MessageBeep(MB_ICONINFORMATION);
end;
procedure BeepWarning;
begin
        Windows.MessageBeep(MB_ICONWARNING);
end;


procedure PlaySound(const Filename: string);
begin
//'C:\Windows\Media\sound.wav',
    sndPlaySound(pWideChar(FileName),
    SND_NODEFAULT Or SND_ASYNC  or SND_FILENAME);
end;

end.
