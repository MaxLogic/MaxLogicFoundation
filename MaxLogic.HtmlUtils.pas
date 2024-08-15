unit MaxLogic.HtmlUtils;

{
  moved methods from srHTML.pas and from pawel1.pas
}

interface


uses
  winApi.windows, System.classes, System.sysUtils, IdGlobal,
  System.StrUtils, generics.collections;

type
  TExtractProgressEvent = procedure(Pos, Size: integer) of object;

const
  SINGLEQUOTE = '''';

type
  THTMLDecoder = class
  private
    class var fEntitiDic: TDictionary<String, integer>;
    class Constructor CreateClass;
    class destructor DestroyClass;
  strict private
  private
    Src: String;
    CurrentSrcPos, CurrentResultPos: integer;
    Result: string;

    procedure PerformDecode(const aSrc: String);
    procedure _CopyCurrentSrcPosCharToResult;
    procedure _CopyCharToResult(aUnicodeOrdEntity: integer; aNewCurrentSrcPos: integer);
    Function TryStrToInt(Str: string; out value: integer): boolean;
  public
    class function Decode(const aHtml: string): string;
  end;

function URL2FileName(const url: string): string; overload;
function URL2FileName(const url, Path, Ext: string): string; overload;

function ConCatURL(const s1, s2: string): string;
Function HtmlEncode(const aSrc: string): String;
procedure ExtractLinks(const Htmlbody: string; Links: TStringList; OnProgres: TExtractProgressEvent = nil);
procedure DecodeLink(const ParentURL, aLink: string; Out url, Title: string);
Function ExtractListItems(const HTML: String; Items: TStrings): boolean;
// this one do NOT remove the http://www. part neither the last backslash
function ExtractURLRoot(const url: string): String;

// the result is a pure domain name, without http://www. or a backslash at the end
function ExtractURLDomain(const url: string): String;
function ExtractURLPath(const url: string): string;
function PreHttp(const s: string): String;
function isPreHttp(const url: string): boolean;
Function RemPreHttp(const url: string): String;
function BackSlash(const s: string): string;
function NoBackSlash(const s: string): string;
function MakeValidURL(const url: string): String;
function ExtractAttribut(const Tag, Attribute: string; TagLen, AttributeLen: integer): String;
function Extract(Const body, sTag, eTag: string; bLen: integer;
  Var Offset: integer; out SubStr: string): boolean;
function IsQuoteChar(CheckChar: char; out QuoteChar: char): boolean;
function HTMLDecode(const Src: String): String;

// Return only the HTML of a string
Function ExtractHTML(s: String): String;
// converts a given html to plain text
Function HtmlToText(s: String): String;
// Take all HTML out of a string
Function ExtractNonHTML(s: String): String;
// Used by ExtractHTML and ExtractNonHTML
Function StripHTMLorNonHTML(s: String; WantHTML: boolean): String;

implementation

uses
  MaxLogic.ioUtils, REST.Utils, MaxLogic.StrUtils, autoFree;

Const
  Chars: string = '~!@#$%^&()+|]}[{''";:/?>,<';

CONST
  EncChars: string = '%7e%21%40%23%24%25%5e%26%28%29%2b%7c%5d%7d%5b%7b%27%22%3b%3a%2f%3f%3e%2c%3c';

function HTMLDecode(const Src: String): String;
begin
  Result := THTMLDecoder.Decode(Src)
end;

function IsQuoteChar(CheckChar: char; out QuoteChar: char): boolean;
begin
  if (CheckChar = '"') then
  begin
    Result := True;
    QuoteChar := '"';
  end
  else if CheckChar = SINGLEQUOTE then
  begin
    Result := True;
    QuoteChar := SINGLEQUOTE;
  end
  else
    Result := false;
end;

function URL2FileName(const url, Path, Ext: string): string;
begin
  Result := url;
  for var lPrefix in ['http://', 'https://', 'www.'] do
    if StartsText(lPrefix, Result) then
      delete(Result, 1, Length(lPrefix));

  Result := ReplaceText(Result, '/', '_$S_');
  Result := ReplaceText(Result, '\', '_$S_');
  Result := ReplaceText(Result, ':', '_$D_');
  Result := ReplaceText(Result, '?', '_$Q_');

  Result := Slash(Path) + Result + Ext;
end;

function URL2FileName(const url: string): string;
var
  Ext: string;
begin
  Ext := AnsiLowercase(ExtractFileExt(Result));
  IF (Ext <> '.htm') and (Ext <> '.html') and
    (Ext <> '.txt') and (Ext <> '.pdf') and (Ext <> '.jgp') and
    (Ext <> '.gif') and (Ext <> '.png') and (Ext <> '.css') and (Ext <> '.shtml')
  THEN
    Ext := '.html';

  Result := URL2FileName(url, '', Ext);
end;

function ExtractAttribut(const Tag, Attribute: string; TagLen, AttributeLen: integer): String;
var
  a: string;
  i, st, et: integer;
  QuoteChar: char;
  lLowercasedTag: String;
begin
  lLowercasedTag := AnsiLowercase(Tag);
  if Attribute[AttributeLen] <> '=' then
  begin
    a := ConCat(Attribute, '=');
    Inc(AttributeLen);
  end
  else
    a := Attribute;
  a := AnsiLowercase(a);

  if a[1] <> ' ' then
  begin
    insert(' ', a, 1);
    Inc(AttributeLen);
  end;

  if (TagLen <= 0) or (AttributeLen <= 0) then
    i := -1
  else
    i := Pos(lLowercasedTag, a);

  if i <= 0 then
    Result := ''
  else begin
    Inc(i, AttributeLen);
    st := i;

    If IsQuoteChar(Tag[i], QuoteChar) then
    begin
      Inc(st);
      et := posEx(lLowercasedTag, QuoteChar, st)
    end else begin
      et := posEx(lLowercasedTag, ' ', i + 1);
      i := posEx(lLowercasedTag, '>', i + 1);
      if et <= 0 then
        et := TagLen + 1;
      if i <= 0 then
        i := TagLen + 1;
      if i < et then
        et := i;
    end;
    Result := copy(Tag, st, et - st);
  end;
end;

procedure ExtractLinks(const Htmlbody: string; Links: TStringList; OnProgres: TExtractProgressEvent = nil);
var
  e, i, Offset, bLen: integer;
  Link: string;
  LowercasedBody: string;
begin
  LowercasedBody := AnsiLowercase(Htmlbody);
  Offset := 1;
  bLen := Length(Htmlbody);
  if Assigned(OnProgres) then
    OnProgres(0, bLen);

  WHILE Offset < bLen do
  Begin
    Offset := posEx('<a ', LowercasedBody, Offset);
    IF Offset <= 0 then
      break;
    i := Offset;
    e := posEx('</a>', LowercasedBody, Offset) + 4;
    Link := copy(Htmlbody, i, e - i);
    Offset := e + 1;
    Links.add(Link);

    if Assigned(OnProgres) then
      OnProgres(e, bLen);
  End;
end;

procedure DecodeLink(const ParentURL, aLink: string; out url, Title: string);
var
  l, i, e: integer;
  QuoteChar: char;
  lLowercasedLink: String;
begin
  lLowercasedLink := AnsiLowercase(aLink);
  l := Length(aLink);
  i := posEx(lLowercasedLink, ' href=', 1) + 6;

  if IsQuoteChar(aLink[i], QuoteChar) then
  begin
    Inc(i);
    e := posEx(lLowercasedLink, QuoteChar, i);
    url := copy(aLink, i, e - i);
  end else begin

    e := i;
    WHILE (aLink[e] <> ' ') and (aLink[e] <> '>') do
    begin
      url := copy(aLink, i, e - i);
      Inc(e);
    end;
  end;
  Title := ExtractNonHTML(aLink);
  Title := HTMLDecode(Title);
  if not isPreHttp(url) then
  begin
    if AnsiLowercase(copy(url, 1, 4)) = 'www.' then
      url := ConCat('http://' + url)
    else if (url <> '') and (url[1] = '/') then
      url := ConCat(NoBackSlash(ExtractURLRoot(ParentURL)), url)
    else
      url := ConCat(BackSlash(ExtractURLPath(ParentURL)), url);
  end;
end;

function ExtractURLRoot(const url: string): String;
var
  s: string;
  i: integer;
begin
  if isPreHttp(url) then
    s := copy(url, 8, Length(url))
  else
    s := url;
  i := posEx(s, '/', 1);
  if i <= 0 then
    Result := url
  else
    Result := PreHttp(copy(s, 1, i));
end;

function ExtractURLDomain(const url: string): String;
var
  s: string;
  i: integer;
begin
  s := url;
  if 'http://' = AnsiLowercase(copy(s, 1, 7)) then
    delete(s, 1, 7);
  if 'www.' = AnsiLowercase(copy(s, 1, 4)) then
    delete(s, 1, 4);

  i := posEx(s, '/', 1);
  if i <= 0 then
    Result := s
  else
    Result := copy(s, 1, i - 1);

end;

function ExtractURLPath(const url: string): string;
var
  s: string;
  i: integer;
  slen: integer;
begin
  s := PreHttp(url);
  slen := Length(s);
  i := Pos(ReverseString(s), '/');
  IF i > 8 then
    Result := copy(s, 1, i)
  ELSE
    Result := s;

end;

function PreHttp(const s: string): String;
begin
  if AnsiLowercase(copy(s, 1, 7)) <> 'http://' then
    Result := ConCat('http://', s)
  else
    Result := s;
end;

function isPreHttp(const url: string): boolean;
begin
  Result := AnsiLowercase(copy(url, 1, 7)) = 'http://'
end;

Function RemPreHttp(const url: string): String;
begin
  if isPreHttp(url) then
    Result := copy(url, 8, Length(url))
  else
    Result := url;
end;

function BackSlash(const s: string): string;
begin
  if (s <> '') and (s[Length(s)] <> '/') then
    Result := ConCat(s, '/')
  else
    Result := s;

end;

function NoBackSlash(const s: string): string;
begin
  if (s <> '') and (s[Length(s)] = '/') then
    Result := copy(s, 1, Length(s))
  else
    Result := s;
end;

function MakeValidURL(const url: string): String;
begin
  if not isPreHttp(url) then
    Result := ConCat('http://', url)
  else
    Result := url;
end;

Function HtmlEncode(const aSrc: string): String;
{ var
  i: integer;
  const
  NoConversion =['A' .. 'Z', 'a' .. 'z', '@', '.', '_', '-', '0', '1' .. '9'];
}
begin
  Result :=
    REST.Utils.URIEncode(aSrc);

  { Result := '';
    for i := 1 to Length(aSrc) do
    begin
    // Changed the parameter encoding: Even in parameters, a space
    // is much more likely to be meaning "space" than "this is
    // a new parameter"
    // ref: Message-ID: <3de30169@newsgroups.borland.com> borland.public.delphi.internet.winsock
    // Most low-ascii is actually Ok in parameters encoding.

    if not CharInSet(aSrc[i], NoConversion) then
    begin
    Result := Result + '%' + IntToHex(Ord(aSrc[i]), 2);
    end else begin
    Result := Result + aSrc[i];
    end;
    end;
  }
end;

function ConCatURL(const s1, s2: string): string;
begin
  IF (s2 <> '') and (s2[1] = '/') then
  Begin
    Result := s2;
    delete(Result, 1, 1);
  end
  else
    Result := s2;

  if (s1 <> '') and (s1[Length(s1)] = '/') then
    Result := s1 + Result
  else
    Result := s1 + '/' + Result;
end;

function Extract(Const body, sTag, eTag: string; bLen: integer;
  Var Offset: integer; out SubStr: string): boolean;
VAR
  x, st, i, len: integer;
  l: TStringList;
  s: String;
  Failed: boolean;
  lLowercasedBody, lLowercasedStartTag, lLowercasedEndTag: String;
begin
  lLowercasedBody := AnsiLowercase(body);
  lLowercasedStartTag := AnsiLowercase(sTag);
  lLowercasedEndTag := AnsiLowercase(eTag);

  Failed := false;
  Result := false;
  SubStr := '';
  gc(l, TStringList.Create);
  MaxLogic.StrUtils.Split(sTag, '*', l);

  i := Offset;
  For x := 0 to l.count - 1 do
  Begin
    s := l[x];
    len := Length(s);
    i := posEx(lLowercasedBody, AnsiLowercase(s), i);
    if i <= 0 then
    begin
      Failed := True;
      break;
    end;
    Inc(i, len);
  end;
  st := i;

  if Failed then
    Exit;

  i := posEx(lLowercasedBody, lLowercasedEndTag, st);
  if i <= 0 then
    Exit;

  SubStr := copy(body, st, i - st);
  Offset := i;
  Result := SubStr <> '';
end;

// This will extract all items from a list enumeration
// example:
// html := '<ul><li>.NET</li><li>C/C++</li><li>J2EE</li><li>JSP</li><li>Windows</li></ul>';
// ExtractListItems(html, items);
// NOW ITEMS CONTAILS THE FOLLOWING STRINGS : ['.NET', 'C/C++', 'J2EE', 'JSP', 'Windows']
Function ExtractListItems(const HTML: String; Items: TStrings): boolean;
var
  bLen, len, i, Offset: integer;
  item: string;
  lLowercasedHtml: String;
begin
  lLowercasedHtml := AnsiLowercase(HTML);
  bLen := Length(lLowercasedHtml);
  Offset := 1;
  i := posEx(lLowercasedHtml, '<li>', Offset);
  while (i > 0) and (i < bLen) do
  begin
    Offset := i + 4;
    i := posEx(lLowercasedHtml, '<li>', Offset);
    len := i - Offset;
    if len <= 0 then
      len := bLen - Offset;
    item := copy(HTML, Offset, len);
    item := Trim(ReplaceText(item, '</li>', ''));
    item := Trim(ReplaceText(item, '</ul>', ''));
    Items.add(item);
  end;
  Result := Items.count > 0;
end;

{ THTMLDecoder }

procedure THTMLDecoder._CopyCurrentSrcPosCharToResult;
Begin
  Result[CurrentResultPos] := Src[CurrentSrcPos];
  Inc(CurrentResultPos);
  Inc(CurrentSrcPos);
end;

procedure THTMLDecoder._CopyCharToResult(aUnicodeOrdEntity: integer; aNewCurrentSrcPos: integer);
Var
  aUTF8String: String;
  K: integer;
Begin
  aUTF8String := WideChar(aUnicodeOrdEntity);
  For K := 1 to Length(aUTF8String) do
  begin
    Result[CurrentResultPos] := aUTF8String[K];
    Inc(CurrentResultPos);
  end;
  CurrentSrcPos := aNewCurrentSrcPos;
end;

class destructor THTMLDecoder.DestroyClass;
begin
  if fEntitiDic <> nil then
  begin
    fEntitiDic.free;
    fEntitiDic := nil;
  end;
end;

procedure THTMLDecoder.PerformDecode(const aSrc: String);
var
  j: integer;
  aTmpInteger: integer;
  SrcLength: integer;
begin
  { init var }
  Src := aSrc;
  CurrentSrcPos := 1;
  CurrentResultPos := 1;
  SrcLength := Length(Src);
  SetLength(Result, SrcLength);

  { start loop }
  while (CurrentSrcPos <= SrcLength) do
  begin

    { HTMLentity detected }
    If Src[CurrentSrcPos] = '&' then
    begin

      { extract the HTML entity }
      j := CurrentSrcPos;
      while (j <= SrcLength) and (Src[j] <> ';') and (j - CurrentSrcPos <= 12) do
        Inc(j);

      { HTML entity is valid }
      If (j <= SrcLength) and (j - CurrentSrcPos <= 12) then
      Begin

        { HTML entity is numeric }
        IF (Src[CurrentSrcPos + 1] = '#') then
        begin

          { HTML entity is hexa }
          IF (Src[CurrentSrcPos + 2] = 'x') then
          begin
            if TryStrToInt('$' + copy(Src,
                CurrentSrcPos + 3,
                j - CurrentSrcPos - 3),
              aTmpInteger)
            then
              _CopyCharToResult(aTmpInteger, j + 1)
            else
              _CopyCurrentSrcPosCharToResult;
          end

          { HTML entity is numeric }
          else
          begin

            { numeric HTML entity is valid }
            if TryStrToInt(copy(Src,
                CurrentSrcPos + 2,
                j - CurrentSrcPos - 2),
              aTmpInteger)
            then
              _CopyCharToResult(aTmpInteger, j + 1)
            else
              _CopyCurrentSrcPosCharToResult;

          end;

        end

        { HTML entity is litteral }
        else
        begin

          if fEntitiDic.TryGetValue(copy(Src,
              CurrentSrcPos + 1,
              j - CurrentSrcPos - 1), aTmpInteger)
          then
            _CopyCharToResult(aTmpInteger, j + 1)
          else
            _CopyCurrentSrcPosCharToResult;

        end;

      end
      else
        _CopyCurrentSrcPosCharToResult;

    end
    else
      _CopyCurrentSrcPosCharToResult;

  end;

  SetLength(Result, CurrentResultPos - 1);
end;

function THTMLDecoder.TryStrToInt(Str: string; out value: integer): boolean;
const
  invalid = -123456789;
var
  i: integer;
begin
  i := StrToIntDef(Str, -123456789);
  if i <> invalid then
  begin
    Result := True;
    value := i;
  end
  else
    Result := false;
end;

class constructor THTMLDecoder.CreateClass;
Begin
  fEntitiDic := TDictionary<String, integer>.Create;

  fEntitiDic.add('zwnj', (8204)); // zero width non-joiner,   U+200C NEW RFC 2070 -->
  fEntitiDic.add('zwj', (8205)); // zero width joiner, U+200D NEW RFC 2070 -->
  fEntitiDic.add('zeta', (950)); // greek small letter zeta, U+03B6 ISOgrk3 -->
  fEntitiDic.add('Zeta', (918)); // greek capital letter zeta, U+0396 -->
  fEntitiDic.add('yuml', (255)); // latin small letter y with diaeresis, U+00FF ISOlat1 -->
  fEntitiDic.add('Yuml', (376)); // latin capital letter Y with diaeresis,   U+0178 ISOlat2 -->
  fEntitiDic.add('yen', (165)); // yen sign = yuan sign, U+00A5 ISOnum -->
  fEntitiDic.add('yacute', (253)); // latin small letter y with acute, U+00FD ISOlat1 -->
  fEntitiDic.add('Yacute', (221)); // latin capital letter Y with acute, U+00DD ISOlat1 -->
  fEntitiDic.add('xi', (958)); // greek small letter xi, U+03BE ISOgrk3 -->
  fEntitiDic.add('Xi', (926)); // greek capital letter xi, U+039E ISOgrk3 -->
  fEntitiDic.add('weierp', (8472)); // script capital P = power set    = Weierstrass p, U+2118 ISOamso -->
  fEntitiDic.add('uuml', (252)); // latin small letter u with diaeresis, U+00FC ISOlat1 -->
  fEntitiDic.add('Uuml', (220)); // latin capital letter U with diaeresis, U+00DC ISOlat1 -->
  fEntitiDic.add('upsilon', (965)); // greek small letter upsilon,   U+03C5 ISOgrk3 -->
  fEntitiDic.add('Upsilon', (933)); // greek capital letter upsilon,   U+03A5 ISOgrk3 -->
  fEntitiDic.add('upsih', (978)); // greek upsilon with hook symbol,   U+03D2 NEW -->
  fEntitiDic.add('uml', (168)); // diaeresis = spacing diaeresis, U+00A8 ISOdia -->
  fEntitiDic.add('ugrave', (249)); // latin small letter u with grave, U+00F9 ISOlat1 -->
  fEntitiDic.add('Ugrave', (217)); // latin capital letter U with grave, U+00D9 ISOlat1 -->
  fEntitiDic.add('ucirc', (251)); // latin small letter u with circumflex, U+00FB ISOlat1 -->
  fEntitiDic.add('Ucirc', (219)); // latin capital letter U with circumflex, U+00DB ISOlat1 -->
  fEntitiDic.add('uArr', (8657)); // upwards double arrow, U+21D1 ISOamsa -->
  fEntitiDic.add('uarr', (8593)); // upwards arrow, U+2191 ISOnum-->
  fEntitiDic.add('uacute', (250)); // latin small letter u with acute, U+00FA ISOlat1 -->
  fEntitiDic.add('Uacute', (218)); // latin capital letter U with acute, U+00DA ISOlat1 -->
  fEntitiDic.add('trade', (8482)); // trade mark sign, U+2122 ISOnum -->
  fEntitiDic.add('times', (215)); // multiplication sign, U+00D7 ISOnum -->
  fEntitiDic.add('tilde', (732)); // small tilde, U+02DC ISOdia -->
  fEntitiDic.add('thorn', (254)); // latin small letter thorn, U+00FE ISOlat1 -->
  fEntitiDic.add('THORN', (222)); // latin capital letter THORN, U+00DE ISOlat1 -->
  fEntitiDic.add('thinsp', (8201)); // thin space, U+2009 ISOpub -->
  fEntitiDic.add('thetasym', (977)); // greek small letter theta symbol,   U+03D1 NEW -->
  fEntitiDic.add('theta', (952)); // greek small letter theta,   U+03B8 ISOgrk3 -->
  fEntitiDic.add('Theta', (920)); // greek capital letter theta,   U+0398 ISOgrk3 -->
  fEntitiDic.add('there4', (8756)); // therefore, U+2234 ISOtech -->
  fEntitiDic.add('tau', (964)); // greek small letter tau, U+03C4 ISOgrk3 -->
  fEntitiDic.add('Tau', (932)); // greek capital letter tau, U+03A4 -->
  fEntitiDic.add('szlig', (223)); // latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
  fEntitiDic.add('supe', (8839)); // superset of or equal to,    U+2287 ISOtech -->
  fEntitiDic.add('sup3', (179)); // superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
  fEntitiDic.add('sup2', (178)); // superscript two = superscript digit two = squared, U+00B2 ISOnum -->
  fEntitiDic.add('sup1', (185)); // superscript one = superscript digit one, U+00B9 ISOnum -->
  fEntitiDic.add('sup', (8835)); // superset of, U+2283 ISOtech -->
  fEntitiDic.add('sum', (8721)); // n-ary sumation, U+2211 ISOamsb -->
  fEntitiDic.add('sube', (8838)); // subset of or equal to, U+2286 ISOtech -->
  fEntitiDic.add('sub', (8834)); // subset of, U+2282 ISOtech -->
  fEntitiDic.add('spades', (9824)); // black spade suit, U+2660 ISOpub -->
  fEntitiDic.add('sim', (8764)); // tilde operator = varies with = similar to,    U+223C ISOtech -->
  fEntitiDic.add('sigmaf', (962)); // greek small letter final sigma,   U+03C2 ISOgrk3 -->
  fEntitiDic.add('sigma', (963)); // greek small letter sigma,   U+03C3 ISOgrk3 -->
  fEntitiDic.add('Sigma', (931)); // greek capital letter sigma,   U+03A3 ISOgrk3 -->
  fEntitiDic.add('shy', (173)); // soft hyphen = discretionary hyphen, U+00AD ISOnum -->
  fEntitiDic.add('sect', (167)); // section sign, U+00A7 ISOnum -->
  fEntitiDic.add('sdot', (8901)); // dot operator, U+22C5 ISOamsb -->
  fEntitiDic.add('scaron', (353)); // latin small letter s with caron,   U+0161 ISOlat2 -->
  fEntitiDic.add('Scaron', (352)); // latin capital letter S with caron,   U+0160 ISOlat2 -->
  fEntitiDic.add('sbquo', (8218)); // single low-9 quotation mark, U+201A NEW -->
  fEntitiDic.add('rsquo', (8217)); // right single quotation mark,   U+2019 ISOnum -->
  fEntitiDic.add('rsaquo', (8250)); // single right-pointing angle quotation mark,   U+203A ISO proposed -->
  fEntitiDic.add('rlm', (8207)); // right-to-left mark, U+200F NEW RFC 2070 -->
  fEntitiDic.add('rho', (961)); // greek small letter rho, U+03C1 ISOgrk3 -->
  fEntitiDic.add('Rho', (929)); // greek capital letter rho, U+03A1 -->
  fEntitiDic.add('rfloor', (8971)); // right floor, U+230B ISOamsc  -->
  fEntitiDic.add('reg', (174)); // registered sign = registered trade mark sign, U+00AE ISOnum -->
  fEntitiDic.add('real', (8476)); // blackletter capital R = real part symbol,    U+211C ISOamso -->
  fEntitiDic.add('rdquo', (8221)); // right double quotation mark,   U+201D ISOnum -->
  fEntitiDic.add('rceil', (8969)); // right ceiling, U+2309 ISOamsc  -->
  fEntitiDic.add('rArr', (8658)); // rightwards double arrow,    U+21D2 ISOtech -->
  fEntitiDic.add('rarr', (8594)); // rightwards arrow, U+2192 ISOnum -->
  fEntitiDic.add('raquo', (187)); // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
  fEntitiDic.add('rang', (9002)); // right-pointing angle bracket = ket,    U+232A ISOtech -->
  fEntitiDic.add('radic', (8730)); // square root = radical sign,    U+221A ISOtech -->
  fEntitiDic.add('quot', (34)); // quotation mark = APL quote,   U+0022 ISOnum -->
  fEntitiDic.add('psi', (968)); // greek small letter psi, U+03C8 ISOgrk3 -->
  fEntitiDic.add('Psi', (936)); // greek capital letter psi,   U+03A8 ISOgrk3 -->
  fEntitiDic.add('prop', (8733)); // proportional to, U+221D ISOtech -->
  fEntitiDic.add('prod', (8719)); // n-ary product = product sign,    U+220F ISOamsb -->
  fEntitiDic.add('Prime', (8243)); // double prime = seconds = inches,    U+2033 ISOtech -->
  fEntitiDic.add('prime', (8242)); // prime = minutes = feet, U+2032 ISOtech -->
  fEntitiDic.add('pound', (163)); // pound sign, U+00A3 ISOnum -->
  fEntitiDic.add('plusmn', (177)); // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->
  fEntitiDic.add('piv', (982)); // greek pi symbol, U+03D6 ISOgrk3 -->
  fEntitiDic.add('pi', (960)); // greek small letter pi, U+03C0 ISOgrk3 -->
  fEntitiDic.add('Pi', (928)); // greek capital letter pi, U+03A0 ISOgrk3 -->
  fEntitiDic.add('phi', (966)); // greek small letter phi, U+03C6 ISOgrk3 -->
  fEntitiDic.add('Phi', (934)); // greek capital letter phi,   U+03A6 ISOgrk3 -->
  fEntitiDic.add('perp', (8869)); // up tack = orthogonal to = perpendicular,    U+22A5 ISOtech -->
  fEntitiDic.add('permil', (8240)); // per mille sign, U+2030 ISOtech -->
  fEntitiDic.add('part', (8706)); // partial differential, U+2202 ISOtech  -->
  fEntitiDic.add('para', (182)); // pilcrow sign = paragraph sign, U+00B6 ISOnum -->
  fEntitiDic.add('ouml', (246)); // latin small letter o with diaeresis, U+00F6 ISOlat1 -->
  fEntitiDic.add('Ouml', (214)); // latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
  fEntitiDic.add('otimes', (8855)); // circled times = vector product,    U+2297 ISOamsb -->
  fEntitiDic.add('otilde', (245)); // latin small letter o with tilde, U+00F5 ISOlat1 -->
  fEntitiDic.add('Otilde', (213)); // latin capital letter O with tilde, U+00D5 ISOlat1 -->
  fEntitiDic.add('oslash', (248)); // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1 -->
  fEntitiDic.add('Oslash', (216)); // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
  fEntitiDic.add('ordm', (186)); // masculine ordinal indicator, U+00BA ISOnum -->
  fEntitiDic.add('ordf', (170)); // feminine ordinal indicator, U+00AA ISOnum -->
  fEntitiDic.add('or', (8744)); // logical or = vee, U+2228 ISOtech -->
  fEntitiDic.add('oplus', (8853)); // circled plus = direct sum,    U+2295 ISOamsb -->
  fEntitiDic.add('omicron', (959)); // greek small letter omicron, U+03BF NEW -->
  fEntitiDic.add('Omicron', (927)); // greek capital letter omicron, U+039F -->
  fEntitiDic.add('omega', (969)); // greek small letter omega,   U+03C9 ISOgrk3 -->
  fEntitiDic.add('Omega', (937)); // greek capital letter omega,   U+03A9 ISOgrk3 -->
  fEntitiDic.add('oline', (8254)); // overline = spacing overscore,    U+203E NEW -->
  fEntitiDic.add('ograve', (242)); // latin small letter o with grave, U+00F2 ISOlat1 -->
  fEntitiDic.add('Ograve', (210)); // latin capital letter O with grave, U+00D2 ISOlat1 -->
  fEntitiDic.add('oelig', (339)); // latin small ligature oe, U+0153 ISOlat2 -->
  fEntitiDic.add('OElig', (338)); // latin capital ligature OE,   U+0152 ISOlat2 -->
  fEntitiDic.add('ocirc', (244)); // latin small letter o with circumflex, U+00F4 ISOlat1 -->
  fEntitiDic.add('Ocirc', (212)); // latin capital letter O with circumflex, U+00D4 ISOlat1 -->
  fEntitiDic.add('oacute', (243)); // latin small letter o with acute, U+00F3 ISOlat1 -->
  fEntitiDic.add('Oacute', (211)); // latin capital letter O with acute, U+00D3 ISOlat1 -->
  fEntitiDic.add('nu', (957)); // greek small letter nu, U+03BD ISOgrk3 -->
  fEntitiDic.add('Nu', (925)); // greek capital letter nu, U+039D -->
  fEntitiDic.add('ntilde', (241)); // latin small letter n with tilde, U+00F1 ISOlat1 -->
  fEntitiDic.add('Ntilde', (209)); // latin capital letter N with tilde, U+00D1 ISOlat1 -->
  fEntitiDic.add('nsub', (8836)); // not a subset of, U+2284 ISOamsn -->
  fEntitiDic.add('notin', (8713)); // not an element of, U+2209 ISOtech -->
  fEntitiDic.add('not', (172)); // not sign, U+00AC ISOnum -->
  fEntitiDic.add('ni', (8715)); // contains as member, U+220B ISOtech -->
  fEntitiDic.add('ne', (8800)); // not equal to, U+2260 ISOtech -->
  fEntitiDic.add('ndash', (8211)); // en dash, U+2013 ISOpub -->
  fEntitiDic.add('nbsp', (160)); // no-break space = non-breaking space, U+00A0 ISOnum -->
  fEntitiDic.add('nabla', (8711)); // nabla = backward difference,    U+2207 ISOtech -->
  fEntitiDic.add('mu', (956)); // greek small letter mu, U+03BC ISOgrk3 -->
  fEntitiDic.add('Mu', (924)); // greek capital letter mu, U+039C -->
  fEntitiDic.add('minus', (8722)); // minus sign, U+2212 ISOtech -->
  fEntitiDic.add('middot', (183)); // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
  fEntitiDic.add('micro', (181)); // micro sign, U+00B5 ISOnum -->
  fEntitiDic.add('mdash', (8212)); // em dash, U+2014 ISOpub -->
  fEntitiDic.add('macr', (175)); // macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->
  fEntitiDic.add('lt', (60)); // less-than sign, U+003C ISOnum -->
  fEntitiDic.add('lsquo', (8216)); // left single quotation mark,   U+2018 ISOnum -->
  fEntitiDic.add('lsaquo', (8249)); // single left-pointing angle quotation mark,   U+2039 ISO proposed -->
  fEntitiDic.add('lrm', (8206)); // left-to-right mark, U+200E NEW RFC 2070 -->
  fEntitiDic.add('loz', (9674)); // lozenge, U+25CA ISOpub -->
  fEntitiDic.add('lowast', (8727)); // asterisk operator, U+2217 ISOtech -->
  fEntitiDic.add('lfloor', (8970)); // left floor = apl downstile,    U+230A ISOamsc  -->
  fEntitiDic.add('le', (8804)); // less-than or equal to, U+2264 ISOtech -->
  fEntitiDic.add('ldquo', (8220)); // left double quotation mark,   U+201C ISOnum -->
  fEntitiDic.add('lceil', (8968)); // left ceiling = apl upstile,    U+2308 ISOamsc  -->
  fEntitiDic.add('lArr', (8656)); // leftwards double arrow, U+21D0 ISOtech -->
  fEntitiDic.add('larr', (8592)); // leftwards arrow, U+2190 ISOnum -->
  fEntitiDic.add('laquo', (171)); // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
  fEntitiDic.add('lang', (9001)); // left-pointing angle bracket = bra,    U+2329 ISOtech -->
  fEntitiDic.add('lambda', (955)); // greek small letter lambda,   U+03BB ISOgrk3 -->
  fEntitiDic.add('Lambda', (923)); // greek capital letter lambda,   U+039B ISOgrk3 -->
  fEntitiDic.add('kappa', (954)); // greek small letter kappa,   U+03BA ISOgrk3 -->
  fEntitiDic.add('Kappa', (922)); // greek capital letter kappa, U+039A -->
  fEntitiDic.add('iuml', (239)); // latin small letter i with diaeresis, U+00EF ISOlat1 -->
  fEntitiDic.add('Iuml', (207)); // latin capital letter I with diaeresis, U+00CF ISOlat1 -->
  fEntitiDic.add('isin', (8712)); // element of, U+2208 ISOtech -->
  fEntitiDic.add('iquest', (191)); // inverted question mark = turned question mark, U+00BF ISOnum -->
  fEntitiDic.add('iota', (953)); // greek small letter iota, U+03B9 ISOgrk3 -->
  fEntitiDic.add('Iota', (921)); // greek capital letter iota, U+0399 -->
  fEntitiDic.add('int', (8747)); // integral, U+222B ISOtech -->
  fEntitiDic.add('infin', (8734)); // infinity, U+221E ISOtech -->
  fEntitiDic.add('image', (8465)); // blackletter capital I = imaginary part,    U+2111 ISOamso -->
  fEntitiDic.add('igrave', (236)); // latin small letter i with grave, U+00EC ISOlat1 -->
  fEntitiDic.add('Igrave', (204)); // latin capital letter I with grave, U+00CC ISOlat1 -->
  fEntitiDic.add('iexcl', (161)); // inverted exclamation mark, U+00A1 ISOnum -->
  fEntitiDic.add('icirc', (238)); // latin small letter i with circumflex, U+00EE ISOlat1 -->
  fEntitiDic.add('Icirc', (206)); // latin capital letter I with circumflex, U+00CE ISOlat1 -->
  fEntitiDic.add('iacute', (237)); // latin small letter i with acute, U+00ED ISOlat1 -->
  fEntitiDic.add('Iacute', (205)); // latin capital letter I with acute, U+00CD ISOlat1 -->
  fEntitiDic.add('hellip', (8230)); // horizontal ellipsis = three dot leader,    U+2026 ISOpub  -->
  fEntitiDic.add('hearts', (9829)); // black heart suit = valentine,    U+2665 ISOpub -->
  fEntitiDic.add('hArr', (8660)); // left right double arrow,    U+21D4 ISOamsa -->
  fEntitiDic.add('harr', (8596)); // left right arrow, U+2194 ISOamsa -->
  fEntitiDic.add('gt', (62)); // greater-than sign, U+003E ISOnum -->
  fEntitiDic.add('ge', (8805)); // greater-than or equal to,    U+2265 ISOtech -->
  fEntitiDic.add('gamma', (947)); // greek small letter gamma,   U+03B3 ISOgrk3 -->
  fEntitiDic.add('Gamma', (915)); // greek capital letter gamma,   U+0393 ISOgrk3 -->
  fEntitiDic.add('frasl', (8260)); // fraction slash, U+2044 NEW -->
  fEntitiDic.add('frac34', (190)); // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
  fEntitiDic.add('frac14', (188)); // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
  fEntitiDic.add('frac12', (189)); // vulgar fraction one half = fraction one half, U+00BD ISOnum -->
  fEntitiDic.add('forall', (8704)); // for all, U+2200 ISOtech -->
  fEntitiDic.add('fnof', (402)); // latin small f with hook = function   = florin, U+0192 ISOtech -->
  fEntitiDic.add('exist', (8707)); // there exists, U+2203 ISOtech -->
  fEntitiDic.add('euro', (8364)); // euro sign, U+20AC NEW -->
  fEntitiDic.add('euml', (235)); // latin small letter e with diaeresis, U+00EB ISOlat1 -->
  fEntitiDic.add('Euml', (203)); // latin capital letter E with diaeresis, U+00CB ISOlat1 -->
  fEntitiDic.add('eth', (240)); // latin small letter eth, U+00F0 ISOlat1 -->
  fEntitiDic.add('ETH', (208)); // latin capital letter ETH, U+00D0 ISOlat1 -->
  fEntitiDic.add('eta', (951)); // greek small letter eta, U+03B7 ISOgrk3 -->
  fEntitiDic.add('Eta', (919)); // greek capital letter eta, U+0397 -->
  fEntitiDic.add('equiv', (8801)); // identical to, U+2261 ISOtech -->
  fEntitiDic.add('epsilon', (949)); // greek small letter epsilon,   U+03B5 ISOgrk3 -->
  fEntitiDic.add('Epsilon', (917)); // greek capital letter epsilon, U+0395 -->
  fEntitiDic.add('ensp', (8194)); // en space, U+2002 ISOpub -->
  fEntitiDic.add('emsp', (8195)); // em space, U+2003 ISOpub -->
  fEntitiDic.add('empty', (8709)); // empty set = null set = diameter,    U+2205 ISOamso -->
  fEntitiDic.add('egrave', (232)); // latin small letter e with grave, U+00E8 ISOlat1 -->
  fEntitiDic.add('Egrave', (200)); // latin capital letter E with grave, U+00C8 ISOlat1 -->
  fEntitiDic.add('ecirc', (234)); // latin small letter e with circumflex, U+00EA ISOlat1 -->
  fEntitiDic.add('Ecirc', (202)); // latin capital letter E with circumflex, U+00CA ISOlat1 -->
  fEntitiDic.add('eacute', (233)); // latin small letter e with acute, U+00E9 ISOlat1 -->
  fEntitiDic.add('Eacute', (201)); // latin capital letter E with acute, U+00C9 ISOlat1 -->
  fEntitiDic.add('divide', (247)); // division sign, U+00F7 ISOnum -->
  fEntitiDic.add('diams', (9830)); // black diamond suit, U+2666 ISOpub -->
  fEntitiDic.add('delta', (948)); // greek small letter delta,   U+03B4 ISOgrk3 -->
  fEntitiDic.add('Delta', (916)); // greek capital letter delta,   U+0394 ISOgrk3 -->
  fEntitiDic.add('deg', (176)); // degree sign, U+00B0 ISOnum -->
  fEntitiDic.add('dArr', (8659)); // downwards double arrow, U+21D3 ISOamsa -->
  fEntitiDic.add('darr', (8595)); // downwards arrow, U+2193 ISOnum -->
  fEntitiDic.add('Dagger', (8225)); // double dagger, U+2021 ISOpub -->
  fEntitiDic.add('dagger', (8224)); // dagger, U+2020 ISOpub -->
  fEntitiDic.add('curren', (164)); // currency sign, U+00A4 ISOnum -->
  fEntitiDic.add('cup', (8746)); // union = cup, U+222A ISOtech -->
  fEntitiDic.add('crarr', (8629)); // downwards arrow with corner leftwards    = carriage return, U+21B5 NEW -->
  fEntitiDic.add('copy', (169)); // copyright sign, U+00A9 ISOnum -->
  fEntitiDic.add('cong', (8773)); // approximately equal to, U+2245 ISOtech -->
  fEntitiDic.add('clubs', (9827)); // black club suit = shamrock,    U+2663 ISOpub -->
  fEntitiDic.add('circ', (710)); // modifier letter circumflex accent,   U+02C6 ISOpub -->
  fEntitiDic.add('chi', (967)); // greek small letter chi, U+03C7 ISOgrk3 -->
  fEntitiDic.add('Chi', (935)); // greek capital letter chi, U+03A7 -->
  fEntitiDic.add('cent', (162)); // cent sign, U+00A2 ISOnum -->
  fEntitiDic.add('cedil', (184)); // cedilla = spacing cedilla, U+00B8 ISOdia -->
  fEntitiDic.add('ccedil', (231)); // latin small letter c with cedilla, U+00E7 ISOlat1 -->
  fEntitiDic.add('Ccedil', (199)); // latin capital letter C with cedilla, U+00C7 ISOlat1 -->
  fEntitiDic.add('cap', (8745)); // intersection = cap, U+2229 ISOtech -->
  fEntitiDic.add('bull', (8226)); // bullet = black small circle,    U+2022 ISOpub  -->
  fEntitiDic.add('brvbar', (166)); // broken bar = broken vertical bar, U+00A6 ISOnum -->
  fEntitiDic.add('beta', (946)); // greek small letter beta, U+03B2 ISOgrk3 -->
  fEntitiDic.add('Beta', (914)); // greek capital letter beta, U+0392 -->
  fEntitiDic.add('bdquo', (8222)); // double low-9 quotation mark, U+201E NEW -->
  fEntitiDic.add('auml', (228)); // latin small letter a with diaeresis, U+00E4 ISOlat1 -->
  fEntitiDic.add('Auml', (196)); // latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
  fEntitiDic.add('atilde', (227)); // latin small letter a with tilde, U+00E3 ISOlat1 -->
  fEntitiDic.add('Atilde', (195)); // latin capital letter A with tilde, U+00C3 ISOlat1 -->
  fEntitiDic.add('asymp', (8776)); // almost equal to = asymptotic to,    U+2248 ISOamsr -->
  fEntitiDic.add('aring', (229)); // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
  fEntitiDic.add('Aring', (197)); // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
  fEntitiDic.add('ang', (8736)); // angle, U+2220 ISOamso -->
  fEntitiDic.add('and', (8743)); // logical and = wedge, U+2227 ISOtech -->
  fEntitiDic.add('amp', (38)); // ampersand, U+0026 ISOnum -->
  fEntitiDic.add('alpha', (945)); // greek small letter alpha,   U+03B1 ISOgrk3 -->
  fEntitiDic.add('Alpha', (913)); // greek capital letter alpha, U+0391 -->
  fEntitiDic.add('alefsym', (8501)); // alef symbol = first transfinite cardinal,    U+2135 NEW -->
  fEntitiDic.add('agrave', (224)); // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
  fEntitiDic.add('Agrave', (192)); // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
  fEntitiDic.add('aelig', (230)); // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
  fEntitiDic.add('AElig', (198)); // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
  fEntitiDic.add('acute', (180)); // acute accent = spacing acute, U+00B4 ISOdia -->
  fEntitiDic.add('acirc', (226)); // latin small letter a with circumflex, U+00E2 ISOlat1 -->
  fEntitiDic.add('Acirc', (194)); // latin capital letter A with circumflex, U+00C2 ISOlat1 -->
  fEntitiDic.add('aacute', (225)); // latin small letter a with acute, U+00E1 ISOlat1 -->
  fEntitiDic.add('Aacute', (193)); // latin capital letter A with acute, U+00C1 ISOlat1 -->

  fEntitiDic.trimexcess;

end;

class function THTMLDecoder.Decode(const aHtml: string): string;
var
  dc: THTMLDecoder;
begin
  dc := THTMLDecoder.Create;
  dc.PerformDecode(aHtml);
  Result := dc.Result;
  dc.free;
end;

Function ExtractHTML(s: String): String;
Begin
  Result := StripHTMLorNonHTML(s, True);
End;

Function ExtractNonHTML(s: String): String;
Begin
  Result := StripHTMLorNonHTML(s, false);
End;

Function HtmlToText(s: String): String;
Begin

  s := StringReplace(s, '  ', ' ', [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '<br>', CR, [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '<br />', CR, [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '<br/>', CR, [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '<p>', CR, [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '</p>', CR, [rfReplaceAll, rfIgnoreCase]);

  s := ExtractNonHTML(s);
  s := HTMLDecode(s);
  Result := s;
End;

Function StripHTMLorNonHTML(s: String; WantHTML: boolean): String;
Var
  x, TagCnt: integer;
  OutString: String;
  OutIndex: integer;
Begin
  TagCnt := 0;
  Result := '';

  SetLength(OutString, Length(s));
  OutIndex := 0;

  For x := 1 To Length(s) Do
  Begin
    Case s[x] Of
      '<':
        Inc(TagCnt);
      '>':
        Dec(TagCnt);
    Else
      Case WantHTML Of
        false:
          If TagCnt <= 0 Then
          Begin
            Inc(OutIndex);
            OutString[OutIndex] := s[x];
            TagCnt := 0;
          End;
        True:
          If TagCnt >= 1 Then
          Begin
            Inc(OutIndex);
            OutString[OutIndex] := s[x];
          End
          Else If TagCnt < 0 Then
            TagCnt := 0;
      End;
    End;
  End;

  SetLength(OutString, OutIndex);

  OutString := StringReplace(OutString, '&nbsp;', ' ',
    [rfReplaceAll, rfIgnoreCase]);
  OutString := StringReplace(OutString, '&amp;', '&',
    [rfReplaceAll, rfIgnoreCase]);
  OutString := StringReplace(OutString, '&lt;', '<',
    [rfReplaceAll, rfIgnoreCase]);
  OutString := StringReplace(OutString, '&gt;', '>',
    [rfReplaceAll, rfIgnoreCase]);
  OutString := StringReplace(OutString, '&quot;', '"',
    [rfReplaceAll, rfIgnoreCase]);

  Result := OutString;
End;

end.
