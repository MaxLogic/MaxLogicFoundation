unit maxLogic.RandomPronounceableWord;

{
  Version: 1
  Description:

  I was generating some samples for a customer where I needed some random text. So I thought I would generate some random pronounceable  words.



  The baiscs
  Generating pronounceable random words isn't that difficult.
  Just start by a vowel or a consonant, then add more


  But that generates way to simple words.
  The thing is, that natural words also use a few consecutive consonant / vowel  combinations.                                  I tried to implement this as well.

  WOrd length:
  Words have two units of measure: syllables and letters. the average word length in syllables is 1.74 and that in letters is 5.67.
  Most used words are rather short, the long ones get not to be used that frequently.

  Sentences length:
  Plain English recommends short sentences.
  press associations in the USA have laid down a readability table.
  Their survey shows readers find sentences of 8 words or less very easy to read;
  11 words, easy;
  14 words fairly easy;
  17 words standard;
  21 words fairly difficult;
  25 words difficult and
  29 words or more, very difficult.
  The avarage is between 15 and 20 words.

  Well, we need not be that accurate, after all that is just random giberish that I generate.
}
interface

uses
  SysUtils;

type
  TRandomPronounceableWord = class
  private
    type
    TStrArray = array of string;

  class Constructor CreateClass;
  CLASS FUNCTION GetSound(UseConsonant: BOOLEAN; CurWordLen: integer): STRING;
  public
    // if you need you can change those
    class var AllConsonantSounds, ConsonantSoundsThatCanStartAWord, wovels: TStrArray;

    class function Get(MaxWordLength: integer): string; overload;
    class function Get: string; overload;
    class function Getsentence: string;
  end;

implementation

{ TRandomPronounceableWord }

class constructor TRandomPronounceableWord.CreateClass;
var
  ConsonantSoundsThatCantStartAWord: TStrArray;
begin
  ConsonantSoundsThatCanStartAWord := [
  // single consonants. Beware of Q, it's often awkward in words
    'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'r', 's', 't', 'v', 'w', 'x', 'z',
  // possible combinations excluding those which cannot start a word
  'pt', 'gl', 'gr', 'ch', 'ph', 'ps', 'sh', 'st', 'th', 'wh'
    ];

  // consonant combinations that cannot start a word
  ConsonantSoundsThatCantStartAWord := [
    'ck', 'cm', 'dr', 'ds', 'ft', 'gh', 'gn', 'kr', 'ks', 'ls', 'lt', 'lr', 'mp', 'mt', 'ms', 'ng', 'ns', 'rd', 'rg', 'rs', 'rt', 'ss', 'ts', 'tch'
    ];

  // wovels
  wovels := [
  // single vowels
    'a', 'e', 'i', 'o', 'u', 'y',
  // vowel combinations your language allows
  'ee', 'oa', 'oo'
    ];

  AllConsonantSounds := ConsonantSoundsThatCanStartAWord + ConsonantSoundsThatCantStartAWord;
end;

class function TRandomPronounceableWord.Get(MaxWordLength: integer): string;
var
  sound, word: string;
  UseConsonant: BOOLEAN;
  len, soundlEN: integer;
begin
  // start with a Consonant or a wovel?
  // you can change the propaability as you like
  UseConsonant := random() >= 0.5;
  word := '';
  len := 0;

  repeat
    sound := GetSound(UseConsonant, len);
    soundlEN := Length(sound);
    if len + soundlEN <= MaxWordLength then

    BEGIN
      word := word + sound;
      INC(len, soundlEN);
      UseConsonant := NOT UseConsonant;
    END;

  until len = MaxWordLength;
  result := word;
end;

class function TRandomPronounceableWord.GetSound(UseConsonant: BOOLEAN; CurWordLen: integer): STRING;
begin
  if UseConsonant then
  begin
    if CurWordLen = 0 then
      result := ConsonantSoundsThatCanStartAWord[random(Length(ConsonantSoundsThatCanStartAWord))]
    else
      result := AllConsonantSounds[random(Length(AllConsonantSounds))];
  end
  else
    result := wovels[random(Length(wovels))];

end;

class
  function TRandomPronounceableWord.Get: string;
var
  len: integer;
begin
  // this way we shift the propability a bit more towards short words. I didn't care to go with weighted random generators
  len := random(10) - 3;
  if len < 2 then
    len := 2;
  result := Get(len);
end;

class
  function TRandomPronounceableWord.Getsentence: string;
var
  WordCount, x: integer;
  s: string;
begin
  // again we shift the propability a bit towards the lower word count
  WordCount := random(32) - 8;
  if WordCount < 8 then
    WordCount := 8;
  s := Get();
  for x := 1 to WordCount - 1 do
    s := s + ' ' + Get();
  s := s + '.';
  result := s;

end;

end.
