unit MeaningUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

const
  SpaceCode= 254;
  EoFCode= 255;
  
type
  TChar= String;
  TWordEncoding= (weStart, wePersian, weEnglish, weEnd);
//  TCharSet= set of TChar;

  { EAnInvalidCharEncountered }

  EAnInvalidCharEncountered= class (Exception)
  private
  public
    constructor Create (Char: String);
    
  end;
  EEndOfMeaningOccured= class (Exception);
  EInvalidFile= class (Exception);
  EWMCIndexCollectionCannotBeMade= class (Exception);
  
  { TWord }

  TWord= class (TObject)
  private
    FChars: array of TChar;
    FEncoding: TWordEncoding;
    function GetChars (Index: Integer): TChar;
    function GetLength: Integer;
    
    function ReadNextCharFromStream (InputStream: TStream): string;
  public
    property Chars [Index: Integer]: TChar read GetChars;
    property Length: Integer read GetLength;
    property Encoding: TWordEncoding read FEncoding;
    
    procedure AddChar (NewChar: TChar);
    
    function StartWith (Str: String): Boolean;
    function EndWith (Str: String): Boolean;
    
    function Copy: TWord;
    
    constructor Create (WordEncoding: TWordEncoding= weEnglish);
    procedure Free;

    procedure LoadFromString (InputString: String);
    procedure LoadFromStream (InputStream: TStream; Version: Integer= 1);
    procedure LoadFromFile (var InputFile: TextFile; Version: Integer= 1);
    procedure SaveToStream (OutputStream: TStream; Version: Integer= 1);
    
    function ToString (WithSpace: Boolean= True): String;
    procedure DeleteChar (Index: Integer);
    
  end;
  
  { TWordCollection }

  TWordCollection= class (TBaseCollection)
  private
    function GetWord (Index: Integer): TWord;
    
  public
    property Word [Index: Integer]: TWord read GetWord;

    procedure AddWord (NewWord: TWord);
    procedure DeleteWord (Index: Integer);


    procedure Free;

    procedure SaveToStream (OutputStream: TStream; Version: Integer);
    procedure LoadFromStream (InputStream: TStream; Version: Integer);
    procedure LoadFromString (Str: String);
    
    function ToString (WithSpace: Boolean= True): String;
    function Copy: TWordCollection;
    
  end;

  { TWordColCollection }

  TWordColCollection= class (TBaseCollection)
  private
    function GetWordCollection (Index: Integer): TWordCollection;
  public
    property WordCollection [Index: Integer]: TWordCollection read GetWordCollection;

    procedure AddWordCollection (NewWordCollection: TWordCollection);
    procedure DeleteWordCollection (Index: Integer);


    procedure Free;

    procedure SaveToStream (OutputStream: TStream; Version: Integer);
    procedure LoadFromStream (InputStream: TStream; Version: Integer);
    
    function ToString (WithSpace: Boolean= True): String;
  end;

  { TQueryWord }

  TQueryWord= class (TWordCollection)
  private
    FCharCount: Integer;
    FPosition: Integer;
    FWordIndex: Integer;
    function GetChar(Index: Integer): TChar;
    function GetEoW: Boolean;
    function GetLastWord: TWord;

  public
    property EOW: Boolean read GetEoW;
    property CharAt [Index: Integer]: TChar read GetChar;
    property CharCount: Integer read FCharCount;
    property LastWord: TWord read GetLastWord;
    
    constructor Create;
    procedure Clear;

    function GetNextChar: TChar;
    function Copy: TQueryWord;
    procedure DeleteLastChar;
    
    function StartWith (Str: String): Boolean;
    function EndWith (Str: String): Boolean;
    
    procedure AddWord (NewWord: TWord);

  end;
  
  { TWordMeaning }

  TWordMeaning= class (TObject)
  private
    FMeanings: TWordColCollection;
    FMeaninigPosOnFile: Int64;
    FRefrenceCounter: Int64;
    FWord: TWordCollection;
    FWordPosOnFile: Int64;
    
    function GetMeaning (Index: Integer): TWordCollection;
    
  public
    property Word: TWordCollection read FWord;
    property Meaning [Index: Integer]: TWordCollection read GetMeaning;
    property WordPosOnFile: Int64 read FWordPosOnFile;
    property MeaninigPosOnFile: Int64 read FMeaninigPosOnFile;
    property RefrenceCounter: Int64 read FRefrenceCounter;
    
    constructor Create;
    procedure Free;
    
    procedure SetWordMeaning (NewWord: TWordCollection; NewMean: TWordColCollection);
    procedure LoadFromString (InputString: String);
    procedure LoadFromStringNew (InputString: String);
    procedure LoadFromFile (var InputFile: TextFile; Version: Integer);
    procedure SaveToTextStream (var OutputStream: TStream; Version: Integer);
    procedure SaveToStream (OutputStream: TStream; Version: Integer);
    procedure LoadFromStream (InputStream: TStream; Version: Integer);

    function ToString (WithSpace: Boolean= True): String;
    
  end;
  
  TWMIndex= class;

  { TWordMeaningCollection }

  TWordMeaningCollection= class (TBaseCollection)
  private
    function GetWordMeaning (Index: Integer): TWordMeaning;
  public
    property WordMeaning [Index: Integer]: TWordMeaning read GetWordMeaning;
    procedure Free;
    
    procedure AddWordMeaning (NewWordMeaning: TWordMeaning);
    procedure DeleteWordMeaning (Index: Integer);
    
    procedure SaveToStream (OutputStream: TStream; Version: Integer);
    procedure LoadFromStream (InputStram: TStream);
    
    procedure SaveToFile (FileName: String; Version: Integer= 1);
    procedure LoadFromFile (FileName: String);

    function GenerateIndexFile: TwMIndex;
    
    function ToString (WithSpace: Boolean= True): String;
  end;
  
  TWMIndexCollection= class;
  TFileManager= class;
  
  { TWMIndex }

  TWMIndex = class (TObject)
  private
    FHaveWord: Boolean;
    FChilds: TWMIndexCollection;
    FMeaninigPosOnFile: Int64;
    
    function GetChilds (Index: String): TWMIndex;

    procedure SaveToStream (OutputStream: TStream; Version: Integer);

  public
    property HaveWord: Boolean read FHaveWord;
    property Childs [Index: String]: TWMIndex read GetChilds;
    property MeaninigPosOnFile: Int64 read FMeaninigPosOnFile;
    
    constructor Create;
    procedure Free;

    procedure AddChild (Word: TWordCollection;
          Meanings: TWordColCollection; MeaninigPos: Int64);

    function FindMeaning (Word: TQueryWord; FileManager: TFileManager):
      TWordColCollection;
      
    function FindMeaningWithSpellChecking (Word: TQueryWord;
             FileManager: TFileManager): TWordMeaningCollection;
    
    procedure LoadFromFile (FileName: String);
    procedure SaveToFile (FileName: String; Version: Integer);
    
    procedure LoadFromStream (InputStream: TStream; Version: Integer);
    function ToString (Space: String= ''): String;

  end;
  
  { TWMIndexCollection }

  TWMIndexCollection= class (TNameValueCollection)
  private
    function GetWMIndex (Index: String; CreateIt: Boolean= False): TWMIndex;
    
  public
    property WMIndex [Index: String]: TWMIndex read GetWMIndex;
    
    constructor Create;
    procedure Free;

    procedure AddWMIndex (NewWMIndex: TWMIndex; Key: String);
    
    procedure SaveToStream (OutputStream: TStream; Version: Integer);
    procedure LoadFromStream (InputStream: TStream; Version: Integer);

  end;
  
  { TFileReader }

  TFileReader= class (TObject)
  private
    FInput: TFileStream;
    
  public
    constructor Create (FileName: String);
    procedure Free;
    
    function ReadMeaning (Position: Int64; Version: Integer): TWordColCollection;
    
  end;
  
  { TFileReaderCollection }

  TFileReaderCollection= class (TBaseCollection)
  private
    function GetFileReader(Index: Integer): TFileReader;
    
  public
    property FileReader [Index: Integer]: TFileReader read GetFileReader;
    
    constructor Create;
    procedure Free;
    
    procedure AddFileReader (NewFileReader: TFileReader);
    
  end;
  
  { TFileManager }

  TFileManager= class (TObject)
  private
    FFileReaderCollection: TFileReaderCollection;
    LastIndex: Integer;
    
  public
    constructor Create (MaxFileRead: Integer; FileName: String);
    procedure Free;

    function GetMeaninig (MeaninigPos: Int64): TWordColCollection;
    
  end;
  
  { EInvalidVersion }

  EInvalidVersion= class (Exception)
  public
    constructor Create (AClassName, AMethodName: String);
    
  end;

const
  CharCount: array [weStart..weEnd] of Integer= (0, 32, 26, 0);
  PersianAlphabet: array [0..43] of String= ('آ', 'ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'ك', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی', 'ي',
      'ئ', 'ؤ', 'أ', 'ﺋ', 'ة', #217#137, #217#146, '،', #226#128#140 {Zero width disjoiner});
  PersianIndex: array [0..43] of Integer=   (0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 31, 31,
        29, 0, 31, 3, 31, 2, -1, -2);
  EnglishAlphabet: array [0..25] of String= ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
      'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');
  EnglishIndex: array [0..25] of Integer=   (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25);
      
  IgnoredCharacters: array [0..28] of String= ('ء', '(', ')', '-', '_', '.', 'ً', #13,
     '۰', '۱', '۲', '۳', '۴', '۵', '۶', '۷', '۸', '۹', #194#171, #194#187,
     '،', '!', '?', '؟', ':', '؛', #217#143{o}, '=', #61
      );
     
  SpaceCharSet: array [0..5] of String= (' ', #9, '[', ']', '(', ')');
  ReturnCharSet: array [0..0] of String= (#10);
  
  procedure WriteToStream (AStream: TStream; Str: String);
  
implementation
uses
  ExceptionUnit;

procedure WriteToStream (AStream: TStream; Str: String);
var
  i: Integer;

begin
  for i:= 1 to Length (Str) do
    AStream.WriteByte (Ord (Str [i]));

end;

function ReadFromStream (AStream: TStream): String;
var
  b: Byte;

begin
  Result:= '';
  b:= AStream.ReadByte;
  while b<> 10 do
  begin
    Result:= Result+ Char (b);
    b:= AStream.ReadByte;
    
  end;

end;


{ TWord }

function TWord.GetChars (Index: Integer): TChar;
begin
 if (Index< 0) or  (Length<= Index) then
    raise ERangeCheckError.Create ('GetChars');
    
 Result:= FChars [Index];

end;

function TWord.GetLength: Integer;
begin
  Result:= System.Length (FChars);
  
end;

function TWord.ReadNextCharFromStream (InputStream: TStream): String;
var
  Str: String;
  b: Byte;
  i: Integer;

  function IsSpace (Str: String): Boolean;
  var
    i: Integer;
    
  begin
    Result:= True;
    for i:= 0 to High (SpaceCharSet) do
      if SpaceCharSet [i]= Str then
        Exit;

    if Str= ',' then
      raise EEndOfMeaningOccured.Create (',');

    if Str= #216#140 then
      raise EEndOfMeaningOccured.Create (',');

    Result:= False;
    
  end;

  function IsIgnored (Str: String): Boolean;
  var
    i: Integer;

  begin
    Result:= True;
    for i:= 0 to High (IgnoredCharacters) do
      if IgnoredCharacters [i]= Str then
        Exit;

    Result:= False;

  end;

var
  OpenPar: Boolean;
  
begin
  OpenPar:= False;
  
  if InputStream.Position< InputStream.Size then
  begin

    case FEncoding of
      weEnglish:
      begin
        Result:= '';
        while (InputStream.Position< InputStream.Size) and ((Result= '') or (OpenPar)) do
        begin
          b:= InputStream.ReadByte;
          Str:= Chr (b);
          if IsSpace (Str) then
          begin
            Result:= Result+ Chr (b);
            continue;
            
          end;

          if IsIgnored (Str) then
            Continue;

          Result:= Result+ Chr (b);
          if b= Ord ('(') then
            OpenPar:= True
          else if (b= Ord (')')) then
          begin

            if OpenPar then
              Exit
            else
              raise EAnInvalidCharEncountered.Create ('()');

          end;

        end;

      end;

      wePersian:
      begin
        Result:= '';

        while (InputStream.Position< InputStream.Size) and
          ((Result= '') or (OpenPar)) do
        begin

          b:= InputStream.ReadByte;
          Str:= Chr (b);
          if IsSpace (Str) then
          begin
            Result:= Result+ ' ';
            Continue;

          end;

          if IsIgnored (Str) then
            Continue;

          if Chr (b) in ['(', ')', '.', 'A'..'Z', 'a'..'z', '<', '>'] then
          begin
            Result:= Result+ Chr (b);

            if b= Ord ('(') then
              OpenPar:= True
            else if (b= Ord (')')) then
            begin
              if OpenPar then
                Exit
              else
                raise EAnInvalidCharEncountered.Create ('()');

            end;
            Str:= '';
            Continue;
            
          end;

          Str:= Str+ Chr (InputStream.ReadByte);
          if IsSpace (Str) then
          begin
            Result:= Result+ ' ';
            Continue;
            
          end;

          if IsIgnored (Str) then
            Continue;

          for i:= 0 to High (PersianAlphabet) do
            if PersianAlphabet [i]= Str then
            begin
              Result:= Result+ Str;
              Str:= '';
              Break;

            end;
            
          if Str= '' then// It is persian char
            Continue;

          Str:= Str+ Chr (InputStream.ReadByte);

          for i:= 0 to High (PersianAlphabet) do
            if PersianAlphabet [i]= Str then
            begin
              Result:= Result+ Str;
              Str:= '';
              Break;

            end;

          if Str= '' then// It is persian char
            Continue;

          if IsIgnored (Str) then
            Continue;

          if IsSpace (Str) then
          begin
            Result:= Result+ ' ';
            Continue;

          end;

          raise EAnInvalidCharEncountered.Create (Str);

        end;

      end;

    end;
    
  end
  else
    Result:= 'EOF';
    

end;

procedure TWord.AddChar (NewChar: TChar);
begin
  SetLength (FChars, Length+ 1);
  FChars [Length- 1]:= NewChar;
  
end;

function TWord.StartWith (Str: String): Boolean;
var
  i, l, StrLen: Integer;
  CharString: String;
  
begin
  StrLen:= System.Length (Str);
  Result:= False;
  
  for i:= 0 to Length- 1 do
  begin
    CharString:= Chars [i];
    l:= System.Length (CharString);
    
    if System.Copy (Str, 1, l)<> CharString then
      Exit;
      
    Delete (Str, 1, l);
    Dec (StrLen, l);
    if StrLen= 0 then
    begin
      Result:= True;
      Exit;
      
    end;
    
  end;

end;

function TWord.EndWith(Str: String): Boolean;
var
  i, l, StrLen: Integer;
  CharString: String;

begin
  
  StrLen:= System.Length (Str);
  Result:= False;

  for i:= Length- 1 downto 0 do
  begin
    CharString:= Chars [i];
    l:= System.Length (CharString);

    if System.Copy (Str, StrLen- l+ 1, l)<> CharString then
      Exit;
      
    Delete (Str, StrLen- l+ 1, l);
    Dec (StrLen, l);
    if StrLen= 0 then
    begin
      Result:= True;
      Exit;

    end;

  end;
  
end;

function TWord.Copy: TWord;
var
  i: Integer;
  
begin
  Result:= TWord.Create (FEncoding);
  
  for i:= 0 to Length- 1 do
    Result.AddChar (Chars [i]);
    
end;

{
constructor TWord.Create;
begin
  inherited;
  
  SetLength (FChars, 0);
  FEncoding:= weStart;
  
end;
}

constructor TWord.Create (WordEncoding: TWordEncoding);
begin
  inherited Create;

  FEncoding:= WordEncoding;
  SetLength (FChars, 0);

end;

procedure TWord.Free;
begin
  SetLength (FChars, 0);

  inherited;
  
end;

procedure TWord.LoadFromString (InputString: String);
var
  InputStream: TStringStream;
  
begin
  InputStream:= TStringStream.Create (InputString);
  LoadFromStream (InputStream, 0);
  
  InputStream.Free;
  
end;


procedure TWord.LoadFromStream (InputStream: TStream; Version: Integer);
var
  c: Char;
  Ch: String;
  Len: Integer;
  
begin

  case Version of
    0:
      try
        Ch:= ReadNextCharFromStream (InputStream);

        while (Ch<> ' ') and (Ch<> 'EOF') do
        begin
          Self.AddChar (Ch);
          Ch:= ReadNextCharFromStream (InputStream);

        end;

      except
        on e: EAnInvalidCharEncountered do
          WriteLn (e.Message);
      end;
      
    1:
      try
        Len:= InputStream.ReadByte;

        while Len> 0 do
        begin
          Ch:= '';
          c:= Char (InputStream.ReadByte);

          while c<> #0 do
          begin
            Ch:= Ch+ c;
            c:= Char (InputStream.ReadByte);

          end;

          Self.AddChar (Ch);
          Dec (Len);

        end;

      except
        on e: EAnInvalidCharEncountered do
          WriteLn (e.Message);
      end
    else
      raise EInvalidVersion.Create ('TWord', 'SaveToFile');
      
  end;
  
end;

procedure TWord.LoadFromFile (var InputFile: TextFile; Version: Integer);
begin
  case Version of
    1:
      raise ENotImplementedYet.Create ('TWord', 'LoadFromFile');
    else
      raise EInvalidVersion.Create ('TWord', 'LoadFromFile');
  end;

end;

procedure TWord.SaveToStream (OutputStream: TStream; Version: Integer);
var
  i: Integer;
  
begin
  case Version of
    1:
    begin
      OutputStream.WriteByte (Byte (Length));

      for i:= 0 to Length- 1 do
        WriteToStream (OutputStream, FChars [i]+#0);

    end;
      
    else
      raise EInvalidVersion.Create ('TWord', 'SaveToFile');
      
  end;

end;

function TWord.ToString (WithSpace: Boolean= True): String;
var
  i: Integer;
  
begin
  if WithSpace then
  begin
    Result:= '(';//+ IntToStr (Ord (Encoding));

    for i:= 0 to Length- 1 do
      Result:= Result+ ' '+ FChars [i];

    Result:= Result+ ')';
    
  end
  else
  begin
    Result:= '';//+ IntToStr (Ord (Encoding));

    for i:= 0 to Length- 1 do
      Result:= Result+ FChars [i];

  end;
  
end;

procedure TWord.DeleteChar (Index: Integer);
var
  i: Integer;
  
begin
  Chars [Index];
  
  for i:= Index+ 1 to Length- 1 do
    FChars [i- 1]:= FChars [i];
  SetLength (FChars, Length- 1);
  
end;

{ TWordCollection }

function TWordCollection.GetWord (Index: Integer): TWord;
begin
  Result:= Member [Index] as TWord;
  
end;

procedure TWordCollection.AddWord (NewWord: TWord);
begin
  Add (NewWord);
  
end;

procedure TWordCollection.DeleteWord (Index: Integer);
begin
  Word [Index].Free;
  Member [Index]:= nil;

  Delete (Index);
  
end;

procedure TWordCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    Word [i].Free;

  inherited;
  
end;

procedure TWordCollection.SaveToStream (OutputStream: TStream; Version: Integer);

  procedure SaveToStreamVer1 (OutputStream: TStream);
  var
    i: Integer;
    
  begin
    OutputStream.WriteWord (Size);
    for i:= 0 to Size- 1 do
      Word [i].SaveToStream (OutputStream, Version);
      
  end;
  
begin
  case Version of
    1:
      SaveToStreamVer1 (OutputStream)
      
    else
      raise EInvalidVersion.Create ('TWordCollection', 'SaveToStream');
      
  end;
  
end;

procedure TWordCollection.LoadFromStream (InputStream: TStream; Version: Integer);

  procedure LoadFromStreamVer1 (InputputStream: TStream);
  var
    i: Integer;
    NewWord: TWord;

  begin
    i:= InputStream.ReadWord;
    while i<> 0 do
    begin
      NewWord:= TWord.Create;
      NewWord.LoadFromStream (InputStream, Version);
      Self.AddWord (NewWord);
      Dec (i);
      
    end;

  end;

begin
  case Version of
    1:
      LoadFromStreamVer1 (InputStream)

    else
      raise EInvalidVersion.Create ('TWordCollection', 'SaveToStream');

  end;

end;

procedure TWordCollection.LoadFromString (Str: String);
var
  InputStream: TStringStream;
  NewWord: TWord;
  
begin
  NewWord:= TWord.Create;
  NewWord.LoadFromString (Str);
  if NewWord.Length= 0 then
    NewWord.Free
  else
    Self.AddWord (NewWord);
  
end;

function TWordCollection.ToString (WithSpace: Boolean): String;
var
  i: Integer;
  
begin
  if WithSpace then
  begin
    Result:= '(';

    for i:= 0 to Size- 1 do
      Result:= Result+ ' '+ Word [i].ToString;
    Result:= Result+ ' )';
    
  end
  else
  begin
    Result:= '';

    for i:= 0 to Size- 1 do
      Result:= Result+ ' '+ Word [i].ToString (False);
    
   end;
  
end;

function TWordCollection.Copy: TWordCollection;
var
  i, j: Integer;
  ActiveWord,
  NewWord: TWord;
  
begin
  Result:= TWordCollection.Create;
  
  for i:= 0 to Size- 1 do
  begin
    ActiveWord:= Word [i];
    NewWord:= TWord.Create (ActiveWord.Encoding);
    
    for j:= 0 to ActiveWord.Length- 1 do
      NewWord.AddChar (ActiveWord.Chars [j]);
      
    Result.AddWord (NewWord);
    
  end;

end;

{ TWordMeaning }

function TWordMeaning.GetMeaning (Index: Integer): TWordCollection;
begin
  Result:= FMeanings.Member [Index] as TWordCollection;
  
end;

constructor TWordMeaning.Create;
begin
  inherited;
  
  FWord:= TWordCollection.Create;
  FMeanings:= TWordColCollection.Create;
  FWordPosOnFile:= -1;
  FMeaninigPosOnFile:= -1;
  FRefrenceCounter:= 0;
  
end;

procedure TWordMeaning.Free;
begin
  FWord.Free;
  FMeanings.Free;

  inherited;
  
end;

procedure TWordMeaning.SetWordMeaning (NewWord: TWordCollection;
  NewMean: TWordColCollection);
begin
  FWord.Free;
  FWord:= NewWord;
  FMeanings.Free;
  FMeanings:= NewMean;
  
end;

procedure TWordMeaning.LoadFromString (InputString: String);
var
  NewWord: TWord;
  NewMean: TWordCollection;
  EnglishWordString: String;
  Index: Integer;
  InputStream: TStringStream;
  
begin
  
  Index:= Length (InputString);
  while InputString [Index]<> '|' do
    Dec (Index);
  Dec (Index);
  
  EnglishWordString:= '';
  while InputString [Index]<> '|' do
  begin
    EnglishWordString:= InputString [Index]+ EnglishWordString;
    Dec (Index);
    
  end;

  InputStream:= TStringStream.Create (EnglishWordString);

  while InputStream.Position< InputStream.Size do
  begin
    NewWord:= TWord.Create (weEnglish);
    NewWord.LoadFromStream (InputStream, 0);
    if NewWord.Length> 0 then
      FWord.AddWord (NewWord)
    else
      NewWord.Free;

  end;

  InputStream.Free;

  Delete (InputString, Index, Length (InputString)- Index+ 1);
  InputStream:= TStringStream.Create (InputString);
  
  NewMean:= TWordCollection.Create;
  
  while InputStream.Position< InputStream.Size do
  begin
    try
      NewWord:= TWord.Create (wePersian);
      NewWord.LoadFromStream (InputStream, 0);
      if NewWord.Length> 0 then
        NewMean.AddWord (NewWord)
      else
        NewWord.Free;

    except
      on e: EEndOfMeaningOccured do
        begin
          if NewWord.Length<> 0 then
            NewMean.AddWord (NewWord)
          else
            NewWord.Free;
          
          if NewMean.Size<> 0 then
            FMeanings.AddWordCollection (NewMean)
          else
            NewMean.Free;

          NewMean:= TWordCollection.Create;
        end;
      
    end;

  end;
  
  if NewMean.Size<> 0 then
    FMeanings.AddWordCollection (NewMean)
  else
    NewMean.Free;
    
  InputStream.Free;
  
end;

procedure TWordMeaning.LoadFromStringNew (InputString: String);
var
  NewWord: TWord;
  NewMean: TWordCollection;
  EnglishWordString: String;
  IndexPtr: PChar;
  Index: Integer;
  InputStream: TStringStream;

begin
  InputString:= Trim (InputString);
  Index:= Length (InputString);

  IndexPtr:= @InputString [1];
  EnglishWordString:= '';
  while IndexPtr^<> ':' do
  begin
    EnglishWordString:= EnglishWordString+ IndexPtr^;
    Inc (IndexPtr);
    if IndexPtr^= ' ' then
      Break;

  end;
  while IndexPtr^<> ':' do
    Inc (IndexPtr);
    
  if IndexPtr^<> ':' then
    raise EAnInvalidCharEncountered.Create (IndexPtr);
  Inc (IndexPtr);
  if IndexPtr^<> ':' then
    raise EAnInvalidCharEncountered.Create (IndexPtr);
  Inc (IndexPtr);

  InputStream:= TStringStream.Create (EnglishWordString);

  while InputStream.Position< InputStream.Size do
  begin
    NewWord:= TWord.Create (weEnglish);
    NewWord.LoadFromStream (InputStream, 0);
    if NewWord.Length> 0 then
      FWord.AddWord (NewWord)
    else
      NewWord.Free;

  end;

  InputStream.Free;

  while IndexPtr^= ' ' do
    Inc (IndexPtr);
    
  InputStream:= TStringStream.Create (IndexPtr);

  NewMean:= TWordCollection.Create;

  while InputStream.Position< InputStream.Size do
  begin
    try
      NewWord:= TWord.Create (wePersian);
      NewWord.LoadFromStream (InputStream, 0);
      if NewWord.Length> 0 then
        NewMean.AddWord (NewWord)
      else
        NewWord.Free;

    except
      on e: EEndOfMeaningOccured do
        begin
          if NewWord.Length<> 0 then
            NewMean.AddWord (NewWord)
          else
            NewWord.Free;

          if NewMean.Size<> 0 then
            FMeanings.AddWordCollection (NewMean)
          else
            NewMean.Free;

          NewMean:= TWordCollection.Create;
        end;

    end;

  end;

  if NewMean.Size<> 0 then
    FMeanings.AddWordCollection (NewMean)
  else
    NewMean.Free;

  InputStream.Free;

end;

procedure TWordMeaning.LoadFromFile (var InputFile: TextFile; Version: Integer);
begin

end;

procedure TWordMeaning.SaveToTextStream(var OutputStream: TStream;
  Version: Integer);
begin

end;

procedure TWordMeaning.SaveToStream (OutputStream: TStream; Version: Integer);

  procedure SaveToStreamVer1;
  begin
    FWordPosOnFile:= OutputStream.Position;
    FWord.SaveToStream (OutputStream, Version);

    FMeaninigPosOnFile:= OutputStream.Position;
    FMeanings.SaveToStream (OutputStream, Version);
    OutputStream.WriteByte (0);

  end;

  procedure SaveToStreamVer2;
  begin
    FWordPosOnFile:= OutputStream.Position;
    FWord.SaveToStream (OutputStream, 1);

    FMeaninigPosOnFile:= OutputStream.Position;
    FMeanings.SaveToStream (OutputStream, 1);
    OutputStream.WriteDWord (FRefrenceCounter);
    OutputStream.WriteByte (0);

  end;

begin
  case Version of
    1:
      SaveToStreamVer1;
    2:
      SaveToStreamVer2
    else
      raise EInvalidVersion.Create ('TWordMeaning', 'SaveToStream');
      
  end;
  
end;

procedure TWordMeaning.LoadFromStream (InputStream: TStream; Version: Integer);

  procedure LoadFromStreamVer1;
  var
    i: Integer;

  begin
    if FWord<> nil then
      FWord.Free;
    FWord:= TWordCollection.Create;
    FWordPosOnFile:= InputStream.Position;

    FWord.LoadFromStream (InputStream, Version);

    if FMeanings<> nil then
      FMeanings.Free;
    FMeanings:= TWordColCollection.Create;
    FMeaninigPosOnFile:= InputStream.Position;
    FMeanings.LoadFromStream (InputStream, Version);

    if InputStream.ReadByte<> 0 then
      raise EInvalidFile.Create ('Invalid WordMeaning');

  end;

  procedure LoadFromStreamVer2;
  var
    i: Integer;

  begin
    if FWord<> nil then
      FWord.Free;
    FWord:= TWordCollection.Create;
    FWordPosOnFile:= InputStream.Position;

    FWord.LoadFromStream (InputStream, 1);

    if FMeanings<> nil then
      FMeanings.Free;
    FMeanings:= TWordColCollection.Create;
    FMeaninigPosOnFile:= InputStream.Position;
    FMeanings.LoadFromStream (InputStream, 1);
    FRefrenceCounter:= InputStream.ReadDWord;

    if InputStream.ReadByte<> 0 then
      raise EInvalidFile.Create ('Invalid WordMeaning');

  end;

begin
  case Version of
    1:
      LoadFromStreamVer1;
    2:
      LoadFromStreamVer2;
    else
      raise EInvalidVersion.Create ('TWordMeaning', 'LoadFromStream');

  end;
  
end;

function TWordMeaning.ToString (WithSpace: Boolean): String;
begin
  if WithSpace then
    Result:= '('+ Word.ToString+ ':'#10+ FMeanings.ToString+ ')'
  else
    Result:= Word.ToString (False)+ ':'+ FMeanings.ToString (False);

end;


{ TWordColCollection }

function TWordColCollection.GetWordCollection (Index: Integer): TWordCollection;
begin
  Result:= Member [Index] as TWordCollection;
  
end;

procedure TWordColCollection.AddWordCollection (NewWordCollection: TWordCollection);
begin
  Add (NewWordCollection);
  
end;

procedure TWordColCollection.DeleteWordCollection (Index: Integer);
begin
  WordCollection [Index].Free;
  DeleteWordCollection (Index);
  
end;

procedure TWordColCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    WordCollection [i].Free;

  inherited;
  
end;

procedure TWordColCollection.SaveToStream (OutputStream: TStream;
          Version: Integer);
          
  procedure SaveToStreamVer1;
  var
    i: Integer;

  begin
    OutputStream.WriteByte (Size);
    
    for i:= 0 to Size- 1 do
      WordCollection [i].SaveToStream (OutputStream, Version);
      
  end;
  
begin
  case Version of
    1:
      SaveToStreamVer1
    else
      raise EInvalidVersion.Create ('TWordColleciton', 'SaveToFile');
      
  end;
  
end;

procedure TWordColCollection.LoadFromStream (InputStream: TStream;
  Version: Integer);

  procedure LoadFromStreamVer1;
  var
    i: Integer;
    NewWordCollection: TWordCollection;
    
  begin
    i:= InputStream.ReadByte;

    while i<> 0 do
    begin
      NewWordCollection:= TWordCollection.Create;
      NewWordCollection.LoadFromStream (InputStream, Version);
      Self.AddWordCollection (NewWordCollection);
      Dec (i);
      
    end;

  end;

begin
  case Version of
    1:
      LoadFromStreamVer1;
      
    else
      raise EInvalidVersion.Create ('TWordColleciton', 'SaveToFile');

  end;
  
end;

function TWordColCollection.ToString (WithSpace: Boolean= True): String;
var
  i: Integer;
  
begin
  if WithSpace then
  begin
    Result:= '(';

    for i:= 0 to Size- 1 do
      Result:= Result+ ' '+ WordCollection [i].ToString+ #10;
    Result:= Result+ ')'#10;

  end
  else
  begin
    Result:= '';

    for i:= 0 to Size- 2 do
      Result:= Result+ WordCollection [i].ToString (False)+ ',';
      
    if Size<> 0 then
      Result:= Result+ WordCollection [Size- 1].ToString (False);


  end;
  
end;

{ TWordMeaningCollection }

function TWordMeaningCollection.GetWordMeaning (Index: Integer): TWordMeaning;
begin
  Result:= Member [Index] as TWordMeaning;
  
end;

procedure TWordMeaningCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    WordMeaning [i].Free;

  inherited;
  
end;

procedure TWordMeaningCollection.AddWordMeaning (NewWordMeaning: TWordMeaning);
begin
  Add (NewWordMeaning);
  
end;

procedure TWordMeaningCollection.DeleteWordMeaning (Index: Integer);
begin
  WordMeaning [Index].Free;
  Member [Index]:= nil;
  Delete (Index);
  
end;

procedure TWordMeaningCollection.SaveToStream (OutputStream: TStream; Version: Integer= 1);
var
  i: Integer;
  
  procedure SaveHeader (OutputStram: TStream; Version: Integer);
  var
    S: String;

  begin
    WriteToStream (OutputStram, 'Version: '+ IntToStr (Version)+ #10);
    WriteToStream (OutputStram, 'Dictionary File'#10);
    WriteToStream (OutputStram, 'Developed by Amir Aavani'#10);

  end;

begin
  SaveHeader (OutputStream, Version);
  OutputStream.WriteWord (Size);
  
  for i:= 0 to Size- 1 do
    WordMeaning [i].SaveToStream (OutputStream, Version);
    
end;

procedure TWordMeaningCollection.LoadFromStream (InputStram: TStream);

  function ReadHeader (InputStram: TStream): Integer;
  var
    S: String;
    
  begin
    S:= ReadFromStream (InputStram);
    if Copy (S, 1, Length ('Version: '))<> 'Version: ' then
      raise EInvalidFile.Create ('invalid Header!');

    System.Delete (S, 1, Length ('Version: '));
    Result:= StrToInt (S);

    if ReadFromStream (InputStram)<> 'Dictionary File' then
      raise EInvalidFile.Create ('Invalid Header');
    if ReadFromStream (InputStram)<> 'Developed by Amir Aavani' then
      raise EInvalidFile.Create ('Invalid Header');

  end;

var
  Version, i: Integer;
  NewWordMeaning: TWordMeaning;

begin
  Version:= ReadHeader (InputStram);
  
  i:= InputStram.ReadWord;
  
  while i<> 0 do
  begin
    NewWordMeaning:= TWordMeaning.Create;
    NewWordMeaning.LoadFromStream (InputStram, Version);
    Self.AddWordMeaning (NewWordMeaning);
    Dec (i);
    
  end;

end;

procedure TWordMeaningCollection.SaveToFile (FileName: String; Version: Integer= 1);
var
  OutputStream: TFileStream;
  
begin
  OutputStream:= TFileStream.Create (FileName, fmCreate);

  SaveToStream (OutputStream, Version);
  
  OutputStream.Free;
  
end;

procedure TWordMeaningCollection.LoadFromFile (FileName: String);
var
  InputStream: TFileStream;

begin
  InputStream:= TFileStream.Create (FileName, fmOpenRead);

  LoadFromStream (InputStream);
  if InputStream.Position<> InputStream.Size then
    raise EInvalidFile.Create ('EoF!');
    
  InputStream.Free;

end;

function TWordMeaningCollection.GenerateIndexFile: TWMIndex;
var
  i: Integer;
  Index: TwmIndex;
  ActiveWordMeaning: TWordMeaning;
  
begin
  WriteLn (Size);
  
  if Size> 0 then
    if WordMeaning [0].WordPosOnFile= -1 then
      raise EWMCIndexCollectionCannotBeMade.Create
        ('Word Meaning Collection Must be loaded from file!');
        
  Result:= TWMIndex.Create;
  for i:= 0 to Size- 1 do
  begin
    ActiveWordMeaning:= WordMeaning [i];
    
    Result.AddChild (ActiveWordMeaning.Word,
             ActiveWordMeaning.FMeanings,
             ActiveWordMeaning.MeaninigPosOnFile
             );
             
  end;
  
end;

function TWordMeaningCollection.ToString (WithSpace: Boolean): String;
var
  i: Integer;
  
begin
  if WithSpace then
  begin
    Result:= '(';

    for i:= 0 to Size- 1 do
      Result:= Result+ ' '+ WordMeaning [i].ToString;

    Result:= Result+ ')';
    
  end
  else
  begin
    Result:= '';

    for i:= 0 to Size- 1 do
      Result:= Result+ ' '+ WordMeaning [i].ToString (False);

  end;
  
end;

{ TWMIndex }

function TWMIndex.GetChilds (Index: String): TWMIndex;
begin
  Result:= FChilds.WMIndex [Index];
  
end;

constructor TWMIndex.Create;
begin
  inherited;
  
  FChilds:= TWMIndexCollection.Create;
  FMeaninigPosOnFile:= 0;
  FHaveWord:= False;
  
end;

procedure TWMIndex.Free;
var
  i: Integer;
  
begin
  FChilds.Free;
  
  inherited;
  
end;

procedure TWMIndex.AddChild (Word: TWordCollection;
  Meanings: TWordColCollection; MeaninigPos: Int64);
  
var
  NewWMIndex: TWMIndex;
  NewWord: TWordCollection;
  
begin
  if Word.Size= 0 then
  begin
    FHaveWord:= True;
    FMeaninigPosOnFile:= MeaninigPos;
    
  end
  else
  begin
    NewWMIndex:= FChilds.GetWMIndex (Word.Word [0].Chars [0], True);
    NewWord:= Word.Copy;
    NewWord.Word [0].DeleteChar (0);
    if NewWord.Word [0].Length= 0 then
      NewWord.DeleteWord (0);
      
    NewWMIndex.AddChild (NewWord, Meanings, MeaninigPos);
    NewWord.Free;
    
  end;


end;

function TWMIndex.FindMeaning (Word: TQueryWord; FileManager: TFileManager):
      TWordColCollection;
var
  FirstChar: TChar;
  NextChild: TWMIndex;
  
begin

  if Word.EOW then
  begin
    if MeaninigPosOnFile<> 0 then
      Result:= FileManager.GetMeaninig (FMeaninigPosOnFile)
    else
      Result:= nil;
    
  end
  else
  begin
    FirstChar:= Word.GetNextChar;
    NextChild:= FChilds.GetWMIndex (FirstChar, False);

    if NextChild= nil then
    begin
      Result:= nil
      
    end
    else
      Result:= NextChild.FindMeaning (Word, FileManager);
      
  end;
    
end;

function TWMIndex.FindMeaningWithSpellChecking (Word: TQueryWord;
  FileManager: TFileManager): TWordMeaningCollection;
const
  MaxEndWithString= 5;
  EndWithString: array [0..MaxEndWithString] of String=
    ('s', 'es', 'd', 'ed', 'ing', 'ing' );
  ReplaceEndWithString: array [0..MaxEndWithString] of String=
    ('', '', '', '', 'e', '');
var
  WordMeaning: TWordMeaning;
  Meaning: TWordColCollection;
  NewWord: TQueryWord;
  i, j: Integer;
  
begin
  Result:= TWordMeaningCollection.Create;

  Meaning:= FindMeaning (Word, FileManager);

  if Meaning<> nil then
  begin
    WordMeaning:= TWordMeaning.Create;
    WordMeaning.SetWordMeaning (Word, Meaning);
    Result.AddWordMeaning (WordMeaning);
    
  end
  else
  begin
    NewWord:= nil;

    for i:= 0 to MaxEndWithString do
    begin

      if Word.EndWith (EndWithString [i]) then
      begin
        NewWord:= Word.Copy;
        
        for j:= 1 to Length (EndWithString [i]) do
          NewWord.DeleteLastChar;

        if ReplaceEndWithString [i]<> '' then
          for j:= 1 to Length (ReplaceEndWithString [i]) do
            NewWord.LastWord.AddChar (ReplaceEndWithString [i][j]);
          
        Meaning:= FindMeaning (NewWord, FileManager);
        if Meaning<> nil then
        begin
          WordMeaning:= TWordMeaning.Create;
          WordMeaning.SetWordMeaning (NewWord, Meaning);

          Result.AddWordMeaning (WordMeaning);

        end
        else
        begin
          NewWord.Free;

        end;

      end;
      
    end;


  end;
  
end;

procedure TWMIndex.LoadFromFile (FileName: String);
var
  InputStream: TFileStream;
  
  function ReadHeader (InputStream: TStream): Integer;
  var
    S: String;

  begin
    S:= ReadFromStream (InputStream);
    if Copy (S, 1, Length ('Version: '))<> 'Version: ' then
      raise EInvalidFile.Create ('invalid Header!');

    Delete (S, 1, Length ('Version: '));
    Result:= StrToInt (S);

    case Result of
      1:
      begin
        S:= ReadFromStream (InputStream);
        if UpperCase (S)<> 'DICTIONARY INDEX FILE' then
          raise EInvalidFile.Create ('Invalid Header');

        S:= ReadFromStream (InputStream);
        if UpperCase (S)<> 'DEVELOPED BY AMIR AAVANI' then
          raise EInvalidFile.Create ('Invalid Header');

      end
      else
        EInvalidVersion.Create ('TWordMeaningCollection', 'SaveToStream');

    end;

  end;
  
var
  Version: Integer;
  
begin
  InputStream:= TFileStream.Create (FileName, fmOpenRead);
  Version:= ReadHeader (InputStream);
  LoadFromStream (InputStream, Version);
  InputStream.Free;
  
end;

procedure TWMIndex.SaveToFile (FileName: String; Version: Integer);
var
  OutputStream: TFileStream;

  procedure WriteHeader (OutputStream: TStream; Version: Integer);
  begin
    case Version of
      1:
      begin
        WriteToStream (OutputStream, 'Version: '+ IntToStr (Version)+ #10);
        WriteToStream (OutputStream, 'Dictionary Index File'#10);
        WriteToStream (OutputStream, 'Developed by Amir Aavani'#10);

      end
      else
        raise EInvalidVersion.Create ('TWMIndex', 'SaveToStream');

    end;

  end;

begin
  OutputStream:= TFileStream.Create (FileName, fmCreate);
  WriteHeader (OutputStream, Version);
  SaveToStream (OutputStream, Version);
  OutputStream.Free;

end;

procedure TWMIndex.LoadFromStream (InputStream: TStream; Version: Integer);
var
  i: Integer;
  ActiveNameValue: TNameValue;

begin
  FHaveWord:= InputStream.ReadByte= $FF;
  FMeaninigPosOnFile:= InputStream.ReadDWord;

  FChilds.Free;
  FChilds:= TWMIndexCollection.Create;
  FChilds.LoadFromStream (InputStream, Version);

end;

function TWMIndex.ToString (Space: String): String;
var
  i: Integer;
  
begin
  if HaveWord then
    Result:= Space+ '(+'+ IntToStr (FMeaninigPosOnFile)
  else
    Result:= Space+ '(-'+ IntToStr (FMeaninigPosOnFile);

   for i:= 0 to FChilds.Size- 1 do
     Result:= Result+  #10+ '('+ FChilds.NameValue [i].Name+ ':'+
      (FChilds.NameValue [i].Value as TWMIndex).ToString (Space+ '  ')+
      ')';
     
   Result:= Result+ ')'+ #10;
  
end;

procedure TWMIndex.SaveToStream (OutputStream: TStream; Version: Integer);
var
  i: Integer;
  ActiveNameValue: TNameValue;
  
begin
  if HaveWord then
    OutputStream.WriteByte ($FF)
  else
    OutputStream.WriteByte (0);
  OutputStream.WriteDWord (FMeaninigPosOnFile);
  
  FChilds.SaveToStream (OutputStream, Version);

end;

{ TWMIndexCollection }

function TWMIndexCollection.GetWMIndex (Index: String; CreateIt: Boolean): TWMIndex;
begin
  try
    Result:= NameValueByName [Index].Value as TWMIndex;
    
  except
    on e: ENameNotFound do
    begin
      if CreateIt then
      begin
        Result:= TWMIndex.Create;
        Self.AddWMIndex (Result, Index);

      end
      else
        Result:= nil;
        
    end;
      
  end;

end;

constructor TWMIndexCollection.Create;
begin
  inherited;
  
end;

procedure TWMIndexCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
  begin
    (NameValue [i].Value as TWMIndex).Free;
    NameValue [i].Free;
    
  end;
    
  inherited;
  
end;

procedure TWMIndexCollection.AddWMIndex (NewWMIndex: TWMIndex; Key: String);
begin
  Add (TNameValue.Create (Key, NewWMIndex));
  
end;

procedure TWMIndexCollection.SaveToStream (OutputStream: TStream; Version: Integer);
var
  i: Integer;
  ActiveNameValue: TNameValue;

begin
  OutputStream.WriteByte (Size);
  
  for i:= 0 to Size- 1 do
  begin
    ActiveNameValue:= NameValue [i];

    if Length (ActiveNameValue.Name)= 1 then
      WriteToStream (OutputStream, #0+ ActiveNameValue.Name)
    else if Length (ActiveNameValue.Name)= 2 then
      WriteToStream (OutputStream, ActiveNameValue.Name)
    else
      WriteToStream (OutputStream, ActiveNameValue.Name);
    (ActiveNameValue.Value as TWMIndex).SaveToStream (OutputStream, Version);

  end;

end;

procedure TWMIndexCollection.LoadFromStream (InputStream: TStream; Version: Integer);
var
  i: Integer;
  ActiveNameValue: TNameValue;
  Index: TWMIndex;
  b: Byte;
  Name: String;
  
begin
  case Version of
    1:
    begin
      i:= InputStream.ReadByte;

      while i> 0 do
      begin
        b:= InputStream.ReadByte;

        if b= 0 then
          Name:= Char (InputStream.ReadByte)
        else
        begin
          Name:= Char (b)+ Char (InputStream.ReadByte);

        end;
        Index:= TWMIndex.Create;
        Index.LoadFromStream (InputStream, Version);
        AddWMIndex (Index, Name);

        Dec (i);

      end;
      
    end
    else
      raise EInvalidVersion.Create ('TWMIndexCollection', 'LoadFromStream');
      
  end;
  
end;

{ TQueryWord }

function TQueryWord.GetEoW: Boolean;
begin
  Result:= (Size<= FWordIndex);
  
end;

function TQueryWord.GetLastWord: TWord;
begin
  Result:= Word [Size- 1];
  
end;

function TQueryWord.GetChar (Index: Integer): TChar;
var
  i: Integer;
  
begin
  Result:= #0;
  
  for i:= 0 to Size- 1 do
    if Word [i].Length< Index then
      Result:= Word [i].Chars [Index]
    else
      Dec (Index, Word [i].Length);


end;

constructor TQueryWord.Create;
begin
  inherited;
  
  FPosition:= 0;
  FWordIndex:= 0;
  FCharCount:= 0;
  
end;

procedure TQueryWord.Clear;
begin
  inherited;
  
end;

function TQueryWord.GetNextChar: TChar;
begin
  Result:= Word [FWordIndex].Chars [FPosition];
  Inc (FPosition);
  
  if Word [FWordIndex].Length= FPosition then
  begin
    Inc (FWordIndex);
    FPosition:= 0;
    
  end;
  
end;

function TQueryWord.Copy: TQueryWord;
var
  i: Integer;
  
begin
  Result:= TQueryWord.Create;
  
  for i:= 0 to Size- 1 do
    Result.AddWord (Word [i].Copy);

end;

procedure TQueryWord.DeleteLastChar;
var
  Activeword: TWord;
  
begin
  if Size<> 0 then
  begin
    Activeword:= Word [Size- 1];
    Activeword.DeleteChar (Activeword.Length- 1);
    
    if Activeword.Length= 0 then
    begin
      Activeword.Free;
      DeleteWord (Size- 1);
      
    end;
    
  end;

end;

function TQueryWord.StartWith (Str: String): Boolean;
begin
  if Size= 0 then
    Result:= False
  else if Word [0].Length< Length (Str) then
    Result:= Word [0].StartWith (Str)
  else
    Result:= False;
    
end;

function TQueryWord.EndWith (Str: String): Boolean;
begin

  if Size= 0 then
    Result:= False
  else if Length (Str)< Word [Size- 1].Length then
    Result:= Word [Size- 1].EndWith (Str)
  else
    Result:= False;

end;

procedure TQueryWord.AddWord(NewWord: TWord);
begin
  inherited;
end;

{ TFileReader }

constructor TFileReader.Create (FileName: String);
begin
  inherited Create;
  
  FInput:= TFileStream.Create (FileName, fmOpenRead);
  
end;

procedure TFileReader.Free;
begin
  FInput.Free;
  
  inherited;
  
end;

function TFileReader.ReadMeaning (Position: Int64; Version: Integer): TWordColCollection;
var
  i: Integer;

begin
  Result:= TWordColCollection.Create;
  FInput.Position:= Position;
  
  Result.LoadFromStream (FInput, Version);
  
end;

{ TFileManager }

constructor TFileManager.Create (MaxFileRead: Integer; FileName: String);
var
  i: Integer;
  NewFileReader: TFileReader;
  
begin
  inherited Create;
  
  FFileReaderCollection:= TFileReaderCollection.Create;

  for i:= 0 to MaxFileRead- 1 do
  begin
    NewFileReader:= TFileReader.Create (FileName);
    FFileReaderCollection.AddFileReader (NewFileReader);
    
  end;
  LastIndex:= 0;

end;

procedure TFileManager.Free;
begin
  FFileReaderCollection.Free;
  
  inherited;
  
end;

function TFileManager.GetMeaninig (MeaninigPos: Int64): TWordColCollection;
begin
  LastIndex:= (LastIndex+ 1) mod FFileReaderCollection.Size;
  Result:= FFileReaderCollection.FileReader [LastIndex].ReadMeaning (MeaninigPos, 1);
  
end;

{ TFileReaderCollection }

function TFileReaderCollection.GetFileReader (Index: Integer): TFileReader;
begin
  Result:= Member [Index] as TFileReader;
  
end;

constructor TFileReaderCollection.Create;
begin
  inherited;
  
end;

procedure TFileReaderCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    FileReader [i].Free;
    
  inherited;
  
end;

procedure TFileReaderCollection.AddFileReader (NewFileReader: TFileReader);
begin
  inherited Add (NewFileReader);
  
end;

{ EAnInvalidCharEncountered }

constructor EAnInvalidCharEncountered.Create (Char: String);
var
  i: Integer;
  Str: String;
  
begin
  Str:= 'Invalid Char= ';
  for i:= 1 to Length (Char) do
    Str:= Str+ IntToStr (Ord (Char [i]))+ ' ';

  inherited Create (Str);
  
end;

{ EInvalidVersion }

constructor EInvalidVersion.Create(AClassName, AMethodName: String);
begin
  inherited Create ('Invalid Versoin in '+ AClassName+ '.'+ AMethodName);
  
end;

end.

