unit tokentypes;  {$ifdef fpc} {$mode objfpc} {$h+} {$endif}

interface

type
  TPasToken = (
    ptInvalidToken,  {bad}
    ptEndOfFile,
    ptAddress,       { @ }
    ptAssign,        { := }
    ptCaret,         { caret: ^ }
    ptCloseBracket,  { close square bracket: ] }
    ptCloseParen,    { ) }
    ptColon,         { : }
    ptComma,         { , }
    ptComment,       { all comments  }
    ptCR,            { carriage return}
    ptDirective,     { preprocessor directive or conditional }
    ptDivide,
    ptEquals,        { = }
    ptGreater,       { > }
    ptGreaterEqual,  { >= }
    ptHash,          { pound/hash # }
    ptHexNumber,     { starts with $ }
    ptIdentifier,    { i.e. words that aren't keywords }
    ptKeyword,       { i.e. if, while, do, ... }
    ptLess,          { < }
    ptLessEqual,     { <= }
    ptLF,            { line feed}
    ptMinus,
    ptMultiply,
    ptNotEquals,     { <> }
    ptNumber,        { sequence of digits, maybe with radix point }
    ptOpenBracket,   { open square bracket: [ }
    ptOpenParen,     { ( }
    ptPeriod,        { . }
    ptPlus,
    ptRange,         { [x..x] }
    ptSemicolon,     { ; }
    ptSpace,
    ptStartOfFile,
    ptString,        { string or character constant }
    ptWhitespace    { #1..#32 }
    );

implementation

end.
