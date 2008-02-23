{-- parser fake object --------------------------------------------------------}

{ NOTE: do not access privf, this is equivilent to Class private fields!
  That is why it is labelled as private below. (as old records are always 
  public, there is more power to shoot yourself in the foot if you really need 
  to, though. The contract states don't access them - but you could, if you 
  wanted. 
   
  Records are used because records are more DLL/DSO compatible than objects. 
  This was a fake object experiment in order to make my object inside a record 
  but with predictable DSO/DLL behaviour. Using real objects/classes would have 
  meant that I may have unpredictible DLL/DSO behaviour since we don't know 
  everything about how objects work in a DLL.      }                

type
  TPasParserPriv = record
     FStrm: PChrStrm;
     FKeywords: PStrlist;
  end; { private fields }

type
  PPasParser = ^TPasParser;
  TPasParser = record
  { private }
    privf: TPasParserPriv;
  { public }
    GetToken: procedure(var ATokType: TPasToken; var ATok: astr; 
                        ParserSelf: PPasParser; self: PChrStrm);
  end;

{------------------------------------------------------------------------------}
