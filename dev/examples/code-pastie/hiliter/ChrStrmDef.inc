{-- ChrStream fake object ---------------------------------------------------}

{ NOTE: do not access privf or privm, this is equivilent to Class private.
      privm = private methods, DO NOT ACCESS outside implementation
      privf = private fields, DO NOT ACCESS outside implementation

  If need to, you can shoot yourself in the foot and access private outside 
  of the implementation. But you were warned, and the contract told you not 
  to.

  The private methods and fields are only to be accessed like a class accesses 
  private fields and methods only in it's implementation unit. The end user 
  should not access private methods or fields. Should not, as in he really can 
  if he wants that power - but the contract says NO. 

  Records are used because records are more DLL/DSO compatible than objects. 
  This was a fake object experiment in order to make my object inside a record 
  but with predictable DSO/DLL behaviour. Using real objects/classes would have 
  meant that I may have unpredictible DLL/DSO behaviour since we don't know 
  everything about how objects work in a DLL.      }                


type
  TChrStrmPriv = record   
    FBufEnd: integer;
    FBufPos: integer;
    FPutBackInx: integer;
    sReadCount: integer; // keep track of how many times FTextContent.astring was read from
    FBuffer: PByteArray;
    FPutBackBuf: array [0..1] of char;
    FTextContent: TMultiType; //multi types: textfile, string, this allows us
                              //to extend FTextContent in the future to contain
			      //more text types: pchar, array, etc.
  end; { private fields } 

  TChrStrmPrivMethods = record  
    GetBuffer: procedure(var self: TChrStrmPriv);
  end; { private methods } 

type
  PChrStrm = ^TChrStrm;
  TChrStrm = record
  { private }
    privf: TChrStrmPriv; 
    privm: TChrStrmPrivMethods; 
  { public } 
    GetChar: function(self: PChrStrm): char;
    PutBackChar: procedure(aCh: char; self: PChrStrm);
  end;


