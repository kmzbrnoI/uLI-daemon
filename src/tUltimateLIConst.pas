unit tUltimateLIConst;

interface

type
  TuLILogLevel = (tllNo = 0, tllErrors = 1, tllCommands = 2, tllData = 3, tllChanges = 4, tllDetail = 5);
  TuLILogEvent = procedure(Sender:TObject; level:TuLILogLevel; msg:string) of object;

implementation

end.
