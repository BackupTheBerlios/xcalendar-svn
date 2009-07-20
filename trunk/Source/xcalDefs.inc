{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$T-} {- Typed @ operator }
{$H+} {- Use long strings by default }
{$X+} {- Extended syntax }

{$DEFINE Delphi5}
{$DEFINE Delphi6}
{$DEFINE Delphi7}
{$DEFINE Delphi12}

{-- Old Versions of Delphi/C++Builder --}
{$IFDEF VER80}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER90}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER93}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER100}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE Delphi4_Down}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE Delphi4_Down}
{$ENDIF}

{$IFDEF Delphi4_Down}
  {$UNDEF Delphi5}
  {$UNDEF Delphi6}
  {$UNDEF Delphi7}
  {$UNDEF Delphi12}
  {$UNDEF Delphi4_Down}
{$ENDIF}
{---------------------------------------}

{$IFDEF VER130}      // Delphi 5.0
  {$UNDEF Delphi6}
  {$UNDEF Delphi7}
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6.0
  {$UNDEF Delphi7}
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7.0
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER160}
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER170}      // Delphi 9.0
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10.0
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER185}      // Delphi 11.0
  {$UNDEF Delphi12}
{$ENDIF}

{$IFDEF VER190}      // Delphi 11.0
  {$UNDEF Delphi12}
{$ENDIF}


{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}

{$IFNDEF Delphi6}
  {$DEFINE MSWINDOWS}
  {$IFDEF LINUX}
    {$UNDEF MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF Delphi6}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF Delphi7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF Delphi12}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF}

