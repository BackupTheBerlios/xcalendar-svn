{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$T-} {- Typed @ operator }
{$H+} {- Use long strings by default }
{$X+} {- Extended syntax }

{$IFDEF VER120}      // Delphi 4.0
  {$DEFINE Delphi4}
{$ENDIF}

{$IFDEF VER130}      // Delphi 5.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER170}      // Delphi 9.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$DEFINE Delphi10}
  {$DEFINE RICHBCB}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER185}      // Delphi 11.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$DEFINE Delphi10}
  {$DEFINE Delphi11}
  {$DEFINE RICHBCB}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER190}      // Delphi 11.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$DEFINE Delphi10}
  {$DEFINE Delphi11}
  {$DEFINE RICHBCB}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER125}      // Borland C++ Builder 4.0
  {$DEFINE Delphi4}
  {$ObjExportAll On}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER130}      // Borland C++ Builder 5.0
  {$IFDEF BCB}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}      // Borland C++ Builder 6.0
  {$IFDEF BCB}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFNDEF Delphi6}
  {$DEFINE MSWINDOWS}
  {$IFDEF LINUX}
    {$UNDEF MSWINDOWS}
  {$ENDIF}
{$ENDIF}
