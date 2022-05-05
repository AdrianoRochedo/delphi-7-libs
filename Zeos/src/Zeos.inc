// Compilation directives for Delphi4
{$IFDEF VER120}
{$DEFINE VER125BELOW}
{$DEFINE VER130BELOW}
{$DEFINE VER130KBELOW}
{$ENDIF}

// Compilation directives for C++ Builder 4
{$IFDEF VER125}
{$DEFINE VER125BELOW}
{$DEFINE VER130BELOW}
{$DEFINE VER130KBELOW}
{$ENDIF}

// Compilation directives for Delphi 5
{$IFDEF VER130}
{$DEFINE VER130BELOW}
{$DEFINE VER130KBELOW}
{$ENDIF}

// Compilation directives for Kylix 1
{$IFDEF LINUX}
{$DEFINE VER130KBELOW}
{$ENDIF}

// Supresses output messages in unit tests
{$DEFINE SUPPRESS_TEST_OUTPUT}

// Supresses memory leaks test
{$DEFINE SUPPRESS_MEMORY_TEST}

// Excludes old ZeosDBO from the performance tests
{$DEFINE EXCLUDE_OLD_ZEOS_TEST}

// Excludes DBExpress from the performance tests
{$DEFINE EXCLUDE_DBX_TEST}

// Excludes IBX from the performance tests
{$DEFINE EXCLUDE_IBX_TEST}

// Excludes BDE from the performance tests
{$DEFINE EXCLUDE_BDE_TEST}

// Registers property editors for the components.
{$DEFINE WITH_PROPERTY_EDITOR}

// Disables checking code.
// You may uncomment it off for optimization
//{$DEFINE DISABLE_CHECKING}
