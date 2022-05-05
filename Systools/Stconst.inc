{*********************************************************}
{*                   STCONST.INC 1.05                    *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

{string table constants include file}

const
  {string table constants for STREGINI}
  stscFalseString          = 0;
  stscTrueString           = 1;
  stscNoFileKey            = 2;
  stscInvalidPKey          = 3;
  stscNoWin32S             = 4;
  stscCreateKeyFail        = 5;
  stscOpenKeyFail          = 6;
  stscIniWriteFail         = 7;
  stscRegWriteFail         = 8;
  stscNoKeyName            = 9;
  stscQueryKeyFail         = 10;
  stscEnumKeyFail          = 11;
  stscEnumValueFail        = 12;
  stscIniDeleteFail        = 13;
  stscKeyHasSubKeys        = 14;
  stscDeleteKeyFail        = 15;
  stscIniDelValueFail      = 16;
  stscRegDelValueFail      = 17;
  stscOutputFileExists     = 18;
  stscFileHasExtension     = 19;
  stscSaveKeyFail          = 20;
  stscNo16bitSupport       = 21;
  stscCantFindInputFile    = 22;
  stscLoadKeyFail          = 23;
  stscUnloadKeyFail        = 24;
  stscNotWinNTPlatform     = 25;
  stscBadOptionsKeyCombo   = 26;
  stscRestoreKeyFail       = 27;
  stscReplaceKeyFail       = 28;
  stscNoIniFileSupport     = 29;
  stscRemoteKeyIsOpen      = 30;
  stscConnectRemoteKeyFail = 31;
  stscCloseRemoteKeyFail   = 32;
  stscFlushKeyFail         = 33;
  stscBufferDataSizesDif   = 34;
  stscKeyIsEmptyNotExists  = 35;
  stscGetSecurityFail      = 36;
  stscSetSecurityFail      = 37;
  stscByteArrayTooLarge    = 38;
  stscQueryValueFail       = 39;
  stscNoValueNameSpecified = 40;

  {string table constants for container classes}
  stscNoCompare        = 51; {Compare property must be set}
  stscBadType          = 52; {an incompatible class is passed to a method}
  stscBadSize          = 53; {bad size for TStDictionary, TStBits, TStCollection}
  stscDupNode          = 54; {attempt to add duplicate node to TStTree}
  stscBadIndex         = 55; {bad index passed to TStBits or large array}
  stscBadWinMode       = 56; {requires enhanced mode operation}
  stscUnknownClass     = 57; {container class name not registered}
  stscUnknownNodeClass = 58; {container node class name not registered}
  stscNoStoreData      = 59; {container has no store data routine}
  stscNoLoadData       = 60; {container has no load data routine}
  stscWrongClass       = 61; {container class and streamed class not equal}
  stscWrongNodeClass   = 62; {container node class and streamed class not equal}
  stscBadCompare       = 63; {invalid compare function or unable to assign now}
  stscTooManyCols      = 64; {assign a matrix with >1 col to array}
  stscBadColCount      = 65; {assign a matrix with wrong col count to virtual matrix}
  stscBadElSize        = 66; {assign a matrix with wrong elem size to virtual matrix}
  stscBadDups          = 67; {setting Dups to False in a non-empty sorted collection}

  {string table constants for sorting unit}
  stscTooManyFiles = 71; {too many merge files in TStSorter}
  stscFileCreate   = 72; {error creating file in TStSorter}
  stscFileOpen     = 73; {error opening file in TStSorter}
  stscFileWrite    = 74; {error writing file in TStSorter}
  stscFileRead     = 75; {error reading file in TStSorter}
  stscBadState     = 76; {TStSorter in wrong state}

  {string table constants for Bcd unit}
  stscBcdBadFormat   = 81; {bad BCD format}
  stscBcdOverflow    = 82; {BCD larger than 10**64}
  stscBcdDivByZero   = 83; {BCD divide by zero}
  stscBcdBadInput    = 84; {BCD negative input to sqrt, ln, or power}
  stscBcdBufOverflow = 85; {buffer overflow in FormatBcd}

