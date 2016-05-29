@ECHO OFF
SETLOCAL

:: Script for generation of rc VERSIONINFO & StringFileInfo

:: ====================
:: Installation Variables
:: ====================
:: VERSION_FILE - Untracked file to be included in packaged source releases.
::                it should contain a single line in the format:
::                $Project_Name VERSION $tag (ie: Foobar VERSION v1.0.0-alpha0)
SET "VERSION_FILE=.gitversion"

:: DEFAULT_VERSION - Version string to be processed when neither Git nor a
::                packed version file is available.
SET "DEFAULT_VERSION=v0.0.0-0-g0000000"

:: USE_UNANNOTATED_TAGS - Modify git commands to process unannotated tags
::                in addition to annotated tags.
::                   --tags - process unannotated tags as well
::                          - leave empty to process only annotated tags
SET "USE_UNANNOTATED_TAGS=--tags"

:: HASH_ABBREV - Length of the abbreviated SHA-1 commit hash.
::                The git default length is 7 characters. The minimum length is 4.
SET "HASH_ABBREV=7"

:: --------------------
:CHECK_ARGS
:: --------------------
SET "fTESTING=0"
SET "fLEAVE_NOW=0"
SET "fFORCE=0"
SET "fVERBOSE=0"
SET "fQUIET=0"
SET "CACHE_FILE="
SET "HEADER_OUT_FILE="

:: Console output only.
IF [%1] EQU [] GOTO :START

IF "%~1" EQU "--help" GOTO :USAGE
IF "%~1" EQU "--force" SET "fFORCE=1" & SHIFT
IF "%~1" EQU "--quiet" SET "fQUIET=1" & SHIFT
IF "%~1" EQU "--verbose" SET "fVERBOSE=1" & GOTO :START

:: Un-documented switch
IF "%~1" EQU "--test" GOTO :TEST

IF EXIST %~1\NUL (
  REM %1 is a path
  SET "CACHE_FILE=%~s1\%VERSION_FILE%"
  SHIFT
)

IF [%~nx1] NEQ [] (
  REM %1 is a file
  SET "HEADER_OUT_FILE=%~fs1"
  SHIFT
)
:: This should always be the last argument.
IF [%1] NEQ [] GOTO :USAGE

:: Some basic sanity checks.
IF %fQUIET% EQU 1 (
  IF "%HEADER_OUT_FILE%" EQU "" GOTO :USAGE
)

IF "%CACHE_FILE%" NEQ "" (
  SET "CACHE_FILE=%CACHE_FILE:\\=\%"
  IF "%HEADER_OUT_FILE%" EQU "" GOTO :USAGE
)
GOTO :START

:: --------------------
:USAGE
:: --------------------
ECHO usage: [--help] ^| [--verbose] ^| [--force] [--quiet] [CACHE_PATH] [OUT_FILE]
ECHO.
ECHO  When called without arguments, basic version information writes to console.
ECHO  When called with --verbose argument, version information writes to console.
ECHO.
ECHO  --help      - displays this output.
ECHO.
ECHO  --verbose   - Verbose console output.
ECHO.
ECHO  --force     - Ignore cached version information.
ECHO  --quiet     - Suppress console output.
ECHO.
ECHO  CACHE_PATH  - Path for non-tracked file to store git-describe version.
ECHO  OUT_FILE    - Path to writable file that is included in the project`s rc file.
ECHO.
ECHO  Version information is expected to be in the format:
ECHO  vMajor.Minor.Micro[.Revision]-Commits-Hash
ECHO.
ECHO  Example pre-build event:
ECHO  CALL GIT-VS-VERSION-GEN.bat "$(SolutionDir)" "$(SolutionDir)src\gitversion.h"
ECHO.
GOTO :END


:: ===================
:: Entry Point
:: ===================
:START
ECHO.
CALL :INIT_VARS
CALL :GET_VERSION_STRING
IF %fGIT_AVAILABLE% EQU 1 (
  IF %fLEAVE_NOW% EQU 1 GOTO :END
  IF "%CACHE_FILE%" NEQ "" (
    CALL :CHECK_CACHE
  )
)
IF %fLEAVE_NOW% EQU 1 GOTO :END
CALL :PARSE_VERSION_STRING
CALL :WRITE_CACHE
CALL :PREP_OUT
CALL :WRITE_OUT
GOTO :END

:: ====================
:: FUNCTIONS
:: ====================
:: --------------------
:INIT_VARS
:: --------------------
:: The following variables are used for the final version output.
SET strGIT_HEAD_DESCRIBE=
SET strGIT_HEAD_TAG=
SET numGIT_HEAD_COMMITS=0
SET strGIT_HEAD_TAG_VERSION=
SET strGIT_HEAD_STAGE=
SET strGIT_HEAD_HASH=g0000000
SET strGIT_HEAD_DATE=
SET strGIT_HEAD_NAME=
SET strGIT_BRANCH_NAME=
SET numGIT_BRANCH_COMMITS=0

SET strGIT_MAJOR_TAG=
SET strGIT_MAJOR_HASH=
SET numGIT_MAJOR_COMMITS=0
SET strGIT_MINOR_TAG=
SET strGIT_MINOR_HASH=
SET numGIT_MINOR_COMMITS=0
SET strGIT_MICRO_TAG=
SET strGIT_MICRO_HASH=
SET numGIT_MICRO_COMMITS=0

:: Digital Version: Major, Minor, Mirco, Revision
SET numMAJOR_NUMBER=0
SET numMINOR_NUMBER=0
SET numMICRO_NUMBER=0
SET numREVISION_NUMBER=0

:: VERSIONINFO VS_FF_ flags
SET fGIT_AVAILABLE=0
SET fPRIVATE=0
SET fPATCHED=0
SET fPRERELEASE=0
SET fSPECIAL=0

:: Supporting StringFileInfo - not used for clean release builds.
SET strPRIVATE=
SET strCOMMENT=

GOTO :EOF

:: --------------------
:GET_VERSION_STRING
:: --------------------
:: Precedence is Git, CACHE_FILE, then DEFAULT_VERSION.
:: Check if git is available by testing git describe.
CALL git describe>NUL 2>&1
IF NOT ERRORLEVEL 1 (
  SET "fGIT_AVAILABLE=1"
  REM Parse git version string
  CALL :GET_GIT_VERSION_STRING
) ELSE (
  REM Use the CACHE_FILE if it exists.
  IF EXIST "%CACHE_FILE%" (
    SET "line=0"
    FOR /F "tokens=*" %%A IN (%CACHE_FILE%) DO (
      CALL :READ_CACHE %%A
    )
    SET line=
  ) ELSE (
    REM Default to the DEFAULT_VERSION
    SET "strGIT_HEAD_DESCRIBE=%DEFAULT_VERSION%"
  )
)
GOTO :EOF

:: --------------------
:READ_CACHE
:: --------------------
SET /A "line+=1"
IF %line% EQU 1 SET "strGIT_HEAD_DESCRIBE=%*"
IF %line% EQU 2 SET "strGIT_HEAD_DATE=%*"
IF %line% EQU 3 SET "strGIT_HEAD_NAME=%*"

IF %line% EQU 4 SET "strGIT_BRANCH_NAME=%*"
IF %line% EQU 5 SET "numGIT_BRANCH_COMMITS=%*"

IF %line% EQU 6 SET "strGIT_MAJOR_TAG=%*"
IF %line% EQU 7 SET "numGIT_MAJOR_COMMITS=%*"

IF %line% EQU 8 SET "strGIT_MINOR_TAG=%*"
IF %line% EQU 9 SET "numGIT_MINOR_COMMITS=%*"

IF %line% EQU 10 SET "strGIT_MICRO_TAG=%*"
IF %line% EQU 11 SET "numGIT_MICRO_COMMITS=%*"
GOTO :EOF

:: --------------------
:GET_GIT_VERSION_STRING
:: --------------------
FOR /F "tokens=*" %%A IN ('"git describe %USE_UNANNOTATED_TAGS% --long --abbrev=%HASH_ABBREV% --dirty=-dirty"') DO (
  SET "strGIT_HEAD_DESCRIBE=%%A"
)
FOR /F "tokens=*" %%A IN ('"git rev-parse --verify --short=%HASH_ABBREV% HEAD"') DO (
  SET "strGIT_HEAD_HASH=g%%A"
)
FOR /F "tokens=*" %%A IN ('"git show -s --format=%%cN HEAD"') DO (
  SET "strGIT_HEAD_NAME=%%A"
)
FOR /F "tokens=*" %%A IN ('"git show -s --format=%%ci HEAD"') DO (
  SET "strGIT_HEAD_DATE=%%A"
)
FOR /F "tokens=*" %%A IN ('"git rev-parse --verify --abbrev-ref HEAD"') DO (
  SET "strGIT_BRANCH_NAME=%%A"
)
FOR /F "tokens=*" %%A IN ('"git rev-list --count HEAD"') DO (
  SET "numGIT_BRANCH_COMMITS=%%A"
)
GOTO :EOF

:: --------------------
:CHECK_CACHE
:: --------------------
:: Exit early if a cached git built version matches the current version.
IF "%HEADER_OUT_FILE%" NEQ "" (
  IF EXIST "%HEADER_OUT_FILE%" (
    IF %fFORCE% EQU 1 DEL "%CACHE_FILE%"
    IF EXIST "%CACHE_FILE%" (
      FOR /F "tokens=*" %%A IN (%CACHE_FILE%) DO (
        IF "%%A" EQU "%strGIT_HEAD_DESCRIBE%" (
          IF %fQUIET% EQU 0 (
            ECHO Build version is assumed unchanged from: %strGIT_HEAD_DESCRIBE%.
          )
          SET "fLEAVE_NOW=1"
        )
        GOTO :EOF
      )
    )
  )
)
GOTO :EOF

:: --------------------
:WRITE_CACHE
:: --------------------
IF "%CACHE_FILE%" NEQ "" (
  (
    ECHO %strGIT_HEAD_DESCRIBE%
    ECHO %strGIT_HEAD_DATE%
    ECHO %strGIT_HEAD_NAME%

    ECHO %strGIT_BRANCH_NAME%
    ECHO %numGIT_BRANCH_COMMITS%

    ECHO %strGIT_MAJOR_TAG%
    ECHO %numGIT_MAJOR_COMMITS%

    ECHO %strGIT_MINOR_TAG%
    ECHO %numGIT_MINOR_COMMITS%

    ECHO %strGIT_MICRO_TAG%
    ECHO %numGIT_MICRO_COMMITS%
  ) > "%CACHE_FILE%"
)
GOTO :EOF

:: --------------------
:PARSE_VERSION_STRING
:: --------------------
SET tmp=
SET "str=%strGIT_HEAD_DESCRIBE%"
CALL :strip str tmp

:: When HEAD is dirty the build is considered both PRIVATE and PATCHED
:: PATCHED indicates that the HEAD is dirty, and the string Private Build is used.
IF "%tmp%" EQU "dirty" (
  SET "fPATCHED=1"
  SET "fPRIVATE=1"
  CALL :strip str tmp
)

:: Set commit hash from string
IF "%tmp:~0,1%" EQU "g" (
  SET "tmpGIT_HEAD_HASH=%tmp%"
  CALL :strip str tmp
)

SET "numGIT_HEAD_COMMITS=%tmp%"

:: PATCHED indicates that the build is taking place at a non-tagged commit.
IF %numGIT_HEAD_COMMITS% NEQ 0 (
  SET "fPATCHED=1"
)
SET "strGIT_HEAD_TAG=%str%"

:: Identify version number from stage
FOR /F "tokens=1 delims=-" %%A IN ("%str%") DO (
  SET "strGIT_HEAD_TAG_VERSION=%%A"
)
CALL SET "str=%%str:%strGIT_HEAD_TAG_VERSION%=%%"

:: The min vetrsion is X.Y.Z and the max is X.Y.Z.Stage#.Commits.SHA.dirty
:: strTMP_STAGE_PART is a holder for anything past 'X.Y.Z.'.
FOR /F "tokens=1,2,3,4 delims=." %%A IN ("%strGIT_HEAD_TAG_VERSION%") DO (
  IF [%%A] NEQ [] SET "numMAJOR_NUMBER=%%A"
  IF [%%B] NEQ [] SET "numMINOR_NUMBER=%%B"
  IF [%%C] NEQ [] SET "numMICRO_NUMBER=%%C"
  IF [%%D] NEQ [] SET "numREVISION_NUMBER=%%D"
)
IF "%numMAJOR_NUMBER:~,1%" EQU "v" (
  SET "numMAJOR_NUMBER=%numMAJOR_NUMBER:~1%"
)
:: Capture full version maintenance releases. (ie: v1.0.0.1)
IF %numREVISION_NUMBER% NEQ 0 (
  SET "fSPECIAL=1"
)
IF "%str%" NEQ "" (
  SET "strGIT_HEAD_STAGE=%str:~1%"
)
IF "%strGIT_HEAD_STAGE:~,5%" EQU "alpha" (
  SET "fPRERELEASE=1"
) ELSE (
  IF "%strGIT_HEAD_STAGE:~,4%" EQU "beta" (
    SET "fPRERELEASE=1"
  ) ELSE (
    IF "%strGIT_HEAD_STAGE:~,2%" EQU "rc" (
      SET "fPRERELEASE=1"
    )
  )
)
SET "strCOMMENT=%strGIT_HEAD_STAGE%"

IF %fGIT_AVAILABLE% EQU 1 (
  SET "tmp=*%numMAJOR_NUMBER%.0.0*"
  CALL :GET_GIT_PATCHES tmp strGIT_MAJOR_TAG strGIT_MAJOR_HASH numGIT_MAJOR_COMMITS
  SET "tmp=*%numMAJOR_NUMBER%.%numMINOR_NUMBER%.0*"
  CALL :GET_GIT_PATCHES tmp strGIT_MINOR_TAG strGIT_MINOR_HASH numGIT_MINOR_COMMITS
  SET "tmp=*%numMAJOR_NUMBER%.%numMINOR_NUMBER%.%numMICRO_NUMBER%*"
  CALL :GET_GIT_PATCHES tmp strGIT_MICRO_TAG strGIT_MICRO_HASH numGIT_MICRO_COMMITS
)
SET str=
SET tmp=
EXIT /B

:: --------------------
:GET_GIT_PATCHES <in_version> <out_tag> <out_hash> <out_commits>
:: --------------------
:: Read in the description of the current commit in terms of the earliest
:: release stream tag.
SETLOCAL ENABLEDELAYEDEXPANSION
SET tag=
FOR /F "tokens=* usebackq" %%A IN (`"git for-each-ref --count=1 --format=%%(refname:short) --sort=taggerdate refs/tags/!%~1!"`) DO SET "tag=%%A"
IF "%tag%" NEQ "" (
  FOR /F "tokens=* usebackq" %%A IN (`"git describe %USE_UNANNOTATED_TAGS% --long --abbrev=%HASH_ABBREV% --match %tag%"`) DO ( SET "describe=%%A" )
  CALL :strip describe hash
  CALL :strip describe commits
)
ENDLOCAL & IF "%tag%" NEQ "" ( SET "%~2=%tag%" & SET "%~3=%hash%" & SET "%~4=%commits%" )
EXIT /B

:: --------------------
:PREP_OUT
:: --------------------
SET "csvFILE_VERSION=%numMAJOR_NUMBER%,%numMINOR_NUMBER%,%numMICRO_NUMBER%,%numREVISION_NUMBER%"
SET hexFILE_VERSION=
CALL :SET_HEX

IF %fPATCHED% NEQ 0 SET "fPATCHED=VS_FF_PATCHED"
IF %fPRIVATE% NEQ 0 SET "fPRIVATE=VS_FF_PRIVATEBUILD" & SET "strPRIVATE=Private Build"
IF %fSPECIAL% NEQ 0 SET "fSPECIAL=VS_FF_SPECIALBUILD"
IF %fPRERELEASE% NEQ 0 SET "fPRERELEASE=VS_FF_PRERELEASE"
GOTO :EOF

:: --------------------
:SET_HEX
:: --------------------
:: Iterate Major, Minor, Micro, Revision as set in csvFILEVERSION and convert to
:: hex while appending to the hexFILE_VERION string to give a padded 32bit
:: end result. ie: v1.0.1.34 = 0x0001000000010022
SET "hex_values=0123456789ABCDEF"

FOR /F "tokens=1-4 delims=," %%A IN ("%csvFILE_VERSION%") DO (
  CALL :int2hex %%A
  CALL :int2hex %%B
  CALL :int2hex %%C
  CALL :int2hex %%D
)

SET "hexFILE_VERSION=0x%hexFILE_VERSION%"
SET hex_values=

GOTO :EOF

:int2hex
SETLOCAL ENABLEDELAYEDEXPANSION
SET /A pad=4
SET /A iVal=%1

:hex_loop
SET /A pad=%pad% - 1
SET /A hVal=%iVal% %% 16
SET hVal=!hex_values:~%hVal%,1!
SET hex_word=%hVal%%hex_word%
SET /A iVal=%iVal% / 16
IF %iVal% GTR 0 GOTO :hex_loop

:hex_pad_loop
FOR /L %%A in (1,1,%pad%) DO SET "hex_word=0!hex_word!"
ENDLOCAL & SET "hexFILE_VERSION=%hexFILE_VERSION%%hex_word%"
GOTO :EOF

:: --------------------
:strip <in_out_string> <out_string>
:: --------------------
SETLOCAL ENABLEDELAYEDEXPANSION
SET "str=!%~1!"
SET "len=0"
:strip_loop
SET /A "len+=1"
SET "tmp=!%~1:~-%len%%!"
IF "!tmp!" EQU "!str!" ( GOTO :strip_out )
IF "!tmp:~0,1!" NEQ "-" ( GOTO :strip_loop )
SET "str=!%~1:~0,-%len%%!"
SET "out=!tmp:~1!"
:strip_out
ENDLOCAL & IF "%out%" NEQ "" ( SET "%~1=%str%" & SET "%~2=%out%" )
EXIT /B

:: --------------------
:strlen <in_string> <out_length>
:: --------------------
SETLOCAL ENABLEDELAYEDEXPANSION
SET "str=!%~1!#"
SET "len=0"
FOR %%A IN (4096 2048 1024 512 256 128 64 32 16 8 4 2 1) DO (
  IF "!str:~%%A,1!" NEQ "" (
    SET /A "len+=%%A"
    SET "str=!str:~%%A!"
  )
)
ENDLOCAL & SET "%~2=%len%"
EXIT /B

:: --------------------
:WRITE_OUT
:: --------------------
:: HEADER_OUT falls through to CON_OUT which checks for the QUIET flag.
IF "%HEADER_OUT_FILE%" NEQ "" (
  CALL :OUT_HEADER
) ELSE (
  IF %fTESTING% EQU 0 (
    CALL :CON_OUT
  ) ELSE (
    CALL :TEST_OUT
  )
)
GOTO :EOF

:: --------------------
:OUT_HEADER
:: --------------------
(
ECHO // GIT-VS-VERSION-GEN.bat auto generated resource header.
ECHO #define GIT_VERSION_MAJOR    %numMAJOR_NUMBER%
ECHO #define GIT_VERSION_MINOR    %numMINOR_NUMBER%
ECHO #define GIT_VERSION_MICRO    %numMICRO_NUMBER%
ECHO #define GIT_VERSION_REVISION %numREVISION_NUMBER%
ECHO.
ECHO #define GIT_DIGITAL_VERSION  %csvFILE_VERSION%
ECHO #define GIT_VERSION_HEX      %hexFILE_VERSION%
ECHO.
ECHO #define GIT_HEAD_DESCRIBE    "%strGIT_HEAD_DESCRIBE%"
ECHO #define GIT_HEAD_TAG         "%strGIT_HEAD_TAG%"
ECHO #define GIT_HEAD_TAG_VERSION "%strGIT_HEAD_TAG_VERSION%"
ECHO #define GIT_HEAD_COMMITS     %numGIT_HEAD_COMMITS%
ECHO #define GIT_HEAD_HASH        "%strGIT_HEAD_HASH%"
ECHO #define GIT_HEAD_DATE        "%strGIT_HEAD_DATE%"
ECHO #define GIT_HEAD_NAME        "%strGIT_HEAD_NAME%"
ECHO.
ECHO #define GIT_MAJOR_TAG        "%strGIT_MAJOR_TAG%"
ECHO #define GIT_MAJOR_COMMITS    %numGIT_MAJOR_COMMITS%
ECHO #define GIT_MINOR_TAG        "%strGIT_MINOR_TAG%"
ECHO #define GIT_MINOR_COMMITS    %numGIT_MINOR_COMMITS%
ECHO #define GIT_MICRO_TAG        "%strGIT_MICRO_TAG%"
ECHO #define GIT_MICRO_COMMITS    %numGIT_MICRO_COMMITS%
ECHO.
ECHO #define GIT_BRANCH_NAME      "%strGIT_BRANCH_NAME%"
ECHO #define GIT_BRANCH_COMMITS   %numGIT_BRANCH_COMMITS%
ECHO.
ECHO #define GIT_PATCHED_FLAG     %fPATCHED%
ECHO #define GIT_PRERELEASE_FLAG  %fPRERELEASE%
ECHO #define GIT_SPECIAL_FLAG     %fSPECIAL%
ECHO #define GIT_PRIVATE_FLAG     %fPRIVATE%
ECHO.
ECHO #define GIT_PRIVATE_STRING   "%strPRIVATE%"
ECHO #define GIT_COMMENT_STRING   "%strCOMMENT%"
) > "%HEADER_OUT_FILE%"

:: --------------------
:CON_OUT
:: --------------------
IF %fQUIET% EQU 1 GOTO :EOF
ECHO // GIT-VS-VERSION-GEN.bat auto generated resource header.
IF %fVERBOSE% EQU 1 (
  ECHO #define GIT_VERSION_MAJOR    %numMAJOR_NUMBER%
  ECHO #define GIT_VERSION_MINOR    %numMINOR_NUMBER%
  ECHO #define GIT_VERSION_MICRO    %numMICRO_NUMBER%
  ECHO #define GIT_VERSION_REVISION %numREVISION_NUMBER%
  ECHO.
  ECHO #define GIT_DIGITAL_VERSION  %csvFILE_VERSION%
  ECHO #define GIT_VERSION_HEX      %hexFILE_VERSION%
  ECHO.
)
ECHO #define GIT_HEAD_DESCRIBE    "%strGIT_HEAD_DESCRIBE%"
IF %fVERBOSE% EQU 1 (
  ECHO #define GIT_HEAD_TAG         "%strGIT_HEAD_TAG%"
  ECHO #define GIT_HEAD_TAG_VERSION "%strGIT_HEAD_TAG_VERSION%"
  ECHO #define GIT_HEAD_COMMITS     %numGIT_HEAD_COMMITS%
  ECHO #define GIT_HEAD_HASH        "%strGIT_HEAD_HASH%"
  ECHO #define GIT_HEAD_DATE        "%strGIT_HEAD_DATE%"
  ECHO #define GIT_HEAD_NAME        "%strGIT_HEAD_NAME%"
  ECHO.
  ECHO #define GIT_MAJOR_TAG        "%strGIT_MAJOR_TAG%"
  ECHO #define GIT_MAJOR_COMMITS    %numGIT_MAJOR_COMMITS%
  ECHO #define GIT_MINOR_TAG        "%strGIT_MINOR_TAG%"
  ECHO #define GIT_MINOR_COMMITS    %numGIT_MINOR_COMMITS%
  ECHO #define GIT_MICRO_TAG        "%strGIT_MICRO_TAG%"
  ECHO #define GIT_MICRO_COMMITS    %numGIT_MICRO_COMMITS%
  ECHO.
  ECHO #define GIT_BRANCH_NAME      "%strGIT_BRANCH_NAME%"
  ECHO #define GIT_BRANCH_COMMITS   %numGIT_BRANCH_COMMITS%
  ECHO.
  ECHO #define GIT_PATCHED_FLAG     %fPATCHED%
  ECHO #define GIT_PRERELEASE_FLAG  %fPRERELEASE%
  ECHO #define GIT_SPECIAL_FLAG     %fSPECIAL%
  ECHO #define GIT_PRIVATE_FLAG     %fPRIVATE%
  ECHO.
  ECHO #define GIT_PRIVATE_STRING   "%strPRIVATE%"
  ECHO #define GIT_COMMENT_STRING   "%strCOMMENT%"
)
GOTO :EOF

:: --------------------
:TEST
:: --------------------
:: Create the test directory & repo
SET TERM=
SET fTESTING=1
SET git-vs-version-test-dir=git-vs-version-test
MKDIR %git-vs-version-test-dir%
PUSHD %git-vs-version-test-dir%
CALL git init >NUL 2>&1
IF ERRORLEVEL 1 (
  ECHO Test requires git.
  GOTO :END
)

:: Generate the test patches and tags
SET test_stage=-alpha
:TEST_LOOP
FOR /L %%A IN (0,1,1) DO (
  SET test_ver=%test_stage%%%A
  IF "%test_stage%" EQU "" SET test_ver=
  CALL git commit --allow-empty -m "Commit v1.0.0%%test_ver%%" >NUL
  IF ERRORLEVEL 1 GOTO :END
  CALL git tag -a v1.0.0%%test_ver%% -m "Test v1.0.0%%test_ver%%"
  IF ERRORLEVEL 1 GOTO :END
  FOR /L %%B IN (0,1,2) DO (
    CALL git commit --allow-empty -m "Work on v1.0.0%%test_ver%%" >NUL
    IF ERRORLEVEL 1 GOTO :END
  )
  IF "%test_stage%" EQU "" GOTO :TEST_MAINT
)
IF "%test_stage%" EQU "-alpha" SET test_stage=-beta& GOTO :TEST_LOOP
IF "%test_stage%" EQU "-beta" SET test_stage=-rc& GOTO :TEST_LOOP
IF "%test_stage%" EQU "-rc" SET test_stage=& GOTO :TEST_LOOP
SET test_stage=

:TEST_MAINT
:: Simulate a maint patch
CALL git commit --allow-empty -m "Maint Work on v1.0.0.1" >NUL
CALL git tag -a v1.0.0.1 -m "Test v1.0.0.1"

:: Simulate the first patch for v1.0.1
CALL git commit --allow-empty -m "Starting v1.0.1" >NUL
CALL git tag -a v1.0.1-alpha0 -m "Test v1.0.1-alpha0"

:: Simulate another maint patch
CALL git commit --allow-empty -m "Maint Work on v1.0.0.2" >NUL
CALL git tag -a v1.0.0.2 -m "Test v1.0.0.2"

:: Generate the output
ECHO TAG, Version, Hex, Major, Minor, Micro, Revision, PreRelease, Private, Patched, Comment

SET git_cmd=git for-each-ref --format=%%(refname:short) refs/tags/
FOR /F "tokens=* usebackq" %%A IN (`"%git_cmd%"`) DO (
  CALL git reset --hard %%A >NUL
  CALL :TEST_VERSION %%A
)

:: Builder checked out the parent of v1.0.0
CALL git reset --hard v1.0.0~1 >NUL
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

:: Builder staged a file.
CALL TYPE NUL > README
CALL git add README
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

:: Builder checks out a tagged release and stages a file
CALL git reset --hard v1.0.0 >NUL
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL TYPE NUL > README
CALL git add README
CALL :TEST_VERSION %tmp%

:: Builder commits that file.
CALL git commit -m "Modified Release" >NUL
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

:: Builder creates own tag
CALL git tag v1.0.0-custom -m "Modified Release Tag"
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

:: Builder checked out a maint release and staged a file
CALL git reset --hard v1.0.0.1 >NUL
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL TYPE NUL > README
CALL git add README
CALL :TEST_VERSION %tmp%

:: Builder commits that file.
CALL git commit -m "Modified Maint Release" >NUL
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

:: Builder creates own tag
CALL git tag v1.0.0.1-custom -m "Modified Maint Release Tag"
FOR /F "tokens=*" %%A IN ('"git describe --long HEAD"') DO SET tmp=%%A
CALL :TEST_VERSION %tmp%

ECHO.

:: Cleanup the directory
POPD
RMDIR /S /Q %git-vs-version-test-dir%
GOTO :EOF

:: --------------------
:TEST_VERSION
:: --------------------
SET TEST_OUT_STRING=%1
CALL :START
ECHO %TEST_OUT_STRING%
SET TEST_OUT_STRING=
GOTO :EOF

:: --------------------
:TEST_OUT
:: --------------------
SET TEST_OUT_STRING=%TEST_OUT_STRING%, %strGIT_HEAD_DESCRIBE%
SET csvFILE_VERSION=%csvFILE_VERSION:,=, %
SET TEST_OUT_STRING=%TEST_OUT_STRING%, %csvFILE_VERSION%, %hexFILE_VERSION%
SET TEST_OUT_STRING=%TEST_OUT_STRING%, %fPRERELEASE%, %fPRIVATE%, %fPATCHED%, %strCOMMENT%
GOTO :EOF

:: --------------------
:END
:: --------------------
