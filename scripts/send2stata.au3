#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.2.0
 Author:         Bill Rising

Script Function: for taking clipboard content and sending it to Stata

#ce ----------------------------------------------------------------------------


; arguments are : doThis, tmpDoFile, stataInBack
; stataInBack is empty if Stata should be left unactivated
; stataInstance is needed only if there are multiple Stata's running
; stataVersion is needed only if there are multiple Stata's with overlapping versions
;; declarations
Opt("MustDelcareVars",1)
#include <Array.au3>
Opt("WinTitleMatchMode", 2)

; MsgBox(16,"what was sent", $CmdLineRaw)

Dim $defaultTmpDoFile="feedStata.do"
Dim $numArgs = $CmdLine[0]
; arg  1     2                3                             4               5            6
Dim $doThis, $stataInBack="", $tmpDoFile=$defaultTmpDoFile
; arg  4               5                 6                7              8
Dim $stataInstance="", $stataVersion="", $stataFlavor="", $strictMatch="",  $sendToAll=""
Dim $theStatas, $howManyStatas, $theStataName
Dim $pasteMe

if $numArgs = 0 Then
	badFirstArg("")
Else
	$doThis = $CmdLine[1]
	if ($doThis <> "command") AND ($doThis <>  "menu") AND ($doThis <> "dofile") AND ($doThis <> "include") Then
		badFirstArg($doThis)
	EndIf
	if $numArgs > 1 Then
		$stataInBack = $CmdLine[2]
		if $numArgs > 2 Then
			if $CmdLine[3] <> "" Then
				$tmpDoFile = $CmdLine[3]
			EndIf
			if $numArgs > 3 Then
				$stataInstance = $CmdLine[4]
				if $numArgs > 4 Then
					$stataVersion = $CmdLine[5]
					if $numArgs > 5 Then
						$stataFlavor = $CmdLine[6]
						if $numArgs > 6 Then
							$strictMatch = $CmdLine[7]
							if $numArgs > 7 Then
								$sendToAll = $CmdLine[7]
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf
EndIf

$pasteMe=ClipGet()

if @error=1 Then
	MsgBox(16,"Oh no!","the clipboard was empty!")
	Exit(666)
ElseIf @error=2 Then
	MsgBox(16,"Oh no!","the clipboard had non-text stuff")
	Exit(666)
ElseIf @error > 2 Then
	MsgBox(16,"Oh no!","the clipboard not accessible")
	Exit(666)
EndIf

$pasteMe=stripBlankLines($pasteMe)
if $pasteMe="" Then
	MsgBox(16,"Oh no!","Nothing to send to Stata!")
	Exit(666)
EndIf

;; findMatchingStatas will error out if there is no Stata running
$matchingStatas = findMatchingStatas($stataInstance,$stataFlavor,$stataVersion,$strictMatch)

$numStatas = $matchingStatas[0][0]
if $sendToAll = "" Then
	If $numStatas > 1 Then
		;; need to build list of matchers
		$matchingStataNames = $matchingStatas[1][0]
		for $winNum = 2 to $matchingStatas[0][0]
			$matchingStataNames = $matchingStataNames & @LF & $matchingStatas[$winNum][0]
		Next
		msgbox(16,"Too Many Statas!","You have too many matching Statas: " & @LF & @LF & $matchingStataNames)
		Exit(666)
	EndIf
endif

for $winNum=1 to $numStatas
	$theStataName = $matchingStatas[$winNum][1]
	if $stataInBack = "" Then
		WinActivate($theStataName)
	EndIf

	if $doThis = "command" Then
		sendToCommand($theStataName, $pasteMe)
	Else
		doTmpDofile($theStataName, $tmpDoFile, $doThis, $stataInBack)
	EndIf
Next

Func badFirstArg($badArg)
	if $badArg="" Then
		$badArg="nothing"
	EndIf
	MsgBox(16,"Oh no!","The first argument must be ""command"", ""menu"", or ""dofile""---you had " & $badArg)
	Exit(2)
EndFunc

func errNoStatas($what)
	MsgBox(16,"Oops","No Stata " & $what)
	Exit(666)
endFunc

Func doTmpDofile($theStataName, $tmpDoFile, $doThis, $stataInBack)
	Local $fullTempDo = @TempDir & "\" & $tmpDoFile
	Local $fh = FileOpen($fullTempDo, 2)
	;; MsgBox(0,"Testing...","Want to submit " & ClipGet() & " to file handle " & $fh)
	FileWrite($fh,ClipGet() & @CRLF)
	FileClose($fh)
	if $doThis = "menu" Then
		;; can't jump back if using menus
		WinActivate($theStataName)
		doViaMenu($theStataName)
		if @error Then
			createMenuItems($theStataName,$fullTempDo)
			Sleep(1000)
			doViaMenu($theStataName)
			if @error Then
				MsgBox(0,"Oh No!","Something went wrong with sending to the menu")
				Exit
			EndIf
		EndIf
	Else
		if $stataInBack = "" Then
			WinActivate($theStataName)
		EndIf
		if $doThis = "dofile" Then
			sendToCommand($theStataName, "do " & $fullTempDo)
		Else
			sendToCommand($theStataName, "include " & $fullTempDo)
		EndIf
	EndIf
EndFunc

Func doViaMenu(ByRef $theStataName)
	;; Cannot use WinMenuSelectItems, because Stata's menus are not menus,
	;;   they are popup controls with no names
	;; Stata is already frontmost, but be sure
	;; WinActivate($theStataName)---for whatever reason, this causes an error
	Send("!uat")
	Sleep(20)
	if WinExists("[TITLE:User; CLASS:XTPPopupBar]") = 1 Then
		seterror(1)
	EndIf
EndFunc

Func createMenuItems(ByRef $theStataName, ByRef $tmpDoFile)
	sendToCommand($theStataName, "window menu append submenu ""stUser"" ""&AutoIt helpers""")
	sendToCommand($theStataName, 'window menu append item "AutoIt helpers" "run &tmp file" "do ' & $tmpDoFile & '"')
	sendtoCommand($theStataName, "window menu refresh")
EndFunc

Func sendToCommand(ByRef $theStataName,byRef $theString)
   Local $theBin, $newStr
;	$theString = StringRegExpReplace($theString,"([+{}!^#])","{\1}",0)
;	msgbox(0,"hunh",$theString)
;;  following 2 lines suggested by code from Jeffery Arnold and Matthew Botsch
;;    they look a lot like the lines I had commented out before
	ControlSetText($theStataName,"","[CLASS:RichEdit20A;Instance:1]", $theString)
	ControlSend($theStataName,"","[CLASS:RichEdit20A;Instance:1]", "{ENTER}")
;;  Uhoh, it seems the control type changed in Stata 14
;;    ControlSetText() no longer works... unless things are converted to utf16
   $theBin = StringToBinary($theString,4)
   $theBin &= StringRight("0000",Mod(StringLen($theBin),4)+2)
   $newStr = BinaryToString($theBin,2)
	ControlSetText($theStataName,"","[CLASS:Scintilla;Instance:1]", $newStr)
;	ControlSend($theStataName,"","[CLASS:Scintilla;Instance:1]", $theString,1)
	ControlSend($theStataName,"","[CLASS:Scintilla;Instance:1]", "{ENTER}")
	if @error Then
		MsgBox(16,"Oh no!", "Could not send to Command window")
		exit(666)
	EndIf
EndFunc

Func pasteTmpStata(ByRef $theStataName, ByRef $pasteMe)
	sendToCommand($theStataName, $pasteMe)
EndFunc

Func stripBlankLines($theText) ; perhaps pass by reference
	;; currently a placeholder---AutoIt shouldn't be used for this
	;; MsgBox(0,"buggy",$theText)
	Return $theText
EndFunc

func findMatchingStatas($sInstance="",$sVersion="",$sFlavor="",$strictMatchFlag="")
	Local $theStatas, $numStatas, $matchThisMany
	Local $numFilters = 3 ;; added in case there is some change of the number of possible filters
	;; Local $sOptions[3] declared below ;; holds the options as regexps for looping
	Local $matchingStataRows ;; an indeterminate-sized array
	Local $matchingStatas ;; for returning the results
	Local $matchingInfo[32][2] ;; first col: row in $theStatas, second col: matches of options
	;; sInstance needs work, because 1 means 'start with non number'
	;;           others are 'start with this number a space and a dash'
	if $sInstance = "" Then
		$sInstance = ".*"
	ElseIf $sInstance = "1" Then
		$sInstance = "\AStata"
	Else
		$sInstance = "\A" & $sInstance & " -"
	EndIf

	Local $sOptions[3] = [$sInstance, ".*" & $sVersion & ".*", ".*" & $sFlavor & ".*"]
	$theStatas = WinList("[REGEXPTITLE:(Stata/(IC|SE|MP))|(Small Stata)]")
	;; _ArrayDisplay($theStatas,"Here are the statas")
	$numStatas = $theStatas[0][0]
	if $numStatas = 0 Then
		errNoStatas("Running")
	EndIf
	;; find out how many of the options get matched
	;; not using $theStatas directly, because its first row is accounting information
	For $winNum = 1 to $numStatas
		$matchingInfo[$winNum][0] = $winNum
		For $optNum = 0 to 2
			;; needs some work for the instances
			$matchingInfo[$winNum][1] += StringRegExp($theStatas[$winNum][0], $sOptions[$optNum])
		Next
	;	MsgBox(16,"Matches",$theStatas[$winNum][0] & " matched " & $matchingInfo[$winNum][1] & " option(s)")
	Next
	;; sort in descending order
	;; _ArrayDisplay($matchingInfo,"Matching Info to Start")
	_ArraySort($matchingInfo,1,0,0,1)
	;; _ArrayDisplay($matchingInfo,"Matching Info after Sorting")
	If $strictMatchFlag <> "" Then
		$matchThisMany = $numFilters
	Else
		$matchThisMany = $matchingInfo[0][1]
	EndIf
	$matchingStataRows=_ArrayFindAll($matchingInfo,$matchThisMany,0,0,0,0,1)
	;; _ArrayDisplay($matchingStataRows,"Statas with most matches")
	;; MsgBox(16,"Max Matchers","The number of matchers is " & UBound($matchingStataRows))
	$numStatas = UBound($matchingStataRows)
	if $numStatas = 0 Then
		errNoStatas("Matched Filters")
	EndIf
	Local $matchingStatas[$numStatas+1][2]
	$matchingStatas[0][0] = $numStatas
	For $row = 1 to $numStatas
		$matchingStatas[$row][0]=$theStatas[$matchingInfo[$matchingStataRows[$row-1]][0]][0]
		$matchingStatas[$row][1]=$theStatas[$matchingInfo[$matchingStataRows[$row-1]][0]][1]
	Next
;; these are sorted, so it is easy to look at them
	Return $matchingStatas
EndFunc
