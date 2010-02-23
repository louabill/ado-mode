#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.2.0
 Author:         Bill Rising

Script Function: for taking clipboard content and sending it to Stata

#ce ----------------------------------------------------------------------------


; arguments are : doThis, tmpDoFile, stataInBack
; stataInBack is empty if Stata should be left unactivated
;; declarations
Opt("MustDelcareVars",1)
Dim $defaultTmpDoFile="feedStata.do"
Dim $numArgs = $CmdLine[0]
; arg  1           2          3 
Dim $doThis, $stataInBack, $tmpDoFile
Dim $theStatas, $howManyStatas, $theStataName
Dim $pasteMe

if $numArgs = 0 Then
	badFirstArg("")
Else
	$doThis = $CmdLine[1]
	if ($doThis <> "command") AND ($doThis <>  "menu") AND ($doThis <> "dofile") Then
		badFirstArg($doThis)
	EndIf
	if $numArgs > 1 Then
		$stataInBack = $CmdLine[2]
		if $numArgs > 2 Then
			$tmpDoFile = $CmdLine[2]
			if $tmpDoFile = "" Then
				$tmpDoFile = $defaultTmpDoFile
			EndIf
		EndIf
	Else
		$tmpDoFile = $defaultTmpDoFile
		$stataInBack = ""
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

$theStatas = WinList("Stata")
$howManyStatas = $theStatas[0][0]
if $howManyStatas = 0 Then
	MsgBox(16,"Oh no!","No Stata running!")
	Exit(666)
ElseIf $howManyStatas > 1 Then
	MsgBox(16,"Oh no!","Nothing for multiple Stata's, yet")
	Exit(14)
Else
	$theStataName = $theStatas[1][1]
EndIf

if $stataInBack = "" Then
	WinActivate($theStataName)
EndIf

if $doThis = "command" Then
	sendToCommand($theStataName, $pasteMe)
Else
	doTmpDofile($theStataName, $tmpDoFile, $doThis, $stataInBack)
EndIf

Func badFirstArg($badArg)
	if $badArg="" Then
		$badArg="nothing"
	EndIf	
	MsgBox(16,"Oh no!","The first argument must be ""command"", ""menu"", or ""dofile""---you had " & $badArg)
	Exit(2)
EndFunc

Func doTmpDofile(ByRef $theStataName, ByRef $tempDoFile, ByRef $doThis, ByRef $stataInBack)
	Local $fullTempDo = @TempDir & "\" & $tempDoFile 
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
		sendToCommand($theStataName, "do " & $fullTempDo)
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
	ControlSend($theStataName,"","[CLASS:RichEdit20A;Instance:1]", $theString & @crlf)
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


	
	
