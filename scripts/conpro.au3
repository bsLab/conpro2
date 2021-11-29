#NoTrayIcon
#include <Constants.au3>
#include <EditConstants.au3>
#include <GUIConstantsEx.au3>
#include <GuiEdit.au3>
#include <GuiRichEdit.au3>
#include <WindowsConstants.au3>
#include <File.au3>
#include <GuiTreeView.au3>
#include <Table.au3>
#include "GUIScrollbars_Ex.au3"
#include "FontConstants.au3"
#include <ButtonConstants.au3>
#include <ComboConstants.au3>
#include <StaticConstants.au3>

; ConPro GUI Unix/Wine Version

; Set flag
$SelectCount = 0


Func PathTrim($Path)
  Local $sDrive = "", $sDir = "", $sFilename = "", $sExtension = ""
  Local $aPathSplit = _PathSplit($Path, $sDrive, $sDir, $sFilename, $sExtension)
  Return $aPathSplit[2]
EndFunc

Func PathNormalize($Path)
  Return StringReplace($Path,"\","/")
EndFunc

Local $InstallationPath = RegRead("HKEY_CLASSES_ROOT\SEMC", "InstallationPath")


Local $Help = _
"CP2 ConPro Compiler"&@CRLF& _
"(C) 2006-2017 - Dr. Stefan Bosse"
            
Local $DrivePrefix = 'Z:'  
Local $Program1 = '' 
Local $Program2 = ''
Local $Editor = @WindowsDir & "\Notepad.exe"
Local $HelpFile = 'help.rtf'

Local $sCompileOptions = ''

If  FileExists($DrivePrefix&'/opt/Conpro2/bin/gen/conpro') Then
  $Program2 = $DrivePrefix&'/opt/Conpro2/bin/gen/conpro'
  $HelpFile = $DrivePrefix&'/opt/Conpro2/help/help.rtf'
EndIf
If  FileExists($DrivePrefix&'/opt/Conpro2/bin/win32/vumrun.exe') Then
  $Program1 = $DrivePrefix&'/opt/Conpro2/bin/win32/vumrun.exe'
EndIf
If  FileExists($DrivePrefix&'/opt/Conpro2/bin/win32/SciTE/SciTE.exe') Then
  $Editor=$DrivePrefix&'/opt/Conpro2/bin/win32/SciTE/SciTE.exe'
EndIf

              
GUIRegisterMsg($WM_COMMAND, 'WM_COMMAND')
 
$hGUI = GUICreate("CONPRO2 (C) Dr. Stefan Bosse", 700, 480)

GUIRegisterMsg($WM_SIZE, "WM_SIZE")

Local $xmldoc

Local $idFile = GUICtrlCreateButton('Open', 10, 5, 60, 20)
Local $idNew = GUICtrlCreateButton('New', 80, 5, 60, 20)
Local $idCompile = GUICtrlCreateButton('Compile', 150, 5, 60, 20)
Local $idProj = GUICtrlCreateButton('Project', 220, 5, 60, 20)
Local $idReport = GUICtrlCreateButton('Report', 290, 5, 60, 20)
Local $idSetup = GUICtrlCreateButton('Setup', 360, 5, 60, 20)
Local $idHelpMsg = GUICtrlCreateButton('Help', 430 , 5, 60, 20)
Local $idExit = GUICtrlCreateButton('Exit', 500, 5, 60, 20)

Local $idSource = GUICtrlCreateLabel("Top-level Source: ", 10, 30, 400)
Local $idDir = GUICtrlCreateLabel("Directory: ", 10, 50, 400)

$idLog = GUICtrlCreateEdit("", 100, 70, 600, 400, _
          BitOR($ES_AUTOVSCROLL, $WS_HSCROLL, $WS_VSCROLL, $ES_READONLY ))

GUICtrlSetBkColor($idLog, $COLOR_WHITE)
GUICtrlSendMsg($idLog, $EM_LIMITTEXT, -1, 0)
GUICtrlSetFont($idLog, 8.5, 400,0, 'Courier New')
GUISetState()


;Save the position of the parent window
$ParentWin_Pos = WinGetPos($hGUI, "")
$helpWin = GUICreate('Help', _
                700, _
                400, $ParentWin_Pos[0] + 100, $ParentWin_Pos[1] + 100, _
                BitOr($WS_SIZEBOX,$WS_SYSMENU), -1, $hGUI)

$hRichEdit = _GUICtrlRichEdit_Create($helpWin, "This is a test.", 10, 10, 480, 200, _
              BitOR($ES_MULTILINE, $ES_READONLY),$WS_EX_TRANSPARENT)

$reportWin = GUICreate("Report", _
                500, _
                300, _
                $ParentWin_Pos[0] + 100, $ParentWin_Pos[1] + 100, _
                BitOr($WS_SIZEBOX,$WS_SYSMENU), -1, $hGUI)
Global $reportTab[10] = [0,0,0,0,0,0,0,0]
Global $reportSB = 0

$setupWin = GUICreate("Setup", _
                645, _
                320, _
                $ParentWin_Pos[0] + 100, $ParentWin_Pos[1] + 100, _
                BitOr($WS_SIZEBOX,$WS_SYSMENU), -1, $hGUI)
Global $setupInit = false
Global $setupOK 
Global $setupLoad 
Global $setupSave 
Global $setupCancel 
Global $SynTool
Global $Incl1
Global $Incl2
Global $Incl3
Global $Incl4
Global $setupP1
Global $setupP2
Global $setupP3
Global $setupP4

$idTreeView = _GUICtrlTreeView_Create($hGUI,5,70,90,400,BitOR($TVS_HASBUTTONS, $TVS_HASLINES, $TVS_LINESATROOT, $TVS_DISABLEDRAGDROP, $TVS_SHOWSELALWAYS))

$idProjRoot = _GUICtrlTreeView_Add($idTreeView,0,"Project")

Local $sSourceDir = PathTrim(@MyDocumentsDir)
Local $sSourceDosDir = PathTrim(@MyDocumentsDir)
Local $sSourceFile = ""
Local $sSourceProj = ""

Local $projmods[1]
Local $projfiles[1]

GUIRegisterMsg($WM_NOTIFY, "WM_NOTIFY")

; Intercept the NOTIFY leassages
Func WM_NOTIFY($hWnd, $iMsg, $wParam, $lParam)
    #forceref $hWnd, $iMsg, $wParam
    ; Read the data
    Local $tNMHDR = DllStructCreate($tagNMHDR, $lParam)
    $hWndFrom = HWnd(DllStructGetData($tNMHDR, "hWndFrom"))
    $iCode = DllStructGetData($tNMHDR, "Code")
    ; See if it was out treeview and the required code
    Switch $hWndFrom
        Case $idTreeView
            Switch $iCode
                Case $NM_DBLCLK
                    ; The user has clicked the left mouse button within the control so set the flag
                    $SelectCount = 2
                    Return 0
            EndSwitch
    EndSwitch
EndFunc

Func WM_COMMAND($hWnd, $iMsg, $wParam, $lParam)
    Local $nNotifyCode = BitShift($wParam, 16)

    Switch $lParam
        Case GUICtrlGetHandle($idLog)
            Switch $nNotifyCode
                Case $EN_SETFOCUS
                    DllCall('user32.dll', 'int', 'HideCaret', 'hwnd', $lParam) ; => _WinAPI_HideCaret($lParam)
            EndSwitch
    EndSwitch
    
    Return $GUI_RUNDEFMSG

EndFunc   

Func _FileOpenDialog($sTitle, $sIntitialDirectory, $sFilter, $iLeft = -1, $iTop = -1, $iOptions = 0, $sDefaultName = "")
    Local $hGUI = GUICreate('', -1, -1, $iLeft, $iTop), $sWorkingDir = @WorkingDir
    Local $sFilePath = FileOpenDialog($sTitle, $sIntitialDirectory, $sFilter, $iOptions, $sDefaultName, $hGUI)
    Local $iError = @error
    FileChangeDir($sWorkingDir)
    Return SetError($iError, GUIDelete($hGUI), $sFilePath)
EndFunc   ;==>_FileOpenDialog()

Func GetModulesFromXmlRep ( $sXMLFilePath )
    Local $xml , $x, $len
    Local $i = 1
    

    $projmods[0] = 0
    $projfiles[0] = 0

    $xmldoc = ObjCreate( 'Msxml2.DOMDocument.6.0' )
    If Not IsObj($xmldoc) Then
        MsgBox($MB_OK,"Error","No Microsoft.XMLDOM object found")
        SetError ( 1 )
        Return -1
    EndIf
    $xmldoc.async = False
    $xmldoc.load ( $sXMLFilePath )

    $xml = $xmldoc.SelectNodes ( "//Report/module")
    If IsObj($xml) and $xml.length > 0 Then    
      For $x In $xml
        If $x.NodeName = "module" Then
          $projmods[0] = $projmods[0] + 1
          ReDim $projmods[$projmods[0]+1]
          $projmods[$i] = $x.getAttribute("name")
          $projfiles[0] = $projfiles[0] + 1
          ReDim $projfiles[$projfiles[0]+1]
          $projfiles[$i] = $x.getAttribute("file")
          $i = $i + 1
        EndIf
      Next
    Else
        MsgBox($MB_OK,"Warning","Empty XML document "&$sXMLFilePath&" (No modules found!)")
        SetError ( 1 )
        Return -1      
    EndIf

    Return ($i-1)
EndFunc

Func MakeReport()
  Local $hPOS = WinGetPos($hGUI)
  Local $SubForm = GUICreate("Side Panel", 400, 240, $hPOS[0], $hPOS[1], -1, _
                             -1, $hGUI)
  Local $CellHeight = 15
  Local $Height = 20
  Local $Width = 300
  
  ;Compute Height/Width of Report FIRST
  If IsObj($xmldoc) Then
    $xml = $xmldoc.SelectNodes ( "//Report/module")
    If IsObj($xml) and $xml.length > 0 Then    
      $Height = $Height + 30;
      $Height = $Height + (($xml.length+2)*($CellHeight+1));
    EndIf
  EndIf
  
  If $Height > 240 Then
    _GUIScrollbars_Generate($SubForm, $Width, $Height )
  EndIf
  
  GUISetState(@SW_SHOW)

  If IsObj($xmldoc) Then
    $xml = $xmldoc.SelectNodes ( "//Report/module")
  
    If IsObj($xml) and $xml.length > 0 Then    
      GUICtrlCreateLabel("Modules ", 10, 10, 200)
      Local $mTab = _GUICtrlTable_Create(10, 30, 170, $CellHeight, $xml.length+1, 2)

      _GUICtrlTable_Set_CellColor_Row($mTab,1,0x404040)
      _GUICtrlTable_Set_TextColor_Row($mTab,1,0xFFFFFF)
      _GUICtrlTable_Set_Text_Cell($mTab,1,1," Module")
      _GUICtrlTable_Set_Text_Cell($mTab,1,2," File")
      _GUICtrlTable_Set_Justify_Row($mTab,1,0,1)
      Local $i = 2
      For $x In $xml
        If $x.NodeName = "module" Then
          _GUICtrlTable_Set_Text_Cell($mTab,$i,1," "&$x.getAttribute("name"))
          _GUICtrlTable_Set_Text_Cell($mTab,$i,2," "&$x.getAttribute("file"))
          If Mod($i,2) = 1 Then  
            _GUICtrlTable_Set_CellColor_Row($mTab,$i,0xD0D0D0) 
          EndIf
          _GUICtrlTable_Set_Justify_Row($mTab,$i,0,1)
          $i = $i + 1
        EndIf
      Next
      ;_GUICtrlTable_Set_Border_Row($mTab,$i-1,8,0xFFFFFF)
    EndIf
  EndIf
  While 1
      $msg = GUIGetMsg(1)
      Select
      Case $msg[0] = $GUI_EVENT_CLOSE
          If $msg[1] = $SubForm Then
              GUISwitch($SubForm)
              GUIDelete()
              ExitLoop
          EndIf
      EndSelect
  WEnd
EndFunc

Func MakeSetup()
  If $setupInit = false Then
    $setupInit = true
    $Label1 = GUICtrlCreateLabel("Synthesis Tool", 24, 15, 73, 17)
    $Label2 = GUICtrlCreateLabel("Device Family", 24, 64, 70, 17)
    $Label3 = GUICtrlCreateLabel("Definitions [NAME=VALUE]", 208, 15, 140, 17)
    $Def1 = GUICtrlCreateInput("", 208, 32, 177, 21)
    $Def2 = GUICtrlCreateInput("", 208, 56, 177, 21)
    $Def3 = GUICtrlCreateInput("", 208, 80, 177, 21)
    $Def4 = GUICtrlCreateInput("", 208, 104, 177, 21)
    $Def5 = GUICtrlCreateInput("", 208, 128, 177, 21)
    $Def6 = GUICtrlCreateInput("", 208, 152, 177, 21)
    $Def7 = GUICtrlCreateInput("", 208, 176, 177, 21)
    $Def8 = GUICtrlCreateInput("", 208, 200, 177, 21)
    $Def9 = GUICtrlCreateInput("", 208, 224, 177, 21)
    $Def10 = GUICtrlCreateInput("", 208, 248, 177, 21)
    $Label4 = GUICtrlCreateLabel("Device Size", 24, 104, 61, 17)
    $setupOK = GUICtrlCreateButton("Ok", 24, 224, 73, 17)
    $setupLoad = GUICtrlCreateButton("Load", 24, 248, 73, 17)
    $setupSave = GUICtrlCreateButton("Save", 112, 248, 73, 17)
    $setupCancel = GUICtrlCreateButton("Cancel", 112, 224, 73, 17)
    $Label5 = GUICtrlCreateLabel("Device Package", 24, 144, 84, 17)
    $SynTool = GUICtrlCreateCombo("", 24, 32, 145, 25, BitOR($CBS_DROPDOWN,$CBS_AUTOHSCROLL,$WS_VSCROLL))
    GUICtrlSetData(-1, "xilinx6|xilinx9|xilinx11|dc|leonardo|synplicity89|alliance")
    $DeviceFamily = GUICtrlCreateCombo("", 24, 80, 145, 25, BitOR($CBS_DROPDOWN,$CBS_AUTOHSCROLL,$WS_VSCROLL))
    GUICtrlSetData(-1, "a3p|a3pe|xc2s|xc3s")
    $DeviceSize = GUICtrlCreateCombo("", 24, 120, 145, 25, BitOR($CBS_DROPDOWN,$CBS_AUTOHSCROLL,$WS_VSCROLL))
    GUICtrlSetData(-1, "100|500|600|1000|1500|2000|3000")
    $DevicePackage = GUICtrlCreateCombo("", 24, 160, 145, 25, BitOR($CBS_DROPDOWN,$CBS_AUTOHSCROLL,$WS_VSCROLL))
    GUICtrlSetData(-1, "fbga144|fbga256|fbga324|fg144|tq144")  
    $Label7 = GUICtrlCreateLabel("Include Paths", 424, 15, 69, 17)
    $Incl1 = GUICtrlCreateInput("", 424, 32, 161, 21)
    $Incl2 = GUICtrlCreateInput("", 424, 56, 161, 21)
    $Incl3 = GUICtrlCreateInput("", 424, 80, 161, 21)
    $Incl4 = GUICtrlCreateInput("", 424, 104, 161, 21)
    $setupP1 = GUICtrlCreateButton("Set", 592, 32, 33, 22)
    $setupP2 = GUICtrlCreateButton("Set", 592, 56, 33, 20)
    $setupP3 = GUICtrlCreateButton("Set", 592, 80, 33, 20)
    $setupP4 = GUICtrlCreateButton("Set", 592, 104, 33, 20)  

    ; Some are not supported yet
    GUICtrlSetState($Label2,$GUI_DISABLE)
    GUICtrlSetState($Label3,$GUI_DISABLE)
    GUICtrlSetState($Label4,$GUI_DISABLE)
    GUICtrlSetState($Label5,$GUI_DISABLE)
    GUICtrlSetState($Label7,$GUI_DISABLE)
    GUICtrlSetState($setupP1,$GUI_DISABLE)
    GUICtrlSetState($setupP2,$GUI_DISABLE)
    GUICtrlSetState($setupP3,$GUI_DISABLE)
    GUICtrlSetState($setupP4,$GUI_DISABLE)
    GUICtrlSetState($setupLoad,$GUI_DISABLE)
    GUICtrlSetState($setupSave,$GUI_DISABLE)
  EndIf
  
  GUISetState(@SW_SHOW)
EndFunc

Func UpdateSetup()
  $sCompileOptions = '';
  Local $sComboRead = GUICtrlRead($SynTool)
  If $sComboRead <> '' Then
    $sCompileOptions = '-tool '&$sComboRead
  EndIf
EndFunc

Func ToMod ($name)
  Local $postfix = StringRight($name,StringLen($name)-1)
  Local $prefix = StringLeft($name,1)
  Return (StringUpper($prefix)&$postfix)
EndFunc

Func OfMod ($name)
  Local $postfix = StringRight($name,StringLen($name)-1)
  Local $prefix = StringLeft($name,1)
  Return (StringLower($prefix)&$postfix)
EndFunc

While 1
  $idMsg = GUIGetMsg(1)
  
  Switch $idMsg[0]
    Case $GUI_EVENT_CLOSE, $idExit
      If $idMsg[1] = $helpWin Then
        GUISwitch($helpWin)
        GUISetState(@SW_HIDE)
        GUISwitch($hGUI)        
      ElseIf $idMsg[1] = $reportWin Then
        GUISwitch($reportWin)
        GUISetState(@SW_HIDE)
        GUISwitch($hGUI)        
      ElseIf $idMsg[1] = $setupWin Then
        GUISwitch($setupWin)
        GUISetState(@SW_HIDE)
        GUISwitch($hGUI)        
      Else
        ExitLoop
      EndIf
      
    Case $idHelpMsg
      _GUICtrlRichEdit_SetText($hRichEdit, "")
      _GUICtrlRichEdit_StreamFromFile($hRichEdit, $HelpFile)
      GUISwitch($helpWin)
      GUISetBkColor($COLOR_WHITE)
      GUISetState(@SW_SHOW)
      _GUICtrlRichEdit_SetScrollPos ($hRichEdit,0,0)
      GUISwitch($hGUI)
    
    Case $idFile
      Local $hPOS = WinGetPos($hGUI)
      $sMessage = "Select a top-level source file."
      $sFileOpenDialog = _FileOpenDialog($sMessage, $DrivePrefix&$sSourceDir, 'CP2 (*.cp)', $hPOS[0], $hPOS[1], $FD_FILEMUSTEXIST)

      If @error Then
        MsgBox($MB_SYSTEMMODAL, "", "No source file was selected.")
      Else
        Local $sDrive = "", $sDir = "", $sFilename = "", $sExtension = ""
        Local $aPathSplit = _PathSplit($sFileOpenDialog, $sDrive, $sDir, $sFilename, $sExtension)
        $sSourceFile = $aPathSplit[3] & $sExtension
        $sSourceProj = $aPathSplit[3]
        $sSourceDosDir = $aPathSplit[2]
        $sSourceDir = PathNormalize($aPathSplit[2])
        $DrivePrefix = $aPathSplit[1]
        GUICtrlSetData($idSource,"Top-level Source: " & $sSourceFile) 
        GUICtrlSetData($idDir,"Directory: " & $sSourceDir) 
      EndIf

    Case $idNew
      $sMessage = "Create a new top-level file."
      $sFileOpenDialog = FileOpenDialog($sMessage, $DrivePrefix&$sSourceDir, 'CP2 (*.cp)', $FD_PROMPTCREATENEW)

      If @error Then
        MsgBox($MB_SYSTEMMODAL, "", "No source file was created.")
      Else
        Local $sDrive = "", $sDir = "", $sFilename = "", $sExtension = ""
        Local $aPathSplit = _PathSplit($sFileOpenDialog, $sDrive, $sDir, $sFilename, $sExtension)
        $sSourceFile = $aPathSplit[3] & $sExtension
        $sSourceProj = $aPathSplit[3]
        $sSourceDosDir = $aPathSplit[2]
        $sSourceDir = PathNormalize($aPathSplit[2])
        $DrivePrefix = $aPathSplit[1]
        _FileCreate($sSourceDosDir&$sSourceFile)
        GUICtrlSetData($idSource,"Top-level Source: " & $sSourceFile) 
        GUICtrlSetData($idDir,"Directory: " & $sSourceDir) 
      EndIf
      
    Case $idCompile
      GUICtrlSetData($idLog, $Program1 & @CRLF)
      _GUICtrlEdit_AppendText($idLog,$Program2 & @CRLF)
      _GUICtrlEdit_AppendText($idLog,$sSourceFile & @CRLF)
      _GUICtrlEdit_AppendText($idLog,$sCompileOptions & @CRLF)

      $_pid = Run($Program1 &' '& $Program2 & ' -notty ' & $sCompileOptions & ' -O "' & $DrivePrefix&$sSourceDir & '" -I "' & $DrivePrefix&$sSourceDir & '" ' & $sSourceFile, @SystemDir, @SW_HIDE, $STDERR_CHILD + $STDOUT_CHILD)
      $answer = ''

      While 1
         $line = StdoutRead($_pid)          ;read contents of STDOUT into $line
         if @error Then ExitLoop            ;if read fails, its the end of the list
         _GUICtrlEdit_AppendText($idLog, $line)
         $answer &= $line
         Sleep(10)
      WEnd

      While 1
         $line = StderrRead($_pid)          ;read contents of STDERR into $line
         if @error Then ExitLoop            ;if read fails, its the end of the list
         _GUICtrlEdit_AppendText($idLog, $line)
         $answer &= $line
         Sleep(10)
      WEnd

    Case $idProj
      If $sSourceFile <> "" Then
        GUICtrlSetData($idLog, $Program1 & @CRLF)
        _GUICtrlEdit_AppendText($idLog,$Program2 & @CRLF)
        _GUICtrlEdit_AppendText($idLog,$sSourceFile & @CRLF)
        $_pid = Run($Program1 &' '& $Program2 & ' +report -notty -O "' & $DrivePrefix&$sSourceDir & '" -I "' & $DrivePrefix&$sSourceDir & '" -parse ' & $sSourceFile, @SystemDir, @SW_HIDE, $STDERR_CHILD + $STDOUT_CHILD)
        $answer = ""

        While 1
           $line = StdoutRead($_pid)          ;read contents of STDOUT into $line
           if @error Then ExitLoop            ;if read fails, its the end of the list
           _GUICtrlEdit_AppendText($idLog, $line)
           $answer &= $line
           Sleep(10)
        WEnd

        While 1
           $line = StderrRead($_pid)          ;read contents of STDERR into $line
           if @error Then ExitLoop            ;if read fails, its the end of the list
           _GUICtrlEdit_AppendText($idLog, $line)
           $answer &= $line
           Sleep(10)
        WEnd
        Local $xml_file = $DrivePrefix&$sSourceDosDir&$sSourceProj&'_rep.xml'
        $n = GetModulesFromXmlRep($xml_file)

        _GUICtrlTreeView_Expand($idTreeView)
        _GUICtrlTreeView_DeleteChildren($idTreeView, $idProjRoot)
        If $projmods[0] = 0 Then
          _GUICtrlTreeView_AddChild($idTreeView, $idProjRoot,ToMod($sSourceProj))
        EndIf
        for $i = 1 to $projmods[0]
          _GUICtrlTreeView_AddChild($idTreeView, $idProjRoot,$projmods[$i])
        Next
        _GUICtrlTreeView_EndUpdate($idTreeView)
        _GUICtrlTreeView_Expand($idTreeView)
        FileDelete($xml_file)
      Else
        MsgBox($MB_OK,"Error","No top-level source file selected")
      EndIf

    Case $idSetup
      GUISwitch($setupWin)
      MakeSetup()
      GUISwitch($hGUI)
    
      
    Case $idReport
      GUISwitch($reportWin)
      GUISetBkColor($COLOR_WHITE)
      MakeReport()
      GUISwitch($hGUI)
      
    Case $setupCancel
      GUISwitch($setupWin)
      GUISetState(@SW_HIDE)
      GUISwitch($hGUI)        

    Case $setupOK
      GUISwitch($setupWin)
      UpdateSetup()
      GUISetState(@SW_HIDE)
      GUISwitch($hGUI)        
    
    Case $setupP1,$setupP2,$setupP3,$setupP4
      Local $hPOS = WinGetPos($hGUI)
      Local $dir = ''
        Switch $idMsg[0] 
          Case $setupP1
            $dir = GUICtrlRead($Incl1) 
          Case $setupP2
            $dir = GUICtrlRead($Incl2) 
          Case $setupP3
            $dir = GUICtrlRead($Incl3) 
          Case $setupP4
            $dir = GUICtrlRead($Incl4) 
        EndSwitch
      $sMessage = 'Select include directory.'
      $sFileOpenDialog = FileSelectFolder($sMessage,'',1,$dir)

      If @error Then
        MsgBox($MB_SYSTEMMODAL, "", "No directory file was selected.")
      Else
        Local $sDrive = "", $sDir = "", $sFilename = "", $sExtension = ""
        Local $aPathSplit = _PathSplit($sFileOpenDialog, $sDrive, $sDir, $sFilename, $sExtension)
        ;$sSourceFile = $aPathSplit[3] & $sExtension
        ;$sSourceProj = $aPathSplit[3]
        ;$sSourceDosDir = $aPathSplit[2]
        ;$sSourceDir = PathNormalize($aPathSplit[2])
        ;$DrivePrefix = $aPathSplit[1]
        Switch $idMsg[0] 
          Case $setupP1
            GUICtrlSetData($Incl1,$sFileOpenDialog) 
          Case $setupP2
            GUICtrlSetData($Incl2,$sFileOpenDialog) 
          Case $setupP3
            GUICtrlSetData($Incl3,$sFileOpenDialog) 
          Case $setupP4
            GUICtrlSetData($Incl4,$sFileOpenDialog) 
        EndSwitch
      EndIf
      
  EndSwitch
  ; If flag is set
  If $SelectCount = 2 Then
    ; Clear flag
    $SelectCount = 0
    ; Read and display the parameter
    ; ConsoleWrite(_GUICtrlTreeView_GetText ($idTreeView, _GUICtrlTreeView_GetSelection($idTreeView)) & @CRLF)
    $SelectText = _GUICtrlTreeView_GetText ($idTreeView, _GUICtrlTreeView_GetSelection($idTreeView))
    If $SelectText <> "Project" Then
      Local $filename = $DrivePrefix&$sSourceDosDir&OfMod($SelectText)&'.cp'
      ConsoleWrite($filename&@CRLF)
      $_pid = Run($Editor&' "'&$filename&'"')
    EndIf
  EndIf

WEnd

GUIDelete($hGUI)
Exit

Func WM_SIZE($hWnd, $iMsg, $wParam, $lParam)
    Local $iWidth = _WinAPI_LoWord($lParam)
    Local $iHeight = _WinAPI_HiWord($lParam)

    If $hWnd = $helpWin Then
      _WinAPI_MoveWindow($hRichEdit, 10, 10, $iWidth - 20, $iHeight - 20)
    EndIf
    
    Return 0
EndFunc   ;==>WM_SIZE
