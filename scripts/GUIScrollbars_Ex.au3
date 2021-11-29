#include-once

; #INDEX# ============================================================================================================
; Title .........: GUIScrollBars_Ex
; AutoIt Version : v3.3.6.0
; Language ......: English
; Description ...: Generates scrollbars for user defined sizes of GUI and aperture and sets proportional thumb sizes
; Remarks .......:
; Note ..........:
; Author(s) .....: Melba23 - with some code based on the WinAPI and GUIScrollBars includes
;                  and contributions from rover, czardas, MrCreatoR and Malkey
; ====================================================================================================================

;#AutoIt3Wrapper_Au3Check_Parameters=-d -w 1 -w 2 -w 3 -w- 4 -w 5 -w 6 -w- 7

; #INCLUDES# =========================================================================================================
#include <GUIConstantsEx.au3>
#include <WindowsConstants.au3>
#include <GuiScrollBars.au3>
#include <ScrollBarConstants.au3>
#include <SendMessage.au3>
#include <WinAPI.au3>

; #GLOBAL VARIABLES# =================================================================================================
Global $__g_aSB_WindowInfo[1][10]
; [0] = Handle to window
; [1] = Not used
; [2] = Average horizontal pixels per char
; [3] = Vertical pixels per char
; [4] = Client area width
; [5] = Client area height
; [6] = Horizontal max setting
; [7] = Vertical max setting
; [8] = Vertical scrollbar position for minimize/restore
; [9] = Horizontal scrollbar position for minimize/restore

Global $__g_aSB_WindowInfoEx[1][7]
; [0] = Horizontal scrollable size
; [1] = Vertical scrollable size
; [2] = Width correction factor
; [3] = Height correction factor
; [4] = Before/After flag
; [5] = Key repeat value
; [6] = Wheel scroll value

; #CURRENT# ==========================================================================================================
; _GUIScrollbars_Generate:    Generates scrollbars for a GUI with a defined aperture with proportional thumb sizes
; _GUIScrollbars_Locate_Ctrl: Calculates coordinates to use to position controls after scrollbar creation
; _GUIScrollbars_Scroll_Page: Scrolls to min, max or page number
; _GUIScrollbars_Minimize:    Stores scrollbar positions on GUI minimize
; _GUIScrollbars_Restore:     Restores scrollbar positions on GUI restore
; ====================================================================================================================

; #INTERNAL_USE_ONLY#=================================================================================================
; _Scrollbars_WM_VSCROLL:     GUIRegisterMsg procedure for vertical scrollbar
; _Scrollbars_WM_HSCROLL:     GUIRegisterMsg procedure for horizontal scrollbar
; _Scrollbars_WM_MOUSEWHEEL:  GUIRegisterMsg procedure for vertical mouse wheel scroll
; _Scrollbars_WM_MOUSEHWHEEL: GUIRegisterMsg procedure for horizontal mouse wheel scroll
; _Scrollbars_WM_KEYUP:       GUIRegisterMsg procedure for scrolling on cursor, pageup/down, home and end key press
;=====================================================================================================================

; #FUNCTION# =========================================================================================================
; Name...........: _GUIScrollbars_Generate
; Description ...: Generates scrollbars for a GUI with a defined aperture with proportional thumb sizes
; Syntax.........: _GUIScrollbars_Generate ($hWnd, $iH_Scroll = 0, [$iV_Scroll = 0, [$iH_Tight = 0, [$iV_Tight = 0, [$fBefore = False, [$iRepeat = 0]]]]])
; Parameters ....: $hWnd      -> GUI to contain scrollbars
;                  $iH_Scroll -> Width in pixels of area to be scrolled
;                  $iV_Scroll -> Height in pixels of area to be scrolled (default = 0)
;                  $iH_Tight  -> 1 = Adjust mean position of right edge of scrolled area to right (default = 0)
;                  $iV_Tight  -> 1 = Adjust mean position of bottom edge of scrolled area down (default = 0)
;                  $fBefore   -> True  = Scrollbars are being generated BEFORE controls
;                                False = Scrollbars are being generated AFTER controls (default) - key scrolling possible
;                  $iRepeat   -> Number of lines/chars moved by a single cursor key press - default 0
; Requirement(s).: v3.3.6.0 or higher
; Return values .: Success - Returns a 4-element array (see remarks for details):
;                            [0] = Actual aperture width ; [1] = Actual aperture height]
;                            [2] = Width correction factor ; [3] = Height correction factor]
;                  Failure - Returns either 0 (UDF error) or negative integer (API error)
;                            If UDF error then @error set as follows:
;                               1 - hWnd not a valid handle
;                               2 - No scroll size parameters
;                               3 - Scrollbar creation or parameter setting failure
;                            If API error then @error and @extended as set by API error.  Return values:
;                               -1 - GetDC failure
;                               -2 - GetTextMetricsW failure
;                               -3 - GetClientRect failure
; Remarks .......; The $fBefore parameter is needed because of the way Windows deals with scrollbars.  When the
;                  scrollbars are generated, the visible part of the scrollable GUI resizes to fit the in the
;                  remaining (smaller) client area.
;                    - If the scrollbars are generated BEFORE any controls, the UDF should be called with the
;                      $fBefore parameter set.  The new client size of the aperture window is returned so that
;                      controls can then be created using these values.
;                    - If controls have been created before the scrollbars are generated then the UDF should be
;                      called without the $fBefore parameter.  The correction factors returned can then be applied to
;                      any subsequent control positioning and sizing.  This is necessary because of the positions and
;                      sizes of existing controls will be slightly altered as the scrollbars are generated and the GUI
;                      resized.  Any controls created subsequently would therefore be slightly misplaced in relation
;                      to the existing ones unless the correction factors are used when positoning and sizing them.
;                    - If existing controls were fixed in place using GUICtrlResizing($GUI_DOCKALL) there is no need
;                      to apply the correction factors as the controls will not have moved with the GUI resizing.
;                  If a value is set for $iRepeat then the UDF will register the WM_KEYUP message to allow scrolling
;                  with the cursor, pageup/down, home and end keys
;                    - Note that in this case any controls must be created after the GUISetState(@SW_SHOW) line or the
;                      key scrolling will not be activated
; Author ........: Melba23 - with some code based on the WinAPI and GUIScrollBars includes
; Example........; Yes
;=====================================================================================================================
Func _GUIScrollbars_Generate($hWnd, $iH_Scroll = 0, $iV_Scroll = 0, $iH_Tight = 0, $iV_Tight = 0, $fBefore = False, $iRepeat = 0)

	; Check if valid window handle
	If Not IsHWnd($hWnd) Then Return SetError(1, 0, 0)
	If $__g_aSB_WindowInfo[0][0] <> "" Then
		ReDim $__g_aSB_WindowInfo[UBound($__g_aSB_WindowInfo) + 1][10]
		ReDim $__g_aSB_WindowInfoEx[UBound($__g_aSB_WindowInfo) + 1][7]
	EndIf

	; If no scroll sizes set, return error
	If $iH_Scroll = 0 And $iV_Scroll = 0 Then Return SetError(2, 0, 0)

	; Confirm Tight values
	If $iH_Tight <> 0 Then $iH_Tight = 1
	If $iV_Tight <> 0 Then $iV_Tight = 1

	; Check Repeat value
	If Not IsInt($iRepeat) Then $iRepeat = 0

	; Create structs
	Local $tTEXTMETRIC = DllStructCreate($tagTEXTMETRIC)
	Local $tSCROLLINFO = DllStructCreate($tagSCROLLINFO)
	DllStructSetData($tSCROLLINFO, "cbSize", DllStructGetSize($tSCROLLINFO))
	Local $tRect = DllStructCreate($tagRECT)

	; Declare local variables
	Local $iIndex = UBound($__g_aSB_WindowInfo) - 1
	Local $iError, $iExtended

	; Save window handle
	$__g_aSB_WindowInfo[$iIndex][0] = $hWnd

	; Determine text size
	Local $hDC = DllCall("user32.dll", "handle", "GetDC", "hwnd", $hWnd)
	If Not @error Then
		$hDC = $hDC[0]
		DllCall("gdi32.dll", "bool", "GetTextMetricsW", "handle", $hDC, "ptr", DllStructGetPtr($tTEXTMETRIC))
		If @error Then
			$iError = @error
			$iExtended = @extended
			DllCall("user32.dll", "int", "ReleaseDC", "hwnd", $hWnd, "handle", $hDC)
			Return SetError($iError, $iExtended, -2)
		EndIf
		DllCall("user32.dll", "int", "ReleaseDC", "hwnd", $hWnd, "handle", $hDC)
	Else
		Return SetError(@error, @extended, -1)
	EndIf
	$__g_aSB_WindowInfo[$iIndex][2] = DllStructGetData($tTEXTMETRIC, "tmAveCharWidth")
	$__g_aSB_WindowInfo[$iIndex][3] = DllStructGetData($tTEXTMETRIC, "tmHeight") + DllStructGetData($tTEXTMETRIC, "tmExternalLeading")

	; Size aperture window without bars
	DllCall("user32.dll", "bool", "GetClientRect", "hwnd", $hWnd, "ptr", DllStructGetPtr($tRect))
	If @error Then Return SetError(@error, @extended, -3)
	Local $iX_Client_Full = DllStructGetData($tRect, "Right") - DllStructGetData($tRect, "Left")
	Local $iY_Client_Full = DllStructGetData($tRect, "Bottom") - DllStructGetData($tRect, "Top")
	$__g_aSB_WindowInfo[$iIndex][4] = $iX_Client_Full
	$__g_aSB_WindowInfo[$iIndex][5] = $iY_Client_Full

	; Hide both scrollbars
	_GUIScrollBars_ShowScrollBar($hWnd, $SB_BOTH, False)
	; Show scrollbars and register scrollbar and mousewheel messages if required
	If $iH_Scroll Then
		_GUIScrollBars_ShowScrollBar($hWnd, $SB_HORZ)
		GUIRegisterMsg($WM_HSCROLL, "_Scrollbars_WM_HSCROLL")
		GUIRegisterMsg($WM_MOUSEHWHEEL, '_Scrollbars_WM_MOUSEHWHEEL')
	EndIf
	If $iV_Scroll Then
		_GUIScrollBars_ShowScrollBar($hWnd, $SB_VERT)
		GUIRegisterMsg($WM_VSCROLL, "_Scrollbars_WM_VSCROLL")
		GUIRegisterMsg($WM_MOUSEWHEEL, "_Scrollbars_WM_MOUSEWHEEL")
	EndIf
	; Only register for key scrolling if required
	If $iRepeat Then
		GUIRegisterMsg($WM_KEYUP, "_Scrollbars_WM_KEYUP")
	EndIf

	; Size aperture window with bars
	DllCall("user32.dll", "bool", "GetClientRect", "hwnd", $hWnd, "ptr", DllStructGetPtr($tRect))
	If @error Then Return SetError(@error, @extended, -3)
	Local $iX_Client_Bar = DllStructGetData($tRect, "Right") - DllStructGetData($tRect, "Left")
	Local $iY_Client_Bar = DllStructGetData($tRect, "Bottom") - DllStructGetData($tRect, "Top")

	; If horizontal scrollbar is required
	Local $iH_FullPage
	If $iH_Scroll Then
		If $fBefore Then
			; Use actual aperture width
			$__g_aSB_WindowInfo[$iIndex][4] = $iX_Client_Bar
			; Determine page size (aperture width / text width)
			$iH_FullPage = Floor($__g_aSB_WindowInfo[$iIndex][4] / $__g_aSB_WindowInfo[$iIndex][2])
			; Determine max size (scroll width / text width - tight)
			$__g_aSB_WindowInfo[$iIndex][6] = Floor($iH_Scroll / $__g_aSB_WindowInfo[$iIndex][2]) - $iH_Tight
		Else
			; Use reduced aperture width only if other scrollbar exists
			If $iV_Scroll Then $__g_aSB_WindowInfo[$iIndex][4] = $iX_Client_Bar
			; Determine page size (aperture width / text width)
			$iH_FullPage = Floor($__g_aSB_WindowInfo[$iIndex][4] / $__g_aSB_WindowInfo[$iIndex][2])
			; Determine max size (scroll width / text width * correction factor for V scrollbar if required - tight)
			$__g_aSB_WindowInfo[$iIndex][6] = Floor($iH_Scroll / $__g_aSB_WindowInfo[$iIndex][2] * $__g_aSB_WindowInfo[$iIndex][4] / $iX_Client_Full) - $iH_Tight
		EndIf
	Else
		$__g_aSB_WindowInfo[$iIndex][6] = 0
	EndIf

	; If vertical scrollbar required
	Local $iV_FullPage
	If $iV_Scroll Then
		If $fBefore Then
			; Use actual aperture height
			$__g_aSB_WindowInfo[$iIndex][5] = $iY_Client_Bar
			; Determine page size (aperture width / text width)
			$iV_FullPage = Floor($__g_aSB_WindowInfo[$iIndex][5] / $__g_aSB_WindowInfo[$iIndex][3])
			; Determine max size (scroll width / text width - tight)
			$__g_aSB_WindowInfo[$iIndex][7] = Floor($iV_Scroll / $__g_aSB_WindowInfo[$iIndex][3]) - $iV_Tight
		Else
			; Use reduced aperture width only if other scrollbar exists
			If $iH_Scroll Then $__g_aSB_WindowInfo[$iIndex][5] = $iY_Client_Bar
			; Determine page size (aperture width / text width)
			$iV_FullPage = Floor($__g_aSB_WindowInfo[$iIndex][5] / $__g_aSB_WindowInfo[$iIndex][3])
			; Determine max size (scroll width / text width * correction factor for H scrollbar if required - tight)
			$__g_aSB_WindowInfo[$iIndex][7] = Floor($iV_Scroll / $__g_aSB_WindowInfo[$iIndex][3] * $__g_aSB_WindowInfo[$iIndex][5] / $iY_Client_Full) - $iV_Tight
		EndIf
	Else
		$__g_aSB_WindowInfo[$iIndex][7] = 0
	EndIf

	Local $aRet[4]
	If $iV_Scroll Then
		$aRet[0] = $iX_Client_Bar
	Else
		$aRet[0] = $iX_Client_Full
	EndIf
	If $iH_Scroll Then
		$aRet[1] = $iY_Client_Bar
	Else
		$aRet[1] = $iY_Client_Full
	EndIf
	$aRet[2] = $iX_Client_Bar / $iX_Client_Full
	$aRet[3] = $iY_Client_Bar / $iY_Client_Full

	; Save extended window info
	$__g_aSB_WindowInfoEx[$iIndex][0] = $iH_Scroll
	$__g_aSB_WindowInfoEx[$iIndex][1] = $iV_Scroll
	$__g_aSB_WindowInfoEx[$iIndex][2] = $aRet[2]
	$__g_aSB_WindowInfoEx[$iIndex][3] = $aRet[3]
	$__g_aSB_WindowInfoEx[$iIndex][4] = $fBefore
	$__g_aSB_WindowInfoEx[$iIndex][5] = $iRepeat
    If $iRepeat Then
      ;( ($iRepeat) ? ($iRepeat) : (7) )
	  $__g_aSB_WindowInfoEx[$iIndex][6] = $iRepeat; Set default 7 for mousewheel if no keys registered
    Else
	  $__g_aSB_WindowInfoEx[$iIndex][6] = 7; Set default 7 for mousewheel if no keys registered
    EndIf

	Local $fSuccess = True
	If _GUIScrollBars_ShowScrollBar($hWnd, $SB_BOTH, False) = False Then $fSuccess = False
	If $iH_Scroll Then
		If _GUIScrollBars_SetScrollInfoMax($hWnd, $SB_HORZ, $__g_aSB_WindowInfo[$iIndex][6]) = False Then $fSuccess = False
		_GUIScrollBars_SetScrollInfoPage($hWnd, $SB_HORZ, $iH_FullPage)
		If @error Then $fSuccess = False
		If _GUIScrollBars_ShowScrollBar($hWnd, $SB_HORZ, True) = False Then $fSuccess = False
	Else
		If _GUIScrollBars_ShowScrollBar($hWnd, $SB_HORZ, False) = False Then $fSuccess = False
	EndIf
	If $iV_Scroll Then
		If _GUIScrollBars_SetScrollInfoMax($hWnd, $SB_VERT, $__g_aSB_WindowInfo[$iIndex][7]) = False Then $fSuccess = False
		_GUIScrollBars_SetScrollInfoPage($hWnd, $SB_VERT, $iV_FullPage)
		If @error Then $fSuccess = False
		If _GUIScrollBars_ShowScrollBar($hWnd, $SB_VERT, True) = False Then $fSuccess = False
	Else
		If _GUIScrollBars_ShowScrollBar($hWnd, $SB_VERT, False) = False Then $fSuccess = False
	EndIf

	If $fSuccess Then Return $aRet
	Return SetError(3, 0, 0)

EndFunc   ;==>_GUIScrollbars_Generate

; #FUNCTION# =========================================================================================================
; Name...........: _GUIScrollbars_Locate_Ctrl
; Description ...: Calculates coordinates to use to position controls after scrollbar creation
; Syntax.........: _GUIScrollbars_Locate_Ctrl ($hWnd, $iX, $iY)
; Parameters ....: $hWnd -> GUI to contain control
;                  $iX   -> Horizontal coordinate relative to scrollable area
;                  $iY   -> Vertical coordinate relative to scrollable area
; Requirement(s).: v3.3.6.0 or higher
; Return values .: Success - Returns a 2-element array:
;                            [0] = Horizontal coordinate
;                            [1] = Vertical coordinate
;                  Failure - Returns either 0 with @error set as follows:
;                            1 - Invalid window handle
;                            2 - Parameter error
;                            3 - Window not found
; Remarks .......;
; Author ........: Melba23
; Example........; Yes
;=====================================================================================================================
Func _GUIScrollbars_Locate_Ctrl($hWnd, $iX, $iY)

	; Check $hWnd
	If Not IsHWnd($hWnd) Then Return SetError(1, 0, 0)

	; Find window info
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex = -1 Then Return SetError(3, 0, 0)

	; Check if location is within scrollable area of the window
	If $iX < 0 Or $iX > $__g_aSB_WindowInfoEx[$iIndex][0] Then Return SetError(2, 0, 0)
	If $iY < 0 Or $iY > $__g_aSB_WindowInfoEx[$iIndex][1] Then Return SetError(2, 0, 0)

	; Calculate factored coordinates if needed
	If Not $__g_aSB_WindowInfoEx[$iIndex][4] Then
		$iX *= $__g_aSB_WindowInfoEx[$iIndex][2]
		$iY *= $__g_aSB_WindowInfoEx[$iIndex][3]
	EndIf

	; Correct for any scrollbar movement
	$iX -= _GUIScrollBars_GetScrollInfoPos($hWnd, $SB_HORZ) * $__g_aSB_WindowInfo[$iIndex][2]
	$iY -= _GUIScrollBars_GetScrollInfoPos($hWnd, $SB_VERT) * $__g_aSB_WindowInfo[$iIndex][3]

	Local $aRet[2] = [$iX, $iY]

	Return $aRet

EndFunc   ;==>_GUIScrollbars_Locate_Ctrl

; #FUNCTION# =========================================================================================================
; Name...........: _GUIScrollbars_Scroll_Page
; Description ...: Scrolls scrollbars generated by _GUIScrollbars_Generate to min, max or page number
; Syntax.........: _GUIScrollbars_Scroll_Page ($hWnd, [$iH_Scroll_Pos = -1, [$iV_Scroll_Pos = -1]])
; Parameters ....: $hWnd          -> GUI to contain scrollbars
;                  $iH_Scroll_Pos -> Horizontal page number:
;                                    0 = No change
;                                    1+ = Scroll to page number
;                                    If page number is over max pages, then scroll to max position
;                  $iV_Scroll_Pos -> As $iH_Scroll_Pos for vertical pages
; Requirement(s).: v3.3.6.0 or higher
; Return values .: Success: @error = 0
;                  Failure: @error set as follows:
;                           1 - hWnd not a valid handle
;                           2 - Scrollbars not generated in that GUI
;                           3 - Invalid position parameters
; Remarks .......;
; Author ........: Melba23
; Example........; Yes
;=====================================================================================================================
Func _GUIScrollbars_Scroll_Page($hWnd, $iH_Scroll_Pos = 0, $iV_Scroll_Pos = 0)

	Local $iPos

	; Check $hWnd
	If Not IsHWnd($hWnd) Then Return SetError(1, 0, 0)

	; Check $iH/V_Scroll_Pos
	If Not (IsInt($iH_Scroll_Pos) And IsInt($iV_Scroll_Pos)) Then Return SetError(3, 0, 0)

	; Find window info
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex = -1 Then Return SetError(2, 0, 0)

	; Get page sizes
	Local $iH_Page = Floor($__g_aSB_WindowInfo[$iIndex][4] / $__g_aSB_WindowInfo[$iIndex][2])
	Local $iV_Page = Floor($__g_aSB_WindowInfo[$iIndex][5] / $__g_aSB_WindowInfo[$iIndex][3])

	If $iH_Scroll_Pos > 0 Then
		$iPos = ($iH_Scroll_Pos - 1) * $iH_Page
		If $iPos > $__g_aSB_WindowInfo[$iIndex][6] Then $iPos = $__g_aSB_WindowInfo[$iIndex][6]
		_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_HORZ, $iPos)
	EndIf
	If $iV_Scroll_Pos > 0 Then
		$iPos = ($iV_Scroll_Pos - 1) * $iV_Page
		If $iPos > $__g_aSB_WindowInfo[$iIndex][7] Then $iPos = $__g_aSB_WindowInfo[$iIndex][7]
		_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_VERT, $iPos)
	EndIf

EndFunc   ;==>_GUIScrollbars_Scroll_Page

; #FUNCTION# =========================================================================================================
; Name...........: _GUIScrollbars_Minimize
; Description ...: Stores scrollbar positions on GUI minimize
; Syntax.........: _GUIScrollbars_Minimize($hWnd)
; Parameters ....: $hWnd -> GUI containing scrollbars
; Requirement(s).: v3.3.6.0 or higher
; Return values .: Success: @error = 0
;                  Failure: @error set as follows:
;                           1 - hWnd not a valid handle
;                           2 - Scrollbars not generated in that GUI
; Remarks .......;
; Author ........: Melba23, based on code from rover and czardas
; Example........; Yes
;=====================================================================================================================
Func _GUIScrollbars_Minimize($hWnd)

	; Check $hWnd
	If Not IsHWnd($hWnd) Then Return SetError(1, 0, 0)

	; Find window info
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex = -1 Then Return SetError(1, 0, 0)

	; Show both scrollbars
	_GUIScrollBars_ShowScrollBar($hWnd, $SB_BOTH, True)
	; Get vertical current position and move to top
	$__g_aSB_WindowInfo[$iIndex][8] = _GUIScrollBars_GetScrollPos($hWnd, $SB_VERT)
	_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_VERT, 0)
	; Get horizontal current position and move to left
	$__g_aSB_WindowInfo[$iIndex][9] = _GUIScrollBars_GetScrollPos($hWnd, $SB_HORZ)
	_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_HORZ, 0)

EndFunc   ;==>_GUIScrollbars_Minimize

; #FUNCTION# =========================================================================================================
; Name...........: _GUIScrollbars_Restore
; Description ...: Restores scrollbar positions on GUI restore
; Syntax.........: _GUIScrollbars_Restore($hWnd[, $fVert = True[, $fHorz = True]])
; Parameters ....: $hWnd  -> GUI containing scrollbars
;                  $fVert -> True (default) = vertical scrollbar visible; False = vertical scrollbar not visible
;                  $fHorz -> True (default) = horizontal scrollbar visible; False = horzontal scrollbar not visible
; Requirement(s).: v3.3.6.0 or higher
; Return values .: Success: @error = 0
;                  Failure: @error set as follows:
;                           1 - hWnd not a valid handle
;                           2 - Scrollbars not generated in that GUI
; Remarks .......;
; Author ........: Melba23, based on code from rover and czardas
; Example........; Yes
;=====================================================================================================================
Func _GUIScrollbars_Restore($hWnd, $fVert = True, $fHorz = True)

	; Check $hWnd
	If Not IsHWnd($hWnd) Then Return SetError(1, 0, 0)

	; Find window info
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex = -1 Then Return SetError(2, 0, 0)

	; Rehide unwanted scrollbars
	If Not $fVert Then
		_GUIScrollBars_ShowScrollBar($hWnd, $SB_VERT, False)
	EndIf
	If Not $fHorz Then
		_GUIScrollBars_ShowScrollBar($hWnd, $SB_HORZ, False)
	EndIf
	; Reset visible scrollbars to position on minimize
	If $fVert Then
		_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_VERT, $__g_aSB_WindowInfo[$iIndex][8])
	EndIf
	If $fHorz Then
		_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_HORZ, $__g_aSB_WindowInfo[$iIndex][9])
	EndIf

EndFunc   ;==>_GUIScrollbars_Restore

; #INTERNAL_USE_ONLY#============================================================================================================
; Name...........: _Scrollbars_WM_VSCROLL
; Description ...: GUIRegisterMsg procedure for vertical scrollbar
; Syntax ........: _Scrollbars_WM_VSCROLL($hWnd, $iMsg, $wParam, $lParam)
; Return values .: None
; Author ........: Taken from AutoIt Help file
; Remarks .......: This function is used internally by _Scrollbars_Generate
; ===============================================================================================================================
Func _Scrollbars_WM_VSCROLL($hWnd, $iMsg, $wParam, $lParam)

	#forceref $iMsg, $wParam, $lParam
	Local $nScrollCode = BitAND($wParam, 0x0000FFFF)
	Local $iIndex = -1, $yChar, $yPos
	Local $Min, $Max, $Page, $Pos, $TrackPos

	For $x = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $__g_aSB_WindowInfo[$x][0] = $hWnd Then
			$iIndex = $x
			$yChar = $__g_aSB_WindowInfo[$iIndex][3]
			ExitLoop
		EndIf
	Next
	If $iIndex = -1 Then Return 0

	Local $tSCROLLINFO = _GUIScrollBars_GetScrollInfoEx($hWnd, $SB_VERT)
	$Min = DllStructGetData($tSCROLLINFO, "nMin")
	$Max = DllStructGetData($tSCROLLINFO, "nMax")
	$Page = DllStructGetData($tSCROLLINFO, "nPage")
	$yPos = DllStructGetData($tSCROLLINFO, "nPos")
	$Pos = $yPos
	$TrackPos = DllStructGetData($tSCROLLINFO, "nTrackPos")

	Switch $nScrollCode
		Case $SB_TOP
			DllStructSetData($tSCROLLINFO, "nPos", $Min)
		Case $SB_BOTTOM
			DllStructSetData($tSCROLLINFO, "nPos", $Max)
		Case $SB_LINEUP
			DllStructSetData($tSCROLLINFO, "nPos", $Pos - 1)
		Case $SB_LINEDOWN
			DllStructSetData($tSCROLLINFO, "nPos", $Pos + 1)
		Case $SB_PAGEUP
			DllStructSetData($tSCROLLINFO, "nPos", $Pos - $Page)
		Case $SB_PAGEDOWN
			DllStructSetData($tSCROLLINFO, "nPos", $Pos + $Page)
		Case $SB_THUMBTRACK
			DllStructSetData($tSCROLLINFO, "nPos", $TrackPos)
	EndSwitch

	DllStructSetData($tSCROLLINFO, "fMask", $SIF_POS)
	_GUIScrollBars_SetScrollInfo($hWnd, $SB_VERT, $tSCROLLINFO)
	_GUIScrollBars_GetScrollInfo($hWnd, $SB_VERT, $tSCROLLINFO)

	$Pos = DllStructGetData($tSCROLLINFO, "nPos")
	If ($Pos <> $yPos) Then
		_GUIScrollBars_ScrollWindow($hWnd, 0, $yChar * ($yPos - $Pos))
		$yPos = $Pos
	EndIf

	Return $GUI_RUNDEFMSG

EndFunc   ;==>_Scrollbars_WM_VSCROLL

; #INTERNAL_USE_ONLY#============================================================================================================
; Name...........: _Scrollbars_WM_HSCROLL
; Description ...: GUIRegisterMsg procedure for horizontal scrollbar
; Syntax ........: _Scrollbars_WM_HSCROLL($hWnd, $Msg, $wParam, $lParam)
; Return values .: None
; Author ........: Taken from AutoIt Help file
; Remarks .......: This function is used internally by _Scrollbars_Generate
; ===============================================================================================================================
Func _Scrollbars_WM_HSCROLL($hWnd, $iMsg, $wParam, $lParam)

	#forceref $iMsg, $lParam
	Local $nScrollCode = BitAND($wParam, 0x0000FFFF)
	Local $iIndex = -1, $xChar, $xPos
	Local $Page, $Pos, $TrackPos

	For $x = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $__g_aSB_WindowInfo[$x][0] = $hWnd Then
			$iIndex = $x
			$xChar = $__g_aSB_WindowInfo[$iIndex][2]
			ExitLoop
		EndIf
	Next
	If $iIndex = -1 Then Return 0

	Local $tSCROLLINFO = _GUIScrollBars_GetScrollInfoEx($hWnd, $SB_HORZ)
	$Page = DllStructGetData($tSCROLLINFO, "nPage")
	$xPos = DllStructGetData($tSCROLLINFO, "nPos")
	$Pos = $xPos
	$TrackPos = DllStructGetData($tSCROLLINFO, "nTrackPos")
	Switch $nScrollCode
		Case $SB_LINELEFT
			DllStructSetData($tSCROLLINFO, "nPos", $Pos - 1)
		Case $SB_LINERIGHT
			DllStructSetData($tSCROLLINFO, "nPos", $Pos + 1)
		Case $SB_PAGELEFT
			DllStructSetData($tSCROLLINFO, "nPos", $Pos - $Page)
		Case $SB_PAGERIGHT
			DllStructSetData($tSCROLLINFO, "nPos", $Pos + $Page)
		Case $SB_THUMBTRACK
			DllStructSetData($tSCROLLINFO, "nPos", $TrackPos)
	EndSwitch

	DllStructSetData($tSCROLLINFO, "fMask", $SIF_POS)
	_GUIScrollBars_SetScrollInfo($hWnd, $SB_HORZ, $tSCROLLINFO)
	_GUIScrollBars_GetScrollInfo($hWnd, $SB_HORZ, $tSCROLLINFO)

	$Pos = DllStructGetData($tSCROLLINFO, "nPos")
	If ($Pos <> $xPos) Then _GUIScrollBars_ScrollWindow($hWnd, $xChar * ($xPos - $Pos), 0)

	Return $GUI_RUNDEFMSG

EndFunc   ;==>_Scrollbars_WM_HSCROLL

; #INTERNAL_USE_ONLY#============================================================================================================
; Name...........: _Scrollbars_WM_MOUSEWHEEL
; Description ...: GUIRegisterMsg procedure for vertical mouse wheel scroll
; Syntax ........: _Scrollbars_WM_MOUSEWHEEL($hWnd, $iMsg, $wParam, $lParam)
; Return values .: None
; Author ........: Based on code from MrCreator & Malkey
; Remarks .......: This function is used internally by _Scrollbars_Generate
;                  Pressing Ctrl or Shft will move the Horizontal scrollbar with the vertical mousewheel
; ===============================================================================================================================
Func _Scrollbars_WM_MOUSEWHEEL($hWnd, $iMsg, $wParam, $lParam)

	#forceref $hWnd, $iMsg, $lParam
	Local $iDirn, $iDelta = BitShift($wParam, 16) ; Mouse wheel movement
	; Find window index
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex <> -1 Then
		If BitAND($wParam, 0x0000FFFF) Then ; If Ctrl or Shft pressed move Horz scrollbar
			$iDirn = $SB_LINERIGHT
			If $iDelta > 0 Then $iDirn = $SB_LINELEFT
			For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][6]
				_SendMessage($hWnd, $WM_HSCROLL, $iDirn)
			Next
		Else ; Move Vert scrollbar
			$iDirn = $SB_LINEDOWN
			If $iDelta > 0 Then $iDirn = $SB_LINEUP
			For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][6]
				_SendMessage($hWnd, $WM_VSCROLL, $iDirn)
			Next
		EndIf
	EndIf

	Return $GUI_RUNDEFMSG

EndFunc   ;==>_Scrollbars_WM_MOUSEWHEEL

; #INTERNAL_USE_ONLY#============================================================================================================
; Name...........: _Scrollbars_WM_MOUSEHWHEEL
; Description ...: GUIRegisterMsg procedure for horizontal mouse wheel scroll
; Syntax ........: _Scrollbars_WM_MOUSEWHEEL($hWnd, $iMsg, $wParam, $lParam)
; Return values .: None
; Author ........: Based on code from MSDN, MrCreator & Malkey
; Remarks .......: This function is used internally by _Scrollbars_Generate
; ===============================================================================================================================
Func _Scrollbars_WM_MOUSEHWHEEL($hWnd, $iMsg, $wParam, $lParam)

	#forceref $hWnd, $iMsg, $lParam
	Local $iDirn = $SB_LINERIGHT
	If BitShift($wParam, 16) > 0 Then $iDirn = $SB_LINELEFT ; Mouse wheel movement
	; Find window index
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex <> -1 Then
		For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][5]
			_SendMessage($hWnd, $WM_HSCROLL, $iDirn)
		Next
	EndIf

	Return $GUI_RUNDEFMSG

EndFunc   ;==>_Scrollbars_WM_MOUSEHWHEEL

; #INTERNAL_USE_ONLY#============================================================================================================
; Name...........: _Scrollbars_WM_KEYUP
; Description ...: GUIRegisterMsg procedure for scrolling with cursor, pageup/down, home, and end keys
; Syntax ........: _Scrollbars_WM_KeyUp($hWnd, $iMsg, $wParam, $lParam)
; Return values .: None
; Author ........: Based on code from Sm0ke_N
; Remarks .......: This function is used internally by _Scrollbars_Generate
;                  Pressing Ctrl with the Home and End keys will move the Horizontal scrollbar
; ===============================================================================================================================
Func _Scrollbars_WM_KEYUP($hWnd, $iMsg, $wParam, $lParam)

	#forceref $hWnd, $iMsg, $lParam

	Local $aRet_Ctrl
	; Find window index
	Local $iIndex = -1
	For $i = 0 To UBound($__g_aSB_WindowInfo) - 1
		If $hWnd = $__g_aSB_WindowInfo[$i][0] Then $iIndex = $i
	Next
	If $iIndex <> -1 Then
		; Check if Ctrl pressed
		Local $bCtrl = False
		$aRet_Ctrl = DllCall("user32.dll", "short", "GetAsyncKeyState", "int", "0x11")
		If $aRet_Ctrl[0] Then $bCtrl = True
		; Check key pressed
		Switch $wParam
			Case 0x21 ; PageUp
				If $bCtrl Then
					_SendMessage($hWnd, $WM_HSCROLL, $SB_PAGELEFT)
				Else
					_SendMessage($hWnd, $WM_VSCROLL, $SB_PAGEUP)
				EndIf
			Case 0x22 ; PageDown
				If $bCtrl Then
					_SendMessage($hWnd, $WM_HSCROLL, $SB_PAGERIGHT)
				Else
					_SendMessage($hWnd, $WM_VSCROLL, $SB_PAGEDOWN)
				EndIf
			Case 0x23 ; End
				If $bCtrl Then
					_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_HORZ, $__g_aSB_WindowInfo[$iIndex][6])
				Else
					_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_VERT, $__g_aSB_WindowInfo[$iIndex][7])
				EndIf
			Case 0x24 ; Home
				If $bCtrl Then
					_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_HORZ, 0)
				Else
					_GUIScrollBars_SetScrollInfoPos($hWnd, $SB_VERT, 0)
				EndIf
			Case 0x25 ; Left
				For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][5]
					_SendMessage($hWnd, $WM_HSCROLL, $SB_LINELEFT)
				Next
			Case 0x26 ; Up
				For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][5]
					_SendMessage($hWnd, $WM_VSCROLL, $SB_LINEUP)
				Next
			Case 0x27 ; Right
				For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][5]
					_SendMessage($hWnd, $WM_HSCROLL, $SB_LINERIGHT)
				Next
			Case 0x28 ; Down
				For $i = 1 To $__g_aSB_WindowInfoEx[$iIndex][5]
					_SendMessage($hWnd, $WM_VSCROLL, $SB_LINEDOWN)
				Next
		EndSwitch
	EndIf
	Return $GUI_RUNDEFMSG
EndFunc   ;==>_Scrollbars_WM_KEYUP
