﻿Imports System.IO
Imports System.Text

Partial Public Class VM
    Public Sub RunStep()
        While callStack.Count > 0 AndAlso Regs(6) > callStack.Peek().Regs(6) - 2
            callStack.Pop()
        End While
        prevState = New VMState(Me)
        If verbose Then WriteState(prevState, False)

        Select Case Me(PC + 1) >> 4
            Case 0
                Exec0()
                Return
            Case 1 ' mov: MOVe
                Dim oprs = GetSrcDst(2)
                Dim src = oprs.Item1.GetValue()
                oprs.Item2.SetValue(src)
                SetFlags(src = 0, ConvShort(src) < 0, C, False)
                Return
            Case 2 ' cmp: CoMPare
                Dim oprs = GetSrcSrc(2)
                Dim src = oprs.Item1.GetValue()
                Dim dst = oprs.Item2.GetValue()
                Dim val = CInt(ConvShort(src)) - CInt(ConvShort(dst))
                SetFlags(val = 0, val < 0, src < dst, val < -&H8000)
                Return
            Case 3 ' bit: BIt Test
                Dim oprs = GetSrcSrc(2)
                Dim val = oprs.Item1.GetValue() And oprs.Item2.GetValue()
                SetFlags(val = 0, (val And &H8000) <> 0, C, False)
                Return
            Case 4 ' bic: BIt Clear
                Dim oprs = GetSrcDst(2)
                Dim val = (Not oprs.Item1.GetValue()) And oprs.Item2.GetValue()
                oprs.Item2.SetValue(val)
                SetFlags(val = 0, (val And &H8000) <> 0, C, False)
                Return
            Case 5 ' bis: BIt Set
                Dim oprs = GetSrcDst(2)
                Dim val = oprs.Item1.GetValue() Or oprs.Item2.GetValue()
                oprs.Item2.SetValue(val)
                SetFlags(val = 0, (val And &H8000) <> 0, C, False)
                Return
            Case 6 ' add: ADD
                Dim oprs = GetSrcDst(2)
                Dim src = oprs.Item1.GetValue()
                Dim dst = oprs.Item2.GetValue()
                Dim val = CInt(ConvShort(src)) + CInt(ConvShort(dst))
                oprs.Item2.SetValue(CUShort(val And &HFFFF))
                SetFlags(val = 0, val < 0, CInt(src) + CInt(dst) >= &H10000, val >= &H8000)
                Return
            Case 7
                Exec7()
                Return
            Case &O10
                Exec10()
                Return
            Case &O11 ' movb: MOVe Byte
                Dim oprs = GetSrcDst(1)
                Dim src = oprs.Item1.GetByte()
                oprs.Item2.SetByte(src)
                SetFlags(src = 0, ConvSByte(src) < 0, C, False)
                Return
            Case &O12 ' cmpb: CoMPare Byte
                Dim oprs = GetSrcSrc(1)
                Dim src = oprs.Item1.GetByte()
                Dim dst = oprs.Item2.GetByte()
                Dim val = CInt(ConvSByte(src)) - CInt(ConvSByte(dst))
                SetFlags(val = 0, val < 0, src < dst, val < -&H80)
                Return
            Case &O13 ' bitb: BIt Test Byte
                Dim oprs = GetSrcSrc(1)
                Dim val = oprs.Item1.GetByte() And oprs.Item2.GetByte()
                SetFlags(val = 0, (val And &H80) <> 0, C, False)
                Return
            Case &O14 ' bicb: BIt Clear Byte
                Dim oprs = GetSrcDst(1)
                Dim val = (Not oprs.Item1.GetByte()) And oprs.Item2.GetByte()
                oprs.Item2.SetByte(val)
                SetFlags(val = 0, (val And &H80) <> 0, C, False)
                Return
            Case &O15 ' bisb: BIt Set Byte
                Dim oprs = GetSrcDst(1)
                Dim val = oprs.Item1.GetByte() Or oprs.Item2.GetByte()
                oprs.Item2.SetByte(val)
                SetFlags(val = 0, (val And &H80) <> 0, C, False)
                Return
            Case &O16 ' sub: SUBtract
                Dim oprs = GetSrcDst(2)
                Dim src = oprs.Item1.GetValue()
                Dim dst = oprs.Item2.GetValue()
                Dim val = CInt(ConvShort(dst)) - CInt(ConvShort(src))
                oprs.Item2.SetValue(CUShort(val And &HFFFF))
                SetFlags(val = 0, val < 0, dst < src, val < -&H8000)
                Return
            Case &O17
                Exec17()
                Return
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec0()
        Select Case Me(PC + 1)
            Case 1 ' br: BRanch
                PC = GetOffset(PC)
                Return
            Case 2 ' bne: Branch if Not Equal
                PC = If(Not Z, GetOffset(PC), PC + 2US)
                Return
            Case 3 ' beq: Branch if EQual
                PC = If(Z, GetOffset(PC), PC + 2US)
                Return
            Case 4 ' bge: Branch if Greater or Equal
                PC = If(Not (N Xor Me.V), GetOffset(PC), PC + 2US)
                Return
            Case 5 ' blt: Branch if Less Than
                PC = If(N Xor Me.V, GetOffset(PC), PC + 2US)
                Return
            Case 6 ' bgt: Branch if Greater Than
                PC = If(Not (Z Or (N Xor Me.V)), GetOffset(PC), PC + 2US)
                Return
            Case 7 ' ble: Branch if Less or Equal
                PC = If(Z Or (N Xor Me.V), GetOffset(PC), PC + 2US)
                Return
        End Select
        Dim len = 2US
        Dim v = ReadUInt16(PC)
        If v = &O240 Then PC += 2US : Return ' nop: No OPeration
        Dim v1 = (v >> 9) And 7, v2 = (v >> 6) And 7
        Select Case v1
            Case 0 ' 00 0x xx
                Select Case v2
                    'Case 0 ' 00 00 xx
                    '    Select Case v And &O77
                    '        Case 0 : Return New OpCode("halt", 2)
                    '        Case 1 : Return New OpCode("wait", 2)
                    '        Case 2 : Return New OpCode("rti", 2)
                    '        Case 3 : Return New OpCode("bpt", 2)
                    '        Case 4 : Return New OpCode("iot", 2)
                    '        Case 5 : Return New OpCode("reset", 2)
                    '        Case 6 : Return New OpCode("rtt", 2)
                    '    End Select
                    Case 1 ' jmp: JuMP
                        PC = GetSrc(2).GetAddress()
                        Return
                    Case 2 ' 00 02 xx
                        Select Case (v >> 3) And 7
                            'Case 3 : Return New OpCode("spl " + (v & 7), 2)
                            Case 0 ' rts: ReTurn from Subroutine
                                Dim r = v And 7
                                PC = Regs(r)
                                Regs(r) = ReadUInt16(GetInc(6, 2))
                                Return
                            Case 4 To 7 ' cl*/se*/ccc/scc: CLear/SEt (Condition Codes)
                                Dim f = (v And 16) <> 0
                                If (v And 8) <> 0 Then Me.N = f
                                If (v And 4) <> 0 Then Me.Z = f
                                If (v And 2) <> 0 Then Me.V = f
                                If (v And 1) <> 0 Then Me.C = f
                                PC += 2US
                                Return
                        End Select
                    Case 3 ' swab: SWAp Bytes
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim bh = (val0 >> 8) And &HFF
                        Dim bl = val0 And &HFF
                        Dim val1 = CUShort(((bl << 8) Or bh) And &HFFFF)
                        dst.SetValue(val1)
                        SetFlags(val1 = 0, (val1 And &H8000) <> 0, False, False)
                        Return
                End Select
            Case 4 ' jsr: Jump to SubRoutine
                Dim r = (v >> 6) And 7
                If r = 7 Then callStack.Push(New VMState(Me))
                Dim dst = GetSrc(2).GetAddress()
                Write(GetDec(6, 2), Regs(r))
                Regs(r) = PC
                PC = dst
                Return
            Case 5
                Select Case v2
                    Case 0 ' clr: CLeaR
                        GetDst(2).SetValue(0)
                        SetFlags(True, False, False, False)
                        Return
                    Case 1 ' com: COMplement
                        Dim dst = GetDst(2)
                        Dim val = Not dst.GetValue()
                        dst.SetValue(val)
                        SetFlags(val = 0, (val And &H8000) <> 0, True, False)
                        Return
                    Case 2 ' inc: INCrement
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue())) + 1
                        dst.SetValue(CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C, val = &H8000)
                        Return
                    Case 3 ' dec: DECrement
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue())) - 1
                        dst.SetValue(CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C, val = -&H8001)
                        Return
                    Case 4 ' neg: NEGate
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim val1 = -ConvShort(val0)
                        Dim val2 = CUShort(val1 And &HFFFF)
                        dst.SetValue(val2)
                        SetFlags(val1 = 0, val1 < 0, val1 <> 0, val1 = &H8000)
                        Return
                    Case 5 ' adc: ADd Carry
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue())) + If(C, 1, 0)
                        dst.SetValue(CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C AndAlso val = 0, val = &H8000)
                        Return
                    Case 6 ' sbc: SuBtract Carry
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue())) - If(C, 1, 0)
                        dst.SetValue(CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C AndAlso val = -1, val = -&H8001)
                        Return
                    Case 7 ' tst: TeST
                        Dim dst = ConvShort(GetSrc(2).GetValue())
                        SetFlags(dst = 0, dst < 0, False, False)
                        Return
                End Select
            Case 6
                Select Case v2
                    'Case 5 : Return ReadDst("mfpi", bd, pos)
                    'Case 6 : Return ReadDst("mtpi", bd, pos)
                    Case 0 ' ror: ROtate Right
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim val1 = (val0 >> 1) Or If(C, &H8000US, 0US)
                        dst.SetValue(val1)
                        Dim lsb0 = (val0 And 1) <> 0
                        Dim msb1 = C
                        SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                        Return
                    Case 1 ' rol: ROtate Left
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim val1 = CUShort((CUInt(val0) << 1) And &HFFFF) Or If(C, 1US, 0US)
                        dst.SetValue(val1)
                        Dim msb0 = (val0 And &H8000) <> 0
                        Dim msb1 = (val1 And &H8000) <> 0
                        SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                        Return
                    Case 2 ' asr: Arithmetic Shift Right
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim val1 = ConvShort(val0) >> 1
                        dst.SetValue(CUShort(val1 And &HFFFF))
                        Dim lsb0 = (val0 And 1) <> 0
                        Dim msb1 = val1 < 0
                        SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                        Return
                    Case 3 ' asl: Arithmetic Shift Left
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue()
                        Dim val1 = CUShort((CUInt(val0) << 1) And &HFFFF)
                        dst.SetValue(val1)
                        Dim msb0 = (val0 And &H8000) <> 0
                        Dim msb1 = val1 < 0
                        SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                        Return
                    Case 4 ' mark: MARK
                        Dim nn = v And &O77
                        Regs(6) = CUShort((Regs(6) + 2 * nn) And &HFFFF)
                        PC = Regs(5)
                        Regs(5) = ReadUInt16(GetInc(6, 2))
                        Return
                    Case 7 ' sxt: Sign eXTend
                        GetDst(2).SetValue(If(N, &HFFFFUS, 0US))
                        SetFlags(Not N, N, C, Me.V)
                        Return
                End Select
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec7()
        Dim v = ReadUInt16(PC)
        Select Case (v >> 9) And 7
            'Case 5
            '    Select Case (v >> 3) And &O77
            '        Case 0 : Return ReadReg("fadd", bd, pos)
            '        Case 1 : Return ReadReg("fsub", bd, pos)
            '        Case 2 : Return ReadReg("fmul", bd, pos)
            '        Case 3 : Return ReadReg("fdiv", bd, pos)
            '    End Select
            Case 0 ' mul: MULtiply
                Dim src = ConvShort(GetSrc(2).GetValue())
                Dim r = (v >> 6) And 7
                Dim val = CInt(Regs(r)) * src
                If (r And 1) = 0 Then
                    SetReg32(r, val)
                Else
                    Regs(r) = CUShort(val And &HFFFF)
                End If
                SetFlags(val = 0, val < 0, val < -&H8000 OrElse val >= &H8000, False)
                Return
            Case 1 ' div: DIVide
                Dim src = ConvShort(GetSrc(2).GetValue())
                Dim r = (v >> 6) And 7
                If src = 0 OrElse Math.Abs(ConvShort(Regs(r))) > Math.Abs(src) Then
                    SetFlags(False, False, src = 0, True)
                Else
                    Dim dst = GetReg32(r)
                    Dim r1 = dst \ src
                    Dim r2 = dst Mod src
                    Regs(r) = CUShort(r1 And &HFFFF)
                    Regs((r + 1) And 7) = CUShort(r2 And &HFFFF)
                    SetFlags(r1 = 0, r1 < 0, False, False)
                End If
                Return
            Case 2 ' ash: Arithmetic SHift
                Dim src = GetSrc(2).GetValue() And &O77
                Dim r = (v >> 6) And 7
                Dim val0 = ConvShort(Regs(r))
                If src = 0 Then
                    SetFlags(val0 = 0, val0 < 0, C, False)
                Else
                    If (src And &O40) = 0 Then
                        Dim val1 = val0 << (src - 1)
                        Dim val2 = val1 << 1
                        Regs(r) = CUShort(val2 And &HFFFF)
                        SetFlags(val2 = 0, val2 < 0, (val1 And 1) <> 0, val0 <> val2)
                    Else
                        Dim val1 = val0 >> (63 - src)
                        Dim val2 = val1 >> 1
                        Regs(r) = CUShort(val2 And &HFFFF)
                        SetFlags(val2 = 0, val2 < 0, val1 < 0, val0 <> val2)
                    End If
                End If
                Return
            Case 3 ' ashc: Arithmetic SHift Combined
                Dim src = GetSrc(2).GetValue() And &O77
                Dim r = (v >> 6) And 7
                Dim val0 = GetReg32(r)
                If src = 0 Then
                    SetFlags(val0 = 0, val0 < 0, C, False)
                Else
                    If (src And &O40) = 0 Then
                        Dim val1 = val0 << (src - 1)
                        Dim val2 = val1 << 1
                        SetReg32(r, val2)
                        SetFlags(val2 = 0, val2 < 0, (val1 And 1) <> 0, val0 <> val2)
                    Else
                        Dim val1 = val0 >> (63 - src)
                        Dim val2 = val1 >> 1
                        SetReg32(r, val2)
                        SetFlags(val2 = 0, val2 < 0, val1 < 0, val0 <> val2)
                    End If
                End If
                Return
            Case 4 ' xor: eXclusive OR
                Dim dst = GetDst(2)
                Dim val = Regs((v >> 6) And 7) Xor dst.GetValue()
                dst.SetValue(val)
                SetFlags(val = 0, (val And &H8000) <> 0, C, False)
                Return
            Case 7 ' sob: Subtract One from register, Branch if not zero
                Dim r = (v >> 6) And 7
                Regs(r) = CUShort((Regs(r) - 1) And &HFFFF)
                PC = If(Regs(r) <> 0, CUShort(PC + 2 - (v And &O77) * 2), PC + 2US)
                Return
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec10()
        Select Case Me(PC + 1)
            'Case &H88 : Return New OpCode("emt " + bd.Enc(bd(pos)), 2)
            Case &H80 ' bpl: Branch if PLus
                PC = If(Not N, GetOffset(PC), PC + 2US)
                Return
            Case &H81 ' bmi: Branch if MInus
                PC = If(N, GetOffset(PC), PC + 2US)
                Return
            Case &H82 ' bhi: Branch if HIgher
                PC = If(Not (C Or Z), GetOffset(PC), PC + 2US)
                Return
            Case &H83 ' blos: Branch if LOwer or Same
                PC = If(C Or Z, GetOffset(PC), PC + 2US)
                Return
            Case &H84 ' bvc: Branch if oVerflow Clear
                PC = If(Not Me.V, GetOffset(PC), PC + 2US)
                Return
            Case &H85 ' bvs: Branch if oVerflow Set
                PC = If(Me.V, GetOffset(PC), PC + 2US)
                Return
            Case &H86 ' bcc: Branch if Carry Clear
                PC = If(Not C, GetOffset(PC), PC + 2US)
                Return
            Case &H87 ' bcs: Branch if Carry Set
                PC = If(C, GetOffset(PC), PC + 2US)
                Return
            Case &H89 ' sys
                ExecSys()
                Return
        End Select
        Dim v = ReadUInt16(PC)
        Select Case (v >> 6) And &O77
            'Case &O64 : Return ReadDst("mfpd", bd, pos)
            'Case &O65 : Return ReadDst("mtpd", bd, pos)
            Case &O50 ' clrb: CLeaR Byte
                GetDst(1).SetByte(0)
                SetFlags(True, False, False, False)
                Return
            Case &O51 ' comb: COMplement Byte
                Dim dst = GetDst(1)
                Dim val = Not dst.GetByte()
                dst.SetByte(val)
                SetFlags(val = 0, (val And &H80) <> 0, True, False)
                Return
            Case &O52 ' incb: INCrement Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte())) + 1
                dst.SetByte(CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C, val = &H80)
                Return
            Case &O53 ' decb: DECrement Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte())) - 1
                dst.SetByte(CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C, val = -&H81)
                Return
            Case &O54 ' negb: NEGate Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte()
                Dim val1 = -ConvSByte(val0)
                Dim val2 = CByte(val1 And &HFF)
                dst.SetByte(val2)
                SetFlags(val1 = 0, val1 < 0, val1 <> 0, val1 = &H80)
                Return
            Case &O55 ' adcb: ADd Carry Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte())) + If(C, 1, 0)
                dst.SetByte(CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C AndAlso val = 0, val = &H80)
                Return
            Case &O56 ' sbcb: SuBtract Carry Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte())) - If(C, 1, 0)
                dst.SetByte(CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C AndAlso val = -1, val = -&H81)
                Return
            Case &O57 ' tstb: TeST Byte
                Dim dst = ConvSByte(GetSrc(1).GetByte())
                SetFlags(dst = 0, dst < 0, False, False)
                Return
            Case &O60 ' rorb: ROtate Right Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte()
                Dim val1 = val0 >> 1
                If C Then val1 = CByte(val1 + &H80)
                dst.SetByte(val1)
                Dim lsb0 = (val0 And 1) <> 0
                Dim msb1 = C
                SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                Return
            Case &O61 ' rolb: ROtate Left Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte()
                Dim val1 = CByte(((CUInt(val0) << 1) + If(C, 1, 0)) And &HFF)
                dst.SetByte(val1)
                Dim msb0 = (val0 And &H80) <> 0
                Dim msb1 = (val1 And &H80) <> 0
                SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                Return
            Case &O62 ' asrb: Arithmetic Shift Right Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte()
                Dim val1 = ConvSByte(val0) >> 1
                dst.SetByte(CByte(val1 And &HFF))
                Dim lsb0 = (val0 And 1) <> 0
                Dim msb1 = val1 < 0
                SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                Return
            Case &O63 ' aslb: Arithmetic Shift Left Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte()
                Dim val1 = CByte((CUInt(val0) << 1) And &HFF)
                dst.SetByte(val1)
                Dim msb0 = (val0 And &H80) <> 0
                Dim msb1 = val1 < 0
                SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                Return
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec17()
        Dim v = ReadUInt16(PC)
        Select Case v And &HFFF
            Case 1 ' setf: SET Float
                PC += 2US
                IsDouble = False
                Return
            Case 2 ' seti: SET Integer
                PC += 2US
                IsLong = False
                Return
            Case &O11 ' setd: SET Double
                PC += 2US
                IsDouble = True
                Return
            Case &O12 ' setl: SET Long
                PC += 2US
                IsLong = True
                Return
        End Select
        Abort("not implemented")
    End Sub
End Class
