﻿Imports System.IO
Imports System.Text

Partial Public Class VM
    Inherits BinData

    Public Regs(7) As UShort
    Private bakRegs(7) As UShort

    Public Property PC As UShort
        Get
            Return Regs(7)
        End Get
        Set(value As UShort)
            Regs(7) = value
        End Set
    End Property

    Public Property IsLong As Boolean
    Public Property IsDouble As Boolean
    Public Property HasExited As Boolean

    Public Property Z As Boolean
    Public Property N As Boolean
    Public Property C As Boolean
    Public Property V As Boolean

    Private swt As StringWriter
    Private swo As StringWriter

    Public ReadOnly Property Trace$
        Get
            Return swt.ToString
        End Get
    End Property

    Public ReadOnly Property Output$
        Get
            Return swo.ToString
        End Get
    End Property

    Private aout As AOut

    Public Sub New(aout As AOut)
        MyBase.New(&H10000)
        Array.Copy(aout.Data, aout.Offset, Data, 0, aout.tsize + aout.dsize)
        Me.UseOct = aout.UseOct
        Me.aout = aout
        PC = aout.entry
        SetArgs(New String() {aout.Path})
    End Sub

    Public Sub SetArgs(args$())
        Dim p = &H10000
        Dim list = New List(Of Integer)
        For i = args.Length - 1 To 0 Step -1
            Dim bytes = Encoding.UTF8.GetBytes(args(i))
            Dim len = (bytes.Length \ 2) * 2 + 2
            p -= len
            Array.Copy(bytes, 0, Data, p, bytes.Length)
            Array.Clear(Data, p + bytes.Length, len - bytes.Length)
            list.Add(p)
        Next
        For Each arg In list
            p -= 2
            Write(p, CUShort(arg))
        Next
        p -= 2
        Write(p, CUShort(args.Length))
        Regs(6) = CUShort(p)
    End Sub

    Public Sub Run(args$())
        If args IsNot Nothing Then
            Dim args2$(args.Length)
            Array.Copy(args, 0, args2, 1, args.Length)
            args2(0) = aout.Path
            SetArgs(args2)
        Else
            SetArgs(New String() {aout.Path})
        End If
        Run()
    End Sub

    Public Sub Run()
        HasExited = False
        swt = New StringWriter
        swo = New StringWriter
        Dim cur As Symbol = Nothing
        Dim op = New OpCode("", 0)
        While Not HasExited
            Dim sym = aout.GetSymbol(PC)
            If cur IsNot sym Then
                swt.Write("     ")
                If op.Mnemonic.StartsWith("rts ") Then
                    swt.WriteLine("<{0}", sym.Name)
                ElseIf PC = sym.Address Then
                    swt.WriteLine("{0}", sym)
                Else
                    swt.WriteLine(">{0}", sym.Name)
                End If
                cur = sym
            End If
            op = Disassemble(PC)
            swt.Write("{0}: ", GetRegs)
            If op Is Nothing Then
                swt.WriteLine(Enc(ReadUInt16(PC)))
                Abort("undefined instruction")
            Else
                swt.WriteLine(op.Mnemonic)
                RunStep()
            End If
        End While
    End Sub

    Public Sub RunStep()
        Select Case Me(PC + 1) >> 4
            'Case 3 : Return ReadSrcDst("bit")
            'Case 4 : Return ReadSrcDst("bic")
            'Case 5 : Return ReadSrcDst("bis")
            'Case &O13 : Return ReadSrcDst("bitb")
            'Case &O14 : Return ReadSrcDst("bicb")
            'Case &O15 : Return ReadSrcDst("bisb")
            Case 0
                Exec0()
                Return
            Case 1 ' mov: MOVe
                Dim oprs = GetSrcDst(2)
                Dim src = oprs(0).GetValue(Me)
                oprs(1).SetValue(Me, src)
                SetFlags(src = 0, ConvShort(src) < 0, C, False)
                Return
            Case 2 ' cmp: CoMPare
                Dim oprs = GetSrcDst(2)
                Dim src = oprs(0).GetValue(Me)
                Dim dst = oprs(1).GetValue(Me)
                Dim val = CInt(ConvShort(src)) - CInt(ConvShort(dst))
                SetFlags(val = 0, val < 0, src < dst, val < -&H8000)
                Return
            Case 6 ' add: ADD
                Dim oprs = GetSrcDst(2)
                Dim src = oprs(0).GetValue(Me)
                Dim dst = oprs(1).GetValue(Me)
                Dim val = CInt(ConvShort(src)) + CInt(ConvShort(dst))
                oprs(1).SetValue(Me, CUShort(val And &HFFFF))
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
                Dim src = oprs(0).GetByte(Me)
                oprs(1).SetByte(Me, src)
                SetFlags(src = 0, ConvSByte(src) < 0, C, False)
                Return
            Case &O12 ' cmpb: CoMPare Byte
                Dim oprs = GetSrcDst(1)
                Dim src = oprs(0).GetByte(Me)
                Dim dst = oprs(1).GetByte(Me)
                Dim val = CInt(ConvSByte(src)) - CInt(ConvSByte(dst))
                SetFlags(val = 0, val < 0, src < dst, val < -&H80)
                Return
            Case &O16 ' sub: SUBtract
                Dim oprs = GetSrcDst(2)
                Dim src = oprs(0).GetValue(Me)
                Dim dst = oprs(1).GetValue(Me)
                Dim val = CInt(ConvShort(dst)) - CInt(ConvShort(src))
                oprs(1).SetValue(Me, CUShort(val And &HFFFF))
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
                    'Case 3 : Return ReadDst("swab", bd, pos)
                    Case 1 ' jmp: JuMP
                        PC = GetDst(2).GetAddress(Me)
                        Return
                    Case 2 ' 00 02 xx
                        Select Case (v >> 3) And 7
                            'Case 3 : Return New OpCode("spl " + (v & 7), 2)
                            Case 0 ' rts: ReTurn from Subroutine
                                Dim r = v And 7
                                PC = Regs(r)
                                Regs(r) = ReadUInt16(GetInc(6, 2))
                                Return
                            Case 4 - 7 ' cl*/se*/ccc/scc: CLear/SEt (Condition Codes)
                                Dim f = (v And 16) <> 0
                                If (v And 8) <> 0 Then Me.N = f
                                If (v And 4) <> 0 Then Me.Z = f
                                If (v And 2) <> 9 Then Me.V = f
                                If (v And 1) <> 0 Then Me.C = f
                                PC += 2US
                                Return
                        End Select
                End Select
            Case 4 ' jsr: Jump to SubRoutine
                Dim r = (v >> 6) And 7
                Dim dst = GetDst(2).GetAddress(Me)
                Write(GetDec(6, 2), Regs(r))
                Regs(r) = PC
                PC = dst
                Return
            Case 5
                Select Case v2
                    'Case 1 : Return ReadDst("com", bd, pos)
                    'Case 5 : Return ReadDst("adc", bd, pos)
                    'Case 6 : Return ReadDst("sbc", bd, pos)
                    Case 0 ' clr: CLeaR
                        GetDst(2).SetValue(Me, 0)
                        SetFlags(True, False, False, False)
                        Return
                    Case 2 ' inc: INCrement
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue(Me))) + 1
                        dst.SetValue(Me, CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C, val >= &H8000)
                        Return
                    Case 3 ' dec: DECrement
                        Dim dst = GetDst(2)
                        Dim val = CInt(ConvShort(dst.GetValue(Me))) - 1
                        dst.SetValue(Me, CUShort(val And &HFFFF))
                        SetFlags(val = 0, val < 0, C, val < -&H8000)
                        Return
                    Case 4 ' neg: NEGate
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue(Me)
                        Dim val1 = -ConvShort(val0)
                        Dim val2 = CUShort(val1 And &HFFFF)
                        dst.SetValue(Me, val2)
                        SetFlags(val1 = 0, val1 < 0, val1 <> 0, val1 = &H8000)
                        Return
                    Case 7 ' tst: TeST
                        Dim dst = ConvShort(GetDst(2).GetValue(Me))
                        SetFlags(dst = 0, dst < 0, False, False)
                        Return
                End Select
            Case 6
                Select Case v2
                    'Case 4 : Return ReadNum("mark", bd, pos)
                    'Case 5 : Return ReadDst("mfpi", bd, pos)
                    'Case 6 : Return ReadDst("mtpi", bd, pos)
                    'Case 7 : Return ReadDst("sxt", bd, pos)
                    Case 0 ' ror: ROtate Right
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue(Me)
                        Dim val1 = (val0 >> 1) Or If(C, &H8000US, 0US)
                        dst.SetValue(Me, val1)
                        Dim lsb0 = (val0 And 1) <> 0
                        Dim msb1 = C
                        SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                        Return
                    Case 1 ' rol: ROtate Left
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue(Me)
                        Dim val1 = CUShort((CUInt(val0) << 1) And &HFFFF) Or If(C, 1US, 0US)
                        dst.SetValue(Me, val1)
                        Dim msb0 = (val0 And &H8000) <> 0
                        Dim msb1 = (val1 And &H8000) <> 0
                        SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                        Return
                    Case 2 ' asr: Arithmetic Shift Right
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue(Me)
                        Dim val1 = ConvShort(val0) >> 1
                        dst.SetValue(Me, CUShort(val1 And &HFFFF))
                        Dim lsb0 = (val0 And 1) <> 0
                        Dim msb1 = val1 < 0
                        SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                        Return
                    Case 3 ' asl: Arithmetic Shift Left
                        Dim dst = GetDst(2)
                        Dim val0 = dst.GetValue(Me)
                        Dim val1 = CUShort((CUInt(val0) << 1) And &HFFFF)
                        dst.SetValue(Me, val1)
                        Dim msb0 = (val0 And &H8000) <> 0
                        Dim msb1 = val1 < 0
                        SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                        Return
                End Select
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec7()
        Dim v = ReadUInt16(PC)
        Select Case (v >> 9) And 7
            'Case 0 : Return ReadSrcReg("mul", bd, pos)
            'Case 2 : Return ReadSrcReg("ash", bd, pos)
            'Case 3 : Return ReadSrcReg("ashc", bd, pos)
            'Case 4 : Return ReadRegDst("xor", bd, pos)
            'Case 5
            '    Select Case (v >> 3) And &O77
            '        Case 0 : Return ReadReg("fadd", bd, pos)
            '        Case 1 : Return ReadReg("fsub", bd, pos)
            '        Case 2 : Return ReadReg("fmul", bd, pos)
            '        Case 3 : Return ReadReg("fdiv", bd, pos)
            '    End Select
            Case 1 ' div: DIVision
                Dim src = ConvShort(GetDst(2).GetValue(Me))
                Dim r = (v >> 6) And 7
                Dim dst = GetReg32(r)
                Dim r1 = dst \ src
                Dim r2 = dst Mod src
                Regs(r) = CUShort(r1 And &HFFFF)
                Regs((r + 1) And 7) = CUShort(r2 And &HFFFF)
                SetFlags(r1 = 0, r1 < 0, r2 <> 0, r1 < -&H8000 OrElse r1 >= &H8000)
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
            'Case &O51 : Return ReadDst("comb", bd, pos)
            'Case &O55 : Return ReadDst("adcb", bd, pos)
            'Case &O56 : Return ReadDst("sbcb", bd, pos)
            'Case &O64 : Return ReadDst("mfpd", bd, pos)
            'Case &O65 : Return ReadDst("mtpd", bd, pos)
            Case &O50 ' clrb: CLeaR Byte
                GetDst(1).SetByte(Me, 0)
                SetFlags(True, False, False, False)
                Return
            Case &O52 ' incb: INCrement Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte(Me))) + 1
                dst.SetByte(Me, CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C, val >= &H80)
                Return
            Case &O53 ' decb: DECrement Byte
                Dim dst = GetDst(1)
                Dim val = CInt(ConvSByte(dst.GetByte(Me))) - 1
                dst.SetByte(Me, CByte(val And &HFF))
                SetFlags(val = 0, val < 0, C, val < -&H80)
                Return
            Case &O54 ' negb: NEGate Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte(Me)
                Dim val1 = -ConvSByte(val0)
                Dim val2 = CByte(val1 And &HFF)
                dst.SetByte(Me, val2)
                SetFlags(val1 = 0, val1 < 0, val1 <> 0, val1 = &H80)
                Return
            Case &O57 ' tstb: TeST Byte
                Dim dst = ConvSByte(GetDst(1).GetByte(Me))
                SetFlags(dst = 0, dst < 0, False, False)
                Return
            Case &O60 ' rorb: ROtate Right Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte(Me)
                Dim val1 = val0 >> 1
                If C Then val1 = CByte(val1 + &H80)
                dst.SetByte(Me, val1)
                Dim lsb0 = (val0 And 1) <> 0
                Dim msb1 = C
                SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                Return
            Case &O61 ' rolb: ROtate Left Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte(Me)
                Dim val1 = CByte(((CUInt(val0) << 1) + If(C, 1, 0)) And &HFF)
                dst.SetByte(Me, val1)
                Dim msb0 = (val0 And &H80) <> 0
                Dim msb1 = (val1 And &H80) <> 0
                SetFlags(val1 = 0, msb1, msb0, msb1 <> msb0)
                Return
            Case &O62 ' asrb: Arithmetic Shift Right Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte(Me)
                Dim val1 = ConvSByte(val0) >> 1
                dst.SetByte(Me, CByte(val1 And &HFF))
                Dim lsb0 = (val0 And 1) <> 0
                Dim msb1 = val1 < 0
                SetFlags(val1 = 0, msb1, lsb0, msb1 <> lsb0)
                Return
            Case &O63 ' aslb: Arithmetic Shift Left Byte
                Dim dst = GetDst(1)
                Dim val0 = dst.GetByte(Me)
                Dim val1 = CByte((CUInt(val0) << 1) And &HFF)
                dst.SetByte(Me, val1)
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

    Private Function GetSrcDst(size As UShort) As Operand()
        Dim v = ReadUInt16(PC)
        Dim src = New Operand((v >> 9) And 7, (v >> 6) And 7, Me, PC + 2, size)
        Return New Operand() {src, GetDst(size, src.Length + 2US)}
    End Function

    Private Function GetDst(size As UShort, Optional len As UShort = 2) As Operand
        Dim v = ReadUInt16(PC)
        Dim dst = New Operand((v >> 3) And 7, v And 7, Me, PC + len, size)
        PC += len + dst.Length
        Return dst
    End Function

    Public Sub Abort(msg$)
        swt.WriteLine(msg)
        'sw.WriteLine(GetRegs)
        HasExited = True
    End Sub

    Public Function Disassemble(pos%) As OpCode
        Return Disassembler.Disassemble(Me, pos)
    End Function

    Public Function GetInc(r%, size%) As UShort
        Dim ret = Regs(r)
        Regs(r) = CUShort((Regs(r) + size) And &HFFFF)
        Return ret
    End Function

    Public Function GetDec(r%, size%) As UShort
        Regs(r) = CUShort((Regs(r) - size) And &HFFFF)
        Return Regs(r)
    End Function

    Public Function GetRegs$()
        Return String.Format(
            "{0} r0={1} r1={2} r2={3} r3={4} r4={5} r5={6} sp={7}{{{8} {9} {10} {11}}} pc={12}",
            GetFlags,
            Enc0(Regs(0)), Enc0(Regs(1)), Enc0(Regs(2)), Enc0(Regs(3)), Enc0(Regs(4)), Enc0(Regs(5)),
            Enc0(Regs(6)), Enc0(ReadUInt16(Regs(6))), Enc0(ReadUInt16(Regs(6) + 2)),
            Enc0(ReadUInt16(Regs(6) + 4)), Enc0(ReadUInt16(Regs(6) + 6)), Enc0(Regs(7)))
    End Function

    Public Function GetReg32%(r%)
        Return (CInt(Regs(r)) << 16) Or CInt(Regs((r + 1) And 7))
    End Function

    Public Sub SetReg32(r%, v%)
        Regs(r) = CUShort((v >> 16) And &HFFFF)
        Regs((r + 1) And 7) = CUShort(v And &HFFFF)
    End Sub

    Public Function GetFlags$()
        Dim sb = New StringBuilder
        sb.Append(If(Z, "Z", "-"))
        sb.Append(If(N, "N", "-"))
        sb.Append(If(C, "C", "-"))
        sb.Append(If(V, "V", "-"))
        Return sb.ToString
    End Function

    Public Sub SetFlags(z As Boolean, n As Boolean, c As Boolean, v As Boolean)
        Me.Z = z
        Me.N = n
        Me.C = c
        Me.V = v
    End Sub

    Public Overrides Function EncAddr(addr As UShort) As String
        Return aout.EncAddr(addr) + "{" + Enc0(ReadUInt16(addr)) + "}"
    End Function

    Public Overrides Function GetReg$(r%)
        Return "{" + Enc0(Regs(r)) + "}"
    End Function

    Public Overrides Function GetValue$(r%, size%, d1%, d2%)
        Dim ad = CUShort((Regs(r) + d1) And &HFFFF)
        Dim p = If(size = 2, Enc0(ReadUInt16(ad)), Enc0(Me(ad)))
        Regs(r) = CUShort((Regs(r) + d2) And &HFFFF)
        Return "{" + Enc0(ad) + ":" + p + "}"
    End Function

    Public Overrides Function GetPtr$(r%, size%, d1%, d2%)
        Dim ad = CUShort((Regs(r) + d1) And &HFFFF)
        Dim p = ReadUInt16(ad)
        Regs(r) = CUShort((Regs(r) + d2) And &HFFFF)
        Dim pp = If(size = 2, Enc0(ReadUInt16(p)), Enc0(Me(p)))
        Return "{" + Enc0(ad) + ":" + Enc0(p) + ":" + pp + "}"
    End Function

    Public Sub SaveRegs()
        Array.Copy(Regs, bakRegs, Regs.Length)
    End Sub

    Public Sub LoadRegs()
        Array.Copy(bakRegs, Regs, Regs.Length)
    End Sub
End Class
