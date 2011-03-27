﻿Imports System.IO

Public Class VM
    Inherits BinData

    Public Regs(7) As UShort

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

    Private sw As StringWriter
    Public ReadOnly Property Output$
        Get
            Return sw.ToString
        End Get
    End Property

    Public Sub New(aout As AOut)
        MyBase.New(&H10000)
        Array.Copy(aout.Data, aout.Offset, Data, 0, aout.Data.Length - aout.Offset)
        Me.UseOct = aout.UseOct
        Regs(6) = &HFFF0
    End Sub

    Public Sub Run()
        HasExited = False
        sw = New StringWriter
        While Not HasExited
            RunStep()
        End While
    End Sub

    Public Sub RunStep()
        Dim mne = Disassemble(PC).Mnemonic
        sw.WriteLine("{0}: {1}", GetRegs, mne)
        Select Case Data(PC + 1) >> 4
            'Case 2 : Return ReadSrcDst("cmp")
            'Case 3 : Return ReadSrcDst("bit")
            'Case 4 : Return ReadSrcDst("bic")
            'Case 5 : Return ReadSrcDst("bis")
            'Case 6 : Return ReadSrcDst("add")
            'Case &O10 : Return Read10()
            'Case &O11 : Return ReadSrcDst("movb")
            'Case &O12 : Return ReadSrcDst("cmpb")
            'Case &O13 : Return ReadSrcDst("bitb")
            'Case &O14 : Return ReadSrcDst("bicb")
            'Case &O15 : Return ReadSrcDst("bisb")
            'Case &O16 : Return ReadSrcDst("sub")
            Case 0
                Exec0()
                Return
            Case 1 ' mov: MOVe
                Dim oprs = GetSrcDst()
                oprs(1).SetValue(Me, oprs(0).GetValue(Me))
                Return
            Case &O17
                Exec17()
                Return
        End Select
        Abort("not implemented")
    End Sub

    Private Sub Exec0()
        'Select Case Data(PC + 1)
        '    Case 1 : Return ReadOffset("br", bd, pos)
        '    Case 2 : Return ReadOffset("bne", bd, pos)
        '    Case 3 : Return ReadOffset("beq", bd, pos)
        '    Case 4 : Return ReadOffset("bge", bd, pos)
        '    Case 5 : Return ReadOffset("blt", bd, pos)
        '    Case 6 : Return ReadOffset("bgt", bd, pos)
        '    Case 7 : Return ReadOffset("ble", bd, pos)
        'End Select
        Dim len = 2US
        Dim v = ReadUInt16(PC)
        If v = &HA0 Then PC += 2US : Return ' nop
        Dim v1 = (v >> 9) And 7, v2 = (v >> 6) And 7
        Select Case v1
            'Case 0
            '    Select Case v2
            '        Case 0
            '            Select Case v And &O77
            '                Case 0 : Return New OpCode("halt", 2)
            '                Case 1 : Return New OpCode("wait", 2)
            '                Case 2 : Return New OpCode("rti", 2)
            '                Case 3 : Return New OpCode("bpt", 2)
            '                Case 4 : Return New OpCode("iot", 2)
            '                Case 5 : Return New OpCode("reset", 2)
            '                Case 6 : Return New OpCode("rtt", 2)
            '            End Select
            '        Case 1 : Return ReadSrcOrDst("jmp", bd, pos)
            '        Case 2
            '            Select Case (v >> 3) And 7
            '                Case 0 : Return ReadReg("rts", bd, pos)
            '                Case 3 : Return New OpCode("spl " + (v & 7), 2)
            '            End Select
            '        Case 3 : Return ReadSrcOrDst("swab", bd, pos)
            '    End Select
            'Case 6
            '    Select Case v2
            '        Case 0 : Return ReadSrcOrDst("ror", bd, pos)
            '        Case 1 : Return ReadSrcOrDst("rol", bd, pos)
            '        Case 2 : Return ReadSrcOrDst("asr", bd, pos)
            '        Case 3 : Return ReadSrcOrDst("asl", bd, pos)
            '        Case 4 : Return ReadNum("mark", bd, pos)
            '        Case 5 : Return ReadSrcOrDst("mfpi", bd, pos)
            '        Case 6 : Return ReadSrcOrDst("mtpi", bd, pos)
            '        Case 7 : Return ReadSrcOrDst("sxt", bd, pos)
            '    End Select
            Case 4 ' jsr: Jump to SubRoutine
                Dim r = (v >> 6) And 7
                Dim dst = New Operand((v >> 3) And 7, v And 7, Me, PC + len)
                len += dst.Length
                PC += len
                Dim temp = dst.GetAddress(Me)
                Write(GetDec(6), Regs(r))
                Regs(r) = PC
                PC = temp
                Return
            Case 5
                Select Case v2
                    'Case 0 : Return ReadSrcOrDst("clr", bd, pos)
                    'Case 1 : Return ReadSrcOrDst("com", bd, pos)
                    'Case 2 : Return ReadSrcOrDst("inc", bd, pos)
                    'Case 3 : Return ReadSrcOrDst("dec", bd, pos)
                    'Case 4 : Return ReadSrcOrDst("neg", bd, pos)
                    'Case 5 : Return ReadSrcOrDst("adc", bd, pos)
                    'Case 6 : Return ReadSrcOrDst("sbc", bd, pos)
                    Case 7 ' tst: TeST
                        Dim dst = New Operand((v >> 3) And 7, v And 7, Me, PC + len)
                        len += dst.Length
                        PC += len
                        Dim vv = dst.GetValue(Me)
                        SetFlags(vv = 0, vv >= &H8000, False, False)
                        Return
                End Select
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

    Private Function GetSrcDst() As Operand()
        Dim len = 2US
        Dim v = ReadUInt16(PC)
        Dim opr1 = New Operand((v >> 9) And 7, (v >> 6) And 7, Me, PC + len)
        len += opr1.Length
        Dim opr2 = New Operand((v >> 3) And 7, v And 7, Me, PC + len)
        len += opr2.Length
        PC += len
        Return New Operand() {opr1, opr2}
    End Function

    Public Sub Abort(msg$)
        sw.WriteLine(msg)
        'sw.WriteLine(GetRegs)
        HasExited = True
    End Sub

    Public Function Disassemble(pos%) As OpCode
        Return Disassembler.Disassemble(Me, pos)
    End Function

    Public Function GetInc(r%) As UShort
        Dim ret = Regs(r)
        Regs(r) += 2US
        Return ret
    End Function

    Public Function GetDec(r%) As UShort
        Regs(r) -= 2US
        Return Regs(r)
    End Function

    Public Function GetRegs$()
        Return String.Format(
            "r0={0}, r1={1}, r2={2}, r3={3}, r4={4}, r5={5}, sp={6}, pc={7}",
            Enc(Regs(0)), Enc(Regs(1)), Enc(Regs(2)), Enc(Regs(3)),
            Enc(Regs(4)), Enc(Regs(5)), Enc(Regs(6)), Enc(Regs(7)))
    End Function

    Public Sub SetFlags(z As Boolean, n As Boolean, c As Boolean, v As Boolean)
        Me.Z = z
        Me.N = n
        Me.C = c
        Me.V = v
    End Sub
End Class
