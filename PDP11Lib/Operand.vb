Public Module ModOperand
    Public ReadOnly Operands(63) As Operand

    Sub New()
        For i = 0 To 63
            Operands(i) = New Operand(i)
        Next
    End Sub
End Module

Public Class Operand
    Private type%, reg%, offset%
    Public Property Length As UShort

    Public Sub New(v%)
        type = (v >> 3) And 7
        reg = v And 7
        If type >= 6 OrElse ((type = 2 OrElse type = 3) AndAlso reg = 7) Then Length = 2
    End Sub

    Private Sub New(src As Operand, offset%)
        type = src.type
        reg = src.reg
        Length = src.Length
        Me.offset = offset
    End Sub

    Public Function Check(dst As Operand) As Operand
        If type = 0 AndAlso reg = dst.reg Then
            If dst.reg = 2 OrElse dst.reg = 3 Then
                Return New Operand(Me, 1)
            ElseIf dst.reg = 4 OrElse dst.reg = 5 Then
                Return New Operand(Me, -1)
            End If
        End If
        Return Me
    End Function

    Public ReadOnly Property IsValid As Boolean
        Get
            Return Not (reg = 7 AndAlso (type = 4 OrElse type = 5))
        End Get
    End Property

    Public Shadows Function ToString$(bd As BinData, pc%, size%)
        Dim dist = 0
        If type >= 6 Then dist = bd.ReadInt16(pc) : pc += 2US
        Dim r = RegNames(reg)
        If reg = 7 Then
            Select Case type
                Case 0 : Return r + bd.GetReg(reg, CUShort(pc + offset + offset))
                Case 1 : Return "(" + r + ")" + bd.GetValue(reg, size, 0, 0)
                Case 2 : Return "$" + bd.Enc(bd.ReadUInt16(pc))
                Case 3 : Return "*$" + bd.EncAddr(bd.ReadUInt16(pc))
                Case 6 : Return bd.EncAddr(CUShort((pc + dist) And &HFFFF))
                Case 7 : Return "*" + bd.EncAddr(CUShort((pc + dist) And &HFFFF))
            End Select
        Else
            Select Case type
                Case 0 : Return r + bd.GetReg(reg, CUShort(pc + offset + offset))
                Case 1 : Return "(" + r + ")" + bd.GetValue(reg, size, 0, 0)
                Case 2 : Return "(" + r + ")+" + bd.GetValue(reg, size, 0, size)
                Case 3 : Return "*(" + r + ")+" + bd.GetPtr(reg, size, 0, 2)
                Case 4 : Return "-(" + r + ")" + bd.GetValue(reg, size, -size, -size)
                Case 5 : Return "*-(" + r + ")" + bd.GetPtr(reg, size, -2, -2)
                Case 6 : Return bd.GetRelative(reg, dist, pc - 2US) + bd.GetValue(reg, size, dist, 0)
                Case 7 : Return "*" + bd.GetRelative(reg, dist, pc - 2US) + bd.GetPtr(reg, size, dist, 0)
            End Select
        End If
        Throw New Exception("invalid operand")
    End Function

    Public Function GetAddress(vm As VM) As UShort
        Dim dist = If(type < 6, 0, vm.ReadInt16(vm.GetInc(7, 2)))
        Select Case type
            Case 1 : Return vm.Regs(reg)
            Case 2 : Return vm.GetInc(reg, 2)
            Case 3 : Return vm.ReadUInt16(vm.GetInc(reg, 2))
            Case 4 : Return vm.GetDec(reg, 2)
            Case 5 : Return vm.ReadUInt16(vm.GetDec(reg, 2))
            Case 6 : Return CUShort((vm.Regs(reg) + dist) And &HFFFF)
            Case 7 : Return vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF))
        End Select
        Throw New Exception("invalid operand")
    End Function

    Public Overridable Function GetValue(vm As VM) As UShort
        Dim dist = If(type < 6, 0, vm.ReadInt16(vm.GetInc(7, 2)))
        Select Case type
            Case 0 : Return CUShort(vm.Regs(reg) + offset + offset)
            Case 1 : Return vm.ReadUInt16(vm.Regs(reg))
            Case 2 : Return vm.ReadUInt16(vm.GetInc(reg, 2))
            Case 3 : Return vm.ReadUInt16(vm.ReadUInt16(vm.GetInc(reg, 2)))
            Case 4 : Return vm.ReadUInt16(vm.GetDec(reg, 2))
            Case 5 : Return vm.ReadUInt16(vm.ReadUInt16(vm.GetDec(reg, 2)))
            Case 6 : Return vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF))
            Case 7 : Return vm.ReadUInt16(vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF)))
        End Select
        Throw New Exception("invalid operand")
    End Function

    Public Function PeekValue(vm As VM) As UShort
        Dim dist = If(type < 6, 0, If(reg = 7, 2, 0) + vm.ReadInt16(vm.PC))
        Select Case type
            Case 0 : Return CUShort(vm.Regs(reg) + offset + offset)
            Case 1, 2 : Return vm.ReadUInt16(vm.Regs(reg))
            Case 3 : Return vm.ReadUInt16(vm.ReadUInt16(vm.Regs(reg)))
            Case 4 : Return vm.ReadUInt16(vm.Regs(reg) - 2)
            Case 5 : Return vm.ReadUInt16(vm.ReadUInt16(vm.Regs(reg) - 2))
            Case 6 : Return vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF))
            Case 7 : Return vm.ReadUInt16(vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF)))
        End Select
        Throw New Exception("invalid operand")
    End Function

    Public Sub SetValue(vm As VM, v As UShort)
        Dim dist = If(type < 6, 0, vm.ReadInt16(vm.GetInc(7, 2)))
        Select Case type
            Case 0 : vm.Regs(reg) = v : Return
            Case 1 : vm.Write(vm.Regs(reg), v) : Return
            Case 2 : vm.Write(vm.GetInc(reg, 2), v) : Return
            Case 3 : vm.Write(vm.ReadUInt16(vm.GetInc(reg, 2)), v) : Return
            Case 4 : vm.Write(vm.GetDec(reg, 2), v) : Return
            Case 5 : vm.Write(vm.ReadUInt16(vm.GetDec(reg, 2)), v) : Return
            Case 6 : vm.Write(CUShort((vm.Regs(reg) + dist) And &HFFFF), v) : Return
            Case 7 : vm.Write(vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF)), v) : Return
        End Select
        Throw New Exception("invalid operand")
    End Sub

    Public Overridable Function GetByte(vm As VM) As Byte
        Dim dist = If(type < 6, 0, vm.ReadInt16(vm.GetInc(7, 2)))
        Dim size = If(reg < 6, 1, 2)
        Select Case type
            Case 0 : Return CByte((vm.Regs(reg) + offset) And &HFF)
            Case 1 : Return vm(vm.Regs(reg))
            Case 2 : Return vm(vm.GetInc(reg, size))
            Case 3 : Return vm(vm.ReadUInt16(vm.GetInc(reg, 2)))
            Case 4 : Return vm(vm.GetDec(reg, size))
            Case 5 : Return vm(vm.ReadUInt16(vm.GetDec(reg, 2)))
            Case 6 : Return vm(CUShort((vm.Regs(reg) + dist) And &HFFFF))
            Case 7 : Return vm(vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF)))
        End Select
        Throw New Exception("invalid operand")
    End Function

    Public Function PeekByte(vm As VM) As Byte
        Dim dist = If(type < 6, 0, If(reg = 7, 2, 0) + vm.ReadInt16(vm.PC))
        Dim size = If(reg < 6, 1, 2)
        Select Case type
            Case 0 : Return CByte((vm.Regs(reg) + offset) And &HFF)
            Case 1, 2 : Return vm(vm.Regs(reg))
            Case 3 : Return vm(vm.ReadUInt16(vm.Regs(reg)))
            Case 4 : Return vm(vm.Regs(reg) - size)
            Case 5 : Return vm(vm.ReadUInt16(vm.Regs(reg) - 2))
            Case 6 : Return vm(CUShort((vm.Regs(reg) + dist) And &HFFFF))
            Case 7 : Return vm(vm.ReadUInt16(CUShort((vm.Regs(reg) + dist) And &HFFFF)))
        End Select
        Throw New Exception("invalid operand")
    End Function

    Public Sub SetByte(vm As VM, b As Byte)
        Dim dist = If(type < 6, 0, vm.ReadInt16(vm.GetInc(7, 2)))
        Dim size = If(reg < 6, 1, 2)
        Select Case type
            Case 0 : vm.Regs(reg) = If(b < &H80, b, CUShort((b - &H100) And &HFFFF)) : Return
            Case 1 : vm(vm.Regs(reg)) = b : Return
            Case 2 : vm(vm.GetInc(reg, size)) = b : Return
            Case 3 : vm(vm.ReadUInt16(vm.GetInc(reg, 2))) = b : Return
            Case 4 : vm(vm.GetDec(reg, size)) = b : Return
            Case 5 : vm(vm.ReadUInt16(vm.GetDec(reg, 2))) = b : Return
            Case 6 : vm(CUShort((vm.Regs(reg) + dist) And &HFFFF)) = b : Return
            Case 7 : vm(vm.ReadUInt16((CUShort(vm.Regs(reg) + dist) And &HFFFF))) = b : Return
        End Select
        Throw New Exception("invalid operand")
    End Sub
End Class
