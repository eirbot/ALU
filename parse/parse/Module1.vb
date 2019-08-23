Module Module1
    Dim verbose As Boolean = False


    Sub Main()
        Dim part As New List(Of String)
        Dim net As New List(Of String)

        'parse part file
        Dim idpart As Integer = FreeFile()
        Dim partref As New Dictionary(Of String, String)
        Dim line(3) As String

        FileOpen(idpart, "..\..\..\..\part.txt", OpenMode.Input, OpenAccess.Read)
        While Not EOF(idpart)
            line = LineInput(idpart).Split(",")
            If line(0) = "SV1" Then
                partref.Add("SV1", "SV")
            Else
                partref.Add(line(0), line(1))
            End If
        End While
        FileClose(idpart)

        'parse net file
        Dim idnet As Integer = FreeFile()
        Dim partdict As New Dictionary(Of String, Dictionary(Of String, String))
        Dim all_nets As New Dictionary(Of String, Boolean)
        Dim cur_net As String
        FileOpen(idnet, "..\..\..\..\connect.txt", OpenMode.Input, OpenAccess.Read)
        While Not EOF(idnet)
            ' part, pin, net
            line = LineInput(idnet).Split(",")

            'throwing supply symbols
            If line(0).StartsWith("P+") OrElse line(0).StartsWith("GND") Then
                Continue While
            End If

            'throwing bypass caps and holes
            If line(0).StartsWith("C") OrElse line(0).StartsWith("U$") Then
                Continue While
            End If

            'throwing led, resistor
            If line(0).StartsWith("R") OrElse line(0).StartsWith("LED") Then
                Continue While
            End If

            'thowing supply pin
            If line(0).EndsWith("P") Then
                Continue While
            End If

            cur_net = line(2).Replace("$", "")
            If partdict.ContainsKey(line(0)) Then
                If Not partdict.Item(line(0)).ContainsKey(line(1)) Then
                    partdict.Item(line(0)).Add(line(1), cur_net)
                Else
                    Console.Write("ERROR PART " & line(0) & " ON PIN " & line(1))
                End If
            Else
                partdict.Add(line(0), New Dictionary(Of String, String) From {{line(1), cur_net}})
            End If

            If Not all_nets.ContainsKey(cur_net) Then
                all_nets.Add(cur_net, False)
            End If
        End While
        FileClose(idnet)

        For Each n In partdict.Item("SV1")
            Console.WriteLine(n.Key & " " & n.Value)
        Next

        For Each n In partdict
            Console.WriteLine(partref.Item(n.Key))
        Next

        'Dim elem As List(Of String) = gen_netlist(partdict, partref)
        'For Each e In elem
        '    Console.WriteLine(e)
        'Next
        ' Dim stop as string
        Dim sv As Dictionary(Of String, String) = partdict.Item("SV1")
        Dim inputA As String
        Dim inputB As String
        Dim inputC As String
        Dim count As Integer = 0
        While compute_logic(all_nets, partdict, partref)
            count += 1
            Console.WriteLine("+" & count)
        End While

        Dim allnames As New List(Of String)
        For Each n In all_nets
            allnames.Add(n.Key)
        Next


        'Console.WriteLine(count & "CO : " & (all_nets.Item(sv.Item("45")) And &H1))
        'Console.WriteLine(count & "OUT : " & (all_nets.Item("N66") And &H1) & (all_nets.Item("N94") And &H1))
        Dim ast As String
        Dim bst As String
        Dim res As Integer
        Dim maxprop As Integer = 0
        For c As Integer = 0 To 1
            For b As Integer = 0 To 15
                For a As Integer = 0 To 15
                    ast = cast_bit(CStr(a))
                    bst = cast_bit(CStr(b))
                    For i = 0 To 4
                        all_nets.Item("A" & i) = Cbit(ast(i))
                        all_nets.Item("B" & i) = Cbit(bst(i))
                    Next
                    If c = 0 Then
                        all_nets.Item("C0") = False
                    Else
                        all_nets.Item("C0") = True
                    End If

                    count = 0
                    While compute_logic(all_nets, partdict, partref)
                        count += 1
                    End While
                    res = (all_nets.Item("N2") And &H1) + (all_nets.Item("N24") And &H2) + (all_nets.Item("N33") And &H4) + (all_nets.Item("N38") And &H8) +
                                      (all_nets.Item("C4") And &H10)
                    Console.WriteLine("C : " & c & " A : " & a & ":" & ast & vbTab & " B : " & b & ":" & bst & vbTab &
                                      " => " & count & " OUT = " & vbTab & a + b + c & "=" & vbTab & res & ":" & vbTab &
                                      (all_nets.Item("N2") And &H1) & (all_nets.Item("N24") And &H1) & (all_nets.Item("N33") And &H1) & (all_nets.Item("N38") And &H1) &
                                      (all_nets.Item("C4") And &H1))
                    If res <> a + b + c Then
                        Console.WriteLine("FAIL")
                    End If
                    If count > maxprop Then
                        maxprop = count
                    End If

                    For Each n In allnames
                        all_nets.Item(n) = False
                    Next
                    While compute_logic(all_nets, partdict, partref)
                    End While
                Next
            Next
        Next

        
        Console.WriteLine("max prop " & maxprop)

        Console.Write("stop ?")
        While Console.ReadLine() <> "O"
            count = 0
            inputA = ask_bit("A")
            Console.WriteLine(inputA)
            inputB = ask_bit("B")
            Console.WriteLine(inputB)
            For i = 0 To 4
                all_nets.Item("A" & i) = Cbit(inputA(i))
                all_nets.Item("B" & i) = Cbit(inputB(i))
            Next
            inputC = ask_bit("C")
            Console.WriteLine(inputB)
            all_nets.Item("C0") = Cbit(inputC(0))

            While compute_logic(all_nets, partdict, partref)
                count += 1
            End While


            Console.WriteLine(count & "CO : " & (all_nets.Item("C4") And &H1))
            Console.WriteLine(count & "OUT : " & (all_nets.Item("N2") And &H1) & (all_nets.Item("N24") And &H1) & (all_nets.Item("N33") And &H1) & (all_nets.Item("N38") And &H1) & (all_nets.Item("C4") And &H1))
            Console.Write(" stop ?")
        End While

        Console.ReadLine()
    End Sub

    Function compute_logic(ByRef all_net As Dictionary(Of String, Boolean), ByVal partdict As Dictionary(Of String, Dictionary(Of String, String)), ByRef partref As Dictionary(Of String, String)) As Boolean
        Dim pout As Boolean
        Dim nout As Boolean
        Dim has_chaged As Boolean = False
        Dim new_out As New Dictionary(Of String, Boolean)

        For Each p In partdict
            With p.Value
                Select Case partref.Item(p.Key)
                    Case "74ALS00N"
                        pout = all_net.Item(.Item("O"))
                        nout = Not (all_net.Item(.Item("I0")) And all_net.Item(.Item("I1")))
                        new_out.Add((.Item("O")), nout)
                    Case "74LS151N"
                        pout = all_net.Item(.Item("Y"))
                        Dim adr As Integer = (all_net.Item(.Item("C")) And &H4) Or (all_net.Item(.Item("B")) And &H2) Or (all_net.Item(.Item("B")) And &H1)
                        nout = all_net.Item(.Item("D" & adr)) And (Not all_net.Item(.Item("G")))
                        new_out.Add(.Item("Y"), nout)
                        'new_out.Add(.Item("W"))
                    Case "74LS86N"
                        pout = all_net.Item(.Item("O"))
                        nout = (all_net.Item(.Item("I0")) Xor all_net.Item(.Item("I1")))
                        new_out.Add((.Item("O")), nout)
                    Case Else
                        pout = False
                        nout = False
                End Select
            End With
            If nout <> pout Then
                If verbose Then
                    Console.WriteLine(p.Key)
                End If

                has_chaged = True
            End If
        Next

        If has_chaged = True Then
            For Each n In new_out
                all_net.Item(n.Key) = n.Value
            Next
        End If
        If verbose Then
            Console.WriteLine("")
        End If
        Return has_chaged
    End Function

    Function Cbit(ByVal c As Char) As Boolean
        If c = "1"c Then
            Return True
        Else
            Return False
        End If
    End Function

    Function ask_bit(ByVal val As String) As String
        Console.Write(val & " : ")
        Try
            Return (Strings.StrReverse(Convert.ToString(CInt(Console.ReadLine()), 2).PadLeft(5, "0")))
        Catch
            Return ""
        End Try
    End Function

    Function cast_bit(ByVal x As Integer) As String
        Try
            Return (Strings.StrReverse(Convert.ToString(CInt(x), 2).PadLeft(5, "0")))
        Catch
            Return ""
        End Try
    End Function

    Function gen_netlist(ByVal partdict As Dictionary(Of String, Dictionary(Of String, String)), ByVal partref As Dictionary(Of String, String)) As List(Of String)
        Dim elem As New List(Of String)
        For Each p In partdict
            With p.Value
                Select Case p.Key
                    Case "74ALS00N"
                        elem.Add("7400 : " & .Item("I0") & "," & .Item("I1") & "," & .Item("O"))
                    Case "74LS151N"
                        elem.Add("74151 : " & .Item("A") & "," & .Item("B") & "," & .Item("C") & "," & .Item("G") &
                           .Item("D0") & "," & .Item("D1") & "," & .Item("D2") & "," & .Item("D3") & "," & .Item("D4") &
                           "," & .Item("D5") & "," & .Item("D6") & "," & .Item("D7") & "," &
                           .Item("W") & "," & .Item("Y"))
                End Select
            End With
        Next

        Return elem
    End Function



    Sub write_nand(ByVal file As String)
        System.IO.File.AppendAllText(file, "entity 7400 is" & vbCrLf)
        System.IO.File.AppendAllText(file, "port (a,b : in bit; s: out bit);" & vbCrLf)
        System.IO.File.AppendAllText(file, "end entity" & vbCrLf)
        System.IO.File.AppendAllText(file, "architecture behav of 7400 is" & vbCrLf)
        System.IO.File.AppendAllText(file, "begin" & vbCrLf)
        System.IO.File.AppendAllText(file, "s <= NOT (a AND b) ;" & vbCrLf)
        System.IO.File.AppendAllText(file, "end architecture" & vbCrLf)
    End Sub

    Sub write_mux(ByVal file As String)
        System.IO.File.AppendAllText(file, "entity 74151 is" & vbCrLf)
        System.IO.File.AppendAllText(file, "port (a,b,c,g : in bit; d0,d1,d2,d3,d4,d5,d6,d7 : in bit; w,y: out bit);" & vbCrLf)
        System.IO.File.AppendAllText(file, "end entity" & vbCrLf)
        System.IO.File.AppendAllText(file, "architecture behav of 74151 is" & vbCrLf)
        System.IO.File.AppendAllText(file, "signal state : std_logic_vertor(2 downto 0)" & vbCrLf)
        System.IO.File.AppendAllText(file, "begin" & vbCrLf)
        System.IO.File.AppendAllText(file, "y <= g AND ((a AND b) ;" & vbCrLf)
        System.IO.File.AppendAllText(file, "end architecture" & vbCrLf)
    End Sub
End Module
