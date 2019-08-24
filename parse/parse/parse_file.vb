Class parse_file

    Public Property verbose As Boolean = False

    Sub read(ByRef partref As Dictionary(Of String, String),
            ByRef partdict As Dictionary(Of String, Dictionary(Of String, String)),
            ByRef all_nets As Dictionary(Of String, Boolean))

        Dim line(3) As String

        Dim idpart As Integer = FreeFile()
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

        If verbose Then
            For Each n In partdict
                Console.WriteLine(partref.Item(n.Key))
            Next
        End If

    End Sub
End Class
