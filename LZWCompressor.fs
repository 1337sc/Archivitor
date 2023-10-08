module LZWCompressor

open System.IO
open System
open System.Linq
open System.Threading.Tasks
open Compressor
open System.Text

type LZWCompressor() =
    let writeIntoFile(output: Stream, bytes: byte[]) = 
        async {
            bytes 
            |> output.AsyncWrite
            |> Async.Start
        }
        
    interface ICompressor with
        member this.Compress(path: string) (resultPath: string) = 
            async { 
                let content = File.ReadAllBytes(path)
                let mutable dict = 
                    [for i in 0..2 <<< 8 - 1 do ([i |> byte], i |> uint16)]
                    |> Map.ofList
            
                let output = File.OpenWrite(resultPath)
                let mutable substring = []
                printfn "Started on %s" (DateTime.UtcNow.ToLongTimeString())
                for i in 0..content.Length - 1 do
                    //printfn "Round %i of %i" i (content.Length - 1)
                    substring <- substring @ [content[i]]
                    if not (dict.ContainsKey(substring)) then
                        //printfn "%A is not in dict" (Encoding.ASCII.GetChars(substring.ToArray()))
                    
                        writeIntoFile(output, 
                            dict[substring[..substring.Length - 2]] |> BitConverter.GetBytes)    
                        |> Async.Start
                        //printfn "Wrote %u into output" dict[substring[..substring.Length - 2]]
                    
                        let newValueNum = (dict.Values.Max()) + (1 |> uint16)
                        if newValueNum < (4096 |> uint16) then
                            dict <- dict.Add(substring, (dict.Values.Max()) + (1 |> uint16))
                            //printfn "Put %A into dict with code %u" (Encoding.ASCII.GetChars(substring.ToArray())) (dict.Values.Max())
                    
                        substring <- [substring[substring.Length - 1]]
                        //printfn "%A has left in dict" (Encoding.ASCII.GetChars(substring.ToArray()))
                    //else
                    //    printfn "%A is in dict (code %u)" (Encoding.ASCII.GetChars(substring.ToArray())) dict[substring]
            
                writeIntoFile(output, (dict.Last().Value |> BitConverter.GetBytes))
                |> Async.Start
                //printfn "Wrote %u into output" (dict.Last().Value)
                    
                output.Close()
                printfn "Completed on %s" (DateTime.UtcNow.ToLongTimeString())
            }

        member this.Decompress(path: string) (resultPath: string) = 
            raise (System.NotImplementedException()) 