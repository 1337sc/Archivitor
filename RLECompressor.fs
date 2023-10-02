module RLECompressor

open Compressor
open System.IO
open System
open FileWriter
open System.Linq
open System.Threading.Tasks

type RLECompressor() = 
    interface ICompressor with
        member this.Compress path resultPath = 
            async {
                let! content = File.ReadAllBytesAsync(path) |> Async.AwaitTask
            
                let mutable prevC = content[0] // previous symbol (char) container
                let mutable cnt = 0 |> sbyte // count of the current symbol

                let mutable singleCharsBuffer = []

                let mutable i = 0
                let mutable percents = 0
                let mutable prevPercents = -1
                use outputFile = File.Create(resultPath)
                for c in content do
                    percents <- i * 100 / (content.Length)
                    if (not (prevPercents = percents)) then
                        Console.WriteLine($"Compression progress: {percents}%%")
                        prevPercents <- percents

                    if c = prevC then
                        if Math.Abs(cnt) = 127y then
                            outputFile.WriteByte(cnt |> byte)
                            outputFile.WriteByte(c |> byte)
                            cnt <- 1y
                        else
                            cnt <- cnt + 1y
                    elif cnt = 1y then
                        if singleCharsBuffer.Length = 127 then
                            outputFile.WriteByte(-singleCharsBuffer.Length |> byte)
                            outputFile.Write(singleCharsBuffer.ToArray<byte>()) |> ignore
                            singleCharsBuffer <- []
                        singleCharsBuffer <- singleCharsBuffer @ [prevC]
                        prevC <- c
                    else
                        if singleCharsBuffer.Any() then
                            outputFile.WriteByte(-singleCharsBuffer.Length |> byte)
                            outputFile.Write(singleCharsBuffer.ToArray()) |> ignore
                            singleCharsBuffer <- []
                        outputFile.WriteByte(cnt |> byte)
                        outputFile.WriteByte(prevC |> byte)
                        prevC <- c
                        cnt <- 1y
                    i <- i + 1

                if singleCharsBuffer.Any() then
                    outputFile.WriteByte(-singleCharsBuffer.Length |> byte)
                    outputFile.Write(singleCharsBuffer.ToArray()) |> ignore
                    singleCharsBuffer <- []
                    
                outputFile.WriteByte(cnt |> byte)
                outputFile.WriteByte(prevC |> byte)
            }

        member this.Decompress path resultPath = 
            async {
                let! content = File.ReadAllBytesAsync(path) |> Async.AwaitTask

                use outputFile = File.Create(resultPath)
                let mutable i = 0
                let mutable percents = 0
                let mutable prevPercents = -1
                for _ in 0..(content.Length - 1) do
                    percents <- i * 100 / (content.Length)
                    if (not (prevPercents = percents)) then
                        Console.WriteLine($"Decompression progress: {percents}%%")
                        prevPercents <- percents

                    if i >= (content.Length - 1) then () // the last symbol has already been processed
                    else
                        let convertedContent = content[i] |> sbyte
                        if convertedContent > 0y then // repeat a symbol multiple times
                            for _ in 1..(content[i] |> int) do
                                outputFile.WriteByte(content[i + 1])
                            i <- i + 2
                        else // non-repeated symbols
                            for j in 1..-(convertedContent |> int) do
                                outputFile.WriteByte(content[i + j])
                            i <- i - (convertedContent |> int) + 1
            }