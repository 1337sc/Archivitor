module FileWriter

open System.IO
open System
open System.Linq

type FileWriter() = 

    static member WriteBitsString(str: string, file: FileStream) =
        let bytesStrings = Array.init (str.Length / Constants.ByteLength + 1) (fun index ->
            let start = index * Constants.ByteLength
            if str.Length >= start + Constants.ByteLength then
                str.[start..start + Constants.ByteLength]
            else
                str.[start..])

        for substring in bytesStrings do
            if substring.Length = Constants.ByteLength then
                let curByte = Convert.ToByte(substring, 2)
                file.WriteByte(curByte)

        if bytesStrings.Last().Any() then
            bytesStrings.Last()
        else
            ""
