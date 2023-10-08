open HuffmanCompressor
open LZWCompressor
open RLECompressor
open Compressor

let compressor = new HuffmanCompressor(isAdaptive=false)

((compressor :> ICompressor).Compress 
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\input\vid.mp4" 
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\output\vid.mp4.huff")
|> Async.RunSynchronously

(compressor :> ICompressor).Decompress
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\output\vid.mp4.huff"
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\input\vid_dec.mp4" 
|> Async.RunSynchronously
