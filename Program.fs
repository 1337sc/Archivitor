open HuffmanCompressor
open LZWCompressor
open RLECompressor
open Compressor

let compressor = new RLECompressor()

((compressor :> ICompressor).Compress 
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\input\Autumncame.mp3" 
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\output\Autumncame.mp3.rle")
|> Async.RunSynchronously

(compressor :> ICompressor).Decompress
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\output\Autumncame.mp3.rle"
    @"C:\MyCodeSSD\f#\Archivitor\Archivitor\bin\Debug\net6.0\input\Autumncame.mp3_dec.mp3" 
|> Async.RunSynchronously
