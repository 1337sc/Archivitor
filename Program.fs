open HuffmanCompressor
open Compressor

let compressor = new HuffmanCompressor()

(compressor :> ICompressor).Compress 
    @"input_file.png" 
    @"compressed_file.huff"

(compressor :> ICompressor).Decompress
    @"compressed_file.huff"
    @"input_file.png" 
