module Compressor

type ICompressor =
    abstract member Compress: path: string -> resultPath: string -> unit
    abstract member Decompress: path: string -> resultPath: string -> unit