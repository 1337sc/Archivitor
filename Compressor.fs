module Compressor

type ICompressor =
    abstract member Compress: path: string -> resultPath: string -> Async<unit>
    abstract member Decompress: path: string -> resultPath: string -> Async<unit>