module HuffmanCompressor

open Compressor
open System.Collections.Generic
open System.IO
open Tree
open FileWriter
open System.Linq
open System
open System.Text
open System.Diagnostics

type HuffmanCompressor(isAdaptive: bool) = 

    // get Huffman tree for given frequencies
    let buildFrequencyTree (freqDict: Dictionary<Nullable<byte>, int>): Node<Nullable<byte>> = 
        let mutable freqTree = new List<Node<Nullable<byte>>>()
        for f in freqDict do
            freqTree.Add(new Node<Nullable<byte>>(f.Key, f.Value))

        while freqTree.Count > 1 do
            freqTree.Sort()
            let nodesToMerge = freqTree.Take(2).ToList()

            let nodeToAdd = new Node<Nullable<byte>>(Nullable(), 
                nodesToMerge.Sum(fun n -> n.Weight),
                (nodesToMerge.First() :> ICloneable).Clone() :?> Node<Nullable<byte>>,
                (nodesToMerge.Last() :> ICloneable).Clone() :?> Node<Nullable<byte>>)

            freqTree.Add(nodeToAdd)

            for n in 0..nodesToMerge.Count - 1 do
                freqTree.Remove(nodesToMerge[n]) |> ignore

        freqTree.FirstOrDefault()

    //recursively get paths to each node
    let rec getContents (curNode: Node<Nullable<byte>>, path: uint64, codesDict: SortedDictionary<uint64, Nullable<byte>>) = 
        if isNull curNode.LeftChild && isNull curNode.RightChild then 
            codesDict.Add(path, curNode.Content)
        else 
            getContents(curNode.LeftChild, path * 10UL + 1UL, codesDict)
            getContents(curNode.RightChild, path * 10UL, codesDict)

    let buildCodesTable (tree: Node<Nullable<byte>>) = 
        let table = new SortedDictionary<uint64, Nullable<byte>>()
        getContents(tree, 1UL, table) // 1 is for the most significant digit, should be removed when writing to the file
        table

    //project a tree to a string, where "0" marks a Node and 1 marks a leaf (a Node with Content != null)
    let rec getProjectedPath (curNode: Node<Nullable<byte>>, path: string, pathsDict: Dictionary<Nullable<byte>, string>) = 
        if isNull curNode.LeftChild && isNull curNode.RightChild then 
            pathsDict.Add(curNode.Content, path + "1")
        else 
            getProjectedPath(curNode.LeftChild, path + "0", pathsDict)
            getProjectedPath(curNode.RightChild, path + "0", pathsDict)

    let projectTree (tree: Node<Nullable<byte>>) = 
        let table = new Dictionary<Nullable<byte>, string>()
        getProjectedPath(tree, String.Empty, table)
        table

    //recover nodes from a projected view (see getProjectedPath)
    let rec setChildrenNodes (curNode: Node<Nullable<byte>>, tail: string): bool = 
        if tail.First().Equals('1') then
            if isNull curNode.LeftChild then
                curNode.LeftChild <- new Node<Nullable<byte>>(Convert.ToByte(tail[1..8], 2), 0)
                true
            elif isNull curNode.RightChild then
                    curNode.RightChild <- new Node<Nullable<byte>>(Convert.ToByte(tail[1..8], 2), 0)
                    true
            else false
        else
            if isNull curNode.LeftChild then
                curNode.LeftChild <- new Node<Nullable<byte>>(Nullable(), 0)

            if curNode.LeftChild.Content = Nullable() then
                if setChildrenNodes(curNode.LeftChild, tail[1..]) then
                    true
                else
                    if isNull curNode.RightChild then
                        curNode.RightChild <- new Node<Nullable<byte>>(Nullable(), 0)

                    if curNode.RightChild.Content = Nullable() then
                        setChildrenNodes(curNode.RightChild, tail[1..])
                    else
                        false
            else
                if isNull curNode.RightChild then
                    curNode.RightChild <- new Node<Nullable<byte>>(Nullable(), 0)

                if curNode.RightChild.Content = Nullable() then
                    setChildrenNodes(curNode.RightChild, tail[1..])
                else
                    false

    let getTreeFromModel (model: string, leavesCount: int, bitsSpentOnTree: ref<int>) = 
        let tree = new Node<Nullable<byte>>(Nullable(), 0)
        let mutable p = 0 // model seek position
        for i in 0..leavesCount - 1 do
            let builder = new StringBuilder()
            while not (model[p].Equals('1')) do 
                builder.Append(model[p]) |> ignore
                p <- p + 1
            let content = BitConverter.ToChar([|Convert.ToByte(model[p + 1..p + 8], 2); 0 |> byte|], 0)
            builder.Append(model[p]) |> ignore
            p <- p + 1
            builder.Append(model[p..p + 7]) |> ignore
            p <- p + 8
            setChildrenNodes (tree, builder.ToString()) |> ignore
        bitsSpentOnTree.Value <- p
        tree

    let rec digitCount number = if number < 10UL then 1UL else 1UL + digitCount (number / 10UL)

    interface ICompressor with
        member this.Compress path resultPath = 
            async {
                printfn "\t\t\tCompress"

                let timer = Stopwatch.StartNew()
                timer.Start()
                let content = File.ReadAllBytes(path)
                let frequencies = new Dictionary<Nullable<byte>, int>()
                for c in content do
                    if (frequencies.ContainsKey(c)) then
                        frequencies[c] <- frequencies[c] + 1
                    else
                        frequencies[c] <- 1

                let freqTree = buildFrequencyTree frequencies

                // invert keys and values to increase performance
                let codesTable = (buildCodesTable freqTree).ToDictionary(keySelector=(fun kvp -> kvp.Value), elementSelector=(fun kvp -> kvp.Key))
                let projectTreeTable = projectTree(freqTree)

                let resultBits = new StringBuilder()
                resultBits.Append((sprintf "%B" frequencies.Count).PadLeft(sizeof<int> * Constants.ByteLength, '0')) |> ignore
                
                use file = File.Create(resultPath)
                for c in projectTreeTable do
                    resultBits.Append(c.Value[1..]).Append((sprintf "%B" (c.Key.Value |> int32)).PadLeft(Constants.ByteLength, '0')) |> ignore

                for b in content do
                    let code = codesTable[b].ToString()[1..] //remove the first extra 1 added for int to be functional
                    resultBits.Append(code) |> ignore
                    if resultBits.Length >= Constants.ByteLength then
                        let cutoff = resultBits.Length - resultBits.Length % Constants.ByteLength
                        for bitsIndex in 0..Constants.ByteLength..cutoff - 1 do
                            file.WriteByte(Convert.ToByte(resultBits.ToString()[bitsIndex..(bitsIndex + Constants.ByteLength - 1)], 2))
                        resultBits.Remove(0, cutoff) |> ignore

                let trailingBitsCount = Constants.ByteLength - (resultBits.Length % Constants.ByteLength)
                resultBits.Append(String.Join(String.Empty, [|for i in 1..trailingBitsCount -> '0'|])) |> ignore // 8 bits in each byte
                
                file.WriteByte(Convert.ToByte(resultBits.ToString(), 2))
                file.WriteByte(trailingBitsCount |> byte)

                timer.Stop()
                Console.WriteLine($"Compressing done in {timer.ElapsedMilliseconds} ms")
                let compressionRate = Math.Round((content.LongLength |> double) / (file.Length |> double) - 1., 4)
                
                Console.WriteLine($"Compression rate: {compressionRate * 100.}%%")
            }

        member this.Decompress path resultPath =
            async {
                printfn "\t\t\tDecompress"

                let timer = Stopwatch.StartNew()
                timer.Start()
                let content = File.ReadAllBytes(path)
                let leavesCount = BitConverter.ToInt32(content[..3].Reverse().ToArray())
                let model = String.concat String.Empty (content[4..].Select(fun b -> (sprintf "%B" (b |> int32)).PadLeft(8, '0')))
                let spentOnTree = ref 0
                let tree = getTreeFromModel(model, leavesCount, spentOnTree)

                let table = buildCodesTable(tree)
                let lengthThreshold = table.Keys.Min(fun k -> digitCount k) - 1UL
                
                let trailingBitsCount = content.Last() |> int32

                let message = model[spentOnTree.Value..model.Length - 1 - Constants.ByteLength - trailingBitsCount]
                use file = File.Create(resultPath)
                
                let mutable pStart = 0
                for i in 0..message.Length - 1 do
                    if (i - pStart + 1) |> uint64 >= lengthThreshold then 
                        let mutable kvp = Nullable<byte>.op_Implicit(0uy)
                        let lookup = UInt64.Parse("1" + message[pStart..i])
                        if table.TryGetValue(lookup, &kvp) then
                            file.WriteByte(kvp.Value)
                            pStart <- i + 1

                timer.Stop()
                Console.WriteLine($"Decompressing done in {timer.ElapsedMilliseconds} ms")
            }
    