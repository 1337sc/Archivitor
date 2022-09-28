module HuffmanCompressor

open Compressor
open System.Collections.Generic
open System.IO
open Tree
open System.Linq
open System
open System.Text

type HuffmanCompressor() = 

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
    let rec getContents (curNode: Node<Nullable<byte>>, path: string, codesDict: Dictionary<Nullable<byte>, string>) = 
        if isNull curNode.LeftChild && isNull curNode.RightChild then 
            codesDict.Add(curNode.Content, path)
        else 
            getContents(curNode.LeftChild, path + "1", codesDict)
            getContents(curNode.RightChild, path + "0", codesDict)

    let buildCodesTable (tree: Node<Nullable<byte>>) = 
        let table = new Dictionary<Nullable<byte>, string>()
        getContents(tree, String.Empty, table)
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
        bitsSpentOnTree := p
        tree

    interface ICompressor with
        member this.Compress path resultPath = 
            printfn "\t\t\tCompress"
            let content = File.ReadAllBytes(path)
            let frequencies = new Dictionary<Nullable<byte>, int>()
            for c in content do
                if (frequencies.ContainsKey(c)) then
                    frequencies[c] <- frequencies[c] + 1
                else
                    frequencies[c] <- 1

            let freqTree = buildFrequencyTree frequencies

            freqTree.Print

            let codesTable = buildCodesTable freqTree
            let projectTreeTable = projectTree(freqTree)

            let resultBits = new StringBuilder()
            resultBits.Append((sprintf "%B" frequencies.Count).PadLeft(32, '0')) |> ignore

            for c in projectTreeTable do
                resultBits.Append(c.Value[1..]).Append((sprintf "%B" (c.Key.Value |> int32)).PadLeft(8, '0')) |> ignore

            for b in content do
                let code = codesTable[b]
                resultBits.Append(code) |> ignore

            let trailingBitsCount = 8 - (resultBits.Length % 8)
            resultBits.Append(String.Join(String.Empty, [|for i in 1..trailingBitsCount -> '0'|])) |> ignore // 8 bits in each byte
            
            let resultBitsString = resultBits.ToString()
            
            use file = File.Create(resultPath)
            for b in 0..8..resultBitsString.Length - 1 do
                let substring = resultBitsString[b..b + 7]
                let curByte = Convert.ToByte(substring, 2)
                
                file.WriteByte(curByte)
            file.WriteByte(trailingBitsCount |> byte)
            ()

        member this.Decompress path resultPath =
            printfn "\t\t\tDecompress"
            let content = File.ReadAllBytes(path)
            let leavesCount = BitConverter.ToInt32(content[..3].Reverse().ToArray())
            let model = String.concat String.Empty (content[4..].Select(fun b -> (sprintf "%B" (b |> int32)).PadLeft(8, '0')))
            let spentOnTree = ref 0
            let tree = getTreeFromModel(model, leavesCount, spentOnTree)

            tree.Print

            let table = buildCodesTable(tree)

            let trailingBitsCount = content.Last() |> int32

            let message = model[spentOnTree.Value..model.Length - 1 - 8 - trailingBitsCount]
            use file = File.Create(resultPath)

            let mutable pStart = 0
            for i in 0..message.Length - 1 do
                let kvp = table.Where(fun x -> x.Value = message[pStart..i])
                if kvp.Any() then
                    file.WriteByte(kvp.First().Key.Value)
                    pStart <- i + 1

            ()
    