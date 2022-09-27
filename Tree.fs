module Tree

open System

[<AllowNullLiteral>]
type Node<'T>(content: 'T, weight: int64, ?leftChild: Node<'T>, ?rightChild: Node<'T>) = 
    let rec traversal(content: 'T, weight: int64, leftChild: Node<'T>, rightChild: Node<'T>, level: int) = 
        printfn "%s%O %d" (String.Join(String.Empty, [|for i in 0..level - 1 -> "-"|])) content weight
        if not (isNull leftChild) then
            traversal(leftChild.Content, leftChild.Weight, leftChild.LeftChild, leftChild.RightChild, level + 1)
        if not (isNull rightChild) then
            traversal(rightChild.Content, rightChild.Weight, rightChild.LeftChild, rightChild.RightChild, level + 1)

    member val LeftChild: Node<'T> = defaultArg leftChild null with get, set
    member val RightChild: Node<'T> = defaultArg rightChild null with get, set
    member this.Weight: int64 = weight
    member this.Content: 'T = content

    member this.Print = 
        traversal(this.Content, this.Weight, this.LeftChild, this.RightChild, 0)

    interface ICloneable with
        member this.Clone(): obj = 
            this.MemberwiseClone()
    interface IComparable with
        member this.CompareTo(obj: obj): int =
            let nodeObj = (obj :?> Node<'T>)
            this.Weight.CompareTo(nodeObj.Weight)
    
        