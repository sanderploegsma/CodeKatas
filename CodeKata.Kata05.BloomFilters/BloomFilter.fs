namespace CodeKata.Kata05.BloomFilters

module BloomFilter =

    open System.Collections

    [<Literal>]
    let defaultLength = 1_000_000_000
    let defaultHashes = [HashFunctions.md5; HashFunctions.sha256]

    type HashFunction = string -> int

    type BloomFilter(?length: int, ?hashFunctions: HashFunction list) =
        let hashes = defaultArg hashFunctions defaultHashes
        let data = BitArray(defaultArg length defaultLength, false)
        
        member private this.hash(word: string) =
            hashes
            |> List.map (fun f -> f word)
            |> List.map (fun i -> i % data.Length)
        
        /// Add a word to the bloom filter
        member this.add(word: string) =
            this.hash word
            |> List.iter (fun i -> data.Set(i, true))
            
        /// Check whether the given word exists in the bloom filter
        /// This is guaranteed to return `true` if the word exists, but it may also return false positives.
        member this.contains(word: string) =
            this.hash word
            |> List.forall (data.Get)