namespace CodeKata.Kata05.BloomFilters

module Program =

    open System
    open System.IO
    open BloomFilter

    let rec handleInput (filter: BloomFilter) =
        printfn "Enter word to check, or type :q to quit:"
        match Console.ReadLine() with
        | ":q" -> 0
        | word ->
            printfn "Dictionary contains '%s': %b\n" word (filter.contains word)
            handleInput filter
            
    [<EntryPoint>]
    let main argv =
        let filter = BloomFilter()
        
        printfn "Loading dictionary..."
        for word in File.ReadLines("wordlist.txt") do
            filter.add word
        
        handleInput filter