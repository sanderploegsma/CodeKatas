module Tests

open CodeKata.Kata05.BloomFilters.BloomFilter
open System
open Xunit

let rng = Random()

let randomWord minLength maxLength =
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    let length = rng.Next(minLength, maxLength)
    String(Array.init length (fun _ -> chars.[rng.Next sz]))

[<Fact>]
let ``Empty BloomFilter contains no words`` () =
    let filter = BloomFilter()
    let word = randomWord 1 32
    Assert.False(filter.contains word)
    
[<Fact>]
let ``BloomFilter.contains returns true for initial words`` () =
    let filter = BloomFilter()
    
    let dict = ["do"; "re"; "mi"; "fa"; "sol"; "la"; "ti"; "do"]
    for word in dict do
        filter.add word
    
    for word in dict do
        Assert.True(filter.contains word)
   
[<Fact>]     
let ``BloomFilter.contains returns false for other words`` () =
    // Note: this test is a bit risky, because a Bloom Filter can always return false positives.
    // However, we try to minimize this risk by initializing with longer words, and testing with
    // shorter words.
    
    let filter = BloomFilter()
    
    let dict = Seq.init 100 (fun _ -> randomWord 8 32)
    for word in dict do
        filter.add word
        
    let words = List.except dict ["do"; "re"; "mi"; "fa"; "sol"; "la"; "ti"; "do"]
    for word in words do
        Assert.False(filter.contains word)
        
[<Fact>]
let ``BloomFilter.contains returns false positive when hash function collides`` () =
    // If we use a hash function that just returns the length of each word, then all words with the same length
    // will yield the same hash, thus causing a collision.
    
    let filter = BloomFilter(hashFunctions = [String.length])
    filter.add "foo"
    
    for word in Seq.init 100 (fun _ -> randomWord 3 3) do
        Assert.True(filter.contains word)