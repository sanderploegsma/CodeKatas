[<RequireQualifiedAccess>]
module HashFunctions

open System
open System.Security.Cryptography
open System.Text

let private hash (algo: HashAlgorithm) (str: string): int =
    Encoding.UTF8.GetBytes(str)
    |> (algo.ComputeHash)
    |> fun bytes -> BitConverter.ToInt32(bytes, 0)
    |> abs

let md5 str =
    use md5 = MD5.Create()
    hash md5 str
    
let sha256 str =
    use sha256 = SHA256Managed.Create()
    hash sha256 str