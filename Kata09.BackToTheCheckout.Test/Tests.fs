module Tests

open Kata09.Checkout
open Xunit

[<Fact>]
let ``Normal prices`` () =
    let rules =
        [ NormalPrice("A", 50)
          NormalPrice("B", 30)
          NormalPrice("C", 20)
          NormalPrice("D", 15) ]

    Checkout.create rules
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.total
    |> fun total -> Assert.Equal(100, total)
    
    Checkout.create rules
    |> Checkout.scan "A"
    |> Checkout.scan "B"
    |> Checkout.total
    |> fun total -> Assert.Equal(80, total)

[<Fact>]
let ``Prices with discounts`` () =
    let rules =
        [ BulkDiscountedPrice("A", 50, 3, 130)
          BulkDiscountedPrice("B", 30, 2, 45)
          NormalPrice("C", 20)
          NormalPrice("D", 15) ]

    Checkout.create rules
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.total
    |> fun total -> Assert.Equal(100, total)
    
    Checkout.create rules
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.total
    |> fun total -> Assert.Equal(130, total)
    
    Checkout.create rules
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.scan "A"
    |> Checkout.total
    |> fun total -> Assert.Equal(180, total)
