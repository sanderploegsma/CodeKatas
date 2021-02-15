module Tests

open CodeKata.Kata09.Checkout
open Xunit

[<Fact>]
let ``Normal prices`` () =
    let rules =
        [ { Sku = "A"
            Priority = 1
            Price = NormalPrice(50) }
          { Sku = "B"
            Priority = 1
            Price = NormalPrice(30) }
          { Sku = "C"
            Priority = 1
            Price = NormalPrice(20) }
          { Sku = "D"
            Priority = 1
            Price = NormalPrice(15) } ]

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
        [ { Sku = "A"
            Priority = 2
            Price = SpecialPrice(3, 130) }
          { Sku = "A"
            Priority = 1
            Price = NormalPrice(50) }
          { Sku = "B"
            Priority = 2
            Price = SpecialPrice(2, 45) }
          { Sku = "B"
            Priority = 1
            Price = NormalPrice(30) }
          { Sku = "C"
            Priority = 1
            Price = NormalPrice(20) }
          { Sku = "D"
            Priority = 1
            Price = NormalPrice(15) } ]

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
