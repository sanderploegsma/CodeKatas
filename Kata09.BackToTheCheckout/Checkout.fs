module Kata09.Checkout

type ItemPrice =
    | NormalPrice of int
    | SpecialPrice of quantity: int * price: int

type PricingRule =
    { Sku: string
      Priority: int
      Price: ItemPrice }

type Checkout =
    { PriceRules: PricingRule list
      Skus: string list }

    /// Create a new checkout with the given rules
    static member create rules = { PriceRules = rules; Skus = [] }

    /// Scan a new sku, adding it to the line items in the checkout
    static member scan sku checkout =
        { checkout with
              Skus = checkout.Skus @ [ sku ] }

    /// Calculate the total price of the the given checkout
    static member total checkout =
        checkout.Skus
        |> List.countBy id
        |> List.sumBy checkout.itemPrice

    member private this.itemPrice(sku, quantity) =
        let applyRule price currentTotal currentQuantity =
            match price with
            | SpecialPrice (q, amount) when q <= currentQuantity -> Some(currentTotal + amount, currentQuantity - q)
            | NormalPrice amount -> Some(currentTotal + amount, currentQuantity - 1)
            | _ -> None

        let rules =
            this.PriceRules
            |> List.filter (fun rule -> rule.Sku = sku)
            |> List.sortByDescending (fun rule -> rule.Priority)

        let mutable total = 0
        let mutable quantityLeft = quantity

        while quantityLeft > 0 do
            let newTotal, newQuantityLeft =
                List.pick (fun rule -> applyRule rule.Price total quantityLeft) rules

            total <- newTotal
            quantityLeft <- newQuantityLeft

        total
