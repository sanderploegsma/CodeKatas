module Kata09.Checkout

type PriceRule =
    | NormalPrice of Sku: string * UnitPrice: int
    | BulkDiscountedPrice of Sku: string * UnitPrice: int * DiscountQuantity: int * DiscountPrice: int

type Checkout =
    { PriceRules: PriceRule list
      LineItems: Map<string, int> }
    
    /// Create a new checkout with the given rules
    static member create rules =
        { PriceRules = rules; LineItems = Map.empty }

    /// Scan a new sku, adding it to the line items in the checkout
    static member scan sku checkout =
        let quantity =
            Map.tryFind sku checkout.LineItems
            |> Option.defaultValue 0

        { checkout with
              LineItems = Map.add sku (quantity + 1) checkout.LineItems }

    /// Calculate the total price of the the given checkout
    static member total checkout =
        checkout.LineItems
        |> Map.map checkout.itemPrice
        |> Map.toSeq
        |> Seq.sumBy snd
        
    member private this.itemPrice itemSku itemQuantity =
        let rule =
            this.PriceRules
            |> List.find (fun rule ->
                match rule with
                | NormalPrice (sku, _) -> sku = itemSku
                | BulkDiscountedPrice (sku, _, _, _) -> sku = itemSku)

        match rule with
        | NormalPrice (_, unitPrice) -> itemQuantity * unitPrice
        | BulkDiscountedPrice (_, unitPrice, discountQuantity, discountPrice) ->
            let totalDiscountedPrice =
                (itemQuantity / discountQuantity) * discountPrice

            let restPrice =
                (itemQuantity % discountQuantity) * unitPrice

            totalDiscountedPrice + restPrice