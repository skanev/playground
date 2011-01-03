-module(total_price).
-export([total_price/1]).

total_price(ShoppingList) ->
  [{Item, Quantity * Price} || {Item, Quantity, Price} <- ShoppingList].
