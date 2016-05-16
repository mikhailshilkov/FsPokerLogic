module FacadeTests

open Xunit
open Cards
open PostFlop.Facade

[<Fact>]
let ``pickOopSheet returns limp coorectly for single item`` () =
  let actual = pickOopSheet [Action.Check] 20
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns limp coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.Check; Action.Call; Action.RaiseToAmount 100] 20
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for single items`` () =
  let actual = pickOopSheet [Action.Call] 20
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.Call; Action.Call] 20
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for single item`` () =
  let actual = pickOopSheet [Action.RaiseToAmount 60] 20
  Assert.Equal(Some "hero raise FV vs limp", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.RaiseToAmount 60; Action.RaiseToAmount 100] 20
  Assert.Equal(Some "hero raise FV vs limp", actual)