module FSharpLint.Core.Tests.Rules.Conventions.AvoidOnePipeOperator

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidOnePipeOperatorZahra() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidOnePipeOperator.rule)

    [<Test>]
    member this.``Use pipe operator once``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator twice``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
    |> yetAnotherFunc
"""

        Assert.IsFalse this.ErrorsExist

