module FSharpLint.Core.Tests.Rules.Conventions.AvoidUsingResultPropertyOfTaskVariable

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidUsingResultPropertyOfTaskVariable() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidUsingResultPropertyOfTaskVariable.rule)

    [<Test>]
    member this.``Avoid using .Result property of a Task<'T> variable in a function``() =
        this.Parse """
let GetTaskOutput (task: Task<string>) =
    task.Result"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Using Async.AwaitTask instead of .Result property of a Task<'T> variable in a function``() =
        this.Parse """
let GetTaskOutput (task: Task<string>) =
    Async.AwaitTask(task)"""

        Assert.IsFalse this.ErrorsExist
