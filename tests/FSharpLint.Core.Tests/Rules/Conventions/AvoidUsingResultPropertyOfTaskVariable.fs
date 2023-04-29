module FSharpLint.Core.Tests.Rules.Conventions.AvoidUsingResultPropertyOfTaskVariable

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidUsingResultPropertyOfTaskVariable() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidUsingResultPropertyOfTaskVariable.rule)

    [<Test>]
    member this.``Avoid using .Result property of a Task<'T> variable in a function``() =
        this.Parse
            """
open System.Threading.Tasks

let GetTaskOutput (task: Task<string>) =
    task.Result"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Using Async.AwaitTask instead of .Result property of a Task<'T> variable in a function``() =
        this.Parse
            """
open System.Threading.Tasks

let GetTaskOutput (task: Task<string>) =
    Async.AwaitTask(task)"""

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Using .Result property of a Task<'T> variable when the Task type can't be inferred from the function signature``
        ()
        =
        this.Parse
            """
open System.Threading.Tasks

let GetTaskOutput task =
    task.Result

let task: Task<string> = client.GetStringAsync url
let taskOutput = GetTaskOutput task"""

        Assert.IsTrue this.ErrorsExist
