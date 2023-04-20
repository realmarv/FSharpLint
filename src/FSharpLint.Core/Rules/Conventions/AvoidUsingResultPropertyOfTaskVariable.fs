module FSharpLint.Rules.AvoidUsingResultPropertyOfTaskVariable

open FSharpLint.Framework.Rules

let runner (args:AstNodeRuleParams) =
    Array.empty

let rule =
    { Name = "AvoidUsingResultPropertyOfTaskVariable"
      Identifier = Identifiers.AvoidUsingResultPropertyOfTaskVariable
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
