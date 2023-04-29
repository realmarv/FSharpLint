module FSharpLint.Rules.AvoidUsingResultPropertyOfTaskVariable

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =

    let errorsType1 =
        match args.AstNode with
        | Binding (SynBinding (_, _, _, _, _, _, _, headPat, _, expr, range, _)) ->

            let (mayBeVariableName, mayBeVariableType) =
                match headPat with
                | SynPat.LongIdent
                    (
                        LongIdentWithDots ([ identifier ], _),
                        _,
                        _,
                        SynArgPats.Pats ([ SynPat.Paren (SynPat.Typed (named, app, _), _) ]),
                        _,
                        _
                    ) ->
                    let variableName =
                        match named with
                        | SynPat.Named (_, ident: Ident, _, _, _) -> Some(ident.idText)
                        | _ -> None

                    let variableType =
                        match app with
                        | SynType.App (SynType.LongIdent (LongIdentWithDots ([ ident: Ident ], [])), _, _, _, _, _, _) ->
                            Some(ident.idText)
                        | _ -> None

                    variableName, variableType
                | _ -> None, None

            match (mayBeVariableName, mayBeVariableType) with
            | Some variableName, Some variableType when variableType = "Task" ->
                let (mayBeSecondVariableName, mayBeSecondVariableProperty) =
                    match expr with
                    | SynExpr.LongIdent (_, LongIdentWithDots ([ task; result ], _), _, _) ->
                        Some(task.idText), Some(result.idText)
                    | _ -> None, None

                match mayBeSecondVariableName, mayBeSecondVariableProperty with
                | Some secondVariableName, Some secondVariableProperty when
                    secondVariableName = variableName
                    && secondVariableProperty = "Result"
                    ->
                    { Range = range
                      Message = Resources.GetString "RulesAvoidUsingResultPropertyOfTaskVariableError"
                      SuggestedFix = None
                      TypeChecks = List.Empty }
                    |> Array.singleton

                | _ -> Array.empty
            | _ -> Array.empty
        | _ -> Array.empty

    let errorsType2 =
        match (args.AstNode, args.CheckInfo) with
        | (AstNode.Identifier ([ identifierName; identifierProperty ], range), Some checkInfo) ->
            let partialAssemblySignature = checkInfo.PartialAssemblySignature
            let entity = partialAssemblySignature.Entities.[0]

            if identifierProperty <> "Result" then
                Array.empty
            else
                entity.MembersFunctionsAndValues
                |> Seq.map (fun item ->

                    match item.FullType.BaseType with
                    | Some baseType when
                        baseType.ToString() = "type System.Threading.Tasks.Task"
                        && item.DisplayName = identifierName
                        ->
                        { Range = range
                          Message = Resources.GetString "RulesAvoidUsingResultPropertyOfTaskVariableError"
                          SuggestedFix = None
                          TypeChecks = List.Empty }
                        |> Array.singleton
                    | _ -> Array.empty)
                |> Array.concat
        | _ -> Array.empty

    Array.append errorsType1 errorsType2

let rule =
    { Name = "AvoidUsingResultPropertyOfTaskVariable"
      Identifier = Identifiers.AvoidUsingResultPropertyOfTaskVariable
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
