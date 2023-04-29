module FSharpLint.Rules.AvoidUsingResultPropertyOfTaskVariable

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
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

    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, headPat, _, synExpr, range, _)), _ ->
        let maybeVarName, maybeVarType =
            match headPat with
            | SynPat.LongIdent (_, _, _, synArgPats, _, _) ->
                match synArgPats with
                | SynArgPats.Pats synPats ->
                    match synPats with
                    | [ SynPat.Paren
                            (
                                SynPat.Typed
                                    (
                                        SynPat.Named (_, ident, _, _, _),
                                        SynType.App
                                            (
                                                SynType.LongIdent (LongIdentWithDots (idents, _)), _, _, _, _, _, _
                                            ),
                                        _
                                    ),
                                _
                            ) ] ->
                        let varName = ident
                        let varType = idents
                        Some varName.idText, Some varType
                    | _ -> None, None
                | _ -> None, None
            | _ -> None, None

        let maybeSecondVarName, maybeSecondVarProperty =
            match synExpr with
            | SynExpr.LongIdent (_, LongIdentWithDots ([ secondVarName; secondVarProperty ], _), _, _) ->
                Some secondVarName.idText, Some secondVarProperty.idText
            | _ -> None, None

        match maybeVarName, maybeVarType, maybeSecondVarName, maybeSecondVarProperty with
        | Some varName, Some varType, Some secondVarName, Some secondVarProperty ->

            if secondVarProperty = "Result"
               && (varType.[3].idText = "Task")
               && varName = secondVarName then
                { Range = range
                  Message = Resources.GetString "RulesAvoidUsingResultPropertyOfTaskVariableError"
                  SuggestedFix = None
                  TypeChecks = List.Empty }
                |> Array.singleton
            else
                Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "AvoidUsingResultPropertyOfTaskVariable"
      Identifier = Identifiers.AvoidUsingResultPropertyOfTaskVariable
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
