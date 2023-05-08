module FSharpLint.Rules.AvoidUsingResultPropertyOfTaskVariable

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =

    // let errorsType1 =
    //     match args.AstNode with
    //     | Binding (SynBinding (_, _, _, _, _, _, _, headPat, _, expr, range, _)) ->

    //         let (mayBeVariableName, mayBeVariableType) =
    //             match headPat with
    //             | SynPat.LongIdent
    //                 (
    //                     LongIdentWithDots ([ identifier ], _),
    //                     _,
    //                     _,
    //                     SynArgPats.Pats ([ SynPat.Paren (SynPat.Typed (named, app, _), _) ]),
    //                     _,
    //                     _
    //                 ) ->
    //                 let variableName =
    //                     match named with
    //                     | SynPat.Named (_, ident: Ident, _, _, _) -> Some(ident.idText)
    //                     | _ -> None

    //                 let variableType =
    //                     match app with
    //                     | SynType.App (SynType.LongIdent (LongIdentWithDots ([ ident: Ident ], [])), _, _, _, _, _, _) ->
    //                         Some(ident.idText)
    //                     | _ -> None

    //                 variableName, variableType
    //             | _ -> None, None

    //         match (mayBeVariableName, mayBeVariableType) with
    //         | Some variableName, Some variableType when variableType = "Task" ->
    //             let (mayBeSecondVariableName, mayBeSecondVariableProperty) =
    //                 match expr with
    //                 | SynExpr.LongIdent (_, LongIdentWithDots ([ task; result ], _), _, _) ->
    //                     Some(task.idText), Some(result.idText)
    //                 | _ -> None, None

    //             match mayBeSecondVariableName, mayBeSecondVariableProperty with
    //             | Some secondVariableName, Some secondVariableProperty when
    //                 secondVariableName = variableName
    //                 && secondVariableProperty = "Result"
    //                 ->
    //                 { Range = range
    //                   Message = Resources.GetString "RulesAvoidUsingResultPropertyOfTaskVariableError"
    //                   SuggestedFix = None
    //                   TypeChecks = List.Empty }
    //                 |> Array.singleton

    //             | _ -> Array.empty
    //         | _ -> Array.empty
    //     | _ -> Array.empty
    printfn "-----"
    printfn "astnode:%A" args.AstNode
    // printfn "syntaxarray:%A" args.SyntaxArray
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
                    printfn "item:%A" item
                    printfn "name:%A" item.DisplayName
                    printfn "type:%A" item.FullType
                    // printfn "type:%A" item.FullType.
                    printfn "basetype:%A" item.FullType.BaseType
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
        | AstNode.Binding(SynBinding(synAccessOption, synBindingKind, mustInline, isMutable, synAttributeLists, preXmlDoc, synValData, headPat, maybeSynBindingReturnInfoOption, synExpr, range, debugPointAtBinding)), Some checkInfo ->
            let varName, varType =
                match headPat with
                | SynPat.LongIdent(longIdentWithDots, identOption, synValTyparDeclsOption, synArgPats, synAccessOption, range) ->
                    match synArgPats with
                    | SynArgPats.Pats synPats ->
                        match synPats with
                        | [SynPat.Paren(SynPat.Typed(SynPat.Named(synPat, ident, isSelfIdentifier, synAccessOption, _), SynType.App(SynType.LongIdent(LongIdentWithDots(idents, dotRanges)), rangeOption, typeArgs, commaRanges, greaterRange, isPostfix, _), _), _)] ->
                            let varName = ident
                            let varType = idents
                            printfn "var: %A %A" varName varType
                            Some(varName), Some(varType)
                        | _ -> None, None
                    | _ -> None, None
                | _ -> None, None
            
            match maybeSynBindingReturnInfoOption with
            | Some synBindingReturnInfoOption ->
                match synBindingReturnInfoOption with
                | SynBindingReturnInfoOption
                    printfn "%A" synBindingReturnInfoOption
                    
                Array.empty
            | _ -> Array.empty
            
        | _ -> Array.empty

    errorsType2
    // Array.append errorsType1 errorsType2

let rule =
    { Name = "AvoidUsingResultPropertyOfTaskVariable"
      Identifier = Identifiers.AvoidUsingResultPropertyOfTaskVariable
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
