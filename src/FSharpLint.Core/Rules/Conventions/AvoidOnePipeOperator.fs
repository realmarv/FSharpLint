module FSharpLint.Rules.AvoidOnePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    
    let error =
        match args.AstNode with
        | AstNode.Binding (SynBinding(synAccessOption, synBindingKind, mustInline, isMutable, synAttributeLists, preXmlDoc, synValData, headPat, synBindingReturnInfoOption, synExpr, range, debugPointAtBinding)) ->
            match headPat with
            | LongIdent (LongIdentWithDots(idents, dotRanges))
            
        match args.AstNode with
        | AstNode.Expression (SynExpr.LetOrUse (_isRecursive, _isUse, bindings, body, _range)) ->
            match List.tryHead bindings with
            | Some(SynBinding(_synAccessOption, _synBindingKind, _mustInline, _isMutable, _synAttributeLists, _preXmlDoc, _synValData, headPat, _synBindingReturnInfoOption, _synExpr, _range, _debugPointAtBinding)) ->
                match headPat with
                | SynPat.Named(_synPat, ident, _isSelfIdentifier, _synAccessOption, _range) ->
                    if ident.idText.StartsWith "_" then
                        checkUsedIdent ident body
                    else
                        Array.empty
                | _ ->
                    Array.empty
            | _ -> Array.empty
        | _ -> Array.empty
        
    error
    printfn "%A" args.AstNode
    printfn "--------------"
    let error = Array.empty
      
    error

let rule =
    { Name = "AvoidOnePipeOperator"
      Identifier = Identifiers.AvoidOnePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule