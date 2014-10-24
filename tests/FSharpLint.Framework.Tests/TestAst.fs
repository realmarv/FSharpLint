﻿(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

module TestAst

open NUnit.Framework
open FSharpLint.Framework.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let stubBinding attributes =
    SynBinding.Binding(None, 
                       SynBindingKind.NormalBinding, 
                       false, 
                       false, 
                       attributes, 
                       PreXmlDoc.PreXmlDocEmpty, 
                       SynValData(None, SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None)), None), 
                       SynPat.Wild(range()), 
                       None, 
                       SynExpr.Null(range()), 
                       range(), 
                       SequencePointInfoForBinding.NoSequencePointAtLetBinding)

let stubConstString str = SynExpr.Const(SynConst.String(str, range()), range())

let stubTuple exprs = SynExpr.Tuple(exprs, [], range())

let stubParen expr = SynExpr.Paren(expr, range(), None, range())

let stubAttribute name argExpr =
    { 
        SynAttribute.AppliesToGetterAndSetter = false
        SynAttribute.Range = range()
        SynAttribute.Target = None
        SynAttribute.TypeName = LongIdentWithDots.LongIdentWithDots([Ident(name, range())], [])
        SynAttribute.ArgExpr = argExpr
    }

let stubPropertyInitialiser propertyName value =
    SynExpr.App(ExprAtomicFlag.NonAtomic, 
                false, 
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("op_Equality", range())), SynExpr.Ident(Ident(propertyName, range())), range()), 
                stubConstString value, 
                range())

let stubCurrentNodeInfo suppressedMessages =
    {
        Node = Type(SynType.Anon(range()))
        ChildNodes = []
        Breadcrumbs = []
        SuppressedMessages = suppressedMessages
    }

[<TestFixture>]
type TestAst() =

    [<Test>]
    member this.GetSuppressMessageAttributesWithConstructorArgs() = 
        let attributes = 
            [
                [stubConstString "FSharpLint.Analyser"; stubConstString "Rule"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "FSharpLint.Analyser"; Rule = "Rule" }, List.head attrs)

    [<Test>]
    member this.GetSuppressMessageAttributesWithPropertyInitialisers() = 
        let attributes = 
            [
                [stubPropertyInitialiser "Category" "FSharpLint.Analyser"; stubPropertyInitialiser "CheckId" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "FSharpLint.Analyser"; Rule = "*" }, List.head attrs)

    [<Test>]
    member this.GetSuppressMessageAttributesWithPropertyInitialisersMissingCategoryProperty() = 
        let attributes = 
            [
                [stubPropertyInitialiser "SomeProp" "FSharpLint.Analyser"; stubPropertyInitialiser "CheckId" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member this.GetSuppressMessageAttributesWithPropertyInitialisersMissingCheckIdProperty() = 
        let attributes = 
            [
                [stubPropertyInitialiser "Category" "FSharpLint.Analyser"; stubPropertyInitialiser "SomeProp" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member this.GetSuppressMessageAttributesWithPropertyInitialisersWithExtraProperty() = 
        let attributes = 
            [
                [
                    stubPropertyInitialiser "AnotherProp" "FSharpLint.gwegweg"
                    stubPropertyInitialiser "Category" "FSharpLint.Analyser"
                    stubPropertyInitialiser "CheckId" "*"
                ] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "FSharpLint.Analyser"; Rule = "*" }, List.head attrs)

    [<Test>]
    member this.IsAnalyserSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "FSharpLint.Analyser"; Rule = "*" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("FSharpLint.Analyser"))

    [<Test>]
    member this.IsAnalyserWithDifferentNameSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "FSharpLint.SomeAnalyser"; Rule = "*" }]

        Assert.IsFalse(currentNodeInfo.IsSuppressed("FSharpLint.Analyser"))

    [<Test>]
    member this.IsAnalyserSuppressedWithSpecificAnalyserRuleSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "FSharpLint.Analyser"; Rule = "Rule" }]

        Assert.IsFalse(currentNodeInfo.IsSuppressed("FSharpLint.Analyser"))

    [<Test>]
    member this.IsRuleSuppressedWithSpecificAnalyserRuleSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "FSharpLint.Analyser"; Rule = "Rule" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("FSharpLint.Analyser", "Rule"))

    [<Test>]
    member this.IsRuleSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "FSharpLint.Analyser"; Rule = "*" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("FSharpLint.Analyser", "Rule"))