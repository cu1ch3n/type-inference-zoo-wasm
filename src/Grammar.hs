{-# LANGUAGE QuasiQuotes #-}

module Grammar (grammarContent) where

import Text.RawString.QQ (r)

-- Grammar content embedded as a raw string literal
-- This is much cleaner than escaped strings and works with WASM
grammarContent :: String
grammarContent = [r|{
  "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
  "name": "Lambda Calculus",
  "scopeName": "source.lc",
  "fileTypes": ["lc"],
  "patterns": [
    { "include": "#comments" },
    { "include": "#numbers" },
    { "include": "#booleans" },
    { "include": "#keywords" },
    { "include": "#quantifier" },
    { "include": "#lambda" },
    { "include": "#typeApplication" },
    { "include": "#operators" },
    { "include": "#builtinTypes" },
    { "include": "#typeNames" },
    { "include": "#identifiers" },
    { "include": "#punctuation" }
  ],
  "repository": {
    "comments": {
      "patterns": [
        { "name": "comment.line.double-dash.lc", "match": "--.*$" },
        { "name": "comment.line.double-slash.lc", "match": "//.*$" },
        { "name": "comment.line.number-sign.lc", "match": "#.*$" }
      ]
    },
    "numbers": {
      "name": "constant.numeric.integer.lc",
      "match": "\\b\\d+\\b"
    },
    "booleans": {
      "name": "constant.language.boolean.lc",
      "match": "(?i)\\b(true|false)\\b"
    },
    "keywords": {
      "patterns": [
        {
          "name": "keyword.control.let.lc",
          "match": "(?i)\\b(let|in)\\b"
        },
        {
          "name": "keyword.control.forall.lc",
          "match": "(?i)\\bforall\\b"
        }
      ]
    },
    "quantifier": {
      "name": "meta.quantifier.forall.lc",
      "begin": "(?i)\\bforall\\b",
      "beginCaptures": {
        "0": { "name": "keyword.control.forall.lc" }
      },
      "end": "\\.",
      "patterns": [
        { "name": "punctuation.section.group.begin.lc", "match": "\\(" },
        { "name": "punctuation.section.group.end.lc", "match": "\\)" },
        { "name": "variable.parameter.type.lc", "match": "\\b[a-z_][\\w]*\\b" },
        { "name": "keyword.operator.relation.lc", "match": "<:" },
        { "include": "#builtinTypes" },
        { "include": "#typeNames" },
        { "include": "#operators" }
      ]
    },
    "lambda": {
      "patterns": [
        {
          "name": "meta.lambda.lc",
          "match": "\\\\\\s*([a-z_][\\w]*)\\s*\\.",
          "captures": {
            "0": { "name": "keyword.declaration.lambda.lc" },
            "1": { "name": "variable.parameter.lc" }
          }
        }
      ]
    },
    "typeApplication": {
      "patterns": [
        {
          "name": "keyword.operator.type-application.lc",
          "match": "@"
        },
        {
          "name": "meta.type-application.target.lc",
          "match": "@\\s*([A-Z][\\w]*)",
          "captures": {
            "1": { "name": "entity.name.type.lc" }
          }
        }
      ]
    },
    "operators": {
      "patterns": [
        { "name": "keyword.operator.arrow.lc", "match": "->" },
        { "name": "keyword.operator.intersection.lc", "match": "&" },
        { "name": "keyword.operator.union.lc", "match": "\\|" },
        { "name": "keyword.operator.annotation.lc", "match": ":" },
        { "name": "keyword.operator.assignment.lc", "match": "=" },
        { "name": "keyword.operator.relation.lc", "match": "<:" }
      ]
    },
    "builtinTypes": {
      "patterns": [
        { "name": "support.type.builtin.lc", "match": "\\b(Int|Bool|Top|Bot)\\b" },
        { "name": "support.type.kind.lc", "match": "\\bType\\b" }
      ]
    },
    "typeNames": {
      "name": "entity.name.type.lc",
      "match": "\\b[A-Z][A-Za-z0-9_]*\\b"
    },
    "identifiers": {
      "name": "variable.other.lc",
      "match": "\\b[a-z_][A-Za-z0-9_]*\\b"
    },
    "punctuation": {
      "patterns": [
        { "name": "punctuation.section.group.begin.lc", "match": "\\(" },
        { "name": "punctuation.section.group.end.lc", "match": "\\)" },
        { "name": "punctuation.separator.comma.lc", "match": "," },
        { "name": "punctuation.separator.dot.lc", "match": "\\." }
      ]
    }
  }
}|]
