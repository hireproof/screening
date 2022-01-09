# Screening (WIP)

[![CI & CD](https://github.com/hireproof/screening/actions/workflows/main.yml/badge.svg)](https://github.com/hireproof/screening/actions/workflows/main.yml)
[![screening Scala version support](https://index.scala-lang.org/hireproof/screening/screening-core/latest-by-scala-version.svg?targetType=Sbt)](https://index.scala-lang.org/hireproof/screening/screening-core)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

> Input validation for the functional programmer

## Introduction

*Screening*, like any other validation library, provides composing building blocks that give you fine-grained control over your data checks. However, these features set it apart from other solutions in the ecosystem:

### 🪆**Nested data structures**

Schemas that need to be validated are often represented as case class & enum data structures (e.g. to derive JSON codecs). *Screening* offers a cursor DSL that allows to navigate and validate these structures in a type-safe manner while keeping track of the underlying structure to generate meaningful errors.

### ⚠️ **Structured errors** 

Errors are modeled as data classes that contain all necessary information about the validation (usually the expected and actual values). These errors can be serialized and returned to the client. If you prefer to work with plain `String`s, you can convert them to your liking.

### 🕵️‍♀️ **Introspection** 

*Screening* limits itself to applicative composition of a set of predefined rules. This is a high price in terms of expressiveness, but brings the benefit of introspection, enabling to infer the exact constraints imposed by a validation rule. These insights can be used to feed generated documentations or API schemas.

## Installation

*Screening* is available for Scala 3, 2.13 and Scala.js 1.x.

```scala
libraryDependencies ++=
  "io.hireproof" %%% "screening-core" % "[x.y.z]" ::
  "io.hireproof" %%% "screening-generic" % "[x.y.z]" ::
  "io.hireproof" %%% "screening-json-circe" % "[x.y.z]" ::
  Nil
```

Please use the Maven Central badge above to find out about the latest version.