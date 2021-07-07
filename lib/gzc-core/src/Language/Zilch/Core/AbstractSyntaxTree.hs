module Language.Zilch.Core.AbstractSyntaxTree where

import Data.Located (Located)
import Data.Text (Text)

data Module
  = Module
      (Located Identifier)   -- ^ The name of the module
      [Located Declaration]  -- ^ The list of declarations in the module

data Declaration
  = -- | Function definition
    Def
        (Located FunctionDeclaration)  -- ^ The head of the function declaration
        (Located Expression)           -- ^ The value of the function
  | -- | Type alias
    Type
        (Located Identifier)        -- ^ The name of the alias
        [Located (Parameter Kind)]  -- ^ The type parameters with optional kind annotations
        (Located Type)              -- ^ The type aliased
  | -- | Enum definition
    Enum
        (Located Identifier)                    -- ^ The name of the enumeration
        [Located (Parameter Kind)]              -- ^ The type parameters with optional kind annotations
        [(Located Identifier, [Located Type])]  -- ^ Enumeration constructors
  | -- | Record definition
    Record
        (Located Identifier)                  -- ^ The name of the record
        [Located (Parameter Kind)]            -- ^ The type parameters with optional kind annotations
        [(Located Identifier, Located Type)]  -- ^ Record fields
  | -- | Type class declaration
    Class
        (Located Identifier)           -- ^ Class name
        [Located (Parameter Kind)]     -- ^ Optionally kind-annotated type parameters
        [Located Type]                 -- ^ Type constraints on class head
        [Located FunctionDeclaration]  -- ^ Type class member functions
  | -- | Type class implementation
    Impl
        (Located Identifier)        -- ^ The name of the type class implemented
        [Located (Parameter Kind)]  -- ^ Universally quantified type variables in the head of the implementation, optionally kind-annotated
        [Located Type]              -- ^ The types whose implementation is defined for
        [Located Declaration]       -- ^ The definition of the function members

-- | The head of a function declaration/definition.
data FunctionDeclaration
  = FunDecl
        (Located Identifier)        -- ^ The name of the function
        [Located (Parameter Kind)]  -- ^ Type parameters with optional kind annotations
        [Located Type]              -- ^ Type constraints on function head

-- | A qualified identifier
type Identifier = ([Located Text], Located Text)

-- | An optionally @t@-annotated parameter.
type Parameter t = (Located Identifier, Maybe (Located t))

-- | Types at the type-level
data Type
  = -- | An anonymous function at the type-level
    LambdaT
        [Located (Parameter Kind)]  -- ^ The parameter of the lambda
        (Located Type)              -- ^ The return type of the lambda
  | -- | A function application at the type-level
    ApplicationT
        (Located Type)  -- ^ The function type
        [Located Type]  -- ^ The arguments to the function call
  | -- | A universally quantified type
    ForallT
        [Located (Parameter Kind)]  -- ^ The universally bound type variables with optional kind annotations
        (Located Type)              -- ^ The quantified type
  | -- | A constrained type
    ConstrainedT
        [Located Type]  -- ^ The constraints
        (Located Type)  -- ^ The constrained type
  | -- | A type identified by its name
    NameT
        (Located Identifier) -- ^ The name of the type
--------- Builtin types
  | -- | The pointer type
    PtrT
  | -- | The reference type
    RefT
  | -- | The type of unsigned integers
    UnsignedT
        Integer -- ^ The width of the integer
  | -- | The type of signed integers
    SignedT
        Integer
  | -- | The type of characters
    CharT

-- | Types at the kind-level
data Kind
  = -- | The kind of fully applied types
    TypeK
  | -- | The kind of type-level functions
    FunctionK
        [Located Kind]  -- ^ The parameters of the function
        (Located Kind)  -- ^ The result kind

-- | An expression
data Expression
  = -- | A lambda abstraction
    LambdaE
        [Located (Parameter Type)]  -- ^ The parameters of the lambda with optional type annotations
        (Located Expression)        -- ^ The return expression
  | -- | A conditional expression
    IfE
      (Located Expression)  -- ^ The condition
      (Located Expression)  -- ^ The expression returned if the condition is true
      (Located Expression)  -- ^ The expression returned if the condition is false
  | -- | A do-block
    DoE
      [Located DoStatement] -- ^ The statements in the do-block
  | -- | A pattern-matching expression
    CaseE
      (Located Expression)                    -- ^ The expression whose value is to match
      [(Located Pattern, Located Expression)] -- ^ The branches of the pattern matching
  | -- | A record literal
    RecordE
      [(Located Identifier, Located Expression)] -- ^ The fields of the record
  | -- | A variable
    IdentifierE
      (Located Identifier)  -- ^ The name of the identifier
  | -- | A typed hole
    TypedHoleE
  | -- | A record access
    RecordAccessE
      (Located Expression)  -- ^ The record to access
      (Located Identifier)  -- ^ The name of the fields to access
  | -- | Function application
    ApplicationE
      (Located Expression)  -- ^ The function to apply
      [Located Expression]  -- ^ The arguments of the application
  | -- | An integral number
    IntegerE
      (Located Text)
  | -- | A string
    StringE
      (Located Text)
  | -- | A character
    CharE
      (Located Text)

data DoStatement
  = -- | A function binding
    BindingD
      (Located Identifier)  -- ^ The name of the function
      (Located Expression)  -- ^ The value of the function
  | -- | A single expression
    ExprD
      (Located Expression)

data Pattern
  = -- | The wildcard pattern
    WildcardP
  | -- | An enumeration constructor
    ConstructorP
      [Located Pattern]  -- ^ The constructor parameters
  | -- | An integral number
    IntegerP
      (Located Text)
  | -- | A variable binding
    VariableP
      (Located Identifier)  -- ^ The name of the variable
