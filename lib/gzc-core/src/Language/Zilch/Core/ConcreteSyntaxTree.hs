module Language.Zilch.Core.ConcreteSyntaxTree where

import Data.Located (Located)
import Data.Text (Text)

-- | A Zilch module is composed of imports and many located declarations.
data Module
  = Module
      (Maybe [Located Identifier])   -- ^ The export list
      [Located Import]               -- ^ A list of imports
      [Located TopLevelDeclaration]  -- ^ All the available top-level declarations of the module
  deriving (Show, Eq)

-- | An @import@ statement at the top of a module.
data Import
  = Import
      Bool                                                        -- ^ Is the import opened/unqualified?
      (Located Identifier)                                        -- ^ The module imported
      (Maybe (Located Identifier))                                -- ^ An optional alias for the imported module
      (Maybe [(Located Identifier, Maybe (Located Identifier))])  -- ^ An optional import list with optional aliasing
  deriving (Show, Eq)

-- | A top-level declaration with meta-attributes introduced in the @#[...]@ construct.
type TopLevelDeclaration = ([Located MetaSpecifier], Located Declaration)

-- | Any Zilch declaration
data Declaration
  = -- | A function definition
    Def
        FunctionDeclaration         -- ^ The head of the function
        (Located Expression)        -- ^ The value of the function
        [Located Declaration]       -- ^ @where@-bound functions
  | -- | An enumeration definition
    Enum
      (Located Identifier)                    -- ^ The name of the enumeration
      [Located (Parameter Kind)]              -- ^ The type parameters with optional kind annotations
      [(Located Identifier, [Located Type])]  -- ^ The constructors with their parameters
  | -- | A record definition
    Record
      (Located Identifier)                  -- ^ The name of the record
      [Located (Parameter Kind)]            -- ^ The type parameters with optional kind annotations
      [(Located Identifier, Located Type)]  -- ^ The fields of the record
  | -- | A type alias
    Type
      (Located Identifier)        -- ^ The name of the type alias
      [Located (Parameter Kind)]  -- ^ The type parameters with optional kind annotations
      (Located Type)              -- ^ The type aliased
  | -- | A type class declaration
    Class
      (Located Identifier)                  -- ^ The name of the type class
      [Located (Parameter Kind)]            -- ^ The parameters with optional kind annotations
      [Located Type]                        -- ^ The constraints on the type class head
      [(Located Identifier, Located Type)]  -- ^ The members of the type class
  | -- | A type class implementation
    Impl
      (Located Identifier)                          -- ^ The name of the type class
      ([Located (Parameter Kind)], [Located Type])  -- ^ The type parameters with optional kind annotations with type constraints
      [Located Type]                                -- ^ The types for which the type class is implemented
      [Located Declaration]                         -- ^ Member function definitions
  deriving (Show, Eq)

data FunctionDeclaration
  = Decl
      Bool                                         -- ^ Is the binding recursive?
      (Located Identifier)                         -- ^ The name of the declared function
      ([Located (Parameter Kind)], [Located Type]) -- ^ A list of type parameters with optional kind annotations with type constraints
      (Maybe [Located (Parameter Type)])           -- ^ A list of parameters with optional types
      (Maybe (Located Type))                       -- ^ The optional return type
  deriving (Show, Eq)

-- | A parameter is an identifier whose type may be specified (or not).
type Parameter t = (Located Identifier, Maybe (Located t))

-- | The type of identifiers.
type Identifier = ([Text], Text)

-- | The fixity of an operator (associativity + precedence).
data Fixity
  = InfixL Integer
  | InfixR Integer
  | Infix  Integer
  deriving (Show, Eq)

-- | Meta-specifiers for top-level bindings.
data MetaSpecifier
  = -- | > default
    DefaultImpl
  | OpFix Fixity
  deriving (Show, Eq)

type Expression = [Located ExpressionAtom]

data ExpressionAtom
  = -- | A lambda abstraction
    FnE
      [Located (Parameter Type)]  -- ^ The parameters of the lambda with optional type annotations
      (Located Expression)        -- ^ The return expression
  | -- | A conditional expression
    IfE
      (Located Expression)  -- ^ The condition
      (Located Expression)  -- ^ The expression returned if the condition is true
      (Located Expression)  -- ^ The expression returned if the condition is false
  | -- | A let-in expression
    LetE
      [Located Declaration]  -- ^ The bound variable
      (Located Expression)   -- ^ The expression
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
  | -- | AN operator
    OperatorE
      (Located Identifier)  -- ^ The name of the operator
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
    IntegerE Text
  | -- | A string
    StringE Text
  | -- | A character
    CharE Text
  | -- | A parenthesized expression
    ParensE
      (Located Expression)
  | -- | The wildcard expression for lambda construction
    WildcardE
  deriving (Show, Eq)

type Pattern = [Located PatternAtom]

data PatternAtom
  = -- | The wildcard pattern
    WildcardP
  | -- | An enumeration constructor
    ConstructorP
      (Located Identifier)  -- ^ The name of the constructor
      [Located Pattern]     -- ^ The constructor parameters
  | -- | An integral number
    IntegerP Text
  | -- | A variable binding
    VariableP
      (Located Identifier)  -- ^ The name of the variable
  | -- | A parenthesized pattern
    ParensP
      (Located Pattern)
  | -- | A pattern operator
    OperatorP
      (Located Identifier)
  deriving (Show, Eq)

data Type
  = -- | A universally quantified type
    ForallT
      [Located (Parameter Kind)]   -- ^ The list of universally quantified type variables with optional kind quantification
      (Located Type)               -- ^ The universally quantified type
  | -- | A contrained type
    ConstrainedT
      [Located Type]  -- ^ The type constraints
      (Located Type)  -- ^ The constrained type
  | -- | A type function
    FunctionT
      [Located Type]  -- ^ The arguments of the function
      (Located Type)  -- ^ The result of the function
  | -- | A type-level function application
    ApplicationT
      (Located Type)  -- ^ The type-level function
      [Located Type]  -- ^ The parameters
  | -- | The wildcard type for anonymous type-level function construction
    WildcardT
  | -- | A type identified by its name
    IdentifierT
      (Located Identifier)  -- ^ The name of the type
  | -- | A parenthesized type
    ParensT
      (Located Type)
-------- Builtin type
  | -- | Pointer type
    PtrT
  | -- | Reference type
    RefT
  | -- | An unsigned integer
    UnsignedT
      Integer  -- ^ The width of the integer
  | -- | A signed integer
    SignedT
      Integer  -- ^ The width of the integer
  | -- | A character
    CharT
  deriving (Show, Eq)

-- | The type of types
data Kind
  = -- | The kind of fully applied (concrete) types
    TypeK
  | -- | The kind of type-level functions
    FunctionK
      [Located Kind]  -- ^ The parameters
      (Located Kind)  -- ^ The result
  | -- | A parenthesized kind
    ParensK
      (Located Kind)
  deriving (Show, Eq)
