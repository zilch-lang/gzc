module Language.Zilch.Core.Tokens where

import Data.IndentLocated (Located)
import Data.Text (Text)

-- | A simple type alias for located tokens (tokens with an additional position information).
type LToken = Located Token

-- | The datatype containing all possible tokens in Zilch.
data Token
-------- Keywords
  = -- | Forall type-variable binder (@forall@)
    Forall
    -- | Forall type-variable binder (@∀@ unicode variant)
  | UniForall
    -- | Function definition (@def@)
  | Def
    -- | Enumeration definition (@enum@)
  | Enum
    -- | Record definition (@record@)
  | Record
    -- | Type class definition (@class@)
  | Class
    -- | Type class implementation (@impl@)
  | Impl
    -- | Additional variable binders in function definition (@where@)
  | Where
    -- | Let-expression (@let@)
  | Let
    -- | Let-expression (@in@)
  | In
    -- | Type alias definition and type kind (@alias@)
  | Alias
    -- | Pattern matching beginning delimiter (@case@)
  | Case
    -- | Pattern matching middle delimiter (@of@)
  | Of
    -- | Module definition (@module@)
  | Module
    -- | Anonymous function definition (@fn@)
  | Fn
    -- | FFI specifier on imports/exports (@foreign@)
  | Foreign
    -- | Alias creation (@as@)
  | As
    -- | Open import (@open@)
  | Open
    -- | Module/FFI import (@import@)
  | Import
    -- | Module/FFI export (@export@)
  | Export
    -- | Permission declaration (@perm@)
  | Perm
    -- | Conditional beginning delimiter (@if@)
  | If
    -- | Conditional middle delimiter (@then@)
  | Then
    -- | Conditional end delimiter (@else@)
  | Else
    -- | Pattern alias definition (@pattern@)
  | Pattern
    -- | Definition symbol (@:=@)
  | ColonEquals
    -- | Definition symbol (@≔@ unicode variant)
  | UniColonEquals
    -- | Function type/case delimiter in pattern matching (@->@)
  | RightArrow
    -- | Function type/case delimiter in pattern matching (@→@ unicode variant)
  | UniRightArrow
    -- | Wildcard (@_@)
  | Underscore
    -- | Wildcard (@·@ unicode variant)
  | UniUnderscore
    -- | Dot record access (@.@)
  | Dot
    -- | Typed hole (@?@)
  | Question
    -- | Left parenthesis (@(@)
  | LParen
    -- | Right parenthesis (@)@)
  | RParen
    -- | Left square bracket (@[@)
  | LBrack
    -- | Right square bracket (@]@)
  | RBrack
    -- | Left curly bracket (@{@)
  | LBrace
    -- | Right curly bracket (@}@)
  | RBrace
    -- | Comma (@,@)
  | Comma
    -- | Type specifier (@:@)
  | Colon
    -- | Meta-specifier beginning delimiter (@#@)
  | Hash
    -- | Left angle (@<@)
  | LAngle
    -- | Right angle (@>@)
  | RAngle
    -- | Identifier composed of only alphanumerical letters
  | Identifier Text
    -- | Operator symbol
  | Operator Text
    -- | Inline comments (@-- ...@)
  | InlineComment Text
    -- | Integral numbers
  | Integer Text
    -- | Floating point numbers
  | Float Text
    -- | String
  | String Text
    -- | Character
  | Character Text
  deriving (Show, Eq)

-- | A static list of keywords to check mixfix operator backbones.
staticKeywordsList :: [Text]
staticKeywordsList =
  [ -- keywords
    "forall", "∀"
  , "def", "enum", "record", "alias", "class", "impl"
  , "where"
  , "let", "in", "case", "of", "fn", "if", "then", "else"
  , "module", "import", "open", "foreign", "export", "as"
  , "pattern"
    -- reserved symbols
  , ":=", "≔"
  , "->", "→"
  , "<-", "←"
  , "<:"
  , "_", "·"
  , "."
  , "--"
  , "?"
  , ":"
  ]
