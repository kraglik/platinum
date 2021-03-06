{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Platinum.AST where


import Data.Data
import Data.Text


data SrcPos
    = SrcPos
    { sourceStartLine       :: !Int
    , sourceStartColumn     :: !Int
    , sourceEndLine         :: !Int
    , sourceEndColumn       :: !Int
    , sourcePath            :: !Text
    }
    deriving (Eq, Show, Data)


class Positioned a where
    position :: a -> SrcPos


data TypeVar    = TypeVar   !Text !SrcPos deriving (Eq, Show, Data)
data Var        = Var       !Text !SrcPos deriving (Eq, Show, Data)
data Cons       = Cons      !Text !SrcPos deriving (Eq, Show, Data)
data Cls        = Cls       !Text !SrcPos deriving (Eq, Show, Data)


instance Positioned TypeVar where
    position (TypeVar _ p) = p


instance Positioned Var where
    position (Var _ p) = p


instance Positioned Cons where
    position (Cons _ p) = p


instance Positioned Cls where
    position (Cls _ p) = p


data Suite = Suite ![Statement] !SrcPos
    deriving (Eq, Show, Data)


instance Positioned Suite where
    position (Suite _ p) = p


data Module
    = Module
        { modulePath :: ![Text]
        , moduleImports :: ![Import]
        , moduleRecords :: ![Record]
        , moduleSumTypes :: ![SumType]
        , moduleClasses :: ![Class]
        , moduleInstances :: ![Instance]
        , moduleBindings :: ![Binding]
        , moduleFunctions :: ![Function]
        , moduleTypeInfo :: ![TypeInfo]
        }
    deriving (Eq, Show, Data)


data Import
    = Import ![Text] !SrcPos
    | ImportAs ![Text] !Text !SrcPos
    | ImportShowing ![Text] ![ImportItem] !SrcPos
    | ImportHiding ![Text] ![Text] !SrcPos
    deriving (Eq, Show, Data)


data ImportItem
    = ImportItem !Text !(Maybe Text) !SrcPos
    deriving (Eq, Show, Data)


data Definition
    = Fun !Function
    | Bind !Binding
    | TypeInfo !TypeInfo
    deriving (Eq, Show, Data)


data Class
    = Class !Context !Cls ![TypeVar] ![Definition] !SrcPos
    deriving (Eq, Show, Data)


data Instance
    = Instance !Context !Cls ![Signature] ![Definition] !SrcPos
    deriving (Eq, Show, Data)


data TypeInfo
    = TypeAlias !Cons !Scheme !SrcPos
    | TypeSig !Var !Scheme !SrcPos
    deriving (Eq, Show, Data)


data Record
    = Record 
        { recordForall      :: !Forall 
        , recordContext     :: !Context
        , recordName        :: !Cons
        , recordVars        :: ![TypeVar]
        , recordFields      :: ![(Var, Scheme)]
        }
    deriving (Eq, Show, Data)


data SumType
    = SumType
        { sumTypeForall     :: !Forall 
        , sumTypeContext    :: !Context
        , sumTypeName       :: !Cons
        , sumTypeVars       :: ![TypeVar]
        , sumTypeVariants   :: ![Variant]
        }
    deriving (Eq, Show, Data)


data Function
    = Function 
        { publicFunction    :: !Bool
        , functionArgs      :: ![Var]
        , functionBody      :: !Suite
        }
    deriving (Eq, Show, Data)


data Binding 
    = Binding 
        { publicBinding     :: !Bool
        , bindingName       :: !Var
        , bindingValue      :: !Expression
        , bindingType       :: !(Maybe Scheme)
        , bindingPos        :: !SrcPos
        }
    deriving (Eq, Show, Data)


data Variant
    = UnnamedVariant !Forall !Context ![Signature] !SrcPos
    | NamedVariant ![(Var, Scheme)] !SrcPos
    deriving (Eq, Show, Data)


data Statement
    = Expr !Expression !SrcPos
    | For !Iterator !Suite !SrcPos
    | While !Expression !Suite !SrcPos
    | Definition !Definition !SrcPos
    | Let !Bool !Var !Expression !(Maybe Scheme) !SrcPos
    | TypeInformation !TypeInfo
    | Func !Function
    | Return !Expression !SrcPos
    deriving (Eq, Show, Data)


data Expression
    = Variable          !Var !SrcPos
    | Constructor       !Cons ![Expression] !SrcPos
    | RecordUpdate      !Var ![(Var, Expression)] !SrcPos
    | RecordInstance    !Cons ![(Var, Expression)] !SrcPos
    | Literal           !Literal !SrcPos
    | Tuple             ![Expression] !SrcPos
    | List              ![Expression] !SrcPos
    | Ellipsis          !SrcPos
    | ListComprehension !Expression ![Iterator] !SrcPos
    | If                !Expression !Expression !(Maybe Expression)
    | Match             !Expression ![Guard] !SrcPos
    | RecordField       !Expression !Var !SrcPos
    | Call              !Expression ![Expression] !SrcPos
    | InfixCall         !Expression !Var !Expression !SrcPos
    | PrefixCall        !Var !Expression !SrcPos
    | Lambda            ![Var] !Suite !SrcPos
    deriving (Eq, Show, Data)


data Signature 
    = Arrow !Signature !Signature !SrcPos
    | TypeOperator !Cons ![Signature] !SrcPos
    | TypeVariable !TypeVar !SrcPos
    | TupleType ![Signature] !SrcPos
    | ListType !Signature !SrcPos
    deriving (Eq, Show, Data)


data Assumption
    = IsInstance !Cls ![TypeVar] !SrcPos
    deriving (Eq, Show, Data)


data Context = Context ![Assumption] !SrcPos deriving (Eq, Show, Data)

data Forall = Forall ![TypeVar] !SrcPos deriving (Eq, Show, Data)

data Scheme = Scheme !Forall !Context !Signature !SrcPos deriving (Eq, Show, Data)


data Iterator
    = Iterator 
        { iterable      :: !Expression
        , target        :: !Expression
        , iteratorPos   :: !SrcPos
        }
    deriving (Eq, Show, Data)


data Guard
    = Guard
        { matchExpr :: !Expression
        , matchCond :: !(Maybe Expression)
        , matchBody :: !Suite
        }
    deriving (Eq, Show, Data)


data Literal
    = Integer   !Integer
    | Float     !Double
    | Char      !Char
    | String    !Text
    deriving (Eq, Show, Data)
