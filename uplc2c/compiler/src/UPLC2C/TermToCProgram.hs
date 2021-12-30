{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.TermToCProgram ( termToCProgram ) where


import Data.Text (pack)
import PlutusCore.DeBruijn (DeBruijn (..), Index (..))
import PlutusCore.Default (DefaultFun (..))
import qualified UntypedPlutusCore.Core.Type as UPLC

import UPLC2C.Prelude
import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition (..))
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.CProgramBuilder (CProgramBuilder (..))
import UPLC2C.Types.DeBruijnIndex (DeBruijnIndex (..))
import UPLC2C.Types.UPLCTerm (UPLCTerm)


termToCProgram :: ( Monad m, CProgramBuilder m ) => UPLCTerm -> m CName
termToCProgram =
  \case
    UPLC.Var _ ix@(DeBruijn (Index i)) -> do
      let name = deBruijnIndexToCName ix
      addToProgram name (VariableReference (DeBruijnIndex (fromIntegral i)))
      return name
    UPLC.LamAbs _ _ subterm -> do
      subtermName <- termToCProgram subterm
      absName <- genSym
      addToProgram absName (CreateClosureOver subtermName)
      return absName
    UPLC.Apply _ operator operand -> do
      operatorName <- termToCProgram operator
      operandName <- termToCProgram operand
      applyName <- genSym
      addToProgram applyName (Apply operatorName operandName)
      return applyName
    UPLC.Force _ operand -> do
      operandName <- termToCProgram operand
      forceName <- genSym
      addToProgram forceName (Force operandName)
      return forceName
    UPLC.Delay _ operand -> do
      operandName <- termToCProgram operand
      delayName <- genSym
      addToProgram delayName (Delay operandName)
      return delayName
    UPLC.Constant _ _ -> todo
    UPLC.Builtin _ f -> return (builtinToCName f)
    UPLC.Error _ -> return (CName "builtin_error")


deBruijnIndexToCName :: DeBruijn -> CName
deBruijnIndexToCName (DeBruijn i) = CName $ "lookup_var_" <> pack (show i)


builtinToCName :: DefaultFun -> CName
builtinToCName =
  \case
    AddInteger -> CName "builtin_add_integer"
    SubtractInteger -> CName "builtin_subtract_integer"
    MultiplyInteger -> CName "builtin_multiply_integer"
    DivideInteger -> CName "builtin_divide_integer"
    QuotientInteger -> CName "builtin_quotient_integer"
    RemainderInteger -> CName "builtin_remainder_integer"
    ModInteger -> CName "builtin_mod_integer"
    EqualsInteger -> CName "builtin_equals_integer"
    LessThanInteger -> CName "builtin_less_integer"
    LessThanEqualsInteger -> CName "builtin_leq_integer"
    AppendByteString -> CName "builtin_append_bytestring"
    ConsByteString -> CName "builtin_cons_bytestring"
    SliceByteString -> CName "builtin_slice_bytestring"
    LengthOfByteString -> CName "builtin_length_bytestring"
    IndexByteString -> CName "builtin_index_bytestring"
    EqualsByteString -> CName "builtin_equals_bytestring"
    LessThanByteString -> CName "builtin_less_bytestring"
    LessThanEqualsByteString -> CName "builtin_leq_bytestring"
    Sha2_256 -> CName "builtin_sha2_256"
    Sha3_256 -> CName "builtin_sha3_256"
    Blake2b_256 -> CName "builtin_blake2b_256"
    VerifySignature -> CName "builtin_verify_signature"
    AppendString -> CName "builtin_append_string"
    EqualsString -> CName "builtin_equals_string"
    EncodeUtf8 -> CName "builtin_encode_utf8"
    DecodeUtf8 -> CName "builtin_decode_utf8"
    IfThenElse -> CName "builtin_if_then_else"
    ChooseUnit -> CName "builtin_choose_unit"
    Trace -> CName "builtin_trace"
    FstPair -> CName "builtin_fst_pair"
    SndPair -> CName "builtin_snd_pair"
    ChooseList -> CName "builtin_choose_list"
    MkCons -> CName "builtin_mk_cons"
    HeadList -> CName "builtin_head_list"
    TailList -> CName "builtin_tail_list"
    NullList -> CName "builtin_null_list"
    ChooseData -> CName "builtin_choose_data"
    ConstrData -> CName "builtin_constr_data"
    MapData -> CName "builtin_map_data"
    ListData -> CName "builtin_list_data"
    IData -> CName "builtin_idata"
    BData -> CName "builtin_bdata"
    UnConstrData -> CName "builtin_un_constr_data"
    UnMapData -> CName "builtin_un_map_data"
    UnListData -> CName "builtin_un_list_data"
    UnIData -> CName "builtin_un_idata"
    UnBData -> CName "builtin_un_bdata"
    EqualsData -> CName "builtin_equals_data"
    MkPairData -> CName "builtin_mk_pair_data"
    MkNilData -> CName "builtin_mk_nil_data"
    MkNilPairData -> CName "builtin_mk_nil_pair_data"


todo :: a
todo = todo
