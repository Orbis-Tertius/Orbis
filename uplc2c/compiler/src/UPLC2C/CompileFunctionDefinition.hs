{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}


module UPLC2C.CompileFunctionDefinition ( compileFunctionDefinition ) where


import Data.Text (pack)
import Text.Printf (printf)
import Text.RawString.QQ (r)

import UPLC2C.Prelude
import UPLC2C.Types.CCode (CCode (..))
import UPLC2C.Types.CFunctionDefinition (CFunctionDefinition (..))
import UPLC2C.Types.CName (CName (..))
import UPLC2C.Types.DeBruijnIndex (DeBruijnIndex (..))


compileFunctionDefinition :: CName -> CFunctionDefinition -> CCode
compileFunctionDefinition name (VariableReference i) =
  compileVariableReference name i
compileFunctionDefinition name (CreateClosureOver ref) =
  compileCreateClosure name ref
compileFunctionDefinition name (Apply operator operand) =
  compileApply name operator operand
compileFunctionDefinition name (Delay operand) =
  compileDelay name operand
compileFunctionDefinition name (Force operand) =
  compileForce name operand


compileVariableReference :: CName -> DeBruijnIndex -> CCode
compileVariableReference (CName fnName) (DeBruijnIndex i) =
  CCode . pack $ printf variableReferenceTemplate fnName i


variableReferenceTemplate :: String
variableReferenceTemplate =
  [r|
struct NFData *%s (struct LexicalScope *scope) {
  for (WORD i = 1; i < %i; i++) {
    scope = scope->rest;
  }
  return scope->first;
}
  |]


compileCreateClosure :: CName -> CName -> CCode
compileCreateClosure (CName name) (CName ref) =
  CCode . pack $ printf createClosureTemplate name ref


createClosureTemplate :: String
createClosureTemplate =
  [r|
struct NFData *%s (struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = FunctionType;
  result->value.fn.apply = &%s;
  result->value.fn.data = scope;
  return result;
}
  |]


compileApply :: CName -> CName -> CName -> CCode
compileApply (CName name) (CName operator) (CName operand) =
  CCode . pack $ printf applyTemplate name operator operand


applyTemplate :: String
applyTemplate =
  [r|
struct NFData *%s (struct LexicalScope *scope) {
  struct NFData *operatorResult = %s(scope);
  struct NFData *operandResult = %s(scope);
  if (operatorResult->type == FunctionType) {
    LexicalScope *new_scope = (LexicalScope *)alloc(sizeof(LexicalScope));
    new_scope->first = operandResult
    new_scope->rest = scope;
    return operandResult->value.fn.apply(new_scope);
  } else {
    diverge();
  }
}
  |]


compileDelay :: CName -> CName -> CCode
compileDelay (CName name) (CName operand) =
  CCode . pack $ printf delayTemplate name operand


delayTemplate :: String
delayTemplate =
  [r|
struct NFData *%s (struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = ThunkType;
  result->value.thunk.run = %s;
  result->value.thunk.data = scope;
  return result;
}
  |]


compileForce :: CName -> CName -> CCode
compileForce (CName name) (CName operand) =
  CCode . pack $ printf forceTemplate name operand


forceTemplate :: String
forceTemplate =
  [r|
struct NFData *%s (struct LexicalScope *scope) {
  struct NFData *operandResult = %s(scope);
  if (operandResult->type == ThunkType) {
    Thunk thunk = operandResult->value.thunk;
    return thunk.run(thunk.data);
  } else {
    diverge();
  }
}
  |]
