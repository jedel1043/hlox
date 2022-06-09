{-# LANGUAGE NamedFieldPuns #-}

module Lib.Interpreter.Error (RuntimeError (..)) where

data RuntimeError
    = InvalidNumberOperand
    | InvalidSumOperand
    | InvalidPrimitiveOperand
    | InvalidCallee
    | ComplexObjectStringCast
    | ReferenceError String
    | ControlFlow String
    | InvalidArgsLen {callerLen :: Int, calleeLen :: Int}
    | InvalidAnonFun
    | InvalidAncestorIdx Int
    | InvalidObjectAccess
    | UndefinedProperty String
    | InvalidSuperClass
    | InvalidSuperReference
    | InvalidObjectReference

instance Show RuntimeError where
    show InvalidNumberOperand = "Operands must be numbers."
    show InvalidPrimitiveOperand = "Operands must be primitives and comparable."
    show InvalidSumOperand = "Operands of sum must be numbers or a string and a type castable to string."
    show ComplexObjectStringCast = "Cannot cast complex object to string."
    show (ReferenceError str) = "Undefined variable `" ++ str ++ "`."
    show InvalidCallee = "Can only call functions and classes."
    show (ControlFlow path) = "Detected invalid control flow status " ++ path ++ "."
    show (InvalidArgsLen {callerLen, calleeLen}) =
        "Expected "
            ++ show calleeLen
            ++ " arguments but got "
            ++ show callerLen
            ++ "."
    show InvalidAnonFun = "Anonymous function declarations are not allowed."
    show (InvalidAncestorIdx idx) = "Tried to get the invalid ancestor offset `" ++ show idx ++ "`."
    show InvalidObjectAccess = "Only instances have properties."
    show (UndefinedProperty prop) = "Undefined property `" ++ prop ++ "`."
    show InvalidSuperClass = "Superclass must be a class."
    show InvalidSuperReference = "`super` must point to a valid class."
    show InvalidObjectReference = "`this` must point to a valid object."
