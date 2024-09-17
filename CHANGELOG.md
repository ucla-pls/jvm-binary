# Change log


## Version 0.14.0

- Fix problem with method parameter with zero index

## Version 0.13.0

- Many changes; started to record again.

## Version 0.9.1
- Upgrade to GHC 8.10.4

## Version 0.9.0
- Changed attributeError to evolveError
- Added new instruction NewArray to correctly model array behaviour
- Massive rewrite of AbsMethodId and friends to make them easier to 
  work with
- Changed ClassType in Signature to contain the inner classes in 
  the correct order.
- Fixed problem with defintion of TypedAnnotations
- Fixed spelling mistake in TypeParameter
- Added MethodParameters
- Made more Annotations Ord'able and exported them fully
- Make some data types into newtypes

## Version 0.8.1
- Made all tests work with nix

## Version 0.8.0 
-  Upgrade to GHC 8.6.4

## Version 0.7.0 
- Create instances for InClass
- Fixed bad encoding of VerificationTypeInfo

## Version 0.6.1
- Fix a mistake in anewarray

## Version 0.6.0
- Add Annotations

## Version 0.5.0
- Reintroduce AbsVariableMethodId
- Small cleanups

## Version 0.4.0

- Add Offsets to ByteCode
- Small other changes

## Version 0.3.0

- Change API of TypeParse
- Merge together the New* commands

## Version 0.2.0

- Add filter to evolveClassFile

## Version 0.1.1

- Add EnclosingMethod Attribute
- Add InnerClasses Attribute
- Add ICAccessFlag 
- Add parseing and writing of Signatures
- General optimizations

## Version 0.1.0 

- Fix documentation
- Remove ConstantRef
- Add better method and field descriptions 
- Introducing a stageing system
- Added Code writing
- Changed Megaparsec to Attoparsec of improved performance.
- A lot of extra work

- Add Attributes 
  - LineNumberTable

## Version 0.0.2

- Fix problems with package.yaml file
- Add ConstantValue 
- Add Exceptions
- Add StackMapTable
- Add BootstrapMethods

## Version 0.0.1

The initial version

