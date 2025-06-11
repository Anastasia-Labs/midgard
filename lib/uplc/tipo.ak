pub type Type {
  TBool
  TInteger
  TString
  TByteString
  TUnit
  TList(Type)
  TPair(Type, Type)
  TData
}
