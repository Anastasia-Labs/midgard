use uplc/data.{PlutusData}
use uplc/tipo.{Type}

pub type Constant {
  Integer(Int)
  ByteString(ByteArray)
  String(String)
  Unit
  Bool(Bool)
  ProtoList(Type, List<Constant>)
  ProtoPair(Type, Type, Constant, Constant)
  Data(PlutusData)
}
