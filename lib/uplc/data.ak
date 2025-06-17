pub type PlutusData {
  Constr { tag: Int, any_constructor: Option<Int>, fields: List<PlutusData> }

  Map(List<(PlutusData, PlutusData)>)

  Int(Int)

  BoundedBytes(ByteArray)

  Array(List<PlutusData>)
}
