//// This file is primarily hand-written.
//// TODO? Write a script that generates this file from the Plutus repo.

use aiken/builtin
use aiken/collection/list
use aiken/option
use aiken/primitive/bytearray

/// Copied from Haskell implementation:
/// https://github.com/IntersectMBO/plutus/blob/c268f35b9fe69b69c7f1a11afc20a11ef7b14fc6/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L64
pub type Builtin {
  // Integers
  AddInteger
  SubtractInteger
  MultiplyInteger
  DivideInteger
  QuotientInteger
  RemainderInteger
  ModInteger
  EqualsInteger
  LessThanInteger
  LessThanEqualsInteger
  // Bytestrings
  AppendByteString
  ConsByteString
  SliceByteString
  LengthOfByteString
  IndexByteString
  EqualsByteString
  LessThanByteString
  LessThanEqualsByteString
  // Cryptography and hashes
  Sha2_256
  Sha3_256
  Blake2b_256
  VerifyEd25519Signature
  VerifyEcdsaSecp256k1Signature
  VerifySchnorrSecp256k1Signature
  // Strings
  AppendString
  EqualsString
  EncodeUtf8
  DecodeUtf8
  // Bool
  IfThenElse
  // Unit
  ChooseUnit
  // Tracing
  Trace
  // Pairs
  FstPair
  SndPair
  // Lists
  ChooseList
  MkCons
  HeadList
  TailList
  NullList
  // Data
  ChooseData
  ConstrData
  MapData
  ListData
  IData
  BData
  UnConstrData
  UnMapData
  UnListData
  UnIData
  UnBData
  EqualsData
  SerialiseData
  // Misc monomorphized constructors
  MkPairData
  MkNilData
  MkNilPairData
  // BLS12_381 operations
  // G1
  Bls12_381_G1_add
  Bls12_381_G1_neg
  Bls12_381_G1_scalarMul
  Bls12_381_G1_equal
  Bls12_381_G1_hashToGroup
  Bls12_381_G1_compress
  Bls12_381_G1_uncompress
  // G2
  Bls12_381_G2_add
  Bls12_381_G2_neg
  Bls12_381_G2_scalarMul
  Bls12_381_G2_equal
  Bls12_381_G2_hashToGroup
  Bls12_381_G2_compress
  Bls12_381_G2_uncompress
  // Pairing
  Bls12_381_millerLoop
  Bls12_381_mulMlResult
  Bls12_381_finalVerify
  // Keccak_256, Blake2b_224
  Keccak_256
  Blake2b_224
  // Conversions
  IntegerToByteString
  ByteStringToInteger
  // Logical
  AndByteString
  OrByteString
  XorByteString
  ComplementByteString
  ReadBit
  WriteBits
  ReplicateByte
  // Bitwise
  ShiftByteString
  RotateByteString
  CountSetBits
  FindFirstSetBit
  // Ripemd_160
  Ripemd_160
  // Batch 6
  ExpModInteger
  DropList
  // Arrays
  LengthOfArray
  ListToArray
  IndexArray
}

type DefaultBuiltinInfo {
  builtin: Builtin,
  arity: Int,
  force_count: Int,
}

const defaults: List<DefaultBuiltinInfo> =
  [
    DefaultBuiltinInfo { builtin: AddInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: SubtractInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: MultiplyInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: DivideInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: QuotientInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: RemainderInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: ModInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: EqualsInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: LessThanInteger, arity: 2, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: LessThanEqualsInteger,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: AppendByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: ConsByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: SliceByteString, arity: 3, force_count: 0 },
    DefaultBuiltinInfo { builtin: LengthOfByteString, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: IndexByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: EqualsByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: LessThanByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: LessThanEqualsByteString,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: Sha2_256, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: Sha3_256, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: Blake2b_256, arity: 1, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: VerifyEd25519Signature,
      arity: 3,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: VerifyEcdsaSecp256k1Signature,
      arity: 3,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: VerifySchnorrSecp256k1Signature,
      arity: 3,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: AppendString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: EqualsString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: EncodeUtf8, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: DecodeUtf8, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: IfThenElse, arity: 3, force_count: 1 },
    DefaultBuiltinInfo { builtin: ChooseUnit, arity: 2, force_count: 1 },
    DefaultBuiltinInfo { builtin: Trace, arity: 2, force_count: 1 },
    DefaultBuiltinInfo { builtin: FstPair, arity: 1, force_count: 2 },
    DefaultBuiltinInfo { builtin: SndPair, arity: 1, force_count: 2 },
    DefaultBuiltinInfo { builtin: ChooseList, arity: 3, force_count: 2 },
    DefaultBuiltinInfo { builtin: MkCons, arity: 2, force_count: 1 },
    DefaultBuiltinInfo { builtin: HeadList, arity: 1, force_count: 1 },
    DefaultBuiltinInfo { builtin: TailList, arity: 1, force_count: 1 },
    DefaultBuiltinInfo { builtin: NullList, arity: 1, force_count: 1 },
    DefaultBuiltinInfo { builtin: ChooseData, arity: 6, force_count: 1 },
    DefaultBuiltinInfo { builtin: ConstrData, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: MapData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: ListData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: IData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: BData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: UnConstrData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: UnMapData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: UnListData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: UnIData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: UnBData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: EqualsData, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: SerialiseData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: MkPairData, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: MkNilData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: MkNilPairData, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: Bls12_381_G1_add, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: Bls12_381_G1_neg, arity: 1, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G1_scalarMul,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: Bls12_381_G1_equal, arity: 2, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G1_hashToGroup,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G1_compress,
      arity: 1,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G1_uncompress,
      arity: 1,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: Bls12_381_G2_add, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: Bls12_381_G2_neg, arity: 1, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G2_scalarMul,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: Bls12_381_G2_equal, arity: 2, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G2_hashToGroup,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G2_compress,
      arity: 1,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_G2_uncompress,
      arity: 1,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_millerLoop,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_mulMlResult,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: Bls12_381_finalVerify,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: Keccak_256, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: Blake2b_224, arity: 1, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: IntegerToByteString,
      arity: 3,
      force_count: 0,
    },
    DefaultBuiltinInfo {
      builtin: ByteStringToInteger,
      arity: 2,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: AndByteString, arity: 3, force_count: 0 },
    DefaultBuiltinInfo { builtin: OrByteString, arity: 3, force_count: 0 },
    DefaultBuiltinInfo { builtin: XorByteString, arity: 3, force_count: 0 },
    DefaultBuiltinInfo {
      builtin: ComplementByteString,
      arity: 1,
      force_count: 0,
    },
    DefaultBuiltinInfo { builtin: ReadBit, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: WriteBits, arity: 3, force_count: 0 },
    DefaultBuiltinInfo { builtin: ReplicateByte, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: ShiftByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: RotateByteString, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: CountSetBits, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: FindFirstSetBit, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: Ripemd_160, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: ExpModInteger, arity: 3, force_count: 0 },
    DefaultBuiltinInfo { builtin: DropList, arity: 2, force_count: 0 },
    DefaultBuiltinInfo { builtin: LengthOfArray, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: ListToArray, arity: 1, force_count: 0 },
    DefaultBuiltinInfo { builtin: IndexArray, arity: 2, force_count: 0 },
  ]

pub fn get_builtin_arity(b: Builtin) -> Int {
  expect list.length(defaults) == 91
  defaults
    |> list.find(fn(dbi: DefaultBuiltinInfo) { dbi.builtin == b })
    |> option.map(
        fn(dbi: DefaultBuiltinInfo) { builtin.unconstr_index(dbi.arity) },
      )
    |> option.or_else(0)
}

pub const add_integer = AddInteger |> builtin.unconstr_index

pub const subtract_integer = SubtractInteger |> builtin.unconstr_index

pub const multiply_integer = MultiplyInteger |> builtin.unconstr_index

pub const divide_integer = DivideInteger |> builtin.unconstr_index

pub const quotient_integer = QuotientInteger |> builtin.unconstr_index

pub const remainder_integer = RemainderInteger |> builtin.unconstr_index

pub const mod_integer = ModInteger |> builtin.unconstr_index

pub const equals_integer = EqualsInteger |> builtin.unconstr_index

pub const less_than_integer = LessThanInteger |> builtin.unconstr_index

pub const less_than_equals_integer =
  LessThanEqualsInteger |> builtin.unconstr_index

pub const append_byte_string = AppendByteString |> builtin.unconstr_index

pub const cons_byte_string = ConsByteString |> builtin.unconstr_index

pub const slice_byte_string = SliceByteString |> builtin.unconstr_index

pub const length_of_byte_string = LengthOfByteString |> builtin.unconstr_index

pub const index_byte_string = IndexByteString |> builtin.unconstr_index

pub const equals_byte_string = EqualsByteString |> builtin.unconstr_index

pub const less_than_byte_string = LessThanByteString |> builtin.unconstr_index

pub const less_than_equals_byte_string =
  LessThanEqualsByteString |> builtin.unconstr_index

pub const sha2_256 = Sha2_256 |> builtin.unconstr_index

pub const sha3_256 = Sha3_256 |> builtin.unconstr_index

pub const blake2b_256 = Blake2b_256 |> builtin.unconstr_index

pub const verify_ed25519_signature =
  VerifyEd25519Signature |> builtin.unconstr_index

pub const verify_ecdsa_secp256k1_signature =
  VerifyEcdsaSecp256k1Signature |> builtin.unconstr_index

pub const verify_schnorr_secp256k1_signature =
  VerifySchnorrSecp256k1Signature |> builtin.unconstr_index

pub const append_string = AppendString |> builtin.unconstr_index

pub const equals_string = EqualsString |> builtin.unconstr_index

pub const encode_utf8 = EncodeUtf8 |> builtin.unconstr_index

pub const decode_utf8 = DecodeUtf8 |> builtin.unconstr_index

pub const if_then_else = IfThenElse |> builtin.unconstr_index

pub const choose_unit = ChooseUnit |> builtin.unconstr_index

pub const debug = Trace |> builtin.unconstr_index

pub const fst_pair = FstPair |> builtin.unconstr_index

pub const snd_pair = SndPair |> builtin.unconstr_index

pub const choose_list = ChooseList |> builtin.unconstr_index

pub const mk_cons = MkCons |> builtin.unconstr_index

pub const head_list = HeadList |> builtin.unconstr_index

pub const tail_list = TailList |> builtin.unconstr_index

pub const null_list = NullList |> builtin.unconstr_index

pub const choose_data = ChooseData |> builtin.unconstr_index

pub const constr_data = ConstrData |> builtin.unconstr_index

pub const map_data = MapData |> builtin.unconstr_index

pub const list_data = ListData |> builtin.unconstr_index

pub const i_data = IData |> builtin.unconstr_index

pub const b_data = BData |> builtin.unconstr_index

pub const un_constr_data = UnConstrData |> builtin.unconstr_index

pub const un_map_data = UnMapData |> builtin.unconstr_index

pub const un_list_data = UnListData |> builtin.unconstr_index

pub const un_i_data = UnIData |> builtin.unconstr_index

pub const un_b_data = UnBData |> builtin.unconstr_index

pub const equals_data = EqualsData |> builtin.unconstr_index

pub const serialise_data = SerialiseData |> builtin.unconstr_index

pub const mk_pair_data = MkPairData |> builtin.unconstr_index

pub const mk_nil_data = MkNilData |> builtin.unconstr_index

pub const mk_nil_pair_data = MkNilPairData |> builtin.unconstr_index

pub const bls12_381_g1_add = Bls12_381_G1_add |> builtin.unconstr_index

pub const bls12_381_g1_neg = Bls12_381_G1_neg |> builtin.unconstr_index

pub const bls12_381_g1_scalar_mul =
  Bls12_381_G1_scalarMul |> builtin.unconstr_index

pub const bls12_381_g1_equal = Bls12_381_G1_equal |> builtin.unconstr_index

pub const bls12_381_g1_hash_to_group =
  Bls12_381_G1_hashToGroup |> builtin.unconstr_index

pub const bls12_381_g1_compress =
  Bls12_381_G1_compress |> builtin.unconstr_index

pub const bls12_381_g1_uncompress =
  Bls12_381_G1_uncompress |> builtin.unconstr_index

pub const bls12_381_g2_add = Bls12_381_G2_add |> builtin.unconstr_index

pub const bls12_381_g2_neg = Bls12_381_G2_neg |> builtin.unconstr_index

pub const bls12_381_g2_scalar_mul =
  Bls12_381_G2_scalarMul |> builtin.unconstr_index

pub const bls12_381_g2_equal = Bls12_381_G2_equal |> builtin.unconstr_index

pub const bls12_381_g2_hash_to_group =
  Bls12_381_G2_hashToGroup |> builtin.unconstr_index

pub const bls12_381_g2_compress =
  Bls12_381_G2_compress |> builtin.unconstr_index

pub const bls12_381_g2_uncompress =
  Bls12_381_G2_uncompress |> builtin.unconstr_index

pub const bls12_381_miller_loop = Bls12_381_millerLoop |> builtin.unconstr_index

pub const bls12_381_mul_ml_result =
  Bls12_381_mulMlResult |> builtin.unconstr_index

pub const bls12_381_final_verify =
  Bls12_381_finalVerify |> builtin.unconstr_index

pub const keccak_256 = Keccak_256 |> builtin.unconstr_index

pub const blake2b_224 = Blake2b_224 |> builtin.unconstr_index

pub const integer_to_byte_string = IntegerToByteString |> builtin.unconstr_index

pub const byte_string_to_integer = ByteStringToInteger |> builtin.unconstr_index

pub const and_byte_string = AndByteString |> builtin.unconstr_index

pub const or_byte_string = OrByteString |> builtin.unconstr_index

pub const xor_byte_string = XorByteString |> builtin.unconstr_index

pub const complement_byte_string =
  ComplementByteString |> builtin.unconstr_index

pub const read_bit = ReadBit |> builtin.unconstr_index

pub const write_bits = WriteBits |> builtin.unconstr_index

pub const replicate_byte = ReplicateByte |> builtin.unconstr_index

pub const shift_byte_string = ShiftByteString |> builtin.unconstr_index

pub const rotate_byte_string = RotateByteString |> builtin.unconstr_index

pub const count_set_bits = CountSetBits |> builtin.unconstr_index

pub const find_first_set_bit = FindFirstSetBit |> builtin.unconstr_index

pub const ripemd_160 = Ripemd_160 |> builtin.unconstr_index

pub const exp_mod_integer = ExpModInteger |> builtin.unconstr_index

pub const drop_list = DropList |> builtin.unconstr_index

pub const length_of_array = LengthOfArray |> builtin.unconstr_index

pub const list_to_array = ListToArray |> builtin.unconstr_index

pub const index_array = IndexArray |> builtin.unconstr_index

fn bytes_to_bytearray(bytes: List<Int>) -> ByteArray {
  bytes |> list.foldr(#"", fn(byte, acc) { acc |> bytearray.push(byte) })
}

pub const default_force_counts = {
    expect 0 = 0
    let bytes =
      defaults |> list.map(fn(dbi: DefaultBuiltinInfo) { dbi.force_count })
    bytes |> bytes_to_bytearray
  }

pub const default_arg_counts = {
    expect 0 = 0
    let bytes = defaults |> list.map(fn(dbi: DefaultBuiltinInfo) { dbi.arity })
    bytes |> bytes_to_bytearray
  }
