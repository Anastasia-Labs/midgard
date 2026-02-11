pub const add_integer = 0

pub const subtract_integer = 1

pub const multiply_integer = 2

pub const divide_integer = 3

pub const quotient_integer = 4

pub const remainder_integer = 5

pub const mod_integer = 6

pub const equals_integer = 7

pub const less_than_integer = 8

pub const less_than_equals_integer = 9

pub const append_byte_string = 10

pub const cons_byte_string = 11

pub const slice_byte_string = 12

pub const length_of_byte_string = 13

pub const index_byte_string = 14

pub const equals_byte_string = 15

pub const less_than_byte_string = 16

pub const less_than_equals_byte_string = 17

pub const sha2_256 = 18

pub const sha3_256 = 19

pub const blake2b_256 = 20

pub const blake2b_224 = 21

pub const keccak_256 = 22

pub const verify_ed25519_signature = 23

pub const verify_ecdsa_secp256k1_signature = 24

pub const verify_schnorr_secp256k1_signature = 25

pub const append_string = 26

pub const equals_string = 27

pub const encode_utf8 = 28

pub const decode_utf8 = 29

pub const if_then_else = 30

pub const choose_unit = 31

pub const debug = 32

pub const fst_pair = 33

pub const snd_pair = 34

pub const choose_list = 35

pub const mk_cons = 36

pub const head_list = 37

pub const tail_list = 38

pub const null_list = 39

pub const choose_data = 40

pub const constr_data = 41

pub const map_data = 42

pub const list_data = 43

pub const i_data = 44

pub const b_data = 45

pub const un_constr_data = 46

pub const un_map_data = 47

pub const un_list_data = 48

pub const un_i_data = 49

pub const un_b_data = 50

pub const equals_data = 51

pub const serialise_data = 52

pub const mk_pair_data = 53

pub const mk_nil_data = 54

pub const mk_nil_pair_data = 55

pub const bls12_381_g1_add = 56

pub const bls12_381_g1_neg = 57

pub const bls12_381_g1_scalar_mul = 58

pub const bls12_381_g1_equal = 59

pub const bls12_381_g1_compress = 60

pub const bls12_381_g1_uncompress = 61

pub const bls12_381_g1_hash_to_group = 62

pub const bls12_381_g2_add = 63

pub const bls12_381_g2_neg = 64

pub const bls12_381_g2_scalar_mul = 65

pub const bls12_381_g2_equal = 66

pub const bls12_381_g2_compress = 67

pub const bls12_381_g2_uncompress = 68

pub const bls12_381_g2_hash_to_group = 69

pub const bls12_381_miller_loop = 70

pub const bls12_381_mul_ml_result = 71

pub const bls12_381_final_verify = 72

pub const integer_to_byte_string = 73

pub const byte_string_to_integer = 74

pub const and_byte_string = 75

pub const or_byte_string = 76

pub const xor_byte_string = 77

pub const complement_byte_string = 78

pub const read_bit = 79

pub const write_bits = 80

pub const replicate_byte = 81

pub const shift_byte_string = 82

pub const rotate_byte_string = 83

pub const count_set_bits = 84

pub const find_first_set_bit = 85

pub const ripemd_160 = 86

pub const default_force_counts = {
    expect 0 = 0

    // AddInteger
    // SubtractInteger
    // MultiplyInteger
    // DivideInteger
    // QuotientInteger
    // RemainderInteger
    // ModInteger
    // EqualsInteger
    // LessThanInteger
    // LessThanEqualsInteger
    // AppendByteString
    // ConsByteString
    // SliceByteString
    // LengthOfByteString
    // IndexByteString
    // EqualsByteString
    // LessThanByteString
    // LessThanEqualsByteString
    // Sha2_256
    // Sha3_256
    // Blake2b_256
    // Blake2b_224
    // Keccak_256
    // VerifyEd25519Signature
    // VerifyEcdsaSecp256k1Signature
    // VerifySchnorrSecp256k1Signature
    // AppendString
    // EqualsString
    // EncodeUtf8
    // DecodeUtf8
    // IfThenElse
    // ChooseUnit
    // Trace
    // FstPair
    // SndPair
    // ChooseList
    // MkCons
    // HeadList
    // TailList
    // NullList
    // ChooseData
    // ConstrData
    // MapData
    // ListData
    // IData
    // BData
    // UnConstrData
    // UnMapData
    // UnListData
    // UnIData
    // UnBData
    // EqualsData
    // SerialiseData
    // MkPairData
    // MkNilData
    // MkNilPairData
    // Bls12_381_G1_Add
    // Bls12_381_G1_Neg
    // Bls12_381_G1_ScalarMul
    // Bls12_381_G1_Equal
    // Bls12_381_G1_Compress
    // Bls12_381_G1_Uncompress
    // Bls12_381_G1_HashToGroup
    // Bls12_381_G2_Add
    // Bls12_381_G2_Neg
    // Bls12_381_G2_ScalarMul
    // Bls12_381_G2_Equal
    // Bls12_381_G2_Compress
    // Bls12_381_G2_Uncompress
    // Bls12_381_G2_HashToGroup
    // Bls12_381_MillerLoop
    // Bls12_381_MulMlResult
    // Bls12_381_FinalVerify
    // IntegerToByteString
    // ByteStringToInteger
    // AndByteString
    // OrByteString
    // XorByteString
    // ComplementByteString
    // ReadBit
    // WriteBits
    // ReplicateByte
    // ShiftByteString
    // RotateByteString
    // CountSetBits
    // FindFirstSetBit
    // Ripemd_160
    #[0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      2,
      2,
      2,
      1,
      1,
      1,
      1,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
    ]
  }

pub const default_arg_counts = {
    expect 0 = 0

    // AddInteger
    // SubtractInteger
    // MultiplyInteger
    // DivideInteger
    // QuotientInteger
    // RemainderInteger
    // ModInteger
    // EqualsInteger
    // LessThanInteger
    // LessThanEqualsInteger
    // AppendByteString
    // ConsByteString
    // SliceByteString
    // LengthOfByteString
    // IndexByteString
    // EqualsByteString
    // LessThanByteString
    // LessThanEqualsByteString
    // Sha2_256
    // Sha3_256
    // Blake2b_256
    // Blake2b_224
    // Keccak_256
    // VerifyEd25519Signature
    // VerifyEcdsaSecp256k1Signature
    // VerifySchnorrSecp256k1Signature
    // AppendString
    // EqualsString
    // EncodeUtf8
    // DecodeUtf8
    // IfThenElse
    // ChooseUnit
    // Trace
    // FstPair
    // SndPair
    // ChooseList
    // MkCons
    // HeadList
    // TailList
    // NullList
    // ChooseData
    // ConstrData
    // MapData
    // ListData
    // IData
    // BData
    // UnConstrData
    // UnMapData
    // UnListData
    // UnIData
    // UnBData
    // EqualsData
    // SerialiseData
    // MkPairData
    // MkNilData
    // MkNilPairData
    // Bls12_381_G1_Add
    // Bls12_381_G1_Neg
    // Bls12_381_G1_ScalarMul
    // Bls12_381_G1_Equal
    // Bls12_381_G1_Compress
    // Bls12_381_G1_Uncompress
    // Bls12_381_G1_HashToGroup
    // Bls12_381_G2_Add
    // Bls12_381_G2_Neg
    // Bls12_381_G2_ScalarMul
    // Bls12_381_G2_Equal
    // Bls12_381_G2_Compress
    // Bls12_381_G2_Uncompress
    // Bls12_381_G2_HashToGroup
    // Bls12_381_MillerLoop
    // Bls12_381_MulMlResult
    // Bls12_381_FinalVerify
    // IntegerToByteString
    // ByteStringToInteger
    // AndByteString
    // OrByteString
    // XorByteString
    // ComplementByteString
    // ReadBit
    // WriteBits
    // ReplicateByte
    // ShiftByteString
    // RotateByteString
    // CountSetBits
    // FindFirstSetBit
    // Ripemd_160
    #[2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      3,
      1,
      2,
      2,
      2,
      2,
      1,
      1,
      1,
      1,
      1,
      3,
      3,
      3,
      2,
      2,
      1,
      1,
      3,
      2,
      2,
      1,
      1,
      3,
      2,
      1,
      1,
      1,
      6,
      2,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      2,
      1,
      2,
      1,
      1,
      2,
      1,
      2,
      2,
      1,
      1,
      2,
      2,
      1,
      2,
      2,
      1,
      1,
      2,
      2,
      2,
      2,
      3,
      2,
      3,
      3,
      3,
      1,
      2,
      3,
      2,
      2,
      2,
      1,
      1,
      1,
    ]
  }
