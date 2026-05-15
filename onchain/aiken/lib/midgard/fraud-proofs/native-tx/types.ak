pub const native_tx_version: Int = 1

pub type MidgardTxValidity {
  TxIsValid
  NonExistentInputUtxo
  InvalidSignature
  FailedScript
  FeeTooLow
  UnbalancedTx
}

pub type MidgardTxInput {
  tx_id: ByteArray,
  output_index: Int,
}

pub type MidgardCredential {
  PubKeyCredential(ByteArray)
  ScriptCredential(ByteArray)
}

pub type MidgardAddress {
  protected: Bool,
  network_id: Int,
  payment_credential: MidgardCredential,
  stake_credential: Option<MidgardCredential>,
}

pub type MidgardValue {
  lovelace: Int,
  assets: List<Pair<ByteArray, Int>>,
}

pub type MidgardScriptLanguage {
  NativeCardanoScript
  PlutusV3Script
  MidgardV1Script
}

pub type MidgardVersionedScript {
  language: MidgardScriptLanguage,
  script_bytes: ByteArray,
}

pub type MidgardTxOutput {
  address: MidgardAddress,
  value: MidgardValue,
  datum_cbor: Option<ByteArray>,
  script_ref: Option<MidgardVersionedScript>,
}

pub type MidgardAddressWitness {
  verification_key: ByteArray,
  signature: ByteArray,
}

pub type MidgardScriptWitness =
  MidgardVersionedScript

pub type MidgardRedeemerPurpose {
  SpendRedeemer
  MintRedeemer
  CertRedeemer
  RewardRedeemer
  VoteRedeemer
  ProposeRedeemer
  ReceiveRedeemer
}

pub type MidgardExecutionUnits {
  memory: Int,
  steps: Int,
}

pub type MidgardRedeemerWitness {
  purpose: MidgardRedeemerPurpose,
  index: Int,
  redeemer_cbor: ByteArray,
  execution_units: MidgardExecutionUnits,
}

pub type MidgardTransactionBody {
  inputs: List<MidgardTxInput>,
  reference_inputs: List<MidgardTxInput>,
  outputs: List<MidgardTxOutput>,
  fee: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
  required_observers: List<ByteArray>,
  required_signers: List<ByteArray>,
  mint: Data,
  script_integrity_hash: ByteArray,
  auxiliary_data_hash: ByteArray,
  network_id: Int,
}

pub type MidgardTransactionWitnessSet {
  addr_tx_wits: List<MidgardAddressWitness>,
  script_tx_wits: List<MidgardScriptWitness>,
  redeemer_tx_wits: List<MidgardRedeemerWitness>,
}

pub type MidgardTransaction {
  version: Int,
  validity: MidgardTxValidity,
  body: MidgardTransactionBody,
  witness_set: MidgardTransactionWitnessSet,
}

pub type MidgardTransactionBodyPartialView {
  inputs: Option<List<MidgardTxInput>>,
  reference_inputs: Option<List<MidgardTxInput>>,
  outputs: Option<List<MidgardTxOutput>>,
  fee: Option<Int>,
  validity_interval_start: Option<Int>,
  validity_interval_end: Option<Int>,
  required_observers: Option<List<ByteArray>>,
  required_signers: Option<List<ByteArray>>,
  mint: Option<Data>,
  script_integrity_hash: Option<ByteArray>,
  auxiliary_data_hash: Option<ByteArray>,
  network_id: Option<Int>,
}

pub type MidgardTransactionWitnessSetPartialView {
  addr_tx_wits: Option<List<MidgardAddressWitness>>,
  script_tx_wits: Option<List<MidgardScriptWitness>>,
  redeemer_tx_wits: Option<List<MidgardRedeemerWitness>>,
}

pub type MidgardTransactionPartialView {
  version: Option<Int>,
  validity: Option<MidgardTxValidity>,
  body: Option<MidgardTransactionBodyPartialView>,
  witness_set: Option<MidgardTransactionWitnessSetPartialView>,
}

pub type MidgardTransactionBodyPartialPreimages {
  inputs: Option<ByteArray>,
  reference_inputs: Option<ByteArray>,
  outputs: Option<ByteArray>,
  required_observers: Option<ByteArray>,
  required_signers: Option<ByteArray>,
  mint: Option<ByteArray>,
}

pub type MidgardTransactionWitnessSetPartialPreimages {
  addr_tx_wits: Option<ByteArray>,
  script_tx_wits: Option<ByteArray>,
  redeemer_tx_wits: Option<ByteArray>,
}

pub type NativeTxBodyCompact {
  spend_inputs_hash: ByteArray,
  reference_inputs_hash: ByteArray,
  outputs_hash: ByteArray,
  fee: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
  required_observers_hash: ByteArray,
  required_signers_hash: ByteArray,
  mint_hash: ByteArray,
  script_integrity_hash: ByteArray,
  auxiliary_data_hash: ByteArray,
  network_id: Int,
}

pub type NativeTxCompact {
  body: NativeTxBodyCompact,
  witness_set_hash: ByteArray,
  validity_code: Int,
}

pub type NativeTxWitnessSetCompact {
  addr_tx_wits_hash: ByteArray,
  script_tx_wits_hash: ByteArray,
  redeemer_tx_wits_hash: ByteArray,
}

pub type VerifiedMidgardNativeTxCompact {
  tx_id: ByteArray,
  version: Int,
  body: NativeTxBodyCompact,
  witness_set_hash: ByteArray,
  validity: MidgardTxValidity,
  validity_code: Int,
  compact_cbor: ByteArray,
}

pub type VerifiedMidgardTransaction {
  tx_id: ByteArray,
  transaction: MidgardTransaction,
  compact: NativeTxCompact,
  compact_cbor: ByteArray,
}
