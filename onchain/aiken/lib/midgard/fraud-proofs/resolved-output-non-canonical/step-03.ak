use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  AuthenticatedOutRefV1,
}
use midgard/ledger_output_scan_v1

pub type ReconstructionV1 {
  subject: substrate.VerdictSubjectV1,
  descriptor_cbor: ByteArray,
  control: ledger_output_scan_v1.LedgerOutputScanControlV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
}

pub type Datum =
  ct.StepDatum<AuthenticatedOutRefV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
