//// Family entry for `unusedScriptWitness`. The shared source graph hands over
//// its exact binding; the entry opens the all-purposes walk with the prover's
//// probe, the script hash of the field-6 witness under audit. The witness walk
//// after the scan reads that witness, and the terminal relies on the probe
//// only where it must equal the witness's own hash.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  probe: ByteArray,
  opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
