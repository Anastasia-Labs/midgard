import { MissingNativeScriptTxStep04State } from "./missing-native-script-tx.missing-native-script-is-absent.js";
import {
  MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep06State,
} from "./missing-native-script-tx.missing-native-script-tx-step06-args-schema.js";

/** Exactly the state `step-03` writes for `step-04` (`step-03.ak:70-77`). */
export const missingNativeScriptTxStep04State = ({
  producingTxId,
  badInputOutputIndex,
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly producingTxId: string;
  readonly badInputOutputIndex: bigint;
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep04State => ({
  producing_tx_id: producingTxId.toLowerCase(),
  bad_input_output_index: badInputOutputIndex,
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/**
 * Exactly the state `step-04` writes for `step-05` (`step-04.ak:96-100`),
 * which is also the state `step-05` forwards to `step-06` unchanged.
 */
export const missingNativeScriptTxStep05State = ({
  expectedMissingScriptHash,
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly expectedMissingScriptHash: string;
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep05State => ({
  expected_missing_script_hash: expectedMissingScriptHash.toLowerCase(),
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/** Exact state step-05 writes at the step-06 direct/staged routing boundary. */
export const missingNativeScriptTxStep06ReadyState = (
  state: MissingNativeScriptTxStep05State,
): MissingNativeScriptTxStep06State => ({
  ...state,
  phase: "Ready",
});
