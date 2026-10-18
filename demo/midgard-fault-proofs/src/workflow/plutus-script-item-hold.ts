/**
 * The hold on Plutus-language script items, until the program-bytes
 * migration: admission reads Plutus scripts as program envelopes while the
 * script-decoding families read them as the bytes Cardano decodes, so until
 * the two agree these families prove no Plutus item. The classifier
 * registrations and the script-decoding workflows filter through this one
 * set; emptying it lifts the hold everywhere.
 */
import { OUTPUT_REFERENCE_PLUTUS_SCRIPT_UNDECODABLE_VIOLATION_ID } from "../output-reference-script-decoding/output-reference-script-decoding.js";
import { WITNESS_SCRIPT_DECODING_VIOLATION_IDS } from "../witness-script-decoding/workflow.witness-script-decoding-config-from-binding.js";

const HELD_PLUTUS_SCRIPT_ITEM_VIOLATION_IDS: ReadonlySet<string> = new Set([
  WITNESS_SCRIPT_DECODING_VIOLATION_IDS.PlutusScriptUndecodable,
  OUTPUT_REFERENCE_PLUTUS_SCRIPT_UNDECODABLE_VIOLATION_ID,
]);

/** `detections` without the held Plutus-language script items. */
export const withoutHeldPlutusScriptItems = <
  Detection extends Readonly<{ violationId: string }>,
>(
  detections: readonly Detection[],
): readonly Detection[] =>
  detections.filter(
    ({ violationId }) =>
      !HELD_PLUTUS_SCRIPT_ITEM_VIOLATION_IDS.has(violationId),
  );
