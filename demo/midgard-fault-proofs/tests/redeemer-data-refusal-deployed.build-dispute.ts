import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import { buildValidationDisputeEvidenceBundle } from "@al-ft/midgard-validation";

import { buildForcedValidationDisputeCommitments } from "./support/emulator/validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildDataRefusalTrace } from "./redeemer-data-refusal-deployed.build-trace.js";

/** Commit an accepted operator successor at the exact authenticated refusal boundary. */
export const buildDataRefusalDispute = async (
  data: string,
  operatorVkey: string,
  now: number,
) => {
  const fixture = await buildDataRefusalTrace(data, now);
  const challengerTrace = fixture.trace;
  const disputedLowIndex = challengerTrace.states.length - 3;
  const terminal = challengerTrace.states.at(-1)!;
  const forged = {
    ...terminal,
    verdict: "accepted" as const,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    workRoot: Buffer.alloc(32, 0x7e),
  };
  const operatorStates = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forged,
  );
  const operatorTrace = {
    ...challengerTrace,
    verdict: "accepted" as const,
    rejectionCode: null,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  const evidence = buildValidationDisputeEvidenceBundle({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId: fixture.sourceKey,
    eventKey: fixture.eventKey,
    forcedTransaction: {
      tx_id: fixture.transaction.txId.toString("hex"),
      submitted_source: {
        compact_cbor: fixture.source.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          fixture.source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          fixture.source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: "ForcedTxValid",
    },
    operatorTrace,
    preUtxosRoot: "00".repeat(32),
    postUtxosRoot: "00".repeat(32),
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase: "scriptSources" as const,
    disputedLowIndex,
  };
};
