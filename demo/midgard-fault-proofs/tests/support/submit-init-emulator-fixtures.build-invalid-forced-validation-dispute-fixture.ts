import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKeySchema,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { encodeData } from "../../src/index.js";
import { largeFittingOutputCbor } from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  buildForcedValidationDisputeCommitments,
  makeNativeTx,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

export const buildInvalidForcedValidationDisputeFixture = async ({
  operatorVkey,
  now,
  inlineDatumPayloadBytes = 13_600,
  minimumCompleteItemBytes = MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly inlineDatumPayloadBytes?: number;
  readonly minimumCompleteItemBytes?: number;
}) => {
  const txOrderId = transitionTraceOutRef("e1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: largeFittingOutputCbor(inlineDatumPayloadBytes),
  });
  const forcedCanonicalCbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(forcedNativeTx),
  );
  const decodedForcedNativeTx =
    decodeMidgardForcedTxFullFromCanonicalCbor(forcedCanonicalCbor);
  if (
    decodeMidgardNativeByteListPreimage(
      decodedForcedNativeTx.witnessSet.addrTxWitsPreimageCbor,
      "test.forced_native_tx.addr_tx_wits",
    ).length !== 0
  ) {
    throw new Error(
      "forced validation-dispute fixture unexpectedly contains vkey witnesses",
    );
  }
  const forcedSource =
    deriveMidgardForcedTxProofSourceFromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    submitted_source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.EmptyInputs,
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
    }),
  );
  // #600: the complete-item witness carries the carriage **plan input** — which
  // field, and its whole §5.1 preimage — and the tier is resolved later, at
  // evidence commitment. So the size this fixture selects on is the preimage the
  // step reads, which is what the L1 envelope actually has to hold, and it is
  // read straight off the plan input rather than out of a tier-1 carriage.
  const disputedWitnessIndex = challengerTrace.witnesses.findIndex(
    (witness) =>
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.fieldPreimage.length > minimumCompleteItemBytes,
  );
  if (disputedWitnessIndex < 0) {
    throw new Error(
      "validation-dispute fixture is missing its selected fitting complete item",
    );
  }
  const completeItemWitness = challengerTrace.witnesses[disputedWitnessIndex]!;
  if (completeItemWitness.auxiliary?.kind !== "transactionFieldItem") {
    throw new Error(
      "validation-dispute fixture selected a non-item canonical witness",
    );
  }
  const completeItemBytes = completeItemWitness.auxiliary.fieldPreimage.length;
  const operatorRejectionCodeHash = Buffer.alloc(32);
  const operatorStates = challengerTrace.states.map((state, index) =>
    index >= disputedWitnessIndex + 1
      ? {
          ...state,
          workRoot: Buffer.alloc(32, 0x7e),
          ...(index === challengerTrace.states.length - 1
            ? {
                verdict: "accepted" as const,
                rejectionCodeHash: operatorRejectionCodeHash,
              }
            : {}),
        }
      : state,
  );
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineState),
      "accepted",
      operatorRejectionCodeHash,
    ),
    verdict: "accepted",
    rejectionCode: null,
  };
  const evidence = buildValidationDisputeEvidenceBundle({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    challengerTrace.tree.descriptor,
  );
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor,
    evidence,
    completeItemBytes,
  };
};
