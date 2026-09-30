import {
  computeMidgardNativeTxId,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  hashMidgardValidationWorkWitness,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKeySchema,
  Header,
  type ValidationClaimWitness,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  encodeValidationTerminalWitnessCbor,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { encodeData } from "../../../src/index.js";
import { transitionTraceOutRef } from "./header-fixtures.js";
import { makeNativeTx } from "./native-tx.js";
import {
  buildForcedValidationDisputeCommitments,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT,
  plainOutputCbor,
  replaceTerminalState,
  restampTraceLedgerDeltaRoot,
} from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";

/**
 * Work root of the exact rejecting-terminal witness the canonical V1 machine
 * requires of a rejection successor, i.e. the Aiken
 * `hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root))`.
 */
export const rejectingTerminalWorkRoot = ({
  programCounter,
  rejectionCode,
  priorLedgerRoot,
}: {
  readonly programCounter: number;
  readonly rejectionCode: string;
  readonly priorLedgerRoot: Buffer;
}): Buffer =>
  Buffer.from(
    hashMidgardValidationWorkWitness({
      phase: "terminal",
      programCounter,
      witnessCbor: encodeValidationTerminalWitnessCbor({
        verdict: "rejected",
        rejectionCode,
        priorLedgerRoot,
      }),
    }),
  );

export type ForcedValidationDisputeFixture = {
  readonly header: Header;
  readonly claim: ValidationClaimWitness;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly challengerDescriptor: ReturnType<
    typeof validationTraceDescriptorDataFromCore
  >;
  readonly evidence: ReturnType<typeof buildValidationDisputeEvidenceBundle>;
  readonly claimedLedgerDeltaRoot: Buffer;
};

/**
 * VM-DEFECT-2 regression fixture — the adversarial case the pre-fix rejection
 * surface could not express.
 *
 * The forced source carries `verdict: ForcedTxValid`, which
 * `validation-claim-v1.ak` (`forced_verdict_matches`, and the exactly
 * analogous `descriptor.verdict == Accepted` clause every *normal* L2 source
 * is held to) forces into an `Accepted` committed descriptor. The transaction
 * itself has no spend inputs, so the deterministic machine rejects it with
 * `E_EMPTY_INPUTS` at the `inputSets` instruction. The claimed ledger delta is
 * a real non-empty commitment, exactly as a real transaction's would be.
 *
 * The reference TypeScript builder refuses to emit a rejected trace with a
 * non-empty delta (`validation-machine.ts`: "a rejected transaction must
 * commit an exact ledger no-op"), so the operator's chosen claimed-delta
 * commitment is re-stamped onto every state afterwards. That is faithful:
 * `ledger_delta_root` is immutable context chosen by the operator and pinned
 * pre == post by `immutable_context_matches`, and nothing before the
 * `ledgerDelta` phase reads it.
 */
export const buildAcceptedClaimOverRejectingTransactionFixture = async ({
  operatorVkey,
  now,
  claimedLedgerDeltaRoot,
  clearChallengerTerminalDelta = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly claimedLedgerDeltaRoot: Buffer;
  readonly clearChallengerTerminalDelta?: boolean;
}): Promise<ForcedValidationDisputeFixture> => {
  const txOrderId = transitionTraceOutRef("e2");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: plainOutputCbor(100_000_000n),
  });
  const forcedCanonicalCbor = encodeMidgardForcedTxCanonical(forcedNativeTx);
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
  const honestTrace = await Effect.runPromise(
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
  const restamped = restampTraceLedgerDeltaRoot(
    honestTrace,
    claimedLedgerDeltaRoot,
  );
  // Counterfactual variant: the successor shape the deleted
  // `post.ledger_delta_root == frontier_commitment(0, [])` clause demanded.
  const challengerTrace = clearChallengerTerminalDelta
    ? replaceTerminalState(restamped, {
        terminal: {
          ...restamped.states.at(-1)!,
          ledgerDeltaRoot: EMPTY_CLAIMED_LEDGER_DELTA_ROOT,
        },
        verdict: "rejected",
        rejectionCode: restamped.rejectionCode,
        rejectionCodeHash: restamped.states.at(-1)!.rejectionCodeHash,
      })
    : restamped;
  const operatorTrace = replaceTerminalState(restamped, {
    terminal: {
      ...restamped.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
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
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot,
  };
};

/**
 * Lovelace carried by both sides of the min-Ada journey transaction below.
 * `coins_per_utxo_byte * (160 + |canonical output|)` for an output of that
 * shape is on the order of a million lovelace, so this is a decisive miss,
 * and the fixture asserts the miss rather than trusting the arithmetic.
 */
export const MIN_ADA_JOURNEY_OUTPUT_LOVELACE = 100_000n;
