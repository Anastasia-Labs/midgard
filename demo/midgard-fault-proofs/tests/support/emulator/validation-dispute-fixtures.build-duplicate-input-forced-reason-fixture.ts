import {
  computeMidgardNativeTxId,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKeySchema,
  type OperatorVerdict,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  RejectCodes,
  runPhaseAValidation,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { encodeData, forcedVerdictForRejection } from "../../../src/index.js";
import { transitionTraceOutRef } from "./header-fixtures.js";
import { makeNativeTx } from "./native-tx.js";
import {
  buildForcedValidationDisputeCommitments,
  outRefCbor,
  plainOutputCbor,
} from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";

/**
 * A forced transaction that spends the same out-ref twice. Phase A rejects it
 * at the input-set rule (`E_DUPLICATE_INPUT`) before any input is resolved, so
 * the block's ledger is untouched and the replay needs no ledger witness.
 */
const duplicateInputForcedTransaction = () => {
  const spent = outRefCbor(0x9d);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spent, spent],
    fee: 0n,
    outputCbor: plainOutputCbor(100_000_000n),
  });
  return {
    transactionId: computeMidgardNativeTxId(forcedNativeTx),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forcedNativeTx),
  };
};

/**
 * The verdict the node's forced-transaction classifier writes for the
 * duplicated-input order: its own Phase A over the canonical forced bytes,
 * then the shared forced-rejection writer.
 */
export const nodeVerdictForDuplicateInputForcedOrder =
  async (): Promise<OperatorVerdict> => {
    const { transactionId, forcedCanonicalCbor } =
      duplicateInputForcedTransaction();
    const phaseA = await Effect.runPromise(
      runPhaseAValidation(
        [
          {
            sourceKind: "forced",
            txId: transactionId,
            txCbor: forcedCanonicalCbor,
            programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
              [],
            ),
            arrivalSeq: 0n,
            createdAt: new Date(0),
          },
        ],
        {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          concurrency: 1,
          strictnessProfile: "forced-duplicate-input",
        },
      ),
    );
    const rejection = phaseA.rejected[0];
    if (phaseA.accepted.length !== 0 || rejection === undefined) {
      throw new Error("expected Phase A to reject the duplicated-input order");
    }
    return forcedVerdictForRejection(rejection, "phaseA");
  };

/**
 * One block whose single forced leaf carries `verdict` for the
 * duplicated-input order. The committed trace is the honest one, which
 * rejects with `E_DUPLICATE_INPUT`; whether the leaf's verdict agrees with it
 * is decided on-chain by `forced_verdict_matches` at source verification.
 */
export const buildDuplicateInputForcedReasonFixture = async ({
  operatorVkey,
  now,
  verdict,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly verdict: OperatorVerdict;
}) => {
  const txOrderId = transitionTraceOutRef("d9");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const { transactionId, forcedCanonicalCbor } =
    duplicateInputForcedTransaction();
  const forcedSource =
    deriveMidgardForcedTxProofSourceFromCanonicalCbor(forcedCanonicalCbor);
  const trace = await Effect.runPromise(
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
      expectedRejectionCode: RejectCodes.DuplicateInputInTx,
    }),
  );
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction: {
      tx_id: transactionId.toString("hex"),
      submitted_source: {
        compact_cbor: forcedSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          forcedSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          forcedSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict,
    },
    operatorTrace: trace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  return {
    header,
    claim,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      trace.tree.descriptor,
    ),
  };
};
