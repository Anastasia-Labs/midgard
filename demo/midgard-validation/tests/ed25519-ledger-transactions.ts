import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  RejectCodes,
  validatePhaseASingle,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  makeQueued,
  type NativeTxFixture,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * One-input, one-output native transactions whose address witnesses the
 * caller supplies, with Phase A and the trace builder run over them.
 */
export type VkeyWitnessBuilder = (message: Buffer) => Buffer;

export const spentOutRef = outRefFromByte(0x11);
export const producedOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);

export const transactionWithWitnesses = (
  witnesses: readonly VkeyWitnessBuilder[],
): NativeTxFixture =>
  makeNativeTx({
    version: 1n,
    spendInputs: [spentOutRef],
    outputs: [producedOutput],
    vkeyWitnesses: (message) => witnesses.map((witness) => witness(message)),
  });

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

export const phaseAVerdict = (transaction: NativeTxFixture) =>
  validatePhaseASingle(
    makeQueued(transaction.txId, transaction.txCbor),
    phaseAConfig,
  );

/** The replay context every trace here is built in. */
export const traceContext = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
  ledgerMutationSteps: [],
};

/** Builds the trace that rejects `transaction` for a witness signature. */
export const signatureRejectionTrace = (transaction: NativeTxFixture) =>
  Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: Buffer.alloc(32, 3).toString("hex"),
      postUtxosRoot: Buffer.alloc(32, 3).toString("hex"),
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: producedOutput }],
      expectedLedgerOps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.InvalidSignature,
    }),
  );
