import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { parseJsonUnknown } from "@al-ft/midgard-core/narrowing";
import { validationMachineStateDataFromCore } from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  type ValidationOneStepArgument,
} from "../src/index.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

export const blueprintJson = parseJsonUnknown(
  readFileSync(blueprintPath, "utf8"),
);

export const HUB_ORACLE_POLICY_ID = "11".repeat(28);

export const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);

export const THREAD_ASSET_NAME = "aa".repeat(32);

export const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 7),
);

export const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

// §3.3 execution reserve: at or below the compiled protocol floors with a
// 20% reserve (docs/consensus-profile-v1.md §10).
export const RESERVED_MEMORY_UNITS = Math.floor(
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits * 0.8,
);

export const RESERVED_CPU_UNITS = Math.floor(
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits * 0.8,
);

export const MAX_L1_PROOF_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/**
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). This file
 * used to carry its own copy of the exact-size item builder, producing
 * 10-lovelace items that the ValueAndMint output-descriptor scan now convicts
 * with `E_MIN_ADA`. The shared builder funds each item at its own minimum-Ada
 * floor without moving its length, so every carriage measurement below
 * measures the same number of bytes it did before the wiring.
 */
export const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItem;

/** The §5.1 outputs field, which is the field every case here carries. */
export const OUTPUT_FIELD_INDEX = 2;

/**
 * `NoAuxiliaryWitness` as the committed evidence names it — the Option B
 * (#620) auxiliary half of the canonical-decode resolver's `evidence_hash`.
 * Same literal as `complete-item-route-adversarial-emulator.test.ts` and
 * `complete-item-carriage-tiers-emulator.test.ts`.
 */
export const NO_AUXILIARY_WITNESS_CBOR = Buffer.from("d87980", "hex");

/**
 * The largest complete item this **tier-1** harness can carry.
 *
 * §8.4 partitions on the field's §5.1 preimage, and a single-item field-2
 * envelope is `81 ‖ 59 LLLL ‖ item` — four bytes — so the tier-1 ceiling of
 * `MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES` (14,336) admits an item of at
 * most 14,332 bytes. Anything larger resolves as tier-2 `RawUtxo`, which
 * `buildCanonicalDecodeItemCase` refuses by design.
 *
 * **#580 NOTE — the 64-byte overhang.** The applied publication cap
 * `MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes` is 14,396,
 * which is 64 bytes ABOVE this ceiling: items in (14,332, 14,396] are publishable
 * but cannot be carried inline. Before 2026-08-14 this suite hid that, because it
 * selected its complete-item witness by `(phase, kind)` alone and so measured
 * field 0's few-dozen-byte preimage while claiming to run at the cap. The
 * publication-maximum row moved to
 * `complete-item-carriage-tiers-emulator.test.ts` (tier-2) under the same
 * owner ruling, and **#580 owns re-measuring the overhang**. Retargeting this
 * harness does not resolve it.
 */
/**
 * The four bytes a single-item field-2 §5.1 envelope costs: `81 ‖ 59 LLLL`.
 * Named so the derivation below states the arithmetic instead of restating its
 * result — 14,332 is not an independent measurement, it is the tier-1 ceiling
 * minus this envelope, and a change to that ceiling must move it.
 */
const SINGLE_ITEM_FIELD_ENVELOPE_BYTES = 4;

export const TIER1_MAX_COMPLETE_ITEM_BYTES =
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - SINGLE_ITEM_FIELD_ENVELOPE_BYTES;

export const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

export const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  // The resolved input has to fund every produced output now that each is
  // funded at its own minimum-Ada floor, or stage five would convict this
  // trace with `E_VALUE_NOT_PRESERVED` instead of accepting it. The fee is
  // zero, so the sum is exact.
  const spentOutput = makeOutput(fundingLovelaceForOutputs(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

export type CanonicalDecodeItemCase = {
  readonly trace: DeterministicValidationMachineTrace;
  readonly stateIndex: number;
  readonly itemBytes: number;
  readonly argument: ValidationOneStepArgument;
  readonly transitionData: Data;
  /**
   * #597. `TransactionFieldItemWitness` carries a `FieldCarriageV1` now, and the
   * producer emits tier-1 `Inline`, so the whole wire surface is the field's
   * §5.1 preimage. `carriageData` is that carriage as the redeemer names it;
   * `fieldPreimageHex` is the bytes inside it, which is what a publication
   * holds.
   */
  readonly carriageData: Data;
  readonly fieldPreimageHex: string;
  readonly evidenceHash: string;
  readonly preState: ReturnType<typeof validationMachineStateDataFromCore>;
  readonly claimedSuccessorHash: string;
  /**
   * The four staged datums the chain hands on, derived by
   * `deriveCanonicalDecodeItemStageData` — the same producer
   * `submitValidationDisputeSemanticResolution` uses. Nothing in this file
   * hand-builds a stage datum any more: post-Option-B the observe stage is
   * the size-bearing one, and its datum is not a local restatement of the
   * authenticate stage's.
   */
  readonly preparedThreadDatum: string;
  readonly authenticatedDatum: string;
  readonly preparedDatum: string;
  readonly observedDatum: string;
  readonly verifiedDatum: string;
};
