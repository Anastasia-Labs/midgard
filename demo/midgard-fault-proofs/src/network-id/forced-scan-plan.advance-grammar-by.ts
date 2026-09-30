import { computeHash32 } from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
  type MissingNativeScriptTxGrammarCheckpoint,
  type MissingNativeScriptTxSemanticCheckpoint,
} from "../missing-native-script-tx/staged-walk.js";

/** §2.5 positional index of the transaction body's outputs field. */
export const NETWORK_ID_OUTPUTS_FIELD_INDEX = 2;

/**
 * Grammar items certified per transaction.
 *
 * `grammar_batch` (128) is the wire-pinned upper bound the validator refuses
 * above; it is not the batch a driver should choose. A chunked (tier-3)
 * envelope costs ≈110k execution memory per certified item, so a 128-item
 * batch overruns the protocol memory maximum outright and the payable batch
 * under the 20% reserve — the batch `forced_scan` documents the off-chain
 * driver as certifying — is 64.
 */
export const NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH = 64n;

/**
 * Outputs folded per `Advance` over a chunked (tier-3) view.
 *
 * `scan_batch` (64) is the wire-pinned upper bound and is the batch a fold
 * over an inline or raw-utxo view pays for. Re-deriving each item out of a
 * chunked envelope costs ≈220k execution memory instead, so a full 64-item
 * batch lands above the 20% reserve and the payable certified batch is 48.
 */
export const NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH = 48n;

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");

const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

/** Byte offset of the checkpoint's one-byte field index in both encodings. */
const FIELD_INDEX_BYTE = 36;

export type NetworkIdForcedScanGrammarCheckpoint =
  MissingNativeScriptTxGrammarCheckpoint & { readonly fieldIndex: 2 };

export type NetworkIdForcedScanWalkCheckpoint =
  MissingNativeScriptTxSemanticCheckpoint & { readonly fieldIndex: 2 };

export const asField2Grammar = (
  value: MissingNativeScriptTxGrammarCheckpoint,
): NetworkIdForcedScanGrammarCheckpoint =>
  ({ ...value, fieldIndex: 2 }) as NetworkIdForcedScanGrammarCheckpoint;

export const asField2Walk = (
  value: MissingNativeScriptTxSemanticCheckpoint,
): NetworkIdForcedScanWalkCheckpoint =>
  ({ ...value, fieldIndex: 2 }) as NetworkIdForcedScanWalkCheckpoint;

export const asField6Grammar = (
  value: NetworkIdForcedScanGrammarCheckpoint,
) => ({
  ...value,
  fieldIndex: 6,
});

const asField6Walk = (value: NetworkIdForcedScanWalkCheckpoint) => ({
  ...value,
  fieldIndex: 6,
});

export const encodeNetworkIdForcedScanGrammarCheckpoint = (
  value: NetworkIdForcedScanGrammarCheckpoint,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxGrammarCheckpoint(
    asField6Grammar(value),
  );
  encoded[FIELD_INDEX_BYTE] = NETWORK_ID_OUTPUTS_FIELD_INDEX;
  return encoded;
};

export const encodeNetworkIdForcedScanWalkCheckpoint = (
  value: NetworkIdForcedScanWalkCheckpoint,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxSemanticCheckpoint(
    asField6Walk(value),
  );
  encoded[FIELD_INDEX_BYTE] = NETWORK_ID_OUTPUTS_FIELD_INDEX;
  return encoded;
};

export const hashNetworkIdForcedScanGrammarCheckpoint = (
  value: NetworkIdForcedScanGrammarCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeNetworkIdForcedScanGrammarCheckpoint(value),
    ]),
  ).toString("hex");

export const hashNetworkIdForcedScanWalkCheckpoint = (
  value: NetworkIdForcedScanWalkCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      WALK_DOMAIN,
      encodeNetworkIdForcedScanWalkCheckpoint(value),
    ]),
  ).toString("hex");

/**
 * The shared checkpoint advance refuses a budget above its own family's batch
 * limit (32). A position is a pure function of how many items are behind it, so
 * a larger logical batch is advanced in limit-sized sub-steps and lands on the
 * identical checkpoint the chain's single larger fold reaches.
 */
export const advanceGrammarBy = ({
  checkpoint,
  items,
  budget,
}: {
  readonly checkpoint: NetworkIdForcedScanGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): NetworkIdForcedScanGrammarCheckpoint => {
  let cursor = checkpoint;
  let remaining = budget;
  while (remaining > 0) {
    const step = Math.min(
      remaining,
      MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
    );
    cursor = asField2Grammar(
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: asField6Grammar(cursor),
        items,
        budget: step,
      }),
    );
    remaining -= step;
  }
  return cursor;
};

export const advanceWalkBy = ({
  checkpoint,
  txId,
  items,
  budget,
}: {
  readonly checkpoint: NetworkIdForcedScanWalkCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): NetworkIdForcedScanWalkCheckpoint => {
  let cursor = checkpoint;
  let remaining = budget;
  while (remaining > 0) {
    const step = Math.min(
      remaining,
      MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
    );
    cursor = asField2Walk(
      advanceMissingNativeScriptTxSemanticCheckpoint({
        checkpoint: asField6Walk(cursor),
        txId,
        items,
        budget: step,
      }),
    );
    remaining -= step;
  }
  return cursor;
};

/**
 * One transaction of the scan, named by the redeemer action it builds.
 *
 * `ordinal` indexes this action's *result* checkpoint inside the plan's
 * `grammar` / `walk` arrays, so a driver that restarts mid-scan can locate its
 * position from the committed hash alone.
 */
export type NetworkIdForcedScanStep =
  | { readonly kind: "open" }
  | { readonly kind: "startGrammar"; readonly itemBudget: bigint }
  | {
      readonly kind: "resumeGrammar";
      readonly ordinal: number;
      readonly itemBudget: bigint;
    }
  | { readonly kind: "finishGrammar" }
  | {
      readonly kind: "advance";
      readonly ordinal: number;
      readonly itemBudget: bigint;
      /** True on the batch that completes the walk and writes step 02. */
      readonly completes: boolean;
    };

export type NetworkIdForcedScanPlan = Readonly<{
  /** The canonical §5.1 item bytes the checkpoints are bound to. */
  items: readonly Buffer[];
  outputCount: number;
  tier: string;
  /** Tier 3 cannot be opened directly: its item count is provisional. */
  requiresGrammar: boolean;
  initialGrammar: NetworkIdForcedScanGrammarCheckpoint;
  /** Result checkpoint of each grammar batch, terminal one last. */
  grammar: readonly NetworkIdForcedScanGrammarCheckpoint[];
  /** The item-zero walk position `Open`/`FinishGrammar` derives. */
  initialWalk: NetworkIdForcedScanWalkCheckpoint;
  /** Result checkpoint of each `Advance` batch, terminal one last. */
  walk: readonly NetworkIdForcedScanWalkCheckpoint[];
  steps: readonly NetworkIdForcedScanStep[];
}>;
