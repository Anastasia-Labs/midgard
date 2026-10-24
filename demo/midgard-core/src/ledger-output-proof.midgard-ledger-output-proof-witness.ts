import {
  encodeMidgardBlake2b224TraceControl,
  type MidgardBlake2b224TraceControl,
} from "./blake2b-224-trace.js";
import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
  type MidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import {
  encodeMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
} from "./cek-data-traverse.js";
import { encodeCbor } from "./codec/cbor.js";
import { type MidgardLedgerOutputScanControl } from "./ledger-output-scan.js";
import {
  encodeMidgardLedgerOutputValueControl,
  type MidgardLedgerOutputValueControl,
  type MidgardLedgerOutputValueHead,
} from "./ledger-output-value.js";
import {
  encodeMidgardNativeScriptStructureControl,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
} from "./native-script-scan.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";
import { type MidgardValidationMerkleFrontier } from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_PROOF_VERSION = 1 as const;

export const MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX = 2 as const;

export const MidgardLedgerOutputProofStages = Object.freeze({
  Structure: 0,
  ValueFold: 1,
  DatumTraversal: 2,
  ReferenceScriptCommitment: 3,
  ScriptHash: 4,
  NativeScript: 5,
  Terminal: 6,
} as const);

export type MidgardLedgerOutputProofStage =
  (typeof MidgardLedgerOutputProofStages)[keyof typeof MidgardLedgerOutputProofStages];

export const MidgardLedgerOutputProofResultKinds = Object.freeze({
  Advanced: "advanced",
  InvalidOutput: "invalidOutput",
  InvalidReferenceScript: "invalidReferenceScript",
  NativeScriptNodeLimit: "nativeScriptNodeLimit",
  NativeScriptDepthLimit: "nativeScriptDepthLimit",
} as const);

/// The recorded output span window: an earlier span-attach step proved the
/// chunk merkle membership of these output bytes exactly once, and later
/// span-consuming stage steps bind by digest instead of re-running the chunk
/// merkle verification.
export type MidgardLedgerOutputSpanWindow = {
  readonly start: number;
  readonly length: number;
  readonly digest: Buffer;
};

export type MidgardLedgerOutputProofControl = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_PROOF_VERSION;
  readonly stage: MidgardLedgerOutputProofStage;
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Buffer;
  readonly outputScan: MidgardLedgerOutputScanControl;
  readonly value: MidgardLedgerOutputValueControl | null;
  readonly datum: MidgardCekDataTraverseControl | null;
  readonly referenceScriptFrontier: MidgardValidationMerkleFrontier;
  readonly scriptHash: MidgardBlake2b224TraceControl | null;
  readonly nativeScript: MidgardNativeScriptStructureControl | null;
  readonly spanWindow: MidgardLedgerOutputSpanWindow | null;
  readonly scanFactsFact: Buffer | null;
  readonly referenceScriptFact: Buffer | null;
  readonly datumSummaryFact: Buffer | null;
  readonly valueSummaryFact: Buffer | null;
};

export type MidgardLedgerOutputProofWitness =
  | {
      readonly kind: "chunks";
      readonly chunkProof: MidgardBoundedItemChunkProof;
      readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
    }
  | {
      readonly kind: "value";
      readonly assetIndex: number;
      readonly policyId: Buffer;
      readonly assetName: Buffer;
      readonly quantity: bigint;
      readonly siblings: readonly Uint8Array[];
      readonly previous: MidgardLedgerOutputValueHead | null;
    }
  | {
      readonly kind: "datum";
      readonly action: MidgardCekDataTraverseAction;
      readonly window: Buffer | null;
    }
  | {
      readonly kind: "nativeFrame";
      readonly frame: MidgardNativeScriptScanFrame;
    }
  | {
      readonly kind: "spanAttach";
      readonly chunkProof: MidgardBoundedItemChunkProof;
      readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
    }
  | {
      readonly kind: "window";
      readonly bytes: Buffer;
    }
  | null;

export type MidgardLedgerOutputProofStepResult =
  | {
      readonly kind: typeof MidgardLedgerOutputProofResultKinds.Advanced;
      readonly control: MidgardLedgerOutputProofControl;
    }
  | {
      readonly kind:
        | typeof MidgardLedgerOutputProofResultKinds.InvalidOutput
        | typeof MidgardLedgerOutputProofResultKinds.InvalidReferenceScript
        | typeof MidgardLedgerOutputProofResultKinds.NativeScriptNodeLimit
        | typeof MidgardLedgerOutputProofResultKinds.NativeScriptDepthLimit;
    };

export type MidgardLedgerOutputProofTraceStep = {
  readonly control: MidgardLedgerOutputProofControl;
  readonly witness: MidgardLedgerOutputProofWitness;
  readonly next: MidgardLedgerOutputProofControl;
};

export type MidgardLedgerOutputProofTrace = {
  readonly item: MidgardBoundedItem;
  readonly initial: MidgardLedgerOutputProofControl;
  readonly steps: readonly MidgardLedgerOutputProofTraceStep[];
  readonly terminal: MidgardLedgerOutputProofControl;
};

export const exactNonNegativeSafeInteger = (
  value: number,
  field: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 ledger output proof ${field}`);
  }
  return value;
};

export const optionalNestedControlDataCbor = (
  control:
    | MidgardBlake2b224TraceControl
    | MidgardNativeScriptStructureControl
    | null,
): Buffer => {
  if (control === null) {
    return Buffer.from("d87a80", "hex");
  }
  const nested =
    "chainingValue" in control
      ? encodeMidgardBlake2b224TraceControl(control)
      : encodeMidgardNativeScriptStructureControl(control);
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    nested,
    Buffer.from([0xff]),
  ]);
};

export const optionalDatumControlDataCbor = (
  control: MidgardCekDataTraverseControl | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekDataTraverseControl(control),
        Buffer.from([0xff]),
      ]);

export const optionalSpanWindowDataCbor = (
  window: MidgardLedgerOutputSpanWindow | null,
): Buffer =>
  window === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f83", "hex"),
        encodeCbor(BigInt(window.start)),
        encodeCbor(BigInt(window.length)),
        aikenSerialisedPlutusDataBytes(window.digest),
        Buffer.from([0xff]),
      ]);

export const optionalFactDataCbor = (fact: Buffer | null): Buffer =>
  fact === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        aikenSerialisedPlutusDataBytes(fact),
        Buffer.from([0xff]),
      ]);

export const optionalValueControlDataCbor = (
  control: MidgardLedgerOutputValueControl | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardLedgerOutputValueControl(control),
        Buffer.from([0xff]),
      ]);

export const spanWindowIsWellFormed = (
  control: MidgardLedgerOutputProofControl,
): boolean => {
  if (control.spanWindow === null) return true;
  const window = control.spanWindow;
  return (
    control.stage >= MidgardLedgerOutputProofStages.DatumTraversal &&
    Number.isSafeInteger(window.start) &&
    Number.isSafeInteger(window.length) &&
    window.start >= 0 &&
    window.length > 0 &&
    window.length <= MIDGARD_BOUNDED_ITEM_CHUNK_BYTES &&
    window.start + window.length <= control.totalLength &&
    window.digest.length === 32
  );
};

export const factIsWellFormed = (
  stage: MidgardLedgerOutputProofStage,
  fact: Buffer | null,
): boolean =>
  fact === null ||
  (stage === MidgardLedgerOutputProofStages.Terminal && fact.length === 32);
