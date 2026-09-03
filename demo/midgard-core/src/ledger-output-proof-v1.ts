import {
  advanceMidgardBlake2b224Trace,
  buildMidgardBlake2b224Trace,
  digestMidgardBlake2b224Trace,
  encodeMidgardBlake2b224TraceControl,
  initialMidgardBlake2b224TraceControl,
  isWellFormedMidgardBlake2b224TraceControl,
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  type MidgardBlake2b224TraceControl,
  MidgardBlake2b224TraceStages,
} from "./blake2b-224-trace-v1.js";
import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  commitMidgardBoundedItem,
  hashMidgardBoundedItemChunk,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
  midgardBoundedItemChunkCount,
  type MidgardBoundedItemChunkProof,
  midgardBoundedItemExpectedChunkLength,
  verifyMidgardBoundedItemChunkProof,
} from "./bounded-item-v1.js";
import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  initialMidgardCekDataTraverseControl,
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  nextMidgardCekDataTraverseSpan,
} from "./cek-data-traverse-v1.js";
import {
  emptyMidgardCekDataListSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekSmallConstrData,
} from "./cek-semantic.js";
import { decodeMidgardAddressBytes } from "./codec/address.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
  type MidgardLedgerOutputCommitment,
  type MidgardLedgerOutputDataSummary,
} from "./ledger-output-commitment-v1.js";
import {
  advanceMidgardLedgerOutputScan,
  buildMidgardLedgerOutputScanTrace,
  encodeMidgardLedgerOutputScanControl,
  finishMidgardLedgerOutputScan,
  initialMidgardLedgerOutputScanControl,
  isExactMidgardLedgerOutputScanTerminal,
  isWellFormedMidgardLedgerOutputScanControl,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
} from "./ledger-output-scan-v1.js";
import {
  advanceMidgardLedgerOutputValue,
  buildMidgardLedgerOutputValueTrace,
  encodeMidgardLedgerOutputValueControl,
  finalizeMidgardLedgerOutputValue,
  initialMidgardLedgerOutputValueControl,
  isWellFormedMidgardLedgerOutputValueControl,
  type MidgardLedgerOutputValueControl,
  MidgardLedgerOutputValueStages,
  type MidgardLedgerOutputValueWitness,
} from "./ledger-output-value-v1.js";
import {
  advanceMidgardNativeScriptStructureFrame,
  advanceMidgardNativeScriptStructureToken,
  buildMidgardNativeScriptStructureTrace,
  encodeMidgardNativeScriptStructureControl,
  finalizeMidgardNativeScriptStructure,
  initialMidgardNativeScriptStructureControl,
  isExactMidgardNativeScriptStructureTerminal,
  isWellFormedMidgardNativeScriptStructureControl,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureResultKinds,
  MidgardNativeScriptStructureStages,
} from "./native-script-scan-v1.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";
import {
  appendMidgardValidationMerkleLeaf,
  commitMidgardValidationMerkleFrontier,
  emptyMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
  validateMidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

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
};

export type MidgardLedgerOutputProofWitness =
  | {
      readonly kind: "chunks";
      readonly chunkProof: MidgardBoundedItemChunkProof;
      readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
    }
  | {
      readonly kind: "value";
      readonly policyId: Buffer;
      readonly assetName: Buffer;
      readonly quantity: bigint;
      readonly siblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "datum";
      readonly action: MidgardCekDataTraverseAction;
      readonly chunkProof: MidgardBoundedItemChunkProof | null;
      readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
    }
  | {
      readonly kind: "nativeFrame";
      readonly frame: MidgardNativeScriptScanFrame;
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

const exactNonNegativeSafeInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 ledger output proof ${field}`);
  }
  return value;
};

const optionalNestedControlDataCbor = (
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

const optionalDatumControlDataCbor = (
  control: MidgardCekDataTraverseControl | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekDataTraverseControl(control),
        Buffer.from([0xff]),
      ]);

const optionalValueControlDataCbor = (
  control: MidgardLedgerOutputValueControl | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardLedgerOutputValueControl(control),
        Buffer.from([0xff]),
      ]);

export const isWellFormedMidgardLedgerOutputProofControl = (
  control: MidgardLedgerOutputProofControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_LEDGER_OUTPUT_PROOF_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardLedgerOutputProofStages.Structure ||
      control.stage > MidgardLedgerOutputProofStages.Terminal ||
      exactNonNegativeSafeInteger(control.outputIndex, "output index") !==
        control.outputIndex ||
      exactNonNegativeSafeInteger(control.totalLength, "total length") !==
        control.totalLength ||
      control.totalLength === 0 ||
      ensureHash32(
        control.itemCommitment,
        "ledger_output_proof_v1.item_commitment",
      ).length !== 32 ||
      !isWellFormedMidgardLedgerOutputScanControl(control.outputScan) ||
      control.outputScan.cursor > control.totalLength
    ) {
      return false;
    }
    const scanTerminal = isExactMidgardLedgerOutputScanTerminal({
      control: control.outputScan,
      totalLength: control.totalLength,
    });
    const valueWellFormed =
      control.value !== null &&
      isWellFormedMidgardLedgerOutputValueControl(control.value) &&
      control.value.assetRemaining <= control.outputScan.assetFrontier.count;
    const valueTerminal =
      valueWellFormed &&
      finalizeMidgardLedgerOutputValue(control.value!) !== null;
    const datumPresent = control.outputScan.datumOffset !== -1;
    const datumLength = control.outputScan.datumLength;
    const datumWellFormed =
      control.datum !== null &&
      isWellFormedMidgardCekDataTraverseControl(control.datum) &&
      control.datum.sourceStart === control.outputScan.datumOffset &&
      control.datum.sourceLength === datumLength;
    const datumTerminal =
      datumWellFormed &&
      control.datum!.stage === MidgardCekDataTraverseStages.Terminal &&
      finalizeMidgardCekDataTraverse(control.datum!) !== null;
    const datumComplete = datumPresent ? datumTerminal : control.datum === null;
    const referenceLanguage = control.outputScan.referenceScriptLanguage;
    const referenceLength = control.outputScan.referenceScriptLength;
    const referenceItemLength =
      control.totalLength - control.outputScan.referenceScriptItemOffset;
    validateMidgardValidationMerkleFrontier(control.referenceScriptFrontier);
    const referenceFrontierComplete =
      referenceLanguage !== -1 &&
      referenceItemLength > 0 &&
      control.referenceScriptFrontier.count ===
        midgardBoundedItemChunkCount(referenceItemLength);
    const hashWellFormed =
      control.scriptHash !== null &&
      isWellFormedMidgardBlake2b224TraceControl(control.scriptHash) &&
      control.scriptHash.totalLength === referenceLength + 1;
    const hashTerminal =
      hashWellFormed &&
      digestMidgardBlake2b224Trace(control.scriptHash!) !== null;
    const nativeWellFormed =
      control.nativeScript !== null &&
      isWellFormedMidgardNativeScriptStructureControl(control.nativeScript) &&
      control.nativeScript.startOffset ===
        control.outputScan.referenceScriptOffset &&
      control.nativeScript.endOffset ===
        control.outputScan.referenceScriptOffset + referenceLength;
    const nativeTerminal =
      nativeWellFormed &&
      isExactMidgardNativeScriptStructureTerminal(control.nativeScript!);
    if (control.stage === MidgardLedgerOutputProofStages.Structure) {
      return (
        control.value === null &&
        control.datum === null &&
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (!scanTerminal) return false;
    if (control.stage === MidgardLedgerOutputProofStages.ValueFold) {
      return (
        valueWellFormed &&
        control.datum === null &&
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (!valueTerminal) return false;
    if (!datumPresent && control.datum !== null) {
      return false;
    }
    if (control.stage === MidgardLedgerOutputProofStages.DatumTraversal) {
      return (
        datumPresent &&
        datumLength > 0 &&
        datumWellFormed &&
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (!datumComplete) return false;
    if (referenceLanguage === -1) {
      return (
        control.stage === MidgardLedgerOutputProofStages.Terminal &&
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (
      referenceItemLength <= 0 ||
      control.referenceScriptFrontier.count >
        midgardBoundedItemChunkCount(referenceItemLength)
    ) {
      return false;
    }
    if (
      control.stage === MidgardLedgerOutputProofStages.ReferenceScriptCommitment
    ) {
      return control.scriptHash === null && control.nativeScript === null;
    }
    if (!referenceFrontierComplete || !hashWellFormed) return false;
    if (control.stage === MidgardLedgerOutputProofStages.ScriptHash) {
      return control.nativeScript === null;
    }
    if (
      referenceLanguage === 0 &&
      control.stage === MidgardLedgerOutputProofStages.NativeScript
    ) {
      return hashTerminal && nativeWellFormed;
    }
    if (control.stage === MidgardLedgerOutputProofStages.Terminal) {
      return (
        hashTerminal &&
        (referenceLanguage === 0
          ? nativeTerminal
          : control.nativeScript === null)
      );
    }
    return false;
  } catch {
    return false;
  }
};

export const initialMidgardLedgerOutputProofControl = ({
  outputIndex,
  totalLength,
  itemCommitment,
}: {
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Uint8Array;
}): MidgardLedgerOutputProofControl => {
  const control = {
    version: MIDGARD_LEDGER_OUTPUT_PROOF_VERSION,
    stage: MidgardLedgerOutputProofStages.Structure,
    outputIndex,
    totalLength,
    itemCommitment: ensureHash32(
      itemCommitment,
      "ledger_output_proof_v1.item_commitment",
    ),
    outputScan: initialMidgardLedgerOutputScanControl(),
    value: null,
    datum: null,
    referenceScriptFrontier: emptyMidgardValidationMerkleFrontier(),
    scriptHash: null,
    nativeScript: null,
  } satisfies MidgardLedgerOutputProofControl;
  if (!isWellFormedMidgardLedgerOutputProofControl(control)) {
    throw new Error("Invalid V1 ledger output proof source");
  }
  return control;
};

export const encodeMidgardLedgerOutputProofControl = (
  control: MidgardLedgerOutputProofControl,
): Buffer => {
  if (!isWellFormedMidgardLedgerOutputProofControl(control)) {
    throw new Error("Invalid V1 ledger output proof control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_LEDGER_OUTPUT_PROOF_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.outputIndex)),
    encodeCbor(BigInt(control.totalLength)),
    aikenSerialisedPlutusDataBytes(control.itemCommitment),
    encodeMidgardLedgerOutputScanControl(control.outputScan),
    optionalValueControlDataCbor(control.value),
    optionalDatumControlDataCbor(control.datum),
    encodeCbor(BigInt(control.referenceScriptFrontier.count)),
    encodeCbor(
      control.referenceScriptFrontier.peaks.map(({ height, hash }) => [
        BigInt(height),
        hash,
      ]),
    ),
    optionalNestedControlDataCbor(control.scriptHash),
    optionalNestedControlDataCbor(control.nativeScript),
  ]);
};

const proofMatchesOutputChunk = ({
  control,
  proof,
  chunkIndex,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly proof: MidgardBoundedItemChunkProof;
  readonly chunkIndex: number;
}): boolean =>
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX &&
  proof.itemIndex === control.outputIndex &&
  proof.totalLength === control.totalLength &&
  proof.chunkIndex === chunkIndex &&
  verifyMidgardBoundedItemChunkProof({
    expectedCommitment: control.itemCommitment,
    proof,
  });

const authenticatedChunkWindow = ({
  control,
  cursor,
  witness,
  requireFollowingChunk,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly cursor: number;
  readonly witness: MidgardLedgerOutputProofWitness;
  readonly requireFollowingChunk: boolean;
}): { readonly bytes: Buffer; readonly offset: number } | null => {
  if (witness === null || witness.kind !== "chunks") return null;
  const chunkIndex = Math.floor(cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
  const chunkCount = midgardBoundedItemChunkCount(control.totalLength);
  if (
    !proofMatchesOutputChunk({
      control,
      proof: witness.chunkProof,
      chunkIndex,
    })
  ) {
    return null;
  }
  const hasFollowingChunk = chunkIndex + 1 < chunkCount;
  if (requireFollowingChunk && hasFollowingChunk) {
    if (
      witness.nextChunkProof === null ||
      !proofMatchesOutputChunk({
        control,
        proof: witness.nextChunkProof,
        chunkIndex: chunkIndex + 1,
      })
    ) {
      return null;
    }
    return {
      bytes: Buffer.concat([
        witness.chunkProof.chunk,
        witness.nextChunkProof.chunk,
      ]),
      offset: cursor - chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    };
  }
  if (witness.nextChunkProof !== null) return null;
  return {
    bytes: witness.chunkProof.chunk,
    offset: cursor - chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  };
};

const authenticatedOutputSpan = ({
  control,
  absoluteStart,
  length,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly absoluteStart: number;
  readonly length: number;
  readonly witness: MidgardLedgerOutputProofWitness;
}): Buffer | null => {
  if (
    length <= 0 ||
    length > MIDGARD_BOUNDED_ITEM_CHUNK_BYTES ||
    absoluteStart < 0 ||
    absoluteStart + length > control.totalLength ||
    witness === null ||
    witness.kind !== "chunks"
  ) {
    return null;
  }
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  if (
    lastChunkIndex > firstChunkIndex + 1 ||
    !proofMatchesOutputChunk({
      control,
      proof: witness.chunkProof,
      chunkIndex: firstChunkIndex,
    })
  ) {
    return null;
  }
  if (lastChunkIndex === firstChunkIndex) {
    if (witness.nextChunkProof !== null) return null;
    const localStart =
      absoluteStart - firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
    return Buffer.from(
      witness.chunkProof.chunk.subarray(localStart, localStart + length),
    );
  }
  if (
    witness.nextChunkProof === null ||
    !proofMatchesOutputChunk({
      control,
      proof: witness.nextChunkProof,
      chunkIndex: lastChunkIndex,
    })
  ) {
    return null;
  }
  const localStart =
    absoluteStart - firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  return Buffer.from(
    Buffer.concat([
      witness.chunkProof.chunk,
      witness.nextChunkProof.chunk,
    ]).subarray(localStart, localStart + length),
  );
};

const authenticatedDatumSource = ({
  control,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly witness: Extract<
    MidgardLedgerOutputProofWitness,
    { readonly kind: "datum" }
  >;
}): { readonly sourceBytes: Buffer | null } | null => {
  const span = nextMidgardCekDataTraverseSpan(control.datum!);
  if (span === null) {
    return witness.chunkProof === null && witness.nextChunkProof === null
      ? { sourceBytes: null }
      : null;
  }
  if (witness.chunkProof === null) return null;
  const sourceBytes = authenticatedOutputSpan({
    control,
    absoluteStart: span.absoluteStart,
    length: span.length,
    witness: {
      kind: "chunks",
      chunkProof: witness.chunkProof,
      nextChunkProof: witness.nextChunkProof,
    },
  });
  return sourceBytes === null ? null : { sourceBytes };
};

const advancedOutputProof = (
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputProofStepResult | null =>
  isWellFormedMidgardLedgerOutputProofControl(control)
    ? {
        kind: MidgardLedgerOutputProofResultKinds.Advanced,
        control,
      }
    : null;

const mapNativeStructureResult = (
  result: ReturnType<typeof advanceMidgardNativeScriptStructureToken>,
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputProofStepResult | null => {
  if (result === null) return null;
  if (result.kind === MidgardNativeScriptStructureResultKinds.Advanced) {
    return advancedOutputProof({
      ...control,
      nativeScript: result.control,
    });
  }
  if (result.kind === MidgardNativeScriptStructureResultKinds.NodeLimit) {
    return {
      kind: MidgardLedgerOutputProofResultKinds.NativeScriptNodeLimit,
    };
  }
  if (result.kind === MidgardNativeScriptStructureResultKinds.DepthLimit) {
    return {
      kind: MidgardLedgerOutputProofResultKinds.NativeScriptDepthLimit,
    };
  }
  return {
    kind: MidgardLedgerOutputProofResultKinds.InvalidReferenceScript,
  };
};

export const advanceMidgardLedgerOutputProof = ({
  control,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly witness: MidgardLedgerOutputProofWitness;
}): MidgardLedgerOutputProofStepResult | null => {
  if (!isWellFormedMidgardLedgerOutputProofControl(control)) {
    return null;
  }
  try {
    if (control.stage === MidgardLedgerOutputProofStages.Structure) {
      if (
        isExactMidgardLedgerOutputScanTerminal({
          control: control.outputScan,
          totalLength: control.totalLength,
        })
      ) {
        if (witness !== null) return null;
        return advancedOutputProof({
          ...control,
          stage: MidgardLedgerOutputProofStages.ValueFold,
          value: initialMidgardLedgerOutputValueControl(
            control.outputScan.assetFrontier.count,
          ),
        });
      }
      const finished = finishMidgardLedgerOutputScan({
        control: control.outputScan,
        totalLength: control.totalLength,
      });
      if (finished !== null) {
        return witness === null
          ? advancedOutputProof({ ...control, outputScan: finished })
          : null;
      }
      const authenticated = authenticatedChunkWindow({
        control,
        cursor: control.outputScan.cursor,
        witness,
        requireFollowingChunk:
          control.outputScan.stage <=
          MidgardLedgerOutputScanStages.OptionalField,
      });
      if (authenticated === null) return null;
      const nextScan = advanceMidgardLedgerOutputScan({
        control: control.outputScan,
        totalLength: control.totalLength,
        window: authenticated.bytes,
        windowOffset: authenticated.offset,
      });
      return nextScan === null
        ? { kind: MidgardLedgerOutputProofResultKinds.InvalidOutput }
        : advancedOutputProof({ ...control, outputScan: nextScan });
    }
    if (control.stage === MidgardLedgerOutputProofStages.ValueFold) {
      const value = control.value!;
      if (value.stage === MidgardLedgerOutputValueStages.Terminal) {
        if (witness !== null) return null;
        if (control.outputScan.datumOffset !== -1) {
          return advancedOutputProof({
            ...control,
            stage: MidgardLedgerOutputProofStages.DatumTraversal,
            datum: initialMidgardCekDataTraverseControl({
              sourceStart: control.outputScan.datumOffset,
              sourceLength: control.outputScan.datumLength,
            }),
          });
        }
        return advancedOutputProof({
          ...control,
          stage:
            control.outputScan.referenceScriptLanguage === -1
              ? MidgardLedgerOutputProofStages.Terminal
              : MidgardLedgerOutputProofStages.ReferenceScriptCommitment,
        });
      }
      const valueWitness: MidgardLedgerOutputValueWitness | null =
        witness === null
          ? null
          : witness.kind === "value"
            ? {
                policyId: witness.policyId,
                assetName: witness.assetName,
                quantity: witness.quantity,
                siblings: witness.siblings,
              }
            : null;
      if (witness !== null && witness.kind !== "value") return null;
      const nextValue = advanceMidgardLedgerOutputValue({
        control: value,
        assetFrontier: control.outputScan.assetFrontier,
        lovelace: control.outputScan.lovelace,
        witness: valueWitness,
      });
      return nextValue === null
        ? null
        : advancedOutputProof({ ...control, value: nextValue });
    }
    if (control.stage === MidgardLedgerOutputProofStages.DatumTraversal) {
      const datum = control.datum!;
      if (datum.stage === MidgardCekDataTraverseStages.Terminal) {
        if (witness !== null) return null;
        return advancedOutputProof({
          ...control,
          stage:
            control.outputScan.referenceScriptLanguage === -1
              ? MidgardLedgerOutputProofStages.Terminal
              : MidgardLedgerOutputProofStages.ReferenceScriptCommitment,
        });
      }
      if (witness === null || witness.kind !== "datum") {
        return null;
      }
      const authenticated = authenticatedDatumSource({
        control,
        witness,
      });
      if (authenticated === null) return null;
      const nextDatum = advanceMidgardCekDataTraverse({
        control: datum,
        sourceBytes: authenticated.sourceBytes,
        action: witness.action,
      });
      return nextDatum === null
        ? null
        : advancedOutputProof({ ...control, datum: nextDatum });
    }
    if (
      control.stage === MidgardLedgerOutputProofStages.ReferenceScriptCommitment
    ) {
      const itemOffset = control.outputScan.referenceScriptItemOffset;
      const itemLength = control.totalLength - itemOffset;
      const chunkCount = midgardBoundedItemChunkCount(itemLength);
      const chunkIndex = control.referenceScriptFrontier.count;
      if (chunkIndex === chunkCount) {
        if (witness !== null) return null;
        return advancedOutputProof({
          ...control,
          stage: MidgardLedgerOutputProofStages.ScriptHash,
          scriptHash: initialMidgardBlake2b224TraceControl(
            control.outputScan.referenceScriptLength + 1,
          ),
        });
      }
      const chunkLength = midgardBoundedItemExpectedChunkLength({
        totalLength: itemLength,
        chunkIndex,
      });
      const chunk = authenticatedOutputSpan({
        control,
        absoluteStart:
          itemOffset + chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        length: chunkLength,
        witness,
      });
      if (chunk === null) return null;
      return advancedOutputProof({
        ...control,
        referenceScriptFrontier: appendMidgardValidationMerkleLeaf(
          control.referenceScriptFrontier,
          hashMidgardBoundedItemChunk({
            fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
            itemIndex: control.outputIndex,
            chunkIndex,
            chunk,
          }),
        ),
      });
    }
    if (control.stage === MidgardLedgerOutputProofStages.ScriptHash) {
      const scriptHash = control.scriptHash!;
      if (scriptHash.stage === MidgardBlake2b224TraceStages.Terminal) {
        if (witness !== null) return null;
        if (control.outputScan.referenceScriptLanguage !== 0) {
          return advancedOutputProof({
            ...control,
            stage: MidgardLedgerOutputProofStages.Terminal,
          });
        }
        if (control.outputScan.referenceScriptLength === 0) {
          return {
            kind: MidgardLedgerOutputProofResultKinds.InvalidReferenceScript,
          };
        }
        return advancedOutputProof({
          ...control,
          stage: MidgardLedgerOutputProofStages.NativeScript,
          nativeScript: initialMidgardNativeScriptStructureControl({
            startOffset: control.outputScan.referenceScriptOffset,
            totalLength: control.outputScan.referenceScriptLength,
          }),
        });
      }
      if (scriptHash.stage === MidgardBlake2b224TraceStages.Ready) {
        const expectedLength = Math.min(
          MIDGARD_BLAKE2B_BLOCK_BYTES,
          scriptHash.totalLength - scriptHash.cursor,
        );
        const includesLanguage = scriptHash.cursor === 0;
        const contentLength = expectedLength - (includesLanguage ? 1 : 0);
        let content = Buffer.alloc(0);
        if (contentLength > 0) {
          content =
            authenticatedOutputSpan({
              control,
              absoluteStart:
                control.outputScan.referenceScriptOffset +
                scriptHash.cursor -
                (includesLanguage ? 0 : 1),
              length: contentLength,
              witness,
            }) ?? Buffer.alloc(0);
          if (content.length !== contentLength) return null;
        } else if (witness !== null) {
          return null;
        }
        const block = includesLanguage
          ? Buffer.concat([
              Buffer.from([control.outputScan.referenceScriptLanguage]),
              content,
            ])
          : content;
        const nextHash = advanceMidgardBlake2b224Trace({
          control: scriptHash,
          block,
        });
        return nextHash === null
          ? null
          : advancedOutputProof({
              ...control,
              scriptHash: nextHash,
            });
      }
      if (witness !== null) return null;
      const nextHash = advanceMidgardBlake2b224Trace({
        control: scriptHash,
      });
      return nextHash === null
        ? null
        : advancedOutputProof({ ...control, scriptHash: nextHash });
    }
    if (control.stage === MidgardLedgerOutputProofStages.NativeScript) {
      const nativeScript = control.nativeScript!;
      if (nativeScript.stage === MidgardNativeScriptStructureStages.Terminal) {
        return witness === null
          ? advancedOutputProof({
              ...control,
              stage: MidgardLedgerOutputProofStages.Terminal,
            })
          : null;
      }
      if (nativeScript.stage === MidgardNativeScriptStructureStages.Token) {
        const authenticated = authenticatedChunkWindow({
          control,
          cursor: nativeScript.cursor,
          witness,
          requireFollowingChunk: true,
        });
        if (authenticated === null) return null;
        return mapNativeStructureResult(
          advanceMidgardNativeScriptStructureToken({
            control: nativeScript,
            window: authenticated.bytes,
            windowOffset: authenticated.offset,
          }),
          control,
        );
      }
      if (nativeScript.stage === MidgardNativeScriptStructureStages.Frame) {
        if (witness === null || witness.kind !== "nativeFrame") {
          return null;
        }
        return mapNativeStructureResult(
          advanceMidgardNativeScriptStructureFrame({
            control: nativeScript,
            frame: witness.frame,
          }),
          control,
        );
      }
      if (witness !== null) return null;
      return mapNativeStructureResult(
        finalizeMidgardNativeScriptStructure(nativeScript),
        control,
      );
    }
    return null;
  } catch {
    return null;
  }
};

const chunkWitness = ({
  item,
  chunkIndex,
  nextChunkIndex,
}: {
  readonly item: MidgardBoundedItem;
  readonly chunkIndex: number;
  readonly nextChunkIndex: number | null;
}): MidgardLedgerOutputProofWitness => ({
  kind: "chunks",
  chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
  nextChunkProof:
    nextChunkIndex === null
      ? null
      : buildMidgardBoundedItemChunkProof(item, nextChunkIndex),
});

const spanChunkWitness = ({
  item,
  absoluteStart,
  length,
}: {
  readonly item: MidgardBoundedItem;
  readonly absoluteStart: number;
  readonly length: number;
}): MidgardLedgerOutputProofWitness => {
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  return chunkWitness({
    item,
    chunkIndex: firstChunkIndex,
    nextChunkIndex: lastChunkIndex === firstChunkIndex ? null : lastChunkIndex,
  });
};

export const buildMidgardLedgerOutputProofTrace = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputProofTrace => {
  const bytes = Buffer.from(outputCbor);
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: outputIndex,
    bytes,
  });
  const initial = initialMidgardLedgerOutputProofControl({
    outputIndex,
    totalLength: bytes.length,
    itemCommitment: item.commitment,
  });
  const steps: MidgardLedgerOutputProofTraceStep[] = [];
  let control = initial;
  const append = (witness: MidgardLedgerOutputProofWitness): void => {
    const result = advanceMidgardLedgerOutputProof({
      control,
      witness,
    });
    if (
      result === null ||
      result.kind !== MidgardLedgerOutputProofResultKinds.Advanced
    ) {
      throw new Error(
        `Canonical V1 ledger output proof failed: ${result?.kind ?? "malformed evidence"}`,
      );
    }
    steps.push({ control, witness, next: result.control });
    control = result.control;
  };

  const outputScanTrace = buildMidgardLedgerOutputScanTrace(bytes);
  for (const scanStep of outputScanTrace.steps) {
    append(
      scanStep.chunkIndex === null
        ? null
        : chunkWitness({
            item,
            chunkIndex: scanStep.chunkIndex,
            nextChunkIndex: scanStep.nextChunkIndex,
          }),
    );
    if (
      !encodeMidgardLedgerOutputScanControl(control.outputScan).equals(
        encodeMidgardLedgerOutputScanControl(scanStep.next),
      )
    ) {
      throw new Error("V1 output proof diverged from output scan");
    }
  }
  append(null);
  const valueAssets = outputScanTrace.steps.flatMap(({ asset }) =>
    asset === null ? [] : [asset],
  );
  const valueTrace = buildMidgardLedgerOutputValueTrace({
    assets: valueAssets,
    lovelace: outputScanTrace.terminal.lovelace,
  });
  if (
    valueTrace.frontier.count !==
      outputScanTrace.terminal.assetFrontier.count ||
    valueTrace.frontier.peaks.length !==
      outputScanTrace.terminal.assetFrontier.peaks.length ||
    valueTrace.frontier.peaks.some(
      (peak, index) =>
        peak.height !==
          outputScanTrace.terminal.assetFrontier.peaks[index]?.height ||
        !Buffer.from(peak.hash).equals(
          Buffer.from(
            outputScanTrace.terminal.assetFrontier.peaks[index]!.hash,
          ),
        ),
    )
  ) {
    throw new Error("V1 output proof diverged from the asset frontier");
  }
  for (const valueStep of valueTrace.steps) {
    append(
      valueStep.witness === null
        ? null
        : {
            kind: "value",
            policyId: valueStep.witness.policyId,
            assetName: valueStep.witness.assetName,
            quantity: valueStep.witness.quantity,
            siblings: valueStep.witness.siblings,
          },
    );
    if (
      control.value === null ||
      !encodeMidgardLedgerOutputValueControl(control.value).equals(
        encodeMidgardLedgerOutputValueControl(valueStep.next),
      )
    ) {
      throw new Error("V1 output proof diverged from Value fold");
    }
  }
  append(null);
  if (isExactMidgardLedgerOutputProofTerminal(control)) {
    return { item, initial, steps, terminal: control };
  }

  const datumOffset = outputScanTrace.terminal.datumOffset;
  const datumLength = outputScanTrace.terminal.datumLength;
  if (control.stage === MidgardLedgerOutputProofStages.DatumTraversal) {
    const datumTrace = buildMidgardCekDataTraverseTrace({
      sourceStart: datumOffset,
      source: bytes.subarray(datumOffset, datumOffset + datumLength),
    });
    for (const datumStep of datumTrace.steps) {
      const span = nextMidgardCekDataTraverseSpan(datumStep.control);
      const chunks =
        span === null
          ? null
          : spanChunkWitness({
              item,
              absoluteStart: span.absoluteStart,
              length: span.length,
            });
      if (chunks !== null && chunks.kind !== "chunks") {
        throw new Error("V1 output proof lost datum chunks");
      }
      append({
        kind: "datum",
        action: datumStep.action,
        chunkProof: chunks?.chunkProof ?? null,
        nextChunkProof: chunks?.nextChunkProof ?? null,
      });
      if (
        control.datum === null ||
        !encodeMidgardCekDataTraverseControl(control.datum).equals(
          encodeMidgardCekDataTraverseControl(datumStep.next),
        )
      ) {
        throw new Error("V1 output proof diverged from datum traversal");
      }
    }
    append(null);
    if (
      control.datum === null ||
      finalizeMidgardCekDataTraverse(control.datum) === null
    ) {
      throw new Error("V1 output proof did not authenticate the inline datum");
    }
  }
  if (isExactMidgardLedgerOutputProofTerminal(control)) {
    return { item, initial, steps, terminal: control };
  }

  const referenceItemOffset =
    outputScanTrace.terminal.referenceScriptItemOffset;
  const referenceItemLength = bytes.length - referenceItemOffset;
  const referenceItem = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: outputIndex,
    bytes: bytes.subarray(referenceItemOffset),
  });
  for (
    let chunkIndex = 0;
    chunkIndex < referenceItem.frontier.count;
    chunkIndex += 1
  ) {
    append(
      spanChunkWitness({
        item,
        absoluteStart:
          referenceItemOffset + chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        length: midgardBoundedItemExpectedChunkLength({
          totalLength: referenceItemLength,
          chunkIndex,
        }),
      }),
    );
  }
  append(null);
  if (
    !commitMidgardBoundedItem({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
      itemIndex: outputIndex,
      totalLength: referenceItemLength,
      frontier: control.referenceScriptFrontier,
    }).equals(referenceItem.commitment)
  ) {
    throw new Error(
      "V1 output proof diverged from reference-script commitment",
    );
  }

  const referenceOffset = outputScanTrace.terminal.referenceScriptOffset;
  const referenceLength = outputScanTrace.terminal.referenceScriptLength;
  const referenceLanguage = outputScanTrace.terminal.referenceScriptLanguage;
  const scriptBytes = bytes.subarray(
    referenceOffset,
    referenceOffset + referenceLength,
  );
  const identityMessage = Buffer.concat([
    Buffer.from([referenceLanguage]),
    scriptBytes,
  ]);
  const hashTrace = buildMidgardBlake2b224Trace(identityMessage);
  for (const hashStep of hashTrace) {
    const includesLanguage =
      hashStep.control.stage === MidgardBlake2b224TraceStages.Ready &&
      hashStep.control.cursor === 0;
    const contentLength =
      hashStep.block === null
        ? 0
        : hashStep.block.length - (includesLanguage ? 1 : 0);
    append(
      contentLength === 0
        ? null
        : spanChunkWitness({
            item,
            absoluteStart:
              referenceOffset +
              hashStep.control.cursor -
              (includesLanguage ? 0 : 1),
            length: contentLength,
          }),
    );
    if (
      control.scriptHash === null ||
      !encodeMidgardBlake2b224TraceControl(control.scriptHash).equals(
        encodeMidgardBlake2b224TraceControl(hashStep.next),
      )
    ) {
      throw new Error("V1 output proof diverged from script hash trace");
    }
  }
  append(null);
  if (isExactMidgardLedgerOutputProofTerminal(control)) {
    return { item, initial, steps, terminal: control };
  }

  let nativeTrace;
  try {
    nativeTrace = buildMidgardNativeScriptStructureTrace(
      scriptBytes,
      referenceOffset,
    );
  } catch {
    throw new Error(
      "Canonical V1 ledger output proof failed: invalidReferenceScript",
    );
  }
  for (const nativeStep of nativeTrace) {
    let witness: MidgardLedgerOutputProofWitness;
    if (nativeStep.control.stage === MidgardNativeScriptStructureStages.Token) {
      const chunkIndex = Math.floor(
        nativeStep.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
      );
      const chunkCount = midgardBoundedItemChunkCount(bytes.length);
      witness = chunkWitness({
        item,
        chunkIndex,
        nextChunkIndex: chunkIndex + 1 < chunkCount ? chunkIndex + 1 : null,
      });
    } else if (
      nativeStep.control.stage === MidgardNativeScriptStructureStages.Frame
    ) {
      if (nativeStep.frame === null) {
        throw new Error("V1 native output proof lost a frame");
      }
      witness = { kind: "nativeFrame", frame: nativeStep.frame };
    } else {
      witness = null;
    }
    append(witness);
    if (
      control.nativeScript === null ||
      !encodeMidgardNativeScriptStructureControl(control.nativeScript).equals(
        encodeMidgardNativeScriptStructureControl(nativeStep.next),
      )
    ) {
      throw new Error("V1 output proof diverged from native scan");
    }
  }
  append(null);
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    throw new Error("Canonical V1 ledger output proof did not terminate");
  }
  return { item, initial, steps, terminal: control };
};

export const isExactMidgardLedgerOutputProofTerminal = (
  control: MidgardLedgerOutputProofControl,
): boolean =>
  isWellFormedMidgardLedgerOutputProofControl(control) &&
  control.stage === MidgardLedgerOutputProofStages.Terminal;

export const digestMidgardLedgerOutputReferenceScript = (
  control: MidgardLedgerOutputProofControl,
): Buffer | null =>
  isExactMidgardLedgerOutputProofTerminal(control) &&
  control.scriptHash !== null
    ? digestMidgardBlake2b224Trace(control.scriptHash)
    : null;

export const summarizeMidgardLedgerOutputCardanoSpendDatum = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    return null;
  }
  if (control.outputScan.datumOffset === -1) {
    return summarizeMidgardCekSmallConstrData(
      1n,
      emptyMidgardCekDataListSummary(),
    );
  }
  const datum = finalizeMidgardCekDataTraverse(control.datum!);
  return datum === null
    ? null
    : summarizeMidgardCekSmallConstrData(
        0n,
        prependMidgardCekDataListSummary(
          datum,
          emptyMidgardCekDataListSummary(),
        ),
      );
};

export const summarizeMidgardLedgerOutputValue = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null =>
  isExactMidgardLedgerOutputProofTerminal(control)
    ? finalizeMidgardLedgerOutputValue(control.value!)
    : null;

const summarizeDirectBytesData = (bytes: Uint8Array): MidgardCekDataSummary => {
  const trace = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: encodeCbor(Buffer.from(bytes)),
  });
  const summary = finalizeMidgardCekDataTraverse(trace.terminal);
  if (summary === null) {
    throw new Error("V1 direct bytes Data summary failed closed");
  }
  return summary;
};

const summarizeDataList = (items: readonly MidgardCekDataSummary[]) => {
  let summary = emptyMidgardCekDataListSummary();
  for (let index = items.length - 1; index >= 0; index -= 1) {
    summary = prependMidgardCekDataListSummary(items[index]!, summary);
  }
  return summary;
};

const summarizeSmallConstr = (
  constructor: bigint,
  fields: readonly MidgardCekDataSummary[],
): MidgardCekDataSummary =>
  summarizeMidgardCekSmallConstrData(constructor, summarizeDataList(fields));

const summarizeCredential = (
  kind: "PubKey" | "Script",
  hash: Uint8Array,
): MidgardCekDataSummary =>
  summarizeSmallConstr(kind === "PubKey" ? 0n : 1n, [
    summarizeDirectBytesData(hash),
  ]);

const summarizeOutputAddress = (
  control: MidgardLedgerOutputProofControl,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary => {
  const address = decodeMidgardAddressBytes(control.outputScan.address);
  const payment = summarizeCredential(
    address.paymentCredential.kind,
    address.paymentCredential.hash,
  );
  const stake =
    address.stakeCredential === undefined
      ? summarizeSmallConstr(1n, [])
      : summarizeSmallConstr(0n, [
          summarizeSmallConstr(0n, [
            summarizeCredential(
              address.stakeCredential.kind,
              address.stakeCredential.hash,
            ),
          ]),
        ]);
  return summarizeSmallConstr(
    encoding === "midgard" && address.protected ? 1n : 0n,
    [payment, stake],
  );
};

const summarizeOutputDatum = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (control.outputScan.datumOffset === -1) {
    return summarizeSmallConstr(0n, []);
  }
  const datum = finalizeMidgardCekDataTraverse(control.datum!);
  return datum === null ? null : summarizeSmallConstr(2n, [datum]);
};

const summarizeOutputReferenceScript = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (control.outputScan.referenceScriptLanguage === -1) {
    return summarizeSmallConstr(1n, []);
  }
  const digest = digestMidgardLedgerOutputReferenceScript(control);
  return digest === null
    ? null
    : summarizeSmallConstr(0n, [summarizeDirectBytesData(digest)]);
};

const summarizeOutputTxOut = (
  control: MidgardLedgerOutputProofControl,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    return null;
  }
  const value = finalizeMidgardLedgerOutputValue(control.value!);
  const datum = summarizeOutputDatum(control);
  const referenceScript = summarizeOutputReferenceScript(control);
  return value === null || datum === null || referenceScript === null
    ? null
    : summarizeSmallConstr(0n, [
        summarizeOutputAddress(control, encoding),
        value,
        datum,
        referenceScript,
      ]);
};

export const summarizeMidgardLedgerOutputCardanoTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "cardano");

export const summarizeMidgardLedgerOutputMidgardTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "midgard");

const summariesEqual = (
  left: MidgardCekDataSummary,
  right: MidgardLedgerOutputDataSummary,
): boolean =>
  Buffer.from(left.root).equals(Buffer.from(right.root)) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

export const commitMidgardLedgerOutputReferenceScriptItem = (
  control: MidgardLedgerOutputProofControl,
): Buffer | null => {
  if (
    !isExactMidgardLedgerOutputProofTerminal(control) ||
    control.outputScan.referenceScriptLanguage === -1
  ) {
    return null;
  }
  const totalLength =
    control.totalLength - control.outputScan.referenceScriptItemOffset;
  return commitMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: control.outputIndex,
    totalLength,
    frontier: control.referenceScriptFrontier,
  });
};

/**
 * Verifies every compact ledger descriptor fact against one exact terminal
 * output proof. No descriptor may enter the ledger MPF through this boundary
 * unless the complete independently decoded descriptor is proven equal.
 */
export const verifyMidgardLedgerOutputDescriptor = ({
  control,
  descriptor,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly descriptor: MidgardLedgerOutputCommitment;
}): boolean => {
  if (
    !isExactMidgardLedgerOutputProofTerminal(control) ||
    descriptor.version !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION
  ) {
    return false;
  }
  const cardanoTxOut = summarizeMidgardLedgerOutputCardanoTxOut(control);
  const midgardTxOut = summarizeMidgardLedgerOutputMidgardTxOut(control);
  const cardanoSpendDatum =
    summarizeMidgardLedgerOutputCardanoSpendDatum(control);
  if (
    cardanoTxOut === null ||
    midgardTxOut === null ||
    cardanoSpendDatum === null
  ) {
    return false;
  }
  const referenceLanguage = control.outputScan.referenceScriptLanguage;
  const referenceHash = digestMidgardLedgerOutputReferenceScript(control);
  const referenceItemCommitment =
    commitMidgardLedgerOutputReferenceScriptItem(control);
  const referenceTotalLength =
    referenceLanguage === -1
      ? 0
      : control.totalLength - control.outputScan.referenceScriptItemOffset;
  return (
    descriptor.outputIndex === control.outputIndex &&
    descriptor.totalLength === control.totalLength &&
    Buffer.from(descriptor.itemCommitment).equals(control.itemCommitment) &&
    Buffer.from(descriptor.address).equals(control.outputScan.address) &&
    descriptor.lovelace === control.outputScan.lovelace &&
    descriptor.assetCount === control.outputScan.assetFrontier.count &&
    Buffer.from(descriptor.assetFrontierCommitment).equals(
      commitMidgardValidationMerkleFrontier(control.outputScan.assetFrontier),
    ) &&
    descriptor.cardanoValueSize === control.outputScan.cardanoValueSize &&
    descriptor.referenceScriptLanguage === referenceLanguage &&
    Buffer.from(descriptor.referenceScriptHash).equals(
      referenceHash ?? Buffer.alloc(0),
    ) &&
    descriptor.referenceScriptTotalLength === referenceTotalLength &&
    Buffer.from(descriptor.referenceScriptItemCommitment).equals(
      referenceItemCommitment ?? Buffer.alloc(0),
    ) &&
    summariesEqual(cardanoTxOut, descriptor.cardanoTxOut) &&
    summariesEqual(midgardTxOut, descriptor.midgardTxOut) &&
    summariesEqual(cardanoSpendDatum, descriptor.cardanoSpendDatum)
  );
};
