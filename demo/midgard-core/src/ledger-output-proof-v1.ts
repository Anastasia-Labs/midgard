import {
  advanceMidgardBlake2b224TraceV1,
  buildMidgardBlake2b224TraceV1,
  digestMidgardBlake2b224TraceV1,
  encodeMidgardBlake2b224TraceControlV1,
  initialMidgardBlake2b224TraceControlV1,
  isWellFormedMidgardBlake2b224TraceControlV1,
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  type MidgardBlake2b224TraceControlV1,
  MidgardBlake2b224TraceStagesV1,
} from "./blake2b-224-trace-v1.js";
import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  commitMidgardBoundedItemV1,
  hashMidgardBoundedItemChunkV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  midgardBoundedItemChunkCountV1,
  type MidgardBoundedItemChunkProofV1,
  midgardBoundedItemExpectedChunkLengthV1,
  type MidgardBoundedItemV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "./bounded-item-v1.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  advanceMidgardLedgerOutputScanV1,
  buildMidgardLedgerOutputScanTraceV1,
  encodeMidgardLedgerOutputScanControlV1,
  finishMidgardLedgerOutputScanV1,
  initialMidgardLedgerOutputScanControlV1,
  isExactMidgardLedgerOutputScanTerminalV1,
  isWellFormedMidgardLedgerOutputScanControlV1,
  type MidgardLedgerOutputScanControlV1,
  MidgardLedgerOutputScanStagesV1,
} from "./ledger-output-scan-v1.js";
import {
  advanceMidgardNativeScriptStructureFrameV1,
  advanceMidgardNativeScriptStructureTokenV1,
  buildMidgardNativeScriptStructureTraceV1,
  encodeMidgardNativeScriptStructureControlV1,
  finalizeMidgardNativeScriptStructureV1,
  initialMidgardNativeScriptStructureControlV1,
  isExactMidgardNativeScriptStructureTerminalV1,
  isWellFormedMidgardNativeScriptStructureControlV1,
  type MidgardNativeScriptScanFrameV1,
  type MidgardNativeScriptStructureControlV1,
  MidgardNativeScriptStructureResultKindsV1,
  MidgardNativeScriptStructureStagesV1,
} from "./native-script-scan-v1.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";
import {
  appendMidgardValidationMerkleLeafV1,
  emptyMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
  validateMidgardValidationMerkleFrontierV1,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_PROOF_V1_VERSION = 1 as const;
export const MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1 = 2 as const;

export const MidgardLedgerOutputProofStagesV1 = Object.freeze({
  Structure: 0,
  ReferenceScriptCommitment: 1,
  ScriptHash: 2,
  NativeScript: 3,
  Terminal: 4,
} as const);

export type MidgardLedgerOutputProofStageV1 =
  (typeof MidgardLedgerOutputProofStagesV1)[keyof typeof MidgardLedgerOutputProofStagesV1];

export const MidgardLedgerOutputProofResultKindsV1 = Object.freeze({
  Advanced: "advanced",
  InvalidOutput: "invalidOutput",
  InvalidReferenceScript: "invalidReferenceScript",
  NativeScriptNodeLimit: "nativeScriptNodeLimit",
  NativeScriptDepthLimit: "nativeScriptDepthLimit",
} as const);

export type MidgardLedgerOutputProofControlV1 = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_PROOF_V1_VERSION;
  readonly stage: MidgardLedgerOutputProofStageV1;
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Buffer;
  readonly outputScan: MidgardLedgerOutputScanControlV1;
  readonly referenceScriptFrontier: MidgardValidationMerkleFrontierV1;
  readonly scriptHash: MidgardBlake2b224TraceControlV1 | null;
  readonly nativeScript: MidgardNativeScriptStructureControlV1 | null;
};

export type MidgardLedgerOutputProofWitnessV1 =
  | {
      readonly kind: "chunks";
      readonly chunkProof: MidgardBoundedItemChunkProofV1;
      readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
    }
  | {
      readonly kind: "nativeFrame";
      readonly frame: MidgardNativeScriptScanFrameV1;
    }
  | null;

export type MidgardLedgerOutputProofStepResultV1 =
  | {
      readonly kind: typeof MidgardLedgerOutputProofResultKindsV1.Advanced;
      readonly control: MidgardLedgerOutputProofControlV1;
    }
  | {
      readonly kind:
        | typeof MidgardLedgerOutputProofResultKindsV1.InvalidOutput
        | typeof MidgardLedgerOutputProofResultKindsV1.InvalidReferenceScript
        | typeof MidgardLedgerOutputProofResultKindsV1.NativeScriptNodeLimit
        | typeof MidgardLedgerOutputProofResultKindsV1.NativeScriptDepthLimit;
    };

export type MidgardLedgerOutputProofTraceStepV1 = {
  readonly control: MidgardLedgerOutputProofControlV1;
  readonly witness: MidgardLedgerOutputProofWitnessV1;
  readonly next: MidgardLedgerOutputProofControlV1;
};

export type MidgardLedgerOutputProofTraceV1 = {
  readonly item: MidgardBoundedItemV1;
  readonly initial: MidgardLedgerOutputProofControlV1;
  readonly steps: readonly MidgardLedgerOutputProofTraceStepV1[];
  readonly terminal: MidgardLedgerOutputProofControlV1;
};

const exactNonNegativeSafeInteger = (
  value: number,
  field: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 ledger output proof ${field}`);
  }
  return value;
};

const optionalNestedControlDataCbor = (
  control:
    | MidgardBlake2b224TraceControlV1
    | MidgardNativeScriptStructureControlV1
    | null,
): Buffer => {
  if (control === null) {
    return Buffer.from("d87a80", "hex");
  }
  const nested = "chainingValue" in control
    ? encodeMidgardBlake2b224TraceControlV1(control)
    : encodeMidgardNativeScriptStructureControlV1(control);
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    nested,
    Buffer.from([0xff]),
  ]);
};

export const isWellFormedMidgardLedgerOutputProofControlV1 = (
  control: MidgardLedgerOutputProofControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_LEDGER_OUTPUT_PROOF_V1_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardLedgerOutputProofStagesV1.Structure ||
      control.stage > MidgardLedgerOutputProofStagesV1.Terminal ||
      exactNonNegativeSafeInteger(
        control.outputIndex,
        "output index",
      ) !== control.outputIndex ||
      exactNonNegativeSafeInteger(
        control.totalLength,
        "total length",
      ) !== control.totalLength ||
      control.totalLength === 0 ||
      ensureHash32(
        control.itemCommitment,
        "ledger_output_proof_v1.item_commitment",
      ).length !== 32 ||
      !isWellFormedMidgardLedgerOutputScanControlV1(
        control.outputScan,
      ) ||
      control.outputScan.cursor > control.totalLength
    ) {
      return false;
    }
    const scanTerminal = isExactMidgardLedgerOutputScanTerminalV1({
      control: control.outputScan,
      totalLength: control.totalLength,
    });
    const referenceLanguage =
      control.outputScan.referenceScriptLanguage;
    const referenceLength = control.outputScan.referenceScriptLength;
    const referenceItemLength =
      control.totalLength -
      control.outputScan.referenceScriptItemOffset;
    validateMidgardValidationMerkleFrontierV1(
      control.referenceScriptFrontier,
    );
    const referenceFrontierComplete =
      referenceLanguage !== -1 &&
      referenceItemLength > 0 &&
      control.referenceScriptFrontier.count ===
        midgardBoundedItemChunkCountV1(referenceItemLength);
    const hashWellFormed =
      control.scriptHash !== null &&
      isWellFormedMidgardBlake2b224TraceControlV1(control.scriptHash) &&
      control.scriptHash.totalLength === referenceLength + 1;
    const hashTerminal =
      hashWellFormed &&
      digestMidgardBlake2b224TraceV1(control.scriptHash!) !== null;
    const nativeWellFormed =
      control.nativeScript !== null &&
      isWellFormedMidgardNativeScriptStructureControlV1(
        control.nativeScript,
      ) &&
      control.nativeScript.startOffset ===
        control.outputScan.referenceScriptOffset &&
      control.nativeScript.endOffset ===
        control.outputScan.referenceScriptOffset + referenceLength;
    const nativeTerminal =
      nativeWellFormed &&
      isExactMidgardNativeScriptStructureTerminalV1(
        control.nativeScript!,
      );
    if (control.stage === MidgardLedgerOutputProofStagesV1.Structure) {
      return (
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (!scanTerminal) return false;
    if (referenceLanguage === -1) {
      return (
        control.stage === MidgardLedgerOutputProofStagesV1.Terminal &&
        control.referenceScriptFrontier.count === 0 &&
        control.scriptHash === null &&
        control.nativeScript === null
      );
    }
    if (
      referenceItemLength <= 0 ||
      control.referenceScriptFrontier.count >
        midgardBoundedItemChunkCountV1(referenceItemLength)
    ) {
      return false;
    }
    if (
      control.stage ===
      MidgardLedgerOutputProofStagesV1.ReferenceScriptCommitment
    ) {
      return control.scriptHash === null && control.nativeScript === null;
    }
    if (!referenceFrontierComplete || !hashWellFormed) return false;
    if (
      control.stage === MidgardLedgerOutputProofStagesV1.ScriptHash
    ) {
      return control.nativeScript === null;
    }
    if (
      referenceLanguage === 0 &&
      control.stage === MidgardLedgerOutputProofStagesV1.NativeScript
    ) {
      return hashTerminal && nativeWellFormed;
    }
    if (control.stage === MidgardLedgerOutputProofStagesV1.Terminal) {
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

export const initialMidgardLedgerOutputProofControlV1 = ({
  outputIndex,
  totalLength,
  itemCommitment,
}: {
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Uint8Array;
}): MidgardLedgerOutputProofControlV1 => {
  const control = {
    version: MIDGARD_LEDGER_OUTPUT_PROOF_V1_VERSION,
    stage: MidgardLedgerOutputProofStagesV1.Structure,
    outputIndex,
    totalLength,
    itemCommitment: ensureHash32(
      itemCommitment,
      "ledger_output_proof_v1.item_commitment",
    ),
    outputScan: initialMidgardLedgerOutputScanControlV1(),
    referenceScriptFrontier:
      emptyMidgardValidationMerkleFrontierV1(),
    scriptHash: null,
    nativeScript: null,
  } satisfies MidgardLedgerOutputProofControlV1;
  if (!isWellFormedMidgardLedgerOutputProofControlV1(control)) {
    throw new Error("Invalid V1 ledger output proof source");
  }
  return control;
};

export const encodeMidgardLedgerOutputProofControlV1 = (
  control: MidgardLedgerOutputProofControlV1,
): Buffer => {
  if (!isWellFormedMidgardLedgerOutputProofControlV1(control)) {
    throw new Error("Invalid V1 ledger output proof control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_LEDGER_OUTPUT_PROOF_V1_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.outputIndex)),
    encodeCbor(BigInt(control.totalLength)),
    aikenSerialisedPlutusDataBytes(control.itemCommitment),
    encodeMidgardLedgerOutputScanControlV1(control.outputScan),
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
  readonly control: MidgardLedgerOutputProofControlV1;
  readonly proof: MidgardBoundedItemChunkProofV1;
  readonly chunkIndex: number;
}): boolean =>
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1 &&
  proof.itemIndex === control.outputIndex &&
  proof.totalLength === control.totalLength &&
  proof.chunkIndex === chunkIndex &&
  verifyMidgardBoundedItemChunkProofV1({
    expectedCommitment: control.itemCommitment,
    proof,
  });

const authenticatedChunkWindow = ({
  control,
  cursor,
  witness,
  requireFollowingChunk,
}: {
  readonly control: MidgardLedgerOutputProofControlV1;
  readonly cursor: number;
  readonly witness: MidgardLedgerOutputProofWitnessV1;
  readonly requireFollowingChunk: boolean;
}): { readonly bytes: Buffer; readonly offset: number } | null => {
  if (witness === null || witness.kind !== "chunks") return null;
  const chunkIndex = Math.floor(
    cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const chunkCount = midgardBoundedItemChunkCountV1(
    control.totalLength,
  );
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
      offset:
        cursor -
        chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    };
  }
  if (witness.nextChunkProof !== null) return null;
  return {
    bytes: witness.chunkProof.chunk,
    offset:
      cursor - chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  };
};

const authenticatedOutputSpan = ({
  control,
  absoluteStart,
  length,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControlV1;
  readonly absoluteStart: number;
  readonly length: number;
  readonly witness: MidgardLedgerOutputProofWitnessV1;
}): Buffer | null => {
  if (
    length <= 0 ||
    length > MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 ||
    absoluteStart < 0 ||
    absoluteStart + length > control.totalLength ||
    witness === null ||
    witness.kind !== "chunks"
  ) {
    return null;
  }
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) /
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
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
      absoluteStart -
      firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
    return Buffer.from(
      witness.chunkProof.chunk.subarray(
        localStart,
        localStart + length,
      ),
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
    absoluteStart -
    firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  return Buffer.from(
    Buffer.concat([
      witness.chunkProof.chunk,
      witness.nextChunkProof.chunk,
    ]).subarray(localStart, localStart + length),
  );
};

const advancedOutputProof = (
  control: MidgardLedgerOutputProofControlV1,
): MidgardLedgerOutputProofStepResultV1 | null =>
  isWellFormedMidgardLedgerOutputProofControlV1(control)
    ? {
        kind: MidgardLedgerOutputProofResultKindsV1.Advanced,
        control,
      }
    : null;

const mapNativeStructureResult = (
  result: ReturnType<
    typeof advanceMidgardNativeScriptStructureTokenV1
  >,
  control: MidgardLedgerOutputProofControlV1,
): MidgardLedgerOutputProofStepResultV1 | null => {
  if (result === null) return null;
  if (
    result.kind === MidgardNativeScriptStructureResultKindsV1.Advanced
  ) {
    return advancedOutputProof({
      ...control,
      nativeScript: result.control,
    });
  }
  if (
    result.kind === MidgardNativeScriptStructureResultKindsV1.NodeLimit
  ) {
    return {
      kind:
        MidgardLedgerOutputProofResultKindsV1.NativeScriptNodeLimit,
    };
  }
  if (
    result.kind === MidgardNativeScriptStructureResultKindsV1.DepthLimit
  ) {
    return {
      kind:
        MidgardLedgerOutputProofResultKindsV1.NativeScriptDepthLimit,
    };
  }
  return {
    kind: MidgardLedgerOutputProofResultKindsV1.InvalidReferenceScript,
  };
};

export const advanceMidgardLedgerOutputProofV1 = ({
  control,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControlV1;
  readonly witness: MidgardLedgerOutputProofWitnessV1;
}): MidgardLedgerOutputProofStepResultV1 | null => {
  if (!isWellFormedMidgardLedgerOutputProofControlV1(control)) {
    return null;
  }
  try {
    if (control.stage === MidgardLedgerOutputProofStagesV1.Structure) {
      if (
        isExactMidgardLedgerOutputScanTerminalV1({
          control: control.outputScan,
          totalLength: control.totalLength,
        })
      ) {
        if (witness !== null) return null;
        if (control.outputScan.referenceScriptLanguage === -1) {
          return advancedOutputProof({
            ...control,
            stage: MidgardLedgerOutputProofStagesV1.Terminal,
          });
        }
        return advancedOutputProof({
          ...control,
          stage:
            MidgardLedgerOutputProofStagesV1.ReferenceScriptCommitment,
        });
      }
      const finished = finishMidgardLedgerOutputScanV1({
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
          MidgardLedgerOutputScanStagesV1.OptionalField,
      });
      if (authenticated === null) return null;
      const nextScan = advanceMidgardLedgerOutputScanV1({
        control: control.outputScan,
        totalLength: control.totalLength,
        window: authenticated.bytes,
        windowOffset: authenticated.offset,
      });
      return nextScan === null
        ? { kind: MidgardLedgerOutputProofResultKindsV1.InvalidOutput }
        : advancedOutputProof({ ...control, outputScan: nextScan });
    }
    if (
      control.stage ===
      MidgardLedgerOutputProofStagesV1.ReferenceScriptCommitment
    ) {
      const itemOffset =
        control.outputScan.referenceScriptItemOffset;
      const itemLength = control.totalLength - itemOffset;
      const chunkCount =
        midgardBoundedItemChunkCountV1(itemLength);
      const chunkIndex = control.referenceScriptFrontier.count;
      if (chunkIndex === chunkCount) {
        if (witness !== null) return null;
        return advancedOutputProof({
          ...control,
          stage: MidgardLedgerOutputProofStagesV1.ScriptHash,
          scriptHash: initialMidgardBlake2b224TraceControlV1(
            control.outputScan.referenceScriptLength + 1,
          ),
        });
      }
      const chunkLength =
        midgardBoundedItemExpectedChunkLengthV1({
          totalLength: itemLength,
          chunkIndex,
        });
      const chunk = authenticatedOutputSpan({
        control,
        absoluteStart:
          itemOffset +
          chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        length: chunkLength,
        witness,
      });
      if (chunk === null) return null;
      return advancedOutputProof({
        ...control,
        referenceScriptFrontier:
          appendMidgardValidationMerkleLeafV1(
            control.referenceScriptFrontier,
            hashMidgardBoundedItemChunkV1({
              fieldIndex:
                MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
              itemIndex: control.outputIndex,
              chunkIndex,
              chunk,
            }),
          ),
      });
    }
    if (control.stage === MidgardLedgerOutputProofStagesV1.ScriptHash) {
      const scriptHash = control.scriptHash!;
      if (
        scriptHash.stage === MidgardBlake2b224TraceStagesV1.Terminal
      ) {
        if (witness !== null) return null;
        if (control.outputScan.referenceScriptLanguage !== 0) {
          return advancedOutputProof({
            ...control,
            stage: MidgardLedgerOutputProofStagesV1.Terminal,
          });
        }
        if (control.outputScan.referenceScriptLength === 0) {
          return {
            kind:
              MidgardLedgerOutputProofResultKindsV1.InvalidReferenceScript,
          };
        }
        return advancedOutputProof({
          ...control,
          stage: MidgardLedgerOutputProofStagesV1.NativeScript,
          nativeScript:
            initialMidgardNativeScriptStructureControlV1({
              startOffset:
                control.outputScan.referenceScriptOffset,
              totalLength:
                control.outputScan.referenceScriptLength,
            }),
        });
      }
      if (scriptHash.stage === MidgardBlake2b224TraceStagesV1.Ready) {
        const expectedLength = Math.min(
          MIDGARD_BLAKE2B_BLOCK_BYTES,
          scriptHash.totalLength - scriptHash.cursor,
        );
        const includesLanguage = scriptHash.cursor === 0;
        const contentLength =
          expectedLength - (includesLanguage ? 1 : 0);
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
              Buffer.from([
                control.outputScan.referenceScriptLanguage,
              ]),
              content,
            ])
          : content;
        const nextHash = advanceMidgardBlake2b224TraceV1({
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
      const nextHash = advanceMidgardBlake2b224TraceV1({
        control: scriptHash,
      });
      return nextHash === null
        ? null
        : advancedOutputProof({ ...control, scriptHash: nextHash });
    }
    if (control.stage === MidgardLedgerOutputProofStagesV1.NativeScript) {
      const nativeScript = control.nativeScript!;
      if (
        nativeScript.stage ===
        MidgardNativeScriptStructureStagesV1.Terminal
      ) {
        return witness === null
          ? advancedOutputProof({
              ...control,
              stage: MidgardLedgerOutputProofStagesV1.Terminal,
            })
          : null;
      }
      if (
        nativeScript.stage ===
        MidgardNativeScriptStructureStagesV1.Token
      ) {
        const authenticated = authenticatedChunkWindow({
          control,
          cursor: nativeScript.cursor,
          witness,
          requireFollowingChunk: true,
        });
        if (authenticated === null) return null;
        return mapNativeStructureResult(
          advanceMidgardNativeScriptStructureTokenV1({
            control: nativeScript,
            window: authenticated.bytes,
            windowOffset: authenticated.offset,
          }),
          control,
        );
      }
      if (
        nativeScript.stage ===
        MidgardNativeScriptStructureStagesV1.Frame
      ) {
        if (witness === null || witness.kind !== "nativeFrame") {
          return null;
        }
        return mapNativeStructureResult(
          advanceMidgardNativeScriptStructureFrameV1({
            control: nativeScript,
            frame: witness.frame,
          }),
          control,
        );
      }
      if (witness !== null) return null;
      return mapNativeStructureResult(
        finalizeMidgardNativeScriptStructureV1(nativeScript),
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
  readonly item: MidgardBoundedItemV1;
  readonly chunkIndex: number;
  readonly nextChunkIndex: number | null;
}): MidgardLedgerOutputProofWitnessV1 => ({
  kind: "chunks",
  chunkProof: buildMidgardBoundedItemChunkProofV1(item, chunkIndex),
  nextChunkProof:
    nextChunkIndex === null
      ? null
      : buildMidgardBoundedItemChunkProofV1(item, nextChunkIndex),
});

const spanChunkWitness = ({
  item,
  absoluteStart,
  length,
}: {
  readonly item: MidgardBoundedItemV1;
  readonly absoluteStart: number;
  readonly length: number;
}): MidgardLedgerOutputProofWitnessV1 => {
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) /
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  return chunkWitness({
    item,
    chunkIndex: firstChunkIndex,
    nextChunkIndex:
      lastChunkIndex === firstChunkIndex
        ? null
        : lastChunkIndex,
  });
};

export const buildMidgardLedgerOutputProofTraceV1 = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputProofTraceV1 => {
  const bytes = Buffer.from(outputCbor);
  const item = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
    itemIndex: outputIndex,
    bytes,
  });
  const initial = initialMidgardLedgerOutputProofControlV1({
    outputIndex,
    totalLength: bytes.length,
    itemCommitment: item.commitment,
  });
  const steps: MidgardLedgerOutputProofTraceStepV1[] = [];
  let control = initial;
  const append = (
    witness: MidgardLedgerOutputProofWitnessV1,
  ): void => {
    const result = advanceMidgardLedgerOutputProofV1({
      control,
      witness,
    });
    if (
      result === null ||
      result.kind !== MidgardLedgerOutputProofResultKindsV1.Advanced
    ) {
      throw new Error(
        `Canonical V1 ledger output proof failed: ${result?.kind ?? "malformed evidence"}`,
      );
    }
    steps.push({ control, witness, next: result.control });
    control = result.control;
  };

  const outputScanTrace = buildMidgardLedgerOutputScanTraceV1(bytes);
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
      !encodeMidgardLedgerOutputScanControlV1(
        control.outputScan,
      ).equals(encodeMidgardLedgerOutputScanControlV1(scanStep.next))
    ) {
      throw new Error("V1 output proof diverged from output scan");
    }
  }
  append(null);
  if (isExactMidgardLedgerOutputProofTerminalV1(control)) {
    return { item, initial, steps, terminal: control };
  }

  const referenceItemOffset =
    outputScanTrace.terminal.referenceScriptItemOffset;
  const referenceItemLength = bytes.length - referenceItemOffset;
  const referenceItem = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
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
          referenceItemOffset +
          chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        length: midgardBoundedItemExpectedChunkLengthV1({
          totalLength: referenceItemLength,
          chunkIndex,
        }),
      }),
    );
  }
  append(null);
  if (
    !commitMidgardBoundedItemV1({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
      itemIndex: outputIndex,
      totalLength: referenceItemLength,
      frontier: control.referenceScriptFrontier,
    }).equals(referenceItem.commitment)
  ) {
    throw new Error(
      "V1 output proof diverged from reference-script commitment",
    );
  }

  const referenceOffset =
    outputScanTrace.terminal.referenceScriptOffset;
  const referenceLength =
    outputScanTrace.terminal.referenceScriptLength;
  const referenceLanguage =
    outputScanTrace.terminal.referenceScriptLanguage;
  const scriptBytes = bytes.subarray(
    referenceOffset,
    referenceOffset + referenceLength,
  );
  const identityMessage = Buffer.concat([
    Buffer.from([referenceLanguage]),
    scriptBytes,
  ]);
  const hashTrace = buildMidgardBlake2b224TraceV1(identityMessage);
  for (const hashStep of hashTrace) {
    const includesLanguage =
      hashStep.control.stage ===
        MidgardBlake2b224TraceStagesV1.Ready &&
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
      !encodeMidgardBlake2b224TraceControlV1(
        control.scriptHash,
      ).equals(encodeMidgardBlake2b224TraceControlV1(hashStep.next))
    ) {
      throw new Error("V1 output proof diverged from script hash trace");
    }
  }
  append(null);
  if (isExactMidgardLedgerOutputProofTerminalV1(control)) {
    return { item, initial, steps, terminal: control };
  }

  let nativeTrace;
  try {
    nativeTrace = buildMidgardNativeScriptStructureTraceV1(
      scriptBytes,
      referenceOffset,
    );
  } catch {
    throw new Error(
      "Canonical V1 ledger output proof failed: invalidReferenceScript",
    );
  }
  for (const nativeStep of nativeTrace) {
    let witness: MidgardLedgerOutputProofWitnessV1;
    if (
      nativeStep.control.stage ===
      MidgardNativeScriptStructureStagesV1.Token
    ) {
      const chunkIndex = Math.floor(
        nativeStep.control.cursor /
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
      );
      const chunkCount = midgardBoundedItemChunkCountV1(bytes.length);
      witness = chunkWitness({
        item,
        chunkIndex,
        nextChunkIndex:
          chunkIndex + 1 < chunkCount ? chunkIndex + 1 : null,
      });
    } else if (
      nativeStep.control.stage ===
      MidgardNativeScriptStructureStagesV1.Frame
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
      !encodeMidgardNativeScriptStructureControlV1(
        control.nativeScript,
      ).equals(
        encodeMidgardNativeScriptStructureControlV1(nativeStep.next),
      )
    ) {
      throw new Error("V1 output proof diverged from native scan");
    }
  }
  append(null);
  if (!isExactMidgardLedgerOutputProofTerminalV1(control)) {
    throw new Error("Canonical V1 ledger output proof did not terminate");
  }
  return { item, initial, steps, terminal: control };
};

export const isExactMidgardLedgerOutputProofTerminalV1 = (
  control: MidgardLedgerOutputProofControlV1,
): boolean =>
  isWellFormedMidgardLedgerOutputProofControlV1(control) &&
  control.stage === MidgardLedgerOutputProofStagesV1.Terminal;

export const digestMidgardLedgerOutputReferenceScriptV1 = (
  control: MidgardLedgerOutputProofControlV1,
): Buffer | null =>
  isExactMidgardLedgerOutputProofTerminalV1(control) &&
  control.scriptHash !== null
    ? digestMidgardBlake2b224TraceV1(control.scriptHash)
    : null;

export const commitMidgardLedgerOutputReferenceScriptItemV1 = (
  control: MidgardLedgerOutputProofControlV1,
): Buffer | null => {
  if (
    !isExactMidgardLedgerOutputProofTerminalV1(control) ||
    control.outputScan.referenceScriptLanguage === -1
  ) {
    return null;
  }
  const totalLength =
    control.totalLength -
    control.outputScan.referenceScriptItemOffset;
  return commitMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
    itemIndex: control.outputIndex,
    totalLength,
    frontier: control.referenceScriptFrontier,
  });
};
