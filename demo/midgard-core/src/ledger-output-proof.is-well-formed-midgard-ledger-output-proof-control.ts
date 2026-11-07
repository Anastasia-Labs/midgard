import {
  digestMidgardBlake2b224Trace,
  isWellFormedMidgardBlake2b224TraceControl,
} from "./blake2b-224-trace.js";
import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  type MidgardBoundedItemChunkProof,
  verifyMidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import {
  finalizeMidgardCekDataTraverse,
  isWellFormedMidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  exactNonNegativeSafeInteger,
  factIsWellFormed,
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  MIDGARD_LEDGER_OUTPUT_PROOF_VERSION,
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofWitness,
  optionalDatumControlDataCbor,
  optionalFactDataCbor,
  optionalNestedControlDataCbor,
  optionalSpanWindowDataCbor,
  optionalValueControlDataCbor,
  spanWindowIsWellFormed,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import {
  encodeMidgardLedgerOutputScanControl,
  initialMidgardLedgerOutputScanControl,
  isExactMidgardLedgerOutputScanTerminal,
  isWellFormedMidgardLedgerOutputScanControl,
} from "./ledger-output-scan.js";
import {
  finalizeMidgardLedgerOutputValue,
  isWellFormedMidgardLedgerOutputValueControl,
} from "./ledger-output-value.js";
import {
  isExactMidgardNativeScriptStructureTerminal,
  isWellFormedMidgardNativeScriptStructureControl,
} from "./native-script-scan.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";
import {
  emptyMidgardValidationMerkleFrontier,
  validateMidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

export const isWellFormedMidgardLedgerOutputProofControl = (
  control: MidgardLedgerOutputProofControl,
): boolean => {
  try {
    if (
      !spanWindowIsWellFormed(control) ||
      !factIsWellFormed(control.stage, control.scanFactsFact) ||
      !factIsWellFormed(control.stage, control.referenceScriptFact) ||
      !factIsWellFormed(control.stage, control.datumSummaryFact) ||
      !factIsWellFormed(control.stage, control.valueSummaryFact) ||
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
    spanWindow: null,
    scanFactsFact: null,
    referenceScriptFact: null,
    datumSummaryFact: null,
    valueSummaryFact: null,
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
    optionalSpanWindowDataCbor(control.spanWindow),
    optionalFactDataCbor(control.scanFactsFact),
    optionalFactDataCbor(control.referenceScriptFact),
    optionalFactDataCbor(control.datumSummaryFact),
    optionalFactDataCbor(control.valueSummaryFact),
  ]);
};

export const proofMatchesOutputChunk = ({
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

export const authenticatedChunkWindow = ({
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
