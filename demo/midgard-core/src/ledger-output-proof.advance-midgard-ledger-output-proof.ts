import { blake2b } from "@noble/hashes/blake2.js";

import {
  advanceMidgardBlake2b224Trace,
  initialMidgardBlake2b224TraceControl,
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  MidgardBlake2b224TraceStages,
} from "./blake2b-224-trace.js";
import {
  hashMidgardBoundedItemChunk,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  midgardBoundedItemExpectedChunkLength,
} from "./bounded-item.js";
import {
  advanceMidgardCekDataTraverse,
  initialMidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.js";
import {
  advancedOutputProof,
  authenticatedDatumSource,
  authenticatedOutputSpan,
  boundMidgardLedgerOutputWindowBytes,
  demandedMidgardLedgerOutputProofSpan,
  mapNativeStructureResult,
  midgardLedgerOutputAttachWindowLength,
  midgardLedgerOutputWindowCovers,
} from "./ledger-output-proof.authenticated-output-span.js";
import {
  authenticatedChunkWindow,
  isWellFormedMidgardLedgerOutputProofControl,
} from "./ledger-output-proof.is-well-formed-midgard-ledger-output-proof-control.js";
import {
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofResultKinds,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofStepResult,
  type MidgardLedgerOutputProofWitness,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import {
  advanceMidgardLedgerOutputScan,
  finishMidgardLedgerOutputScan,
  isExactMidgardLedgerOutputScanTerminal,
  MidgardLedgerOutputScanStages,
} from "./ledger-output-scan.js";
import {
  advanceMidgardLedgerOutputValue,
  initialMidgardLedgerOutputValueControl,
  MidgardLedgerOutputValueStages,
  type MidgardLedgerOutputValueWitness,
} from "./ledger-output-value.js";
import {
  advanceMidgardNativeScriptStructureFrame,
  advanceMidgardNativeScriptStructureToken,
  finalizeMidgardNativeScriptStructure,
  initialMidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureStages,
} from "./native-script-scan.js";
import { appendMidgardValidationMerkleLeaf } from "./validation-merkle.js";

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
    if (witness !== null && witness.kind === "spanAttach") {
      const demanded = demandedMidgardLedgerOutputProofSpan(control);
      if (
        demanded === null ||
        midgardLedgerOutputWindowCovers({
          spanWindow: control.spanWindow,
          ...demanded,
        })
      ) {
        return null;
      }
      const start = demanded.absoluteStart;
      const length = midgardLedgerOutputAttachWindowLength(
        control.totalLength,
        start,
      );
      const bytes = authenticatedOutputSpan({
        control,
        absoluteStart: start,
        length,
        witness: {
          kind: "chunks",
          chunkProof: witness.chunkProof,
          nextChunkProof: witness.nextChunkProof,
        },
      });
      if (bytes === null) return null;
      return advancedOutputProof({
        ...control,
        spanWindow: {
          start,
          length,
          digest: Buffer.from(blake2b(bytes, { dkLen: 32 })),
        },
      });
    }
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
                assetIndex: witness.assetIndex,
                policyId: witness.policyId,
                assetName: witness.assetName,
                quantity: witness.quantity,
                siblings: witness.siblings,
                previous: witness.previous,
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
      if (witness === null || witness.kind !== "window") return null;
      const chunk = boundMidgardLedgerOutputWindowBytes({
        spanWindow: control.spanWindow,
        absoluteStart:
          itemOffset + chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        length: chunkLength,
        bytes: witness.bytes,
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
          if (witness === null || witness.kind !== "window") return null;
          content =
            boundMidgardLedgerOutputWindowBytes({
              spanWindow: control.spanWindow,
              absoluteStart:
                control.outputScan.referenceScriptOffset +
                scriptHash.cursor -
                (includesLanguage ? 0 : 1),
              length: contentLength,
              bytes: witness.bytes,
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
