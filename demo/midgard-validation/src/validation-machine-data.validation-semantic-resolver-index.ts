import "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

import {
  advanceMidgardRedeemerItemProof,
  midgardRedeemerItemDescriptor,
} from "@al-ft/midgard-core";

import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";
import {
  cekKind,
  ledgerDeltaControlStatus,
  resolveInputsCursor,
  scriptIntegrityStage,
  scriptSourcesDiscoveryCurrentPurpose,
} from "./validation-machine-data.cek-kind.js";
import {
  scanStage,
  scriptSourcesControlStatus,
  scriptSourcesDiscoveryCurrentScriptHash,
} from "./validation-machine-data.redeemer-item-control-data.js";
import {
  nativePayloadChildCount,
  nativeScanCursor,
  valueAndMintKind,
} from "./validation-machine-data.value-and-mint-kind.js";

export const validationSemanticResolverIndex = (
  witness: ValidationMachineWorkWitness,
): number => {
  const auxiliary = witness.auxiliary;
  switch (witness.phase) {
    case "canonicalDecode":
      if (auxiliary === null) return 0;
      if (
        auxiliary.kind === "transactionFieldChunk" ||
        auxiliary.kind === "transactionFieldItem"
      ) {
        return 1;
      }
      break;
    case "compactBinding":
    case "staticLedgerRules":
      if (auxiliary === null) return 0;
      break;
    case "inputSets":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "signatures":
      if (auxiliary === null) {
        return scanStage(witness, "signatures_control") === 2 ? 3 : 0;
      }
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "requiredSignerItem") return 2;
      break;
    case "phaseANativeScripts": {
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "nativeScriptFrame") return 13;
      if (auxiliary.kind !== "nativeScriptToken") break;
      const { stage, cursor, itemLength } = nativeScanCursor(witness);
      if (stage === 1) return 2;
      if (stage === 3) {
        return {
          // This executor proves malformed signatures with no signer proof.
          none: 8,
          membership: 8,
          empty: 9,
          belowFirst: 10,
          aboveLast: 11,
          between: 12,
        }[auxiliary.signerProof.kind];
      }
      if (stage === 4 || stage === 5) {
        const childCount = nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          itemLength,
          stage,
        });
        return childCount !== null && childCount > 0 ? 3 : 4;
      }
      if (stage === 6) {
        const childCount = nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          itemLength,
          stage,
        });
        return childCount !== null && childCount > 0 ? 5 : 6;
      }
      if (stage === 7 || stage === 8) return 7;
      break;
    }
    case "phaseAScriptPreconditions":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "resolveInputs":
      if (auxiliary === null) {
        return resolveInputsCursor(witness) === 0 ? 0 : 1;
      }
      if (auxiliary.kind === "scheduledLedgerLookup") {
        return auxiliary.value === null ? 5 : 2;
      }
      if (auxiliary.kind === "ledgerOutputProofStep") return 3;
      if (auxiliary.kind === "ledgerOutputProofFinalize") return 4;
      break;
    case "scriptSources": {
      const { stage, pendingHashStage } = scriptSourcesControlStatus(witness);
      if (stage === 0) {
        if (pendingHashStage === null) {
          if (auxiliary?.kind === "transactionFieldChunk") return 5;
          if (auxiliary === null) return 6;
          break;
        }
        if (
          pendingHashStage === 0 &&
          auxiliary?.kind === "scriptSourceHashBlock"
        ) {
          return 7;
        }
        if (
          (pendingHashStage === 1 || pendingHashStage === 2) &&
          auxiliary === null
        ) {
          return 8;
        }
        if (pendingHashStage === 3 && auxiliary === null) return 9;
        break;
      }
      if (stage === 9) {
        if (auxiliary?.kind === "scriptSourceScan") {
          const currentScriptHash =
            scriptSourcesDiscoveryCurrentScriptHash(witness);
          if (!auxiliary.scriptHash.equals(currentScriptHash)) return 10;
          if (auxiliary.scriptLanguageTag === 0) return 11;
          if (
            auxiliary.scriptLanguageTag === 3 ||
            auxiliary.scriptLanguageTag === 128
          ) {
            return 12;
          }
          break;
        }
        if (auxiliary === null) return 13;
        break;
      }
      if (stage === 1) {
        if (auxiliary === null) return 14;
        if (auxiliary.kind === "transactionRedeemerItemBegin") return 15;
        if (
          auxiliary.kind === "redeemerItemStep" &&
          auxiliary.redeemerControl === null
        )
          return 28;
        break;
      }
      if (stage === 11) {
        if (auxiliary === null) return 16;
        if (auxiliary.kind === "scriptSourceScan") return 17;
        break;
      }
      if (stage === 12) {
        if (auxiliary === null) return 18;
        if (
          auxiliary.kind === "redeemerScanBegin" ||
          (auxiliary.kind === "redeemerItemStep" &&
            auxiliary.redeemerControl === null)
        ) {
          return 19;
        }
        break;
      }
      if (stage === 10) {
        if (auxiliary === null) return 20;
        if (auxiliary.kind === "redeemerScanBegin") return 21;
        if (
          auxiliary.kind === "redeemerItemStep" &&
          auxiliary.redeemerControl === null
        ) {
          const next = advanceMidgardRedeemerItemProof({
            control: auxiliary.control,
            witness: auxiliary.witness,
          });
          const descriptor =
            next === null ? null : midgardRedeemerItemDescriptor(next);
          if (descriptor === null) return 21;
          const purpose = scriptSourcesDiscoveryCurrentPurpose(witness);
          return descriptor.purposeTag === [0, 1, 3, 6][purpose.purposeKind] &&
            BigInt(descriptor.pointerIndex) === purpose.purposeIndex
            ? 22
            : 21;
        }
        break;
      }
      if (stage === 8) {
        if (auxiliary === null) return 23;
        if (auxiliary.kind === "scriptPurposeScan") return 24;
        break;
      }
      if (stage === 7) {
        if (auxiliary?.kind === "transactionFieldChunk") return 25;
        if (auxiliary?.kind === "scriptPurposeScan") return 26;
        if (auxiliary === null) return 27;
        break;
      }
      if (stage !== 5) return 0;
      if (auxiliary?.kind === "ledgerOutputProofBegin") return 1;
      if (auxiliary?.kind === "ledgerOutputProofStep") return 2;
      if (auxiliary?.kind === "ledgerOutputProofFinalize") return 3;
      if (auxiliary === null) return 4;
      break;
    }
    case "ledgerDelta": {
      const control = ledgerDeltaControlStatus(witness);
      if (auxiliary === null) {
        if (control.pendingStage === 1) return 6;
        if (control.pendingStage === 0) break;
        if (control.stage === 0) return 2;
        if (control.stage === 1) return 4;
        return 7;
      }
      if (auxiliary.kind === "ledgerDeltaOperation") return 0;
      if (auxiliary.kind === "ledgerDeltaReplay") return 1;
      if (auxiliary.kind === "ledgerDeltaOutput") return 3;
      if (auxiliary.kind === "ledgerDeltaProofFrame") return 5;
      break;
    }
    case "scriptIntegrity":
      if (auxiliary === null) return scriptIntegrityStage(witness);
      break;
    case "nativeScripts":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "nativeExecutionDescriptor") {
        return auxiliary.languageTag === 0 ? 1 : 2;
      }
      break;
    // Both kind switches below are exhaustive over their kind unions and
    // return on every arm, so neither needs (nor may carry) a trailing break.
    case "cek":
      switch (cekKind(witness)) {
        case "core":
          return 3;
        case "context":
          return 2;
        case "selection":
          return 1;
        case "finish":
          return 0;
      }

    // eslint-disable-next-line no-fallthrough
    case "valueAndMint":
      switch (valueAndMintKind(witness)) {
        case "begin":
          return 0;
        case "replayBegin":
          return 1;
        case "replayInput":
          return 2;
        case "replayAsset":
          return 3;
        case "replayFinish":
          return 4;
        case "outputDescriptor":
          return 5;
        case "outputAsset":
          return 6;
        case "outputFinish":
          return 7;
        case "mintAsset":
          return 8;
        case "mintFinish":
          return 9;
        case "finalize":
          return 10;
      }

    // eslint-disable-next-line no-fallthrough
    case "terminal":
      break;
  }
  throw new Error(
    `validation evidence ${witness.phase}/${auxiliary?.kind ?? "none"} has no semantic resolver`,
  );
};

export type ValidationOneStepArgument = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: Buffer;
  readonly auxiliaryCbor: Buffer;
  readonly evidenceCbor: Buffer;
  /** Exact adjacent witness from fresh canonical replay for context settlement. */
  readonly cekContextSuccessorWorkWitnessCbor?: Buffer;
  readonly ledgerOutputProofSuccessorWorkWitnessCbor?: Buffer;
  readonly cekRouteMaterial?: CekRouteMaterial;
};

export type CekRouteMaterial = {
  readonly envelopeCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly programEnvelopeHash: Buffer;
};

export const CEK_ROUTE_MATERIAL_KEYS = Object.freeze([
  "envelopeCbor",
  "programMaterialSidecarCbor",
  "programEnvelopeHash",
] as const);
