import { Constr } from "@lucid-evolution/lucid";

import { midgardCekCoreStepData } from "./cek-data.js";
import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";
import {
  chunkProofData,
  contextPartsControlData,
  finalContextControlData,
  ledgerOutputProofWitnessData,
  redeemerControlData,
  resolvedFieldCarriageData,
  sequenceSummaryData,
  signerProofData,
  txInfoAssemblyControlData,
} from "./validation-machine-data.ledger-output-proof-witness-data.js";
import {
  originKind,
  redeemerItemControlData,
  redeemerItemProofWitnessData,
  sourceKind,
  valueMutationData,
} from "./validation-machine-data.redeemer-item-control-data.js";
import {
  byteList,
  bytes,
  frontierPeaksData,
  inlineFieldCarriageResolver,
  int,
  ledgerDeltaOperationProofData,
  mpfProofFrameData,
  option,
  type PlutusData,
  proofData,
  record,
  type ValidationMachineFieldCarriageResolver,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

export const validationAuxiliaryWitnessData = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
  resolveFieldCarriage: ValidationMachineFieldCarriageResolver = inlineFieldCarriageResolver,
): PlutusData => {
  if (auxiliary === null) return new Constr(0, []);
  switch (auxiliary.kind) {
    case "transactionFieldChunk":
      return new Constr(1, [
        int(auxiliary.fieldIndex),
        int(auxiliary.itemIndex),
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "requiredSignerItem":
      return new Constr(2, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptToken":
      return new Constr(3, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptFrame":
      return new Constr(4, [
        record([
          bytes(auxiliary.frame.tail),
          int(auxiliary.frame.kind),
          int(auxiliary.frame.childCount),
          int(auxiliary.frame.remaining),
          int(auxiliary.frame.validCount),
          auxiliary.frame.required,
        ]),
      ]);
    case "scheduledLedgerLookup": {
      const fields = [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
      ];
      return auxiliary.value === null
        ? new Constr(6, [...fields, proofData(auxiliary.proofCbor)])
        : new Constr(5, [
            ...fields,
            bytes(auxiliary.value),
            proofData(auxiliary.proofCbor),
            signerProofData(auxiliary.signerProof),
          ]);
    }
    case "resolvedInputReplay":
      return new Constr(7, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "scriptPurposeScan":
      return new Constr(8, [
        int(auxiliary.purposeKind),
        auxiliary.purposeIndex,
        bytes(auxiliary.scriptHash),
        bytes(auxiliary.subject),
        byteList(auxiliary.siblings),
      ]);
    case "scriptSourceScan":
      return new Constr(9, [
        int(auxiliary.sourceIndex),
        originKind(auxiliary.originKind),
        bytes(auxiliary.sourceKey),
        int(auxiliary.scriptLanguageTag),
        bytes(auxiliary.scriptHash),
        int(auxiliary.scriptTotalLength),
        bytes(auxiliary.scriptItemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "redeemerScanBegin":
      return new Constr(10, [
        int(auxiliary.itemIndex),
        int(auxiliary.itemCount),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "nativeExecutionScan":
      return new Constr(11, [
        int(auxiliary.executionIndex),
        int(auxiliary.languageTag),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
        int(auxiliary.source.sourceIndex),
        originKind(auxiliary.source.originKind),
        bytes(auxiliary.source.sourceKey),
        int(auxiliary.source.scriptTotalLength),
        bytes(auxiliary.source.scriptItemCommitment),
        byteList(auxiliary.source.siblings),
        bytes(auxiliary.redeemerLeaf),
        byteList(auxiliary.executionSiblings),
        chunkProofData(auxiliary.firstChunkProof),
      ]);
    case "cekCoreStep":
      return new Constr(12, [midgardCekCoreStepData(auxiliary.step)]);
    case "cekResolvedContextItem":
      return new Constr(13, [
        sourceKind(auxiliary.sourceKind),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekOutputContextItem":
      return new Constr(14, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekSignerContextItem":
      return new Constr(15, [
        frontierPeaksData(auxiliary.frontier),
        int(auxiliary.signerIndex),
        bytes(auxiliary.signerHash),
        byteList(auxiliary.siblings),
      ]);
    case "cekMintContextItem":
      return new Constr(16, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
        option(auxiliary.previous, (head) =>
          record([
            bytes(head.assetName),
            head.quantity,
            sequenceSummaryData(head.tail),
          ]),
        ),
      ]);
    case "cekRedeemerContextSelect":
      return new Constr(17, [
        redeemerControlData(auxiliary.control),
        int(auxiliary.itemIndex),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        int(auxiliary.executionLanguageTag),
        bytes(auxiliary.sourceLeaf),
        byteList(auxiliary.executionSiblings),
        bytes(auxiliary.itemFrontierRoot),
      ]);
    case "redeemerItemStep":
      return new Constr(18, [
        option(auxiliary.redeemerControl, redeemerControlData),
        redeemerItemControlData(auxiliary.control),
        redeemerItemProofWitnessData(auxiliary.witness),
      ]);
    case "cekContextFinalize":
      return new Constr(19, [redeemerControlData(auxiliary.redeemerControl)]);
    case "cekContextFinalizeSpend":
      return new Constr(20, [
        redeemerControlData(auxiliary.redeemerControl),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekContextAssemble":
      return new Constr(21, [contextPartsControlData(auxiliary.control)]);
    case "cekTxInfoFinalize":
      return new Constr(22, [txInfoAssemblyControlData(auxiliary.control)]);
    case "cekContextSeed":
      return new Constr(23, [finalContextControlData(auxiliary.control)]);
    case "valueInputAsset":
      return new Constr(24, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.descriptorCbor),
        int(auxiliary.assetIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        frontierPeaksData(auxiliary.assetFrontier),
        byteList(auxiliary.assetSiblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueOutputDescriptor":
      return new Constr(38, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "valueOutputAsset":
      return new Constr(25, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        int(auxiliary.assetIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        frontierPeaksData(auxiliary.assetFrontier),
        byteList(auxiliary.assetSiblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueMintAsset":
      return new Constr(26, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "ledgerDeltaOperation":
      return new Constr(35, [
        int(auxiliary.operationKind === "delete" ? 0 : 1),
        bytes(auxiliary.key),
        bytes(auxiliary.value),
        ledgerDeltaOperationProofData(
          auxiliary.mutationStep.proofFoldTrace.descriptor,
          auxiliary.operationMembership,
        ),
      ]);
    case "ledgerDeltaReplay":
      return new Constr(27, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "ledgerDeltaOutput":
      return new Constr(28, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerDeltaProofFrame":
      return new Constr(34, [
        mpfProofFrameData(auxiliary.frame),
        byteList(auxiliary.siblings),
        bytes(auxiliary.opening),
      ]);
    case "transactionRedeemerItemBegin":
      return new Constr(29, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "transactionFieldItem":
      return new Constr(30, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "ledgerOutputProofBegin":
      return new Constr(31, [
        int(auxiliary.outputIndex),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerOutputProofStep":
      return new Constr(32, [ledgerOutputProofWitnessData(auxiliary.witness)]);
    case "ledgerOutputProofFinalize":
      return new Constr(33, [signerProofData(auxiliary.signerProof)]);
    case "scriptSourceHashBlock":
      return new Constr(36, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
      ]);
    case "mintFoldAsset":
      return new Constr(39, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
      ]);
    case "cekRedeemerContextSkip":
      return new Constr(40, [
        redeemerControlData(auxiliary.control),
        bytes(auxiliary.purposeLeaf),
        bytes(auxiliary.sourceLeaf),
        byteList(auxiliary.executionSiblings),
      ]);
    case "nativeExecutionDescriptor":
      return new Constr(37, [
        int(auxiliary.executionIndex),
        int(auxiliary.languageTag),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
        int(auxiliary.source.sourceIndex),
        originKind(auxiliary.source.originKind),
        bytes(auxiliary.source.sourceKey),
        int(auxiliary.source.scriptTotalLength),
        bytes(auxiliary.source.scriptItemCommitment),
        byteList(auxiliary.source.siblings),
        bytes(auxiliary.redeemerLeaf),
        byteList(auxiliary.executionSiblings),
        option(auxiliary.firstChunkProof, chunkProofData),
        frontierPeaksData(auxiliary.signerFrontier),
      ]);
  }
};
