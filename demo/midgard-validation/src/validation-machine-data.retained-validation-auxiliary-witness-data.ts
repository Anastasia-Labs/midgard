import { midgardFieldCommitment } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Constr } from "@lucid-evolution/lucid";

import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";
import { signerProofData } from "./validation-machine-data.ledger-output-proof-witness-data.js";
import { validationAuxiliaryWitnessData } from "./validation-machine-data.validation-auxiliary-witness-data.js";
import { type PlutusData } from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

/** Retain bounded field references before a consuming L1 transaction supplies carriage indices. */
export const retainedValidationAuxiliaryWitnessData = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
): PlutusData => {
  if (auxiliary === null) return validationAuxiliaryWitnessData(auxiliary);
  switch (auxiliary.kind) {
    case "transactionFieldChunk":
      return new Constr(1, [
        BigInt(auxiliary.fieldIndex),
        BigInt(auxiliary.itemIndex),
        new Constr(0, [
          new Constr(0, [
            BigInt(auxiliary.fieldIndex),
            BigInt(auxiliary.fieldPreimage.length),
            midgardFieldCommitment(auxiliary.fieldPreimage).toString("hex"),
          ]),
        ]),
      ]);
    case "requiredSignerItem":
      return new Constr(2, [
        new Constr(0, [
          new Constr(0, [
            BigInt(auxiliary.fieldIndex),
            BigInt(auxiliary.fieldPreimage.length),
            midgardFieldCommitment(auxiliary.fieldPreimage).toString("hex"),
          ]),
        ]),
        signerProofData(auxiliary.signerProof),
      ]);
    case "transactionRedeemerItemBegin":
    case "transactionFieldItem":
      return new Constr(auxiliary.kind === "transactionFieldItem" ? 30 : 29, [
        new Constr(0, [
          new Constr(0, [
            BigInt(auxiliary.fieldIndex),
            BigInt(auxiliary.fieldPreimage.length),
            midgardFieldCommitment(auxiliary.fieldPreimage).toString("hex"),
          ]),
        ]),
      ]);
    case "ledgerOutputProofBegin":
    case "ledgerOutputProofStep":
    case "ledgerOutputProofFinalize":
    case "nativeScriptToken":
    case "nativeScriptFrame":
    case "scriptSourceHashBlock":
    case "mintFoldAsset":
    case "scheduledLedgerLookup":
    case "resolvedInputReplay":
    case "scriptPurposeScan":
    case "scriptSourceScan":
    case "redeemerScanBegin":
    case "nativeExecutionScan":
    case "nativeExecutionDescriptor":
    case "cekCoreStep":
    case "cekResolvedContextItem":
    case "cekOutputContextItem":
    case "cekSignerContextItem":
    case "cekMintContextItem":
    case "cekRedeemerContextSelect":
    case "redeemerItemStep":
    case "cekContextFinalize":
    case "cekContextFinalizeSpend":
    case "cekContextAssemble":
    case "cekTxInfoFinalize":
    case "cekContextSeed":
    case "valueInputAsset":
    case "valueOutputDescriptor":
    case "valueOutputAsset":
    case "valueMintAsset":
    case "ledgerDeltaOperation":
    case "ledgerDeltaReplay":
    case "ledgerDeltaOutput":
    case "ledgerDeltaProofFrame":
    case "cekRedeemerContextSkip":
      return validationAuxiliaryWitnessData(auxiliary);
  }
};
