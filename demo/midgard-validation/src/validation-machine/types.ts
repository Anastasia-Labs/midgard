/**
 * The validation machine's replay input, work witnesses, signer-set proofs, and the deterministic trace shape.
 */

import {
  type MidgardBoundedItemChunkProofV1,
  type MidgardConsensusProfileV1,
  type MidgardLedgerOutputProofWitnessV1,
  type MidgardMpfProofFrameV1,
  type MidgardRedeemerItemProofControlV1,
  type MidgardRedeemerItemProofWitnessV1,
  type MidgardValidationMachineStateV1,
  type MidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleMembershipV1,
  type MidgardValidationPhaseName,
  type MidgardValidationTraceTree,
} from "@al-ft/midgard-core";

import {
  type MidgardCekContextPartsControlV1,
  type MidgardCekFinalContextControlV1,
  type MidgardCekRedeemerContextControlV1,
  type MidgardCekTxInfoAssemblyControlV1,
} from "../cek-context.js";
import { type MidgardCekExecutionStepV1 } from "../cek-executor.js";
import type { RejectCode } from "../types.js";
import {
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
  type ValidationMachineLedgerOp,
  type ValidationMachineValueMutationStep,
} from "./ledger-mutation.js";
import { type ValidationMachineNativeScriptFrameV1 } from "./native-script-frame.js";

export type ValidationMachineReplayInput = {
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly eventKeyCbor: Buffer;
  readonly transactionId: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor?: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorUtxosRoot: string;
  readonly postUtxosRoot: string;
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly expectedLedgerOps: readonly ValidationMachineLedgerOp[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly expectedVerdict: "accepted" | "rejected";
  readonly expectedRejectionCode: RejectCode | null;
  /**
   * Verdict carried by the COMMITTED forced leaf — the operator's
   * adjudication, which is what `source_binding_is_exact` reveals on-chain
   * and therefore what the machine's `transaction_commitment` must bind.
   * Defaults to the replay's own verdict, which is exact on the classifier
   * path (the leaf is produced from this replay, and the machine aborts on
   * any expected/replayed divergence). A dispute trace replayed AGAINST an
   * operator leaf whose verdict it contests must pass the leaf's verdict
   * here, or its states bind a commitment the committed leaf does not carry.
   */
  readonly committedForcedVerdict?: "accepted" | "rejected";
  readonly blockEndTimeMs: number;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
};

export type ValidationMachineWorkWitness = {
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly cbor: Buffer;
  readonly auxiliary:
    | {
        /**
         * One item of one committed field, reached through §8's door. Nine of
         * the machine's fifteen per-item sites match this arm, across all eight
         * phases that read a field.
         *
         * It used to carry a counted `(collectionProof, chunkProof)` pair
         * checked against the §4 flat field commitment — a predicate no honest
         * prover could satisfy (#592). What replaces the pair is not a smaller
         * proof but *no* proof: the carriage names where the field's preimage
         * is, the door authenticates the whole preimage once against the flat
         * commitment, and the item is then a slice.
         *
         * `fieldIndex` is on the wire because §4 removed field-index domain
         * separation and two phases read more than one slot — `canonicalDecode`,
         * which walks all nine from its own control, and `inputSets`, which
         * alternates fields 0 and 1. `itemIndex` is on the wire because two
         * sites let the prover choose the item order and the claimed successor
         * pins it.
         *
         * `fieldPreimage` is the plan input, not wire: it is replaced by the §8
         * carriage §8.4 admits for its length when the auxiliary is encoded
         * (#600). See {@link ValidationMachineFieldCarriagePlanInputV1}.
         */
        readonly kind: "transactionFieldChunk";
        readonly fieldIndex: number;
        readonly itemIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        /**
         * `canonicalDecode`'s complete-item step: one item read whole rather
         * than chunk by chunk. Field index and item index come from the phase's
         * control, so the carriage is the entire wire surface — `fieldIndex`
         * here is the plan input's, never encoded (#600).
         */
        readonly kind: "transactionFieldItem";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        readonly kind: "ledgerOutputProofBegin";
        readonly outputIndex: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "ledgerOutputProofStep";
        readonly witness: MidgardLedgerOutputProofWitnessV1;
      }
    | {
        readonly kind: "ledgerOutputProofFinalize";
        readonly descriptorCbor: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        /**
         * A field-4 required-signer item plus the signer-set membership
         * evidence the step decides on. No field or item index **on the wire**:
         * the field is 4 by construction and the item index is
         * `control.required_seen`. `fieldIndex` is carried only as the plan
         * input's, and is never encoded (#600).
         */
        readonly kind: "requiredSignerItem";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptToken";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptFrame";
        readonly frame: ValidationMachineNativeScriptFrameV1;
      }
    | {
        readonly kind: "scriptSourceHashBlock";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
      }
    | {
        readonly kind: "mintFoldAsset";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
      }
    | {
        readonly kind: "scheduledLedgerLookup";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer | null;
        readonly proofCbor: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "resolvedInputReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
      }
    | {
        readonly kind: "scriptPurposeScan";
        readonly purposeKind: 0 | 1 | 2 | 3;
        readonly purposeIndex: bigint;
        readonly scriptHash: Buffer;
        readonly subject: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "scriptSourceScan";
        readonly sourceIndex: number;
        readonly originKind: "inline" | "reference";
        readonly sourceKey: Buffer;
        readonly scriptLanguageTag: 0 | 3 | 128;
        readonly scriptHash: Buffer;
        readonly scriptTotalLength: number;
        readonly scriptItemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "redeemerScanBegin";
        readonly itemIndex: number;
        readonly itemCount: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        /**
         * `scriptSources` stage 1 (field 8, one redeemer item) and stage 4
         * (field 2, one output item). Both stages need the item's length and its
         * `bounded_item_v1` commitment and never look at its bytes, so the
         * door's derived commitment is all the carriage has to yield; field
         * index and item index are fixed by the stage and its cursor, so
         * `fieldIndex` here is the plan input's and is never encoded (#600).
         *
         * This is the C21-STAGE4 site. Its evidence is O(1) in output size
         * exactly when the resolved carriage is tier 2 or 3
         * (`onchain/aiken/lib/midgard/validation-machine-v1.ak:9189-9192`), which
         * is what resolving at evidence-commitment time restores.
         */
        readonly kind: "transactionRedeemerItemBegin";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        readonly kind: "nativeExecutionScan";
        readonly executionIndex: number;
        readonly languageTag: 0 | 3 | 128;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly source: {
          readonly sourceIndex: number;
          readonly originKind: "inline" | "reference";
          readonly sourceKey: Buffer;
          readonly scriptTotalLength: number;
          readonly scriptItemCommitment: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly redeemerLeaf: Buffer;
        readonly executionSiblings: readonly Buffer[];
        readonly firstChunkProof: MidgardBoundedItemChunkProofV1;
      }
    | {
        readonly kind: "nativeExecutionDescriptor";
        readonly executionIndex: number;
        readonly languageTag: 0 | 3 | 128;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly source: {
          readonly sourceIndex: number;
          readonly originKind: "inline" | "reference";
          readonly sourceKey: Buffer;
          readonly scriptTotalLength: number;
          readonly scriptItemCommitment: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly redeemerLeaf: Buffer;
        readonly executionSiblings: readonly Buffer[];
        readonly firstChunkProof: MidgardBoundedItemChunkProofV1 | null;
        readonly signerFrontier: MidgardValidationMerkleFrontierV1;
      }
    | {
        readonly kind: "cekCoreStep";
        readonly step: MidgardCekExecutionStepV1;
      }
    | {
        readonly kind: "cekResolvedContextItem";
        readonly sourceKind: "spend" | "reference";
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekOutputContextItem";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekSignerContextItem";
        readonly frontier: MidgardValidationMerkleFrontierV1;
        readonly signerIndex: number;
        readonly signerHash: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekMintContextItem";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekRedeemerContextSelect";
        readonly control: MidgardCekRedeemerContextControlV1;
        readonly itemIndex: number;
        readonly itemCount: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly redeemerSiblings: readonly Buffer[];
        readonly purposeFrontierIndex: number;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
      }
    | {
        readonly kind: "redeemerItemStep";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1 | null;
        readonly control: MidgardRedeemerItemProofControlV1;
        readonly witness: MidgardRedeemerItemProofWitnessV1;
      }
    | {
        readonly kind: "cekContextFinalize";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
      }
    | {
        readonly kind: "cekContextFinalizeSpend";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekContextAssemble";
        readonly control: MidgardCekContextPartsControlV1;
      }
    | {
        readonly kind: "cekTxInfoFinalize";
        readonly control: MidgardCekTxInfoAssemblyControlV1;
      }
    | {
        readonly kind: "cekContextSeed";
        readonly control: MidgardCekFinalContextControlV1;
      }
    | {
        readonly kind: "valueInputAsset";
        readonly sourceKind: "spend";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly descriptorCbor: Buffer;
        readonly assetIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly assetFrontier: MidgardValidationMerkleFrontierV1;
        readonly assetSiblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueOutputDescriptor";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "valueOutputAsset";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly assetIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly assetFrontier: MidgardValidationMerkleFrontierV1;
        readonly assetSiblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueMintAsset";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "ledgerDeltaOperation";
        readonly operationKind: "delete" | "insert";
        readonly key: Buffer;
        readonly value: Buffer;
        readonly mutationStep: ValidationMachineLedgerMutationStep;
        readonly operationMembership: MidgardValidationMerkleMembershipV1;
      }
    | {
        readonly kind: "ledgerDeltaReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
      }
    | {
        readonly kind: "ledgerDeltaOutput";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "ledgerDeltaProofFrame";
        readonly frame: MidgardMpfProofFrameV1;
        readonly siblings: readonly Buffer[];
      }
    | null;
};

export type ValidationMachineSignerSetProof =
  | { readonly kind: "none" }
  | {
      readonly kind: "membership";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly signerIndex: number;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "empty";
      readonly frontier: MidgardValidationMerkleFrontierV1;
    }
  | {
      readonly kind: "belowFirst";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly firstSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "aboveLast";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lastSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "between";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lowerIndex: number;
      readonly lowerSignerHash: Buffer;
      readonly lowerSiblings: readonly Buffer[];
      readonly upperSignerHash: Buffer;
      readonly upperSiblings: readonly Buffer[];
    };

export type DeterministicValidationMachineTrace = {
  readonly validationContextCbor: Buffer;
  /** Canonical, immutable input material for the CEK selection transition. */
  readonly programMaterialSidecarCbor: Buffer;
  readonly states: readonly MidgardValidationMachineStateV1[];
  readonly witnesses: readonly ValidationMachineWorkWitness[];
  readonly tree: MidgardValidationTraceTree;
  readonly verdict: "accepted" | "rejected";
  readonly rejectionCode: RejectCode | null;
  readonly ledgerOps: readonly ValidationMachineLedgerOp[];
};
