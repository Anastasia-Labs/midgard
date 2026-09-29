/**
 * buildDeterministicValidationMachineTrace: the phase-by-phase construction of the deterministic
 * validation-machine trace for one transaction.
 */

import { type MidgardValidationMerkleFrontier } from "@al-ft/midgard-core";
import { type MidgardVersionedScript } from "@al-ft/midgard-core/codec";

export type PhaseANativeScriptsScanControl = {
  readonly stage: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8;
  readonly scriptCount: number;
  readonly scriptSeen: number;
  readonly containsNonNativeScript: 0 | 1;
  readonly itemLength: number;
  readonly itemCommitment: Buffer;
  readonly cursor: number;
  readonly stackRoot: Buffer;
  readonly stackDepth: number;
  readonly nodeCount: number;
  readonly result: -1 | 0 | 1;
};
export type ScriptExecutionProofEntry = {
  readonly purpose: ScriptPurposeProofEntry;
  readonly source: ScriptSourceProofEntry;
  readonly sourceIndex: number;
  readonly languageTag: 0 | 3 | 128;
  readonly redeemerLeaf: Buffer;
  readonly leaf: Buffer;
};
export type ScriptPurposeProofEntry = {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Buffer;
  readonly subject: Buffer;
  readonly leaf: Buffer;
};
export type ResolutionScheduleNode = {
  sourceKind: "spend" | "reference";
  key: Buffer;
  nextScheduleHash: Buffer;
  scheduleHash: Buffer;
  proofCbor: Buffer;
};
export type ScriptSourceProofEntry = {
  readonly originKind: "inline" | "reference";
  readonly sourceKey: Buffer;
  readonly script: MidgardVersionedScript;
  readonly authenticatedVersionedItemBytes: Buffer;
  readonly scriptLanguageTag: 0 | 3 | 128;
  readonly scriptHash: Buffer;
  readonly scriptTotalLength: number;
  readonly scriptItemCommitment: Buffer;
  readonly leaf: Buffer;
};
export type SignatureScanControl = {
  readonly stage: 0 | 1 | 2;
  readonly addressCount: number;
  readonly requiredCount: number;
  readonly addressSeen: number;
  readonly requiredSeen: number;
  readonly previousOrderKey: Buffer;
  readonly previousSignerHash: Buffer;
  readonly signerFrontier: MidgardValidationMerkleFrontier;
  readonly invalidSignatureSeen: 0 | 1;
};
export type MintFoldTraceControl = {
  readonly policyCount: number;
  readonly policyCursor: number;
  readonly previousPolicy: Buffer;
  readonly activePolicy: Buffer;
  readonly itemLength: number;
  readonly itemCommitment: Buffer;
  readonly itemCursor: number;
  readonly assetsRemaining: number;
  readonly policyAssetCursor: number;
  readonly previousAsset: Buffer;
  readonly assetFrontier: MidgardValidationMerkleFrontier;
};
