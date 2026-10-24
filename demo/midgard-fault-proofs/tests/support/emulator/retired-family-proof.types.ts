import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../../../src/runtime.js";

export type RetiredFamilyProofTransaction = Readonly<{
  stage: "init" | "step01" | "step02" | "step03" | "step04" | "remove";
  txHash: string;
  signedCbor: string;
  fee: bigint;
  completeSignedBytes: number;
  executionMemory: bigint;
  executionSteps: bigint;
}>;

export type RetiredFamilyProofInput = Readonly<{
  kind: SDK.EventHistoryKind;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  deployment: Readonly<{
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    references: ReadonlyMap<string, UTxO>;
  }>;
  headerHash: string;
  stateQueueBlockOutRef: string;
  inclusion: Readonly<{
    keyCbor: string;
    valueCbor: string;
    phasRoot: string;
    membershipProofCbor: string;
  }>;
  now: () => number;
  onSigned: (transaction: RetiredFamilyProofTransaction) => void;
}>;

export type RetiredFamilyProofResult = Readonly<{
  kind: SDK.EventHistoryKind;
  headerHash: string;
  verdict: "DepositIdentityAbsent" | "WithdrawalIdentityAbsent";
  computationThreadUnit: string;
  fraudProofUnit: string;
  fraudProofOutRef: string;
  removalTxHashes: readonly string[];
  transactions: readonly RetiredFamilyProofTransaction[];
}>;

export type EligibleFamilyRefusalResult = Readonly<{
  kind: SDK.EventHistoryKind;
  headerHash: string;
  computationThreadUnit: string;
  preservedThreadOutRef: string;
  preservedStateQueueBlockOutRef: string;
  refusalMessage: string;
  transactions: readonly RetiredFamilyProofTransaction[];
}>;
