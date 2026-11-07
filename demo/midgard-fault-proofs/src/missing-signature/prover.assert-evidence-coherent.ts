import {
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { outRefLabel, type ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { MissingSignatureContracts } from "./contracts.js";
import { type MissingSignatureFinding } from "./finding.js";
import {
  type MissingSignatureCatalogueCategory,
  missingSignatureSubmitError,
} from "./submit-common.js";

export type MissingSignatureProverPolicy = {
  /** Minimum L1 depth before a new thread may be initialized. */
  readonly minSettlementDepth: bigint;
  /** Projected fee cap for init, proof steps, resumable scans and removal. */
  readonly maxThreadBudgetLovelace: bigint | null;
  readonly assumedFeePerTxLovelace: bigint;
  /** Enforced by the autonomous adapter. */
  readonly singleFlight: number;
};

export const MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS: MissingSignatureProverPolicy =
  Object.freeze({
    minSettlementDepth: 2_160n,
    maxThreadBudgetLovelace: 20_000_000n,
    assumedFeePerTxLovelace: 1_450_000n,
    singleFlight: 1,
  });

export type MissingSignatureSubjectEvidence = {
  readonly nativeTxCompactCbor: string;
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly MidgardAddressWitness[];
  readonly witnessSetCompact: NativeTxWitnessSetCompact;
};

export type MissingSignatureProverEvidence = {
  /** Counted transaction-root inclusion used by step-01. */
  readonly txInclusion: (
    finding: MissingSignatureFinding,
  ) => Promise<SubmitStep01TxInclusion>;
  /** Canonical field-4/field-7 evidence used by steps 02 and 04. */
  readonly subjectTx: (
    finding: MissingSignatureFinding,
  ) => Promise<MissingSignatureSubjectEvidence>;
};

export type MissingSignatureProverObservations = {
  readonly settlementDepthOf?: (
    fraudulentBlockOutRef: string,
  ) => Promise<bigint>;
};

export type MissingSignatureProverEvent = {
  readonly phase:
    | "boundary"
    | "policy"
    | "init"
    | "step01"
    | "step02"
    | "step03"
    | "step04"
    | "outcome";
  readonly message: string;
  readonly headerHash: string;
  readonly txHash?: string;
  readonly threadOutRef?: string;
};

export type MissingSignatureReferenceScripts = {
  readonly step01: UTxO;
  readonly step02: UTxO;
  readonly step03: UTxO;
  readonly step04: UTxO;
};

export type MissingSignatureFieldCertificates = {
  /** Required only if the actual field-4 plan selects certified carriage. */
  readonly step02?: UTxO;
  /** Required only if the actual field-7 plan selects certified carriage. */
  readonly step04?: UTxO;
};

export type MissingSignatureProverDeps = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingSignatureContracts;
  readonly category: MissingSignatureCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly evidence: MissingSignatureProverEvidence;
  readonly observations: MissingSignatureProverObservations;
  readonly journal: (
    event: MissingSignatureProverEvent,
  ) => void | Promise<void>;
  readonly policy: MissingSignatureProverPolicy;
  /** Owner ruling: all four steps are sourced by reference, never inline. */
  readonly referenceScriptUtxos: MissingSignatureReferenceScripts;
  /** Published shared minting and PHAS witnesses used across the journey. */
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Externally minted §8.6 manifests; certification is deployment-owned. */
  readonly fieldCertificates?: MissingSignatureFieldCertificates;
  /** Force carriage publication in tests; tier selection remains planner-owned. */
  readonly publishCarriage?: boolean;
};

export type MissingSignatureProofOutcome =
  | {
      readonly kind: "proven";
      readonly fraudProofUnit: string;
      readonly fraudProofOutRef: string;
      readonly txHashes: readonly string[];
    }
  | {
      readonly kind: "refused";
      readonly refusal:
        | "classification"
        | "policy"
        | "duplicate"
        | "alreadyProven";
      readonly reason: string;
    }
  | {
      readonly kind: "stalled";
      readonly reason: string;
      readonly threadOutRef: string | null;
      readonly cause: unknown;
    };

export type MissingSignatureThreadPosition =
  | { readonly step: "none" }
  | {
      readonly step: "step01" | "step02" | "step03" | "step04";
      readonly threadUtxo: UTxO;
    };

/** Locate one live thread; multiple matches are refused rather than guessed. */
export const locateMissingSignatureThread = async ({
  lucid,
  contracts,
  threadUnit,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingSignatureContracts;
  readonly threadUnit: string;
}): Promise<MissingSignatureThreadPosition> => {
  let found: Exclude<MissingSignatureThreadPosition, { step: "none" }> | null =
    null;
  for (const stepIndex of [0, 1, 2, 3] as const) {
    const utxos = await lucid.utxosAtWithUnit(
      contracts.steps[stepIndex].spendingScriptAddress,
      threadUnit,
    );
    if (utxos.length > 1) {
      throw missingSignatureSubmitError(
        `found ${utxos.length.toString()} UTxOs carrying thread ${threadUnit} at step 0${(
          stepIndex + 1
        ).toString()} — expected exactly one.`,
      );
    }
    const threadUtxo = utxos[0];
    if (threadUtxo === undefined) continue;
    if (found !== null) {
      throw missingSignatureSubmitError(
        `thread ${threadUnit} appears at both ${outRefLabel(found.threadUtxo)} and ${outRefLabel(threadUtxo)}.`,
      );
    }
    const step = (["step01", "step02", "step03", "step04"] as const)[stepIndex];
    found = { step, threadUtxo };
  }
  return found ?? { step: "none" };
};

export const toError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause));

export const requireDatum = (utxo: UTxO): string => {
  if (utxo.datum == null) {
    throw missingSignatureSubmitError(
      `thread ${outRefLabel(utxo)} has no inline datum.`,
    );
  }
  return utxo.datum;
};

export const assertEvidenceCoherent = ({
  finding,
  txInclusion,
  subject,
}: {
  readonly finding: MissingSignatureFinding;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly subject: MissingSignatureSubjectEvidence;
}): void => {
  if (txInclusion.nativeTxId !== finding.txId) {
    throw missingSignatureSubmitError(
      `inclusion evidence names transaction ${txInclusion.nativeTxId}, not finding transaction ${finding.txId}.`,
    );
  }
  if (txInclusion.nativeTxCompactCbor !== subject.nativeTxCompactCbor) {
    throw missingSignatureSubmitError(
      "step-01 inclusion bytes and field-opening subject bytes differ.",
    );
  }
  if (subject.nativeTxCompactCbor !== finding.nativeTxCompactCbor) {
    throw missingSignatureSubmitError(
      "opening evidence bytes differ from the finding's authenticated compact transaction.",
    );
  }
  if (
    txInclusion.nativeTx.witness_set_hash !== finding.committedWitnessSetHash
  ) {
    throw missingSignatureSubmitError(
      `evidence witness-set hash does not match finding anchor ${finding.committedWitnessSetHash}.`,
    );
  }
  const accused =
    subject.requiredSignerHashes[Number(finding.accusedRequiredSignerIndex)];
  if (accused?.toLowerCase() !== finding.accusedRequiredSignerHash) {
    throw missingSignatureSubmitError(
      `required-signer ordinal ${finding.accusedRequiredSignerIndex.toString()} does not select finding hash ${finding.accusedRequiredSignerHash}.`,
    );
  }
};
