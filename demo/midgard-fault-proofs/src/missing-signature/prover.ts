/**
 * Consumer-agnostic proving core for the `missing-signature` family.
 *
 * The core is deliberately a small state-machine over the four step
 * addresses.  Chain state, rather than a local checkpoint, is authoritative:
 * every invocation reconstructs the position from the computation-thread
 * asset name and resumes from the holding address.  An unexpected failure is
 * returned as `stalled`; cancellation remains an explicit prover operation.
 */
import {
  FraudProofComputationThreadStepDatum,
  type MidgardAddressWitness,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
  missingSignatureThreadTokenAssetName,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { outRefLabel, type ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { MissingSignatureContracts } from "./contracts.js";
import {
  assertMissingSignatureFindingProvable,
  type MissingSignatureFinding,
} from "./finding.js";
import {
  type MissingSignatureCatalogueCategory,
  missingSignatureSubmitError,
} from "./submit-common.js";
import { submitMissingSignatureInit } from "./submit-missing-signature-init.js";
import { submitMissingSignatureStep01 } from "./submit-missing-signature-step-01.js";
import { submitMissingSignatureStep02 } from "./submit-missing-signature-step-02.js";
import { submitMissingSignatureStep03 } from "./submit-missing-signature-step-03.js";
import { submitMissingSignatureStep04 } from "./submit-missing-signature-step-04.js";

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

const toError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause));

const requireDatum = (utxo: UTxO): string => {
  if (utxo.datum == null) {
    throw missingSignatureSubmitError(
      `thread ${outRefLabel(utxo)} has no inline datum.`,
    );
  }
  return utxo.datum;
};

const assertEvidenceCoherent = ({
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

export const runMissingSignatureProver = async (
  finding: MissingSignatureFinding,
  deps: MissingSignatureProverDeps,
): Promise<MissingSignatureProofOutcome> => {
  const { lucid, contracts, policy, signer } = deps;
  const headerHash = finding.headerHash;
  const journal = async (
    event: Omit<MissingSignatureProverEvent, "headerHash">,
  ) => deps.journal({ ...event, headerHash });
  const refused = async (
    refusal: "classification" | "policy" | "duplicate" | "alreadyProven",
    reason: string,
  ): Promise<MissingSignatureProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `refused (${refusal}): ${reason}`,
    });
    return { kind: "refused", refusal, reason };
  };
  const stalled = async (
    reason: string,
    threadOutRef: string | null,
    cause: unknown,
  ): Promise<MissingSignatureProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `STALLED: ${reason}`,
      ...(threadOutRef === null ? {} : { threadOutRef }),
    });
    return { kind: "stalled", reason, threadOutRef, cause };
  };

  try {
    assertMissingSignatureFindingProvable(finding);
  } catch (cause) {
    return refused("classification", toError(cause).message);
  }

  const assetName = missingSignatureThreadTokenAssetName(
    deps.category.categoryId,
    headerHash,
  );
  const threadUnit = toUnit(contracts.computationThread.policyId, assetName);
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, assetName);
  const existingProofs = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    fraudProofUnit,
  );
  if (existingProofs.length > 0) {
    return refused(
      "alreadyProven",
      `fraud-proof token ${fraudProofUnit} already exists at ${outRefLabel(existingProofs[0]!)}.`,
    );
  }

  let position: MissingSignatureThreadPosition;
  try {
    position = await locateMissingSignatureThread({
      lucid,
      contracts,
      threadUnit,
    });
  } catch (cause) {
    return stalled(
      `thread discovery failed: ${toError(cause).message}`,
      null,
      cause,
    );
  }
  if (position.step !== "none") {
    try {
      const datum = Data.from(
        requireDatum(position.threadUtxo),
        FraudProofComputationThreadStepDatum,
      );
      if (datum.fraud_prover !== signer.paymentKeyHash) {
        return refused(
          "duplicate",
          `live thread at ${outRefLabel(position.threadUtxo)} names fraud prover ${datum.fraud_prover}, not this wallet.`,
        );
      }
    } catch (cause) {
      return stalled(
        `live thread datum is invalid: ${toError(cause).message}`,
        outRefLabel(position.threadUtxo),
        cause,
      );
    }
  }

  let txInclusion: SubmitStep01TxInclusion;
  let subject: MissingSignatureSubjectEvidence;
  try {
    [txInclusion, subject] = await Promise.all([
      deps.evidence.txInclusion(finding),
      deps.evidence.subjectTx(finding),
    ]);
    assertEvidenceCoherent({ finding, txInclusion, subject });
  } catch (cause) {
    return position.step === "none"
      ? refused("classification", toError(cause).message)
      : stalled(
          `evidence reconstruction failed on a live thread: ${toError(cause).message}`,
          outRefLabel(position.threadUtxo),
          cause,
        );
  }

  if (position.step === "none") {
    if (policy.minSettlementDepth > 0n) {
      if (deps.observations.settlementDepthOf === undefined) {
        return refused(
          "policy",
          "settlement-depth policy is enabled but no observer is configured.",
        );
      }
      const depth = await deps.observations.settlementDepthOf(
        finding.fraudulentBlockOutRef,
      );
      if (depth < policy.minSettlementDepth) {
        return refused(
          "policy",
          `faulted block depth ${depth.toString()} is below ${policy.minSettlementDepth.toString()}.`,
        );
      }
    }
    if (policy.maxThreadBudgetLovelace !== null) {
      const extraScans = Math.floor(
        Math.max(0, subject.addrTxWits.length - 1) /
          MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
      );
      const projected = BigInt(6 + extraScans) * policy.assumedFeePerTxLovelace;
      if (projected > policy.maxThreadBudgetLovelace) {
        return refused(
          "policy",
          `projected thread cost ${projected.toString()} lovelace exceeds cap ${policy.maxThreadBudgetLovelace.toString()}.`,
        );
      }
    }
  }

  type Cursor = "init" | "step01" | "step02" | "step03" | "step04";
  let cursor: Cursor = position.step === "none" ? "init" : position.step;
  let currentOutRef =
    position.step === "none" ? null : outRefLabel(position.threadUtxo);
  const txHashes: string[] = [];
  let step04Transactions = 0;
  const maximumStep04Transactions =
    Math.floor(
      Math.max(0, subject.addrTxWits.length - 1) /
        MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
    ) + 1;

  try {
    while (true) {
      switch (cursor) {
        case "init": {
          const result = await submitMissingSignatureInit({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            category: deps.category,
            catalogue: deps.catalogue,
            signer,
            fraudulentBlockOutRef: finding.fraudulentBlockOutRef,
            fraudulentHeaderHash: finding.headerHash,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "init",
            message: "computation thread initialized",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step01";
          break;
        }
        case "step01": {
          const result = await submitMissingSignatureStep01({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            stateQueueBlockOutRef: finding.fraudulentBlockOutRef,
            txInclusion,
            referenceScriptUtxo: deps.referenceScriptUtxos.step01,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step01",
            message: "transaction bound",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step02";
          break;
        }
        case "step02": {
          const result = await submitMissingSignatureStep02({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            requiredSignerHashes: subject.requiredSignerHashes,
            nativeTxCompactCbor: subject.nativeTxCompactCbor,
            badRequiredSignerHashIndex: finding.accusedRequiredSignerIndex,
            publishCarriage: deps.publishCarriage,
            certificateUtxo: deps.fieldCertificates?.step02,
            referenceScriptUtxo: deps.referenceScriptUtxos.step02,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step02",
            message: "required signer selected",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step03";
          break;
        }
        case "step03": {
          const result = await submitMissingSignatureStep03({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            missingRequiredSignerVkey: finding.resolvedVkey!,
            referenceScriptUtxo: deps.referenceScriptUtxos.step03,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step03",
            message: "verification-key preimage lifted",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step04";
          break;
        }
        case "step04": {
          step04Transactions += 1;
          if (step04Transactions > maximumStep04Transactions) {
            throw missingSignatureSubmitError(
              `step-04 exceeded its deterministic ${maximumStep04Transactions.toString()}-transaction scan schedule.`,
            );
          }
          const result = await submitMissingSignatureStep04({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            addrTxWits: subject.addrTxWits,
            nativeTxCompactCbor: subject.nativeTxCompactCbor,
            witnessSetCompact: subject.witnessSetCompact,
            publishCarriage: deps.publishCarriage,
            certificateUtxo: deps.fieldCertificates?.step04,
            referenceScriptUtxo: deps.referenceScriptUtxos.step04,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          if (result.kind === "advanced") {
            currentOutRef = result.nextThreadOutRef;
            await journal({
              phase: "step04",
              message: `absence scan advanced to witness ${result.nextItemIndex.toString()}`,
              txHash: result.txHash,
              threadOutRef: currentOutRef,
            });
            break;
          }
          await journal({
            phase: "step04",
            message: "fraud-proof token minted",
            txHash: result.txHash,
            threadOutRef: result.fraudProofOutRef,
          });
          await journal({ phase: "outcome", message: "proven" });
          return {
            kind: "proven",
            fraudProofUnit: result.fraudProofUnit,
            fraudProofOutRef: result.fraudProofOutRef,
            txHashes,
          };
        }
      }
    }
  } catch (cause) {
    return stalled(
      `unexpected abort at ${cursor}: ${toError(cause).message}`,
      currentOutRef,
      cause,
    );
  }
};

/** The proving core as an Effect for watcher/runtime composition. */
export const proveMissingSignatureFault = (
  finding: MissingSignatureFinding,
  deps: MissingSignatureProverDeps,
): Effect.Effect<MissingSignatureProofOutcome, Error> =>
  Effect.tryPromise({
    try: () => runMissingSignatureProver(finding, deps),
    catch: toError,
  });
