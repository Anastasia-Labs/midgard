import {
  assertSecurityGradeEvidence,
  type AuthenticatedStateQueueHeaderObservation,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";

import type { FraudProofWorkflowTerminal } from "./journal.js";
import {
  type FraudProofL1Source,
  fraudProofSignedTransactionRecovery,
  withFraudProofL1Recovery,
} from "./l1-source.js";
import type { FraudProofWorkflowTerminalVerifier } from "./orchestrator.js";
import { FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER } from "./orchestrator.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  deriveFraudProofRawL1FamilyStage,
  deriveRetainedStateQueueHeaderObservationFromRawL1,
  type FraudProofRawL1FamilyDefinition,
  fraudProofRawL1SnapshotRequestForFamily,
} from "./raw-l1-family-derivation.js";
import {
  createFraudProofAuthenticatedPublicationObserver,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseEconomicsPolicy } from "./release-economics-policy.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

export const DOUBLE_SPEND_WORKFLOW_ADAPTER =
  "midgard-double-spend-production-workflow-adapter-v1" as const;

export type DoubleSpendWorkflowStage =
  | { readonly kind: "not_started"; readonly stateQueueBlockOutRef: string }
  | {
      readonly kind: "step_01" | "step_02";
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "step_03" | "step_04";
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "proof_token";
      readonly fraudProofOutRef: string;
      readonly stateQueueBlockOutRef: string;
      /** Changes after every descendant removal, giving each tx a stable id. */
      readonly nextRemovalOutRef: string;
    }
  | {
      readonly kind: "removed";
      readonly terminal: FraudProofWorkflowTerminal;
    };

/**
 * Integration port for L1 stage observations. This type alone is not an
 * authentication boundary: production registration remains blocked until a
 * concrete raw local-node/provider implementation derives these facts.
 */
export interface DoubleSpendL1ObservationPort
  extends Pick<
    FraudProofAuthenticatedPublicationObserver,
    "observeSignedTransaction" | "rebroadcastSignedTransaction"
  > {
  readonly publications?: FraudProofAuthenticatedPublicationObserver;
  observeHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservation>;
  observeRetainedHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservation>;
  transactionConfirmed?(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: { readonly headerHash: string }): Promise<{
    readonly provenance: EvidenceProvenance;
    readonly stage: DoubleSpendWorkflowStage;
  }>;
}

/**
 * Production observation port: the provider returns only untrusted exact bytes;
 * family stage and terminal facts are derived locally after strict admission.
 */
export const createDoubleSpendRawL1ObservationPort = ({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "doubleSpend";
  };
}) => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY ||
    definition.computationThread.steps.length !== 4
  ) {
    throw new Error("double-spend raw L1 observation authority is incomplete");
  }
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error("double-spend raw L1 observation changed the header");
    }
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    });
  };
  return {
    publications: createFraudProofAuthenticatedPublicationObserver({
      authority,
      releaseFinality,
    }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      (await capture(headerHash)).transactions.some(
        (transaction) => transaction.txHash === txHash,
      ),
    observeHeader: async ({ headerHash }) =>
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observeRetainedHeader: async ({ headerHash }) =>
      await deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      const derived = await deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition,
        releaseEconomics,
      });
      const stage: DoubleSpendWorkflowStage =
        derived.kind === "step"
          ? {
              kind: `step_0${derived.step}` as
                | "step_01"
                | "step_02"
                | "step_03"
                | "step_04",
              threadOutRef: derived.threadOutRef,
              stateQueueBlockOutRef: derived.stateQueueBlockOutRef,
            }
          : derived;
      return { provenance: snapshot.provenance, stage };
    },
  } satisfies DoubleSpendL1ObservationPort;
};

/** Production construction over the fault-proof L1 source. */
export const createDoubleSpendL1ObservationPort = ({
  l1,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly l1: FraudProofL1Source;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "doubleSpend";
  };
}): DoubleSpendL1ObservationPort =>
  withFraudProofL1Recovery(
    createDoubleSpendRawL1ObservationPort({
      authority: l1.snapshotAuthority({
        releaseFinality,
        observationDepth: "inclusion",
      }),
      releaseFinality,
      releaseEconomics,
      definition,
    }),
    fraudProofSignedTransactionRecovery(l1, releaseFinality),
  );

const sameTerminal = (
  left: FraudProofWorkflowTerminal,
  right: FraudProofWorkflowTerminal,
): boolean => {
  const facts = (terminal: FraudProofWorkflowTerminal) => ({
    ...terminal,
    observedAt: { ...terminal.observedAt, confirmationDepth: 0 },
  });
  return JSON.stringify(facts(left)) === JSON.stringify(facts(right));
};

/** Second observation through the constrained integration port. */
export const createDoubleSpendAuthenticatedL1TerminalVerifier = (
  l1: DoubleSpendL1ObservationPort,
): FraudProofWorkflowTerminalVerifier => {
  const verify = async (
    {
      identity,
      candidate,
      releaseFinality,
    }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0],
    inclusionOnly: boolean,
  ): Promise<FraudProofWorkflowTerminal> => {
    const minimumDepth = inclusionOnly
      ? 1
      : releaseFinality.policy.confirmationDepth;
    if (identity.target.kind !== "state_queue_header") {
      throw new Error(
        "double-spend terminal requires a state-queue header target",
      );
    }
    const observed = await l1.observe({
      headerHash: identity.target.headerHash,
    });
    const stage = admitSnapshot({
      headerHash: identity.target.headerHash,
      ...observed,
    });
    if (stage.kind !== "removed") {
      throw new Error(
        "authenticated L1 still reports an unfinished correction",
      );
    }
    if (!sameTerminal(stage.terminal, candidate)) {
      throw new Error(
        "adapter terminal candidate differs from independent L1 observation",
      );
    }
    if (
      stage.terminal.observedAt.confirmationDepth < minimumDepth ||
      candidate.observedAt.confirmationDepth < minimumDepth ||
      stage.terminal.observedAt.confirmationDepth <
        candidate.observedAt.confirmationDepth
    ) {
      throw new Error(
        `authenticated terminal depth is below the release threshold: required=${minimumDepth.toString()} actual=${stage.terminal.observedAt.confirmationDepth.toString()} policy=${releaseFinality.policyDigest}`,
      );
    }
    return candidate;
  };
  return {
    verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
    verify: (input) => verify(input, false),
    verifyIncluded: (input) => verify(input, true),
  };
};

const canonicalOutRef = (value: string, label: string): string => {
  if (!/^[0-9a-f]{64}#[0-9]+$/u.test(value)) {
    throw new Error(`${label} must be a canonical Cardano output reference`);
  }
  return value;
};

export const admitSnapshot = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: DoubleSpendWorkflowStage;
}): DoubleSpendWorkflowStage => {
  const admitted = assertSecurityGradeEvidence(provenance);
  if (admitted.trustClass !== "authenticated_cardano_l1") {
    throw new Error(
      "double-spend workflow observation is not authenticated L1",
    );
  }
  if (stage.kind === "removed") {
    if (stage.terminal.headerHash !== headerHash) {
      throw new Error("double-spend terminal targets a different header");
    }
    return stage;
  }
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (
    stage.kind === "step_01" ||
    stage.kind === "step_02" ||
    stage.kind === "step_03" ||
    stage.kind === "step_04"
  ) {
    canonicalOutRef(stage.threadOutRef, "computation-thread outRef");
  }
  if (stage.kind === "proof_token") {
    canonicalOutRef(stage.fraudProofOutRef, "fraud-proof outRef");
    canonicalOutRef(stage.nextRemovalOutRef, "next removal outRef");
  }
  return stage;
};
