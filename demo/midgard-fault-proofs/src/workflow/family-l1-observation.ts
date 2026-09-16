import type {
  AuthenticatedStateQueueHeaderObservation,
  EvidenceProvenance,
  FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "./journal.js";
import {
  createLocalKupmiosHttpOgmiosRawSource,
  type LocalKupmiosHttpOgmiosSourceConfig,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
} from "./local-kupmios-http-ogmios-source.js";
import { createLocalKupmiosFraudProofRawL1SnapshotAuthority } from "./local-kupmios-raw-l1-authority.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import {
  deriveRetainedStateQueueHeaderObservationFromRawL1,
  StateQueueHeaderNotLiveError,
} from "./raw-l1-family-derivation.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  deriveFraudProofRawL1FamilyStage,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1FamilyStage,
  fraudProofRawL1SnapshotRequestForFamily,
} from "./raw-l1-family-derivation.js";
import {
  createFraudProofAuthenticatedPublicationObserver,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Point,
  type FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseEconomicsPolicy } from "./release-economics-policy.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import type {
  SignedTransactionRecoveryObservation,
  SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

export const FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT =
  "midgard-fraud-proof-family-l1-observation-port-v1" as const;

export interface FraudProofFamilyL1ObservationPort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT;
  readonly category: Category;
  /** Raw authenticated authority retained for live global prerequisites. */
  readonly rawL1?: FraudProofRawL1SnapshotAuthority;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  observeSignedTransaction?(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  rebroadcastSignedTransaction?(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
  observeHeader(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservation>;
  /** Recovery from exact NFT creation history; does not assert a live target. */
  observeRetainedHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservation>;
  /** Latest authenticated raw point through which all returned history was reconfirmed. */
  observeBoundary?(input: {
    readonly headerHash: string;
  }): Promise<FraudProofRawL1Point>;
  transactionConfirmed(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: { readonly headerHash: string }): Promise<{
    readonly provenance: EvidenceProvenance;
    readonly stage: FraudProofRawL1FamilyStage;
  }>;
}

/**
 * Header observation for running or resuming a workflow. While the header is
 * in the state queue this is the live observation; once a proof has removed
 * it, the same header is observed from its exact authenticated NFT mint so a
 * workflow that already submitted the removal can resume to its terminal
 * instead of failing closed on every retry.  Neither observation authorizes
 * a new proof action: the family stage machine still decides that.
 */
export const observeFraudProofWorkflowHeader = async <
  Category extends FraudProofCatalogueCategoryName,
>(
  l1: Pick<
    FraudProofFamilyL1ObservationPort<Category>,
    "observeHeader" | "observeRetainedHeader"
  >,
  input: { readonly headerHash: string },
): Promise<AuthenticatedStateQueueHeaderObservation> => {
  try {
    return await l1.observeHeader(input);
  } catch (error) {
    if (
      !(error instanceof StateQueueHeaderNotLiveError) ||
      l1.observeRetainedHeader === undefined
    ) {
      throw error;
    }
    return await l1.observeRetainedHeader(input);
  }
};

/**
 * Family-neutral strict admission over exact raw local Kupo/Ogmios bytes.
 * The provider never supplies a trusted stage or terminal: both are derived
 * locally after the snapshot and complete unit histories are admitted.
 */
export const createFraudProofFamilyRawL1ObservationPort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: Category;
  };
}): FraudProofFamilyL1ObservationPort<Category> => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY ||
    definition.computationThread.steps.length === 0 ||
    definition.computationThread.steps.length > 17
  ) {
    throw new Error(
      `${definition.category} raw L1 observation authority is incomplete`,
    );
  }
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error(
        `${definition.category} raw L1 observation changed the header`,
      );
    }
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    });
  };
  const port: FraudProofFamilyL1ObservationPort<Category> = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category: definition.category,
    rawL1: authority,
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
    observeBoundary: async ({ headerHash }) =>
      (await capture(headerHash)).cursor.point,
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      return {
        provenance: snapshot.provenance,
        stage: await deriveFraudProofRawL1FamilyStage({
          snapshot,
          definition,
          releaseEconomics,
        }),
      };
    },
  };
  return Object.freeze(port);
};

/** Concrete loopback Kupo HTTP + Ogmios WS construction for any family. */
export const createFraudProofFamilyLocalKupmiosL1ObservationPort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  source,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: Category;
  };
}): FraudProofFamilyL1ObservationPort<Category> => {
  const rawSource = createLocalKupmiosHttpOgmiosRawSource({
    ...source,
    releaseFinality,
    observationDepth: "inclusion",
  });
  const port = createFraudProofFamilyRawL1ObservationPort({
    authority: createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: rawSource,
      releaseFinality,
      observationDepth: "inclusion",
    }),
    releaseFinality,
    releaseEconomics,
    definition,
  });
  const recovery = {
    observeSignedTransaction: (input: SignedWorkflowTransaction) =>
      readAdmittedLocalKupmiosSignedTransactionRecovery({
        ...input,
        source: rawSource,
      }),
    rebroadcastSignedTransaction: (
      input: SignedWorkflowTransaction & {
        readonly authorizeResubmission: (
          input: SignedWorkflowTransaction,
        ) => Promise<void>;
      },
    ) =>
      rebroadcastAdmittedLocalKupmiosSignedTransaction({
        ...input,
        source: rawSource,
      }),
  };
  return Object.freeze({
    ...port,
    ...recovery,
    publications: Object.freeze({ ...port.publications, ...recovery }),
  });
};

const sameTerminal = (
  left: FraudProofWorkflowTerminal,
  right: FraudProofWorkflowTerminal,
): boolean => {
  // Slot/hash identify the removal's original inclusion and remain immutable.
  // Only its depth may grow while a durable completion handoff is resumed.
  const facts = (terminal: FraudProofWorkflowTerminal) => ({
    ...terminal,
    observedAt: { ...terminal.observedAt, confirmationDepth: 0 },
  });
  return JSON.stringify(facts(left)) === JSON.stringify(facts(right));
};

/** Independent second raw-L1 observation for terminal acceptance. */
export const createFraudProofFamilyAuthenticatedL1TerminalVerifier = <
  Category extends FraudProofCatalogueCategoryName,
>(
  l1: FraudProofFamilyL1ObservationPort<Category>,
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
    if (
      identity.category !== l1.category ||
      identity.target.kind !== "state_queue_header"
    ) {
      throw new Error(
        `${l1.category} terminal requires its exact state-queue header target`,
      );
    }
    const observed = await l1.observe({
      headerHash: identity.target.headerHash,
    });
    const terminal =
      observed.stage.kind === "removed" ? observed.stage.terminal : undefined;
    if (terminal === undefined) {
      throw new Error(
        `authenticated L1 still reports an unfinished ${l1.category} correction`,
      );
    }
    if (
      terminal.schemaVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION ||
      terminal.category !== l1.category ||
      !sameTerminal(terminal, candidate)
    ) {
      throw new Error(
        `${l1.category} terminal candidate differs from independent L1 observation`,
      );
    }
    if (
      terminal.observedAt.confirmationDepth < minimumDepth ||
      candidate.observedAt.confirmationDepth < minimumDepth ||
      terminal.observedAt.confirmationDepth <
        candidate.observedAt.confirmationDepth
    ) {
      throw new Error(
        `authenticated terminal depth is below the release threshold: required=${minimumDepth.toString()} actual=${terminal.observedAt.confirmationDepth.toString()} policy=${releaseFinality.policyDigest}`,
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
