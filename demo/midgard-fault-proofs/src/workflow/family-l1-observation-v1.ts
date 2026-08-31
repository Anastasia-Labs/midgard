import type {
  AuthenticatedStateQueueHeaderObservationV1,
  EvidenceProvenanceV1,
  FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowTerminalV1,
} from "./journal-v1.js";
import {
  createLocalKupmiosHttpOgmiosRawSourceV1,
  type LocalKupmiosHttpOgmiosSourceConfigV1,
} from "./local-kupmios-http-ogmios-source-v1.js";
import { createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1 } from "./local-kupmios-raw-l1-authority-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  type FraudProofWorkflowTerminalVerifierV1,
} from "./orchestrator-v1.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1V1,
  deriveFraudProofRawL1FamilyStageV1,
  type FraudProofRawL1FamilyDefinitionV1,
  type FraudProofRawL1FamilyStageV1,
  fraudProofRawL1SnapshotRequestForFamilyV1,
} from "./raw-l1-family-derivation-v1.js";
import {
  createFraudProofAuthenticatedPublicationObserverV1,
  type FraudProofAuthenticatedPublicationObserverV1,
} from "./raw-l1-publication-observation-v1.js";
import {
  admitFraudProofRawL1SnapshotV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
  type FraudProofRawL1PointV1,
  type FraudProofRawL1SnapshotAuthorityV1,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseEconomicsPolicyV1 } from "./release-economics-policy-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicyV1 } from "./release-finality-policy-v1.js";

export const FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1 =
  "midgard-fraud-proof-family-l1-observation-port-v1" as const;

export interface FraudProofFamilyL1ObservationPortV1<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1;
  readonly category: Category;
  /** Raw release-final authority retained for live global prerequisites. */
  readonly rawL1?: FraudProofRawL1SnapshotAuthorityV1;
  readonly publications: FraudProofAuthenticatedPublicationObserverV1;
  observeHeader(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservationV1>;
  /** Latest release-final raw point through which all returned history was reconfirmed. */
  observeBoundary?(input: {
    readonly headerHash: string;
  }): Promise<FraudProofRawL1PointV1>;
  transactionConfirmed(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: { readonly headerHash: string }): Promise<{
    readonly provenance: EvidenceProvenanceV1;
    readonly stage: FraudProofRawL1FamilyStageV1;
  }>;
}

/**
 * Family-neutral strict admission over exact raw local Kupo/Ogmios bytes.
 * The provider never supplies a trusted stage or terminal: both are derived
 * locally after the snapshot and complete unit histories are admitted.
 */
export const createFraudProofFamilyRawL1ObservationPortV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthorityV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: Category;
  };
}): FraudProofFamilyL1ObservationPortV1<Category> => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1 ||
    definition.computationThread.steps.length === 0 ||
    definition.computationThread.steps.length > 9
  ) {
    throw new Error(
      `${definition.category} raw L1 observation authority is incomplete`,
    );
  }
  const request = fraudProofRawL1SnapshotRequestForFamilyV1({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error(
        `${definition.category} raw L1 observation changed the header`,
      );
    }
    return admitFraudProofRawL1SnapshotV1({
      value: await authority.capture(request),
      request,
      releaseFinality,
    });
  };
  const port: FraudProofFamilyL1ObservationPortV1<Category> = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
    category: definition.category,
    rawL1: authority,
    publications: createFraudProofAuthenticatedPublicationObserverV1({
      authority,
      releaseFinality,
    }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      (await capture(headerHash)).transactions.some(
        (transaction) => transaction.txHash === txHash,
      ),
    observeHeader: async ({ headerHash }) =>
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1V1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observeBoundary: async ({ headerHash }) =>
      (await capture(headerHash)).cursor.point,
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      return {
        provenance: snapshot.provenance,
        stage: await deriveFraudProofRawL1FamilyStageV1({
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
export const createFraudProofFamilyLocalKupmiosL1ObservationPortV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  source,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly source: Omit<
    LocalKupmiosHttpOgmiosSourceConfigV1,
    "releaseFinality"
  >;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: Category;
  };
}): FraudProofFamilyL1ObservationPortV1<Category> => {
  const rawSource = createLocalKupmiosHttpOgmiosRawSourceV1({
    ...source,
    releaseFinality,
  });
  return createFraudProofFamilyRawL1ObservationPortV1({
    authority: createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1({
      source: rawSource,
      releaseFinality,
    }),
    releaseFinality,
    releaseEconomics,
    definition,
  });
};

const sameTerminal = (
  left: FraudProofWorkflowTerminalV1,
  right: FraudProofWorkflowTerminalV1,
): boolean => JSON.stringify(left) === JSON.stringify(right);

/** Independent second raw-L1 observation for terminal acceptance. */
export const createFraudProofFamilyAuthenticatedL1TerminalVerifierV1 = <
  Category extends FraudProofCatalogueCategoryName,
>(
  l1: FraudProofFamilyL1ObservationPortV1<Category>,
): FraudProofWorkflowTerminalVerifierV1 => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  verify: async ({ identity, candidate, releaseFinality }) => {
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
      terminal.schemaVersion !==
        FRAUD_PROOF_WORKFLOW_TERMINAL_V1_SCHEMA_VERSION ||
      terminal.category !== l1.category ||
      !sameTerminal(terminal, candidate)
    ) {
      throw new Error(
        `${l1.category} terminal candidate differs from independent L1 observation`,
      );
    }
    if (
      terminal.observedAt.confirmationDepth <
      releaseFinality.policy.confirmationDepth
    ) {
      throw new Error(
        `authenticated terminal depth is below the release threshold: required=${releaseFinality.policy.confirmationDepth.toString()} actual=${terminal.observedAt.confirmationDepth.toString()} policy=${releaseFinality.policyDigest}`,
      );
    }
    return terminal;
  },
});
