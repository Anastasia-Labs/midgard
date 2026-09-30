import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { HistoricalNativeScriptSourceRoster } from "../missing-native-script-tx/historical-script.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { ValidationTraceChallenge } from "./challenge-authority.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import type {
  FamilyCategory,
  FaultProofWitnessRole,
  ManifestBoundFamilyWorkflow,
  ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import { familyStepContractNames } from "./family-definition.js";
import type {
  HistoricalNativeScriptCheckpointStore,
  HistoricalNativeScriptHistorySource,
  HistoricalNativeScriptProviderRoster,
} from "./historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";

export const FAMILY_APPLICATION_RECORD =
  "midgard-production-family-application-record-v1" as const;

/**
 * Optional infrastructure a family may require beyond what every family gets.
 * Deliberately capped at what today's 55 families need: growing this set is a
 * design decision, not an implementation detail.
 */
export type FamilyApplicationRequirement =
  | "replayContext"
  | "validationChallenge"
  | "historicalNativeScriptAuthority";

/**
 * The host's handle on the historical native-script history the families that
 * reconstruct pre-genesis scripts read through.
 */
export type FamilyHistoricalNativeScriptAuthority = Readonly<{
  checkpointStore: HistoricalNativeScriptCheckpointStore;
  providerRoster: HistoricalNativeScriptProviderRoster;
  historySource: HistoricalNativeScriptHistorySource;
  l1SourceRoster: HistoricalNativeScriptSourceRoster;
}>;

/**
 * How a family that disputes a validation trace reaches the freshly admitted
 * challenge for this decision. The host owns capture, refresh and the
 * currency assertion; the family only declares that it needs one.
 */
export type FamilyValidationChallengePort = Readonly<{
  currentChallenge(input: {
    readonly headerHash: string;
    readonly decisionDigest: string;
  }): Promise<ValidationTraceChallenge>;
}>;

/**
 * Everything a host builds once per invocation and every family draws from.
 * The first eight members are unconditional; the rest are the optional parts
 * a record names in its `requires`.
 */
export type FamilyCommonInfrastructure = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  /** The admitted decision this invocation acts on, when there is one. */
  decisionDigest?: string;
  replayContext?: CompleteCanonicalReplayContext;
  historicalNativeScriptAuthority?: FamilyHistoricalNativeScriptAuthority;
  validationChallenge?: FamilyValidationChallengePort;
}>;

/**
 * The identity every constructed workflow carries. `decisionDigest` is present
 * only on the families whose record sets `bindsDecisionDigest`. The three
 * recovery members are what a reconciliation-only resume reads back an
 * already-submitted chain through; the shared runtime refuses to reconcile a
 * workflow that omits any of them.
 */
export type FamilyApplicationWorkflowIdentity<
  Category extends FraudProofCatalogueCategoryName,
> = Readonly<{
  binding: Readonly<{
    deploymentFingerprint: string;
    definition: Readonly<{
      category: Category;
      headerHash: string;
    }>;
  }>;
  decisionDigest?: string;
  adapter?: FraudProofFamilyWorkflowAdapter;
  terminalVerifier?: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority?: FraudProofReleaseFinalityAuthority;
}>;

/** Resolved published reference-script UTxOs, keyed by the record's roles. */
export type FamilyResolvedReferenceScripts = Readonly<Record<string, UTxO>>;

export type FamilyApplicationRecord<
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
> = Readonly<{
  recordVersion: typeof FAMILY_APPLICATION_RECORD;
  category: Category;
  /**
   * Role name to deployment-manifest contract name, fixed and complete. Every
   * entry is resolved; there are no optional roles, so a script cannot be in
   * the config and absent from readiness or the reverse.
   */
  roster: Readonly<Record<string, string>>;
  requires: readonly FamilyApplicationRequirement[];
  /**
   * Lays the resolved roster into the family's own manifest-bound config.
   * Asynchronous only where a required part is reached through a port: the
   * validation-trace dispute obtains its challenge from the host's port here.
   */
  bindConfig: (input: {
    readonly infrastructure: FamilyCommonInfrastructure;
    readonly references: FamilyResolvedReferenceScripts;
  }) => Config | Promise<Config>;
  constructWorkflow: (config: Config) => Promise<Workflow>;
  execute: (input: {
    readonly workflow: Workflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
    readonly mode: "run" | "resume";
  }) => Promise<unknown>;
  /** Whether the constructed workflow must carry the invocation's digest. */
  bindsDecisionDigest: boolean;
}>;

/** Freezes a record and infers its exact category, config and workflow types. */
export const defineFamilyApplication = <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  record: Omit<
    FamilyApplicationRecord<Category, Config, Workflow>,
    "recordVersion"
  >,
): FamilyApplicationRecord<Category, Config, Workflow> =>
  Object.freeze({
    recordVersion: FAMILY_APPLICATION_RECORD,
    ...record,
    roster: Object.freeze({ ...record.roster }),
    requires: Object.freeze([...record.requires]),
  });

/** One-based step role name, matching the roster spelling `step01`…`step04`. */
export const familyStepRole = (ordinal: number): string =>
  `step${ordinal.toString().padStart(2, "0")}`;

/** The members of a definition a derived roster is read from. */
export type FamilyRosterDefinition = Readonly<{
  category: FamilyCategory;
  witnessRoles: readonly FaultProofWitnessRole[];
  fieldPreimageCertificate: boolean;
  /** Family-specific published script roles, mapped to manifest contracts. */
  auxiliaryReferenceScripts?: Readonly<Record<string, string>>;
  adapter: Readonly<
    | { kind: "linear" }
    | { kind: "cursor"; stepContractNames: readonly string[] }
  >;
}>;

/**
 * The roster of a family assembled from a definition: one role per chain step
 * in step order, one per declared witness role, the field-preimage
 * certificate minting policy when the definition binds the certificate, and
 * one role per declared auxiliary reference script. Every witness role and
 * the certificate role name their manifest contract identically, so that
 * mapping is the identity; an auxiliary role maps to the contract its
 * definition names.
 */
export const familyDefinitionRoster = (
  definition: FamilyRosterDefinition,
): Readonly<Record<string, string>> =>
  Object.freeze({
    ...Object.fromEntries(
      familyStepContractNames(definition).map((contractName, index) => [
        familyStepRole(index + 1),
        contractName,
      ]),
    ),
    ...Object.fromEntries(definition.witnessRoles.map((role) => [role, role])),
    ...(definition.fieldPreimageCertificate
      ? { fieldPreimageCertificateMint: "fieldPreimageCertificateMint" }
      : {}),
    ...definition.auxiliaryReferenceScripts,
  });

/**
 * Refuses a derived roster that does not cover its definition: one role per
 * chain step, in step order and naming that step's contract; one per declared
 * witness role; the certificate role exactly when the definition binds it;
 * one per declared auxiliary reference script; and nothing besides.
 *
 * `bindConfig` reads exactly these roles, so a roster that drifts from the
 * definition would otherwise surface as an unresolved reference while a fault
 * is being proved, against the clock. Written from the definition
 * independently of the builder above, and called where records are derived,
 * so a change to the shared builder refuses at import instead.
 */
export const assertFamilyDefinitionRoster = (
  definition: FamilyRosterDefinition,
  roster: Readonly<Record<string, string>>,
): void => {
  const refuse = (detail: string): never => {
    throw new Error(`${definition.category} derived roster ${detail}`);
  };
  const steps = familyStepContractNames(definition);
  const expected = new Map<string, string>([
    ...steps.map((contractName, index): readonly [string, string] => [
      familyStepRole(index + 1),
      contractName,
    ]),
    ...definition.witnessRoles.map((role): readonly [string, string] => [
      role,
      role,
    ]),
    ...(definition.fieldPreimageCertificate
      ? ([
          ["fieldPreimageCertificateMint", "fieldPreimageCertificateMint"],
        ] as const)
      : []),
    ...Object.entries(definition.auxiliaryReferenceScripts ?? {}),
  ]);
  for (const [role, contractName] of expected) {
    if (roster[role] !== contractName) {
      refuse(`omits ${role}, which its definition declares`);
    }
  }
  for (const role of Object.keys(roster)) {
    if (!expected.has(role)) {
      refuse(`carries ${role}, which its definition does not declare`);
    }
  }
};

/**
 * A linear family's record: config, workflow and step count all follow from
 * its definition, so the only per-family facts are the ones the definition
 * already states.
 */
export type LinearFamilyApplicationRecord<
  Category extends LinearFamilyCategory,
> = FamilyApplicationRecord<
  Category,
  ManifestBoundFamilyWorkflowConfig<Category, FaultProofWitnessRole, boolean>,
  ManifestBoundFamilyWorkflow<Category, boolean>
>;

export const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** What a host must tell the shared loop about the run it is applying. */
/**
 * The one identity check a constructed manifest-bound workflow passes before
 * it may act: it must be the workflow the invocation admitted, and — for a
 * family whose record binds it — carry that invocation's decision digest.
 * Shared so the runner and the application loop refuse in the same words.
 */
export const assertManifestBoundWorkflowIdentity = <
  Category extends FraudProofCatalogueCategoryName,
>({
  workflow,
  category,
  deploymentFingerprint,
  headerHash,
  bindsDecisionDigest,
  decisionDigest,
}: {
  readonly workflow: FamilyApplicationWorkflowIdentity<Category>;
  readonly category: Category;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly bindsDecisionDigest: boolean;
  readonly decisionDigest: string | undefined;
}): void => {
  if (
    workflow.binding.deploymentFingerprint !== deploymentFingerprint ||
    workflow.binding.definition.category !== category ||
    workflow.binding.definition.headerHash !== headerHash
  ) {
    throw new Error(
      "manifest-bound workflow identity differs from the compiled CLI invocation",
    );
  }
  if (bindsDecisionDigest && workflow.decisionDigest !== decisionDigest) {
    throw new Error(
      `${category} manifest-bound workflow decision digest differs from invocation`,
    );
  }
};
