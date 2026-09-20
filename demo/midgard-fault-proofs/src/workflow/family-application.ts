/**
 * Family application record: the per-family data a host needs to install and
 * run one fault family, declared once beside the family's constructor.
 *
 * A definition (`family-definition.ts`) says how a family's workflow is
 * assembled from an already-bound deployment. A record says how a host gets
 * there: which deployed contracts the family spends by reference, which
 * optional parts of the host's common infrastructure it requires, how the
 * resolved reference scripts are laid into the family's own manifest-bound
 * config, how the workflow is constructed and executed, and whether the
 * constructed workflow binds the admitted decision digest.
 *
 * This slice establishes the record, the registry table and the shared
 * application loop. The runtime's runner table and the watcher's fault-proof
 * application still carry their own per-family branches; moving them onto the
 * registry is the work of the later slices of the parent spec.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { HistoricalNativeScriptSourceRoster } from "../missing-native-script-tx/historical-script.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type WorkflowActuationPermit,
  workflowActuationPermitIsReconciliationOnly,
} from "./actuation-permit.js";
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
 * only on the families whose record sets `bindsDecisionDigest`.
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
  /** Lays the resolved roster into the family's own manifest-bound config. */
  bindConfig: (input: {
    readonly infrastructure: FamilyCommonInfrastructure;
    readonly references: FamilyResolvedReferenceScripts;
  }) => Config;
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

/**
 * The roster of a family assembled from a definition: one role per chain step
 * in step order, one per declared witness role, and the field-preimage
 * certificate minting policy when the definition binds the certificate. Every
 * witness role and the certificate role name their manifest contract
 * identically, so the mapping is the identity.
 */
export const familyDefinitionRoster = (
  definition: Readonly<{
    category: FamilyCategory;
    witnessRoles: readonly FaultProofWitnessRole[];
    fieldPreimageCertificate: boolean;
    adapter: Readonly<
      | { kind: "linear" }
      | { kind: "cursor"; stepContractNames: readonly string[] }
    >;
  }>,
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
  });

/**
 * Refuses a derived roster that does not cover its definition: one role per
 * chain step, in step order and naming that step's contract; one per declared
 * witness role; the certificate role exactly when the definition binds it;
 * and nothing besides.
 *
 * `bindConfig` reads exactly these roles, so a roster that drifts from the
 * definition would otherwise surface as an unresolved reference while a fault
 * is being proved, against the clock. Written from the definition
 * independently of the builder above, and called where records are derived,
 * so a change to the shared builder refuses at import instead.
 */
export const assertFamilyDefinitionRoster = (
  definition: Readonly<{
    category: FamilyCategory;
    witnessRoles: readonly FaultProofWitnessRole[];
    fieldPreimageCertificate: boolean;
    adapter: Readonly<
      | { kind: "linear" }
      | { kind: "cursor"; stepContractNames: readonly string[] }
    >;
  }>,
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

const outRefLabel = (utxo: UTxO): string =>
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

export type FamilyApplicationInvocation = Readonly<{
  deploymentFingerprint: string;
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
  /**
   * Proof that this invocation only reads back an already-submitted chain.
   * Omitted by every invocation that may build, and a family's optional
   * requirements are then in force. Supplying it exempts them, because a
   * reconciliation-only resume reconciles through the recovered adapter and
   * never reaches the evidence pipeline — exactly as it does today.
   *
   * It is a permit rather than a flag because the exemption is the unsafe
   * state: the permit is module-private authority, so a caller that has not
   * been restricted to reconciliation cannot claim it, and a structural
   * lookalike is refused outright.
   */
  reconciliationAuthority?: WorkflowActuationPermit;
}>;

/** How the host resolves one roster entry to its published reference UTxO. */
export type FamilyReferenceScriptResolver = (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly role: string;
  readonly contractName: string;
}) => Promise<UTxO>;

export type AppliedFamilyApplication<Config, Workflow> = Readonly<{
  config: Config;
  workflow: Workflow;
  references: FamilyResolvedReferenceScripts;
  /** One `txHash#index` per roster role, derived from the resolved map. */
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

/** A record only carries authority if it was minted through the definer. */
const assertFamilyApplicationRecordVersion = (
  record: Readonly<{ recordVersion: string; category: string }>,
): void => {
  if (record.recordVersion !== FAMILY_APPLICATION_RECORD) {
    throw new Error(
      `family application record for ${record.category} has an unsupported schema`,
    );
  }
};

/** What a resolved roster yields, before any family config is bound. */
export type ResolvedFamilyApplicationReferences = Readonly<{
  references: FamilyResolvedReferenceScripts;
  /** One `txHash#index` per roster role, derived from the resolved map. */
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

/**
 * Resolves a family's whole roster and reports the out-refs it will spend.
 *
 * This is what startup readiness calls. It deliberately stops here: it binds
 * no config and constructs no workflow, so readiness cannot act and has no
 * need of the optional infrastructure an acting invocation must hold. A
 * family's requirements are therefore not exempted for readiness — readiness
 * never reaches the point where they would apply.
 */
export const resolveFamilyApplicationReferences = async <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>({
  record,
  resolveReferenceScript,
}: {
  readonly record: FamilyApplicationRecord<Category, Config, Workflow>;
  readonly resolveReferenceScript: FamilyReferenceScriptResolver;
}): Promise<ResolvedFamilyApplicationReferences> => {
  assertFamilyApplicationRecordVersion(record);
  const references: FamilyResolvedReferenceScripts = Object.freeze(
    Object.fromEntries(
      await Promise.all(
        Object.entries(record.roster).map(
          async ([role, contractName]): Promise<readonly [string, UTxO]> => [
            role,
            await resolveReferenceScript({
              category: record.category,
              role,
              contractName,
            }),
          ],
        ),
      ),
    ),
  );
  return Object.freeze({
    references,
    referenceScriptOutRefs: Object.freeze(
      Object.fromEntries(
        Object.keys(record.roster).map((role) => [
          role,
          outRefLabel(references[role]!),
        ]),
      ),
    ),
  });
};

/**
 * The shared application loop: refuse a missing required part, resolve the
 * roster, bind the family's config, construct its workflow, check the
 * constructed identity against the invocation, and report the out-refs the
 * family will spend. No per-family branch, here or in any caller.
 */
export const applyFamilyApplicationRecord = async <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>({
  record,
  infrastructure,
  resolveReferenceScript,
  invocation,
}: {
  readonly record: FamilyApplicationRecord<Category, Config, Workflow>;
  readonly infrastructure: FamilyCommonInfrastructure;
  readonly resolveReferenceScript: FamilyReferenceScriptResolver;
  readonly invocation: FamilyApplicationInvocation;
}): Promise<AppliedFamilyApplication<Config, Workflow>> => {
  assertFamilyApplicationRecordVersion(record);
  if (invocation.category !== record.category) {
    throw new Error(
      `family application refuses category ${invocation.category}; expected ${record.category}`,
    );
  }
  if (invocation.reconciliationAuthority === undefined) {
    for (const requirement of record.requires) {
      if (infrastructure[requirement] === undefined) {
        throw new Error(
          `${record.category} application requires ${requirement}, which the host did not supply`,
        );
      }
    }
  } else if (
    !workflowActuationPermitIsReconciliationOnly(
      invocation.reconciliationAuthority,
    )
  ) {
    throw new Error(
      `${record.category} application claimed the reconciliation exemption under a permit that still admits actuation`,
    );
  }
  const { references, referenceScriptOutRefs } =
    await resolveFamilyApplicationReferences({
      record,
      resolveReferenceScript,
    });
  const config = record.bindConfig({ infrastructure, references });
  const workflow = await record.constructWorkflow(config);
  assertManifestBoundWorkflowIdentity({
    workflow,
    category: record.category,
    deploymentFingerprint: invocation.deploymentFingerprint,
    headerHash: invocation.headerHash,
    bindsDecisionDigest: record.bindsDecisionDigest,
    decisionDigest: infrastructure.decisionDigest,
  });
  return Object.freeze({
    config,
    workflow,
    references,
    referenceScriptOutRefs,
  });
};
