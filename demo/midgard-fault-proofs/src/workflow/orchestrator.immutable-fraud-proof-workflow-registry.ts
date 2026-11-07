import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRegistry,
  freezeWorkflowAdapter,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

class ImmutableFraudProofWorkflowRegistry
  implements FraudProofWorkflowRegistry
{
  readonly #entries: ReadonlyMap<
    FraudProofCatalogueCategoryName,
    FraudProofFamilyWorkflowAdapter
  >;

  constructor(
    entries: readonly (readonly [
      FraudProofCatalogueCategoryName,
      FraudProofFamilyWorkflowAdapter,
    ])[],
  ) {
    this.#entries = new Map(entries);
    Object.freeze(this);
  }

  get size(): number {
    return this.#entries.size;
  }

  get [Symbol.toStringTag](): string {
    return "ImmutableFraudProofWorkflowRegistryV1";
  }

  get(category: FraudProofCatalogueCategoryName) {
    return this.#entries.get(category);
  }

  has(category: FraudProofCatalogueCategoryName): boolean {
    return this.#entries.has(category);
  }

  entries() {
    return this.#entries.entries();
  }

  keys() {
    return this.#entries.keys();
  }

  values() {
    return this.#entries.values();
  }

  forEach(
    callbackfn: (
      value: FraudProofFamilyWorkflowAdapter,
      key: FraudProofCatalogueCategoryName,
      map: FraudProofWorkflowRegistry,
    ) => void,
    thisArg?: unknown,
  ): void {
    for (const [key, value] of this.#entries) {
      callbackfn.call(thisArg, value, key, this);
    }
  }

  [Symbol.iterator]() {
    return this.#entries[Symbol.iterator]();
  }
}

const sameSafety = (
  safety: FraudProofFamilyWorkflowAdapter["safety"],
): boolean =>
  safety.evidenceSource === FRAUD_PROOF_WORKFLOW_SAFETY.evidenceSource &&
  safety.scriptCarriage === FRAUD_PROOF_WORKFLOW_SAFETY.scriptCarriage &&
  safety.localEvaluation === FRAUD_PROOF_WORKFLOW_SAFETY.localEvaluation;

/**
 * Creates a closed, versioned registry for the supplied launch scope. Missing,
 * duplicate, extra, legacy-inline-script, or non-local-evaluation adapters fail
 * startup instead of becoming a runtime fallback.
 */
export const createFraudProofWorkflowRegistry = ({
  adapters,
  launchScope = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
}: {
  readonly adapters: readonly FraudProofFamilyWorkflowAdapter[];
  readonly launchScope?: readonly FraudProofCatalogueCategoryName[];
}): FraudProofWorkflowRegistry => {
  const scope = new Set<FraudProofCatalogueCategoryName>();
  for (const category of launchScope) {
    if (scope.has(category)) {
      throw new Error(`duplicate launch-scope category: ${category}`);
    }
    scope.add(category);
  }
  const registry = new Map<
    FraudProofCatalogueCategoryName,
    FraudProofFamilyWorkflowAdapter
  >();
  for (const adapter of adapters) {
    if (adapter.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER) {
      throw new Error(`adapter ${adapter.category} has an unsupported version`);
    }
    if (!scope.has(adapter.category)) {
      throw new Error(`adapter ${adapter.category} is outside launch scope`);
    }
    if (!sameSafety(adapter.safety)) {
      throw new Error(
        `adapter ${adapter.category} does not enforce canonical evidence, local UPLC evaluation, and reference-script-only carriage`,
      );
    }
    if (registry.has(adapter.category)) {
      throw new Error(`duplicate workflow adapter: ${adapter.category}`);
    }
    registry.set(adapter.category, freezeWorkflowAdapter(adapter));
  }
  const missing = [...scope].filter((category) => !registry.has(category));
  if (missing.length > 0) {
    throw new Error(
      `missing launch-scope workflow adapters: ${missing.join(", ")}`,
    );
  }
  return new ImmutableFraudProofWorkflowRegistry([...registry]);
};

export type WorkflowEvidenceBinding = JournalJsonObject &
  (
    | {
        readonly route: "canonical_block";
        readonly headerHash: string;
        readonly payloadEnvelopeSha256: string;
        readonly payloadSha256: string;
        readonly l1BlockHash: string;
        readonly l1Slot: string;
      }
    | {
        readonly route: "authenticated_raw_family";
        readonly category:
          | "fieldPreimageLengthMismatch"
          | "mintDeclaredAssetLimit"
          | "observersForbiddenOnUntaggedNetwork";
        readonly headerHash: string;
        readonly payloadEnvelopeSha256: string;
        readonly payloadSha256: string;
        readonly l1BlockHash: string;
        readonly l1Slot: string;
      }
    | {
        readonly route: "authenticated_source_leaf";
        readonly headerHash: string;
        readonly payloadEnvelopeSha256: string;
        readonly payloadSha256: string;
        readonly committedTransactionsRoot: string;
        readonly l2TransactionCount: string;
        readonly committedTxId: string;
        readonly l1BlockHash: string;
        readonly l1Slot: string;
      }
  );

export type PersistedArtifactEnvelope = JournalJsonObject & {
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly familyArtifact: JournalJsonObject;
};

export const persistedArtifact = ({
  evidenceBinding,
  releaseFinality,
  familyArtifact,
}: {
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly familyArtifact: JournalJsonObject;
}): PersistedArtifactEnvelope =>
  normalizeJournalJson({
    evidenceBinding,
    releaseFinality,
    familyArtifact,
  }) as PersistedArtifactEnvelope;

// Inclusion may move when the same commitment is re-included after rollback.
// Keep the original locator in the journal for provenance; reuse proof material
// only when every content/commitment field still matches fresh admitted evidence.
const workflowEvidenceContentDigest = (
  binding: WorkflowEvidenceBinding,
): string => {
  const { l1BlockHash, l1Slot, ...content } = binding;
  if (
    !/^[0-9a-f]{64}$/u.test(l1BlockHash) ||
    !/^(0|[1-9][0-9]*)$/u.test(l1Slot)
  )
    throw new Error(
      "workflow evidence binding has an invalid L1 source locator",
    );
  return journalJsonDigest(content);
};

export const requirePreparedArtifact = ({
  entries,
  evidenceBinding,
  releaseFinality,
}: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): PersistedArtifactEnvelope | undefined => {
  const prepared = entries.find((entry) => entry.event.kind === "prepared");
  if (prepared === undefined || prepared.event.kind !== "prepared") {
    return undefined;
  }
  if (
    journalJsonDigest(prepared.event.artifact) !== prepared.event.artifactDigest
  ) {
    throw new Error("journaled prepared artifact digest mismatch");
  }
  const envelope = prepared.event.artifact as PersistedArtifactEnvelope;
  if (
    workflowEvidenceContentDigest(envelope.evidenceBinding) !==
      workflowEvidenceContentDigest(evidenceBinding) ||
    envelope.releaseFinality.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    envelope.releaseFinality.blueprintHash !== releaseFinality.blueprintHash ||
    envelope.releaseFinality.policyDigest !== releaseFinality.policyDigest
  ) {
    throw new Error(
      "current authenticated evidence or release-finality identity does not match the proof-critical artifact persisted before submission",
    );
  }
  return envelope;
};

export const canonicalEvidenceBinding = (
  evidence: CanonicalBlockEvidence,
): WorkflowEvidenceBinding =>
  normalizeJournalJson({
    route: "canonical_block",
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    l1BlockHash: evidence.observation.chainPoint.blockHash,
    l1Slot: evidence.observation.chainPoint.slot.toString(),
  }) as WorkflowEvidenceBinding;

export const verifiedReleaseFinality = async ({
  deploymentFingerprint,
  authority,
}: {
  readonly deploymentFingerprint: string;
  readonly authority: FraudProofReleaseFinalityAuthority;
}): Promise<{
  readonly deploymentFingerprint: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}> => {
  if (authority.authorityVersion !== FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY) {
    throw new Error(
      "workflow requires the deployment-manifest release finality authority",
    );
  }
  const normalized = normalizeDaDeploymentFingerprintHex(deploymentFingerprint);
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await authority.verifyForWorkflow({ deploymentFingerprint: normalized }),
  );
  if (releaseFinality.deploymentIdentityDigest !== normalized) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  return { deploymentFingerprint: normalized, releaseFinality };
};

export const normalizeTxHash = (value: string, field: string): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(normalized)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return normalized;
};

export const normalizeOutRef = (value: string, field: string): string => {
  const normalized = value.trim().toLowerCase();
  if (
    normalized !== value ||
    !/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(normalized)
  ) {
    throw new Error(`${field} must be a canonical transaction outRef`);
  }
  return normalized;
};
