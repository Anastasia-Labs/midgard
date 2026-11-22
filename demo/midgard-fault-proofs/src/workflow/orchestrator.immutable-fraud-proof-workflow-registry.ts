import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk/core";

import { normalizeHubOraclePolicyId } from "./identity.js";
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

export const verifiedReleaseFinality = async ({
  hubOraclePolicyId,
  authority,
}: {
  readonly hubOraclePolicyId: string;
  readonly authority: FraudProofReleaseFinalityAuthority;
}): Promise<{
  readonly hubOraclePolicyId: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}> => {
  if (authority.authorityVersion !== FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY) {
    throw new Error(
      "workflow requires the deployment-manifest release finality authority",
    );
  }
  const normalized = normalizeHubOraclePolicyId(hubOraclePolicyId);
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await authority.verifyForWorkflow({ hubOraclePolicyId: normalized }),
  );
  return { hubOraclePolicyId: normalized, releaseFinality };
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
