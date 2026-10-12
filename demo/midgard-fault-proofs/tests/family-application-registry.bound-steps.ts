import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type FamilyCommonInfrastructure } from "../src/workflow/family-application.js";
import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.js";
import { FAMILY_DEFINITIONS } from "../src/workflow/family-definitions.js";

/**
 * The reference-script shapes a bound config carries: the bundle shape
 * (`steps` tuple, `witnesses`, optional certificate and an auxiliary set under
 * the family's own key), the authenticated certificate shape, which keys each
 * step at the top level beside the certificate and the witnesses, the
 * contract-name-keyed map transition-trace reads, and network-id's top-level
 * layout, which names its step tuple and witness map with a `ReferenceScripts`
 * suffix beside the rest of its config.
 */
type BoundReferenceScripts = Readonly<{
  steps?: readonly unknown[];
  stepReferenceScripts?: readonly unknown[];
  witnesses?: Readonly<Record<string, unknown>>;
  witnessReferenceScripts?: Readonly<Record<string, unknown>>;
  fieldPreimageCertificateMint?: unknown;
  removal?: Readonly<Record<string, unknown>>;
}> &
  Readonly<Record<string, unknown>>;

type BoundConfig = Readonly<{
  decisionDigest?: string;
  challenge?: unknown;
  referenceScripts?: BoundReferenceScripts;
}> &
  Readonly<Record<string, unknown>>;

/** Where a bound config lays its reference scripts. */
export const boundReferenceScripts = (
  config: BoundConfig,
): BoundReferenceScripts => config.referenceScripts ?? config;

const stepsOf = (referenceScripts: BoundReferenceScripts) =>
  referenceScripts.steps ?? referenceScripts.stepReferenceScripts;

export const witnessesOf = (referenceScripts: BoundReferenceScripts) =>
  referenceScripts.witnesses ?? referenceScripts.witnessReferenceScripts;

/** The witness roles the shared `FaultProofWitnessReferenceScripts` declares. */
export const WITNESS_ROLES = new Set([
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
]);

const FLAT_SHAPE_NON_STEP_ROLES = new Set([
  "fieldPreimageCertificateMint",
  "witnesses",
  "removal",
]);

const isReference = (value: unknown): value is UTxO =>
  typeof value === "object" &&
  value !== null &&
  typeof (value as { txHash?: unknown }).txHash === "string" &&
  typeof (value as { outputIndex?: unknown }).outputIndex === "number";

/**
 * Every resolved reference a bound config's `referenceScripts` carries, at
 * any depth, independent of the shape it is laid out in.
 */
export const boundReferenceLeaves = (value: unknown): readonly UTxO[] => {
  if (isReference(value)) return [value];
  if (Array.isArray(value)) return value.flatMap(boundReferenceLeaves);
  if (typeof value === "object" && value !== null) {
    return Object.values(value).flatMap(boundReferenceLeaves);
  }
  return [];
};

export const referenceKey = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/**
 * The step references a bound config carries, in step order: the `steps`
 * tuple of the bundle and network-id shapes; on the flat certificate shape
 * every key beginning `step`, which must be exactly the keys the shape has no
 * other name for; on the contract-name-keyed shape the entry each step role's
 * contract names. Each step must be a resolved reference, never absent. A
 * roster without step roles (the interactive dispute) binds no steps.
 */
export const boundSteps = (
  referenceScripts: BoundReferenceScripts,
  roster: Readonly<Record<string, string>>,
  stepRoles: readonly string[],
): readonly unknown[] => {
  if (stepRoles.length === 0) return [];
  const steps = stepsOf(referenceScripts);
  if (steps !== undefined) return steps;
  if (witnessesOf(referenceScripts) === undefined) {
    return Object.keys(roster)
      .filter((role) => /^step[0-9]{2}$/u.test(role))
      .map((role) => referenceScripts[roster[role]!]);
  }
  const stepEntries = Object.entries(referenceScripts).filter(([role]) =>
    role.startsWith("step"),
  );
  const otherRoles = Object.keys(referenceScripts).filter(
    (role) => !role.startsWith("step") && !FLAT_SHAPE_NON_STEP_ROLES.has(role),
  );
  expect(otherRoles).toEqual([]);
  for (const [role, value] of stepEntries) {
    expect(value, role).toBeTypeOf("object");
    expect(value, role).not.toBeNull();
  }
  return stepEntries.map(([, value]) => value);
};

export const registry: Readonly<
  Record<
    string,
    Readonly<{
      category: string;
      roster: Readonly<Record<string, string>>;
      requires: readonly string[];
      bindsDecisionDigest: boolean;
      bindConfig: (input: {
        readonly infrastructure: FamilyCommonInfrastructure;
        readonly references: Readonly<Record<string, UTxO>>;
      }) => unknown;
    }>
  >
> = FAMILY_APPLICATION_REGISTRY;

type Record_ = (typeof registry)[string];

/**
 * Binds through a record uniformly, whether its `bindConfig` is async, and
 * views the family's own config through the shapes asserted here.
 */
export const bind = async (
  record: Record_,
  input: Parameters<Record_["bindConfig"]>[0],
): Promise<BoundConfig> => (await record.bindConfig(input)) as BoundConfig;

/** Definitions of the registered families that are assembled from one. */
export const definitions: Readonly<
  Partial<
    Record<
      string,
      Readonly<{
        category: string;
        witnessRoles: readonly string[];
        fieldPreimageCertificate: boolean;
        auxiliaryReferenceScripts?: Readonly<Record<string, string>>;
        adapter: Readonly<
          | { kind: "linear" }
          | { kind: "cursor"; stepContractNames: readonly string[] }
        >;
      }>
    >
  >
> = FAMILY_DEFINITIONS;

export const definedCategories = Object.keys(registry).filter(
  (category) => definitions[category] !== undefined,
);

export const referenceRoles = (roster: Readonly<Record<string, string>>) =>
  Object.fromEntries(
    Object.keys(roster).map((role, index) => [role, reference(index)]),
  );

const reference = (outputIndex: number): UTxO => ({
  txHash: "22".repeat(32),
  outputIndex,
  address: credentialToAddress("Preprod", {
    type: "Key",
    hash: "11".repeat(28),
  }),
  assets: { lovelace: 2_000_000n },
});

export const DECISION_DIGEST = "44".repeat(32);

/**
 * Sentinels for the optional infrastructure a record may require; each is a
 * distinct object so a bound config can be checked for carrying exactly the
 * parts it reads, and nothing it does not.
 */
const REPLAY_CONTEXT = Object.freeze({ sentinel: "replayContext" });

/** A challenge port whose challenge records the coordinates it was asked for. */
const VALIDATION_CHALLENGE_PORT = Object.freeze({
  currentChallenge: async (input: {
    headerHash: string;
    decisionDigest: string;
  }) => Object.freeze({ sentinel: "validationChallenge", ...input }),
});

export const infrastructure = {
  manifest: {},
  blueprintJson: "{}",
  deploymentInfo: {},
  headerHash: "aa".repeat(28),
  lucid: {} as never,
  signer: {} as never,
  l1Source: {} as never,
  stateQueueMutationLeaseCoordinator: {} as never,
  decisionDigest: DECISION_DIGEST,
  replayContext: REPLAY_CONTEXT as never,
  validationChallenge: VALIDATION_CHALLENGE_PORT as never,
} satisfies FamilyCommonInfrastructure;

export const { decisionDigest: _undecided, ...undecidedInfrastructure } =
  infrastructure;

export const {
  replayContext: _replay,
  validationChallenge: _challenge,
  ...plainInfrastructure
} = infrastructure;

/**
 * The sentinel parts a bound config carries at any depth: the replay context
 * and the challenge, named by their sentinel.
 */
export const boundSentinels = (value: unknown, found = new Set<string>()) => {
  if (typeof value !== "object" || value === null) return found;
  const sentinel = (value as { sentinel?: unknown }).sentinel;
  if (typeof sentinel === "string") found.add(sentinel);
  for (const child of Object.values(value)) boundSentinels(child, found);
  return found;
};

/**
 * The non-linear families whose config carries no decision-digest field, so
 * their records do not bind it; every other registered non-linear family does.
 */
export const NO_DIGEST_FAMILIES = [
  "nativeScriptInvalid",
  "nativeScriptDecoding",
  "mintAuthorization",
  "withdrawalMistag",
  "minAda",
  "transitionTrace",
  "executionNativeScriptInvalid",
  "missingSignature",
  "networkId",
  "valueNotPreserved",
];

export const REPLAY_CONTEXT_FAMILIES = [
  "nonExistentInput",
  "noReferenceInput",
  "nativeScriptDecoding",
  "mintAuthorization",
  "withdrawalMistag",
  "minAda",
  "transitionTrace",
  "resolvedOutputNonCanonical",
  "spendInputSignerMissing",
  "executionNativeScriptInvalid",
];

/**
 * Replays the predecessor only when the classifier admitted one: the context
 * is bound when the host supplies it and never required.
 */
export const OPTIONAL_REPLAY_CONTEXT_FAMILIES = ["valueNotPreserved"];

/** The sole interactive family, and the only record reading the challenge port. */
export const VALIDATION_CHALLENGE_FAMILIES = ["validationTraceDispute"];

/**
 * Hand-written records (no definition to declare it) that follow their proof
 * token with the state-queue removal set.
 */
export const HAND_WRITTEN_REMOVAL_FAMILIES = [
  "valueNotPreserved",
  "validationTraceDispute",
];
