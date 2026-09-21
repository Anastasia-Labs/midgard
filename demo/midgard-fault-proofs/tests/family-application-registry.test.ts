import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES } from "../src/remove-fraudulent-block.js";
import {
  type FamilyCommonInfrastructure,
  familyStepRole,
} from "../src/workflow/family-application.js";
import {
  FAMILY_APPLICATION_REGISTRY,
  REGISTERED_FAMILY_CATEGORIES,
} from "../src/workflow/family-application-registry.js";
import { familyStepContractNames } from "../src/workflow/family-definition.js";
import { FAMILY_DEFINITIONS } from "../src/workflow/family-definitions.js";
import { LINEAR_FAMILY_CATEGORIES } from "../src/workflow/linear-family-spec.js";

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
const boundReferenceScripts = (config: BoundConfig): BoundReferenceScripts =>
  config.referenceScripts ?? config;

const stepsOf = (referenceScripts: BoundReferenceScripts) =>
  referenceScripts.steps ?? referenceScripts.stepReferenceScripts;

const witnessesOf = (referenceScripts: BoundReferenceScripts) =>
  referenceScripts.witnesses ?? referenceScripts.witnessReferenceScripts;

/** The witness roles the shared `FaultProofWitnessReferenceScripts` declares. */
const WITNESS_ROLES = new Set([
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
const boundReferenceLeaves = (value: unknown): readonly UTxO[] => {
  if (isReference(value)) return [value];
  if (Array.isArray(value)) return value.flatMap(boundReferenceLeaves);
  if (typeof value === "object" && value !== null) {
    return Object.values(value).flatMap(boundReferenceLeaves);
  }
  return [];
};

const referenceKey = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;

/**
 * The step references a bound config carries, in step order: the `steps`
 * tuple of the bundle and network-id shapes; on the flat certificate shape
 * every key beginning `step`, which must be exactly the keys the shape has no
 * other name for; on the contract-name-keyed shape the entry each step role's
 * contract names. Each step must be a resolved reference, never absent. A
 * roster without step roles (the interactive dispute) binds no steps.
 */
const boundSteps = (
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

const registry: Readonly<
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
const bind = async (
  record: Record_,
  input: Parameters<Record_["bindConfig"]>[0],
): Promise<BoundConfig> => (await record.bindConfig(input)) as BoundConfig;

/** Definitions of the registered families that are assembled from one. */
const definitions: Readonly<
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

const definedCategories = Object.keys(registry).filter(
  (category) => definitions[category] !== undefined,
);

const referenceRoles = (roster: Readonly<Record<string, string>>) =>
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

const DECISION_DIGEST = "44".repeat(32);

/**
 * Sentinels for the optional infrastructure a record may require; each is a
 * distinct object so a bound config can be checked for carrying exactly the
 * parts it reads, and nothing it does not.
 */
const REPLAY_CONTEXT = Object.freeze({ sentinel: "replayContext" });
const HISTORICAL_AUTHORITY = Object.freeze({
  checkpointStore: Object.freeze({ sentinel: "checkpointStore" }),
  providerRoster: Object.freeze({ sentinel: "providerRoster" }),
  historySource: Object.freeze({ sentinel: "historySource" }),
  l1SourceRoster: Object.freeze({ sentinel: "l1SourceRoster" }),
});
/** A challenge port whose challenge records the coordinates it was asked for. */
const VALIDATION_CHALLENGE_PORT = Object.freeze({
  currentChallenge: async (input: {
    headerHash: string;
    decisionDigest: string;
  }) => Object.freeze({ sentinel: "validationChallenge", ...input }),
});

const infrastructure = {
  manifest: {},
  blueprintJson: "{}",
  deploymentInfo: {},
  headerHash: "aa".repeat(28),
  lucid: {} as never,
  signer: {} as never,
  source: {} as never,
  stateQueueMutationLeaseCoordinator: {} as never,
  decisionDigest: DECISION_DIGEST,
  replayContext: REPLAY_CONTEXT as never,
  historicalNativeScriptAuthority: HISTORICAL_AUTHORITY as never,
  validationChallenge: VALIDATION_CHALLENGE_PORT as never,
} satisfies FamilyCommonInfrastructure;

const { decisionDigest: _undecided, ...undecidedInfrastructure } =
  infrastructure;

const {
  replayContext: _replay,
  historicalNativeScriptAuthority: _authority,
  validationChallenge: _challenge,
  ...plainInfrastructure
} = infrastructure;

/**
 * The sentinel parts a bound config carries at any depth: the replay context
 * and each of the historical authority's parts, named by their sentinel.
 */
const boundSentinels = (value: unknown, found = new Set<string>()) => {
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
const NO_DIGEST_FAMILIES = [
  "nativeScriptInvalid",
  "nativeScriptDecoding",
  "mintAuthorization",
  "withdrawalMistag",
  "minAda",
  "missingNativeScriptTx",
  "missingNativeScriptUtxo",
  "transitionTrace",
  "crossBlockDuplicateEvent",
  "executionNativeScriptInvalid",
  "missingSignature",
  "networkId",
  "valueNotPreserved",
];

const REPLAY_CONTEXT_FAMILIES = [
  "nonExistentInput",
  "noReferenceInput",
  "nativeScriptDecoding",
  "mintAuthorization",
  "withdrawalMistag",
  "valueNotPreserved",
];

/** The sole interactive family, and the only record reading the challenge port. */
const VALIDATION_CHALLENGE_FAMILIES = ["validationTraceDispute"];

/**
 * Hand-written records (no definition to declare it) that follow their proof
 * token with the state-queue removal set.
 */
const HAND_WRITTEN_REMOVAL_FAMILIES = [
  "valueNotPreserved",
  "validationTraceDispute",
];

const HISTORICAL_AUTHORITY_FAMILIES = [
  "minAda",
  "missingNativeScriptTx",
  "missingNativeScriptUtxo",
  "transitionTrace",
  "crossBlockDuplicateEvent",
  "resolvedOutputNonCanonical",
  "spendInputSignerMissing",
  "executionNativeScriptInvalid",
];

describe("family application registry table", () => {
  it.each(Object.keys(registry))(
    "keys %s by the record's own category",
    (key) => {
      expect(registry[key]!.category).toBe(key);
    },
  );

  it("covers the whole catalogue order exactly once", () => {
    expect(REGISTERED_FAMILY_CATEGORIES).toEqual(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    expect([...REGISTERED_FAMILY_CATEGORIES].sort()).toEqual(
      Object.keys(registry).sort(),
    );
    expect(new Set(REGISTERED_FAMILY_CATEGORIES).size).toBe(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
  });
});

/**
 * The deployment-manifest contract vector is the identity a verified finalized
 * manifest is checked against: `validateFinalizedContracts` requires a verified
 * manifest's contract keys to be exactly this vector, so membership here is
 * membership in every verified manifest fixture.
 */
describe("family application rosters name deployed contracts", () => {
  const manifestNames: readonly string[] = DEPLOYMENT_MANIFEST_CONTRACT_NAMES;

  it.each(Object.keys(registry))(
    "resolves every %s roster entry against the manifest contract vector",
    (key) => {
      const roster = registry[key]!.roster;
      expect(Object.keys(roster).length).toBeGreaterThan(0);
      for (const [role, contractName] of Object.entries(roster)) {
        expect(role).not.toBe("");
        expect(manifestNames).toContain(contractName);
      }
    },
  );

  it.each(Object.keys(registry))(
    "binds every %s roster role, and only roster roles, into its config",
    async (key) => {
      const roster = registry[key]!.roster;
      const roles = Object.keys(roster);
      const references = referenceRoles(roster);
      const config = await bind(registry[key]!, { infrastructure, references });
      const referenceScripts = boundReferenceScripts(config);
      const stepRoles = roles.filter((role) => /^step[0-9]{2}$/u.test(role));
      const witnessRoles = roles.filter((role) => WITNESS_ROLES.has(role));
      expect(boundSteps(referenceScripts, roster, stepRoles)).toEqual(
        stepRoles.map((role) => references[role]),
      );
      // Every resolved roster reference lands in the config exactly once,
      // whatever shape the family lays them out in, and nothing else does.
      expect(boundReferenceLeaves(config).map(referenceKey).sort()).toEqual(
        roles.map((role) => referenceKey(references[role]!)).sort(),
      );
      const witnesses = witnessesOf(referenceScripts);
      if (witnesses !== undefined) {
        expect(Object.keys(witnesses).sort()).toEqual([...witnessRoles].sort());
        // Double-spend alone carries the certificate as a bare reference
        // script beside its bundle; every definition-assembled family binds
        // it inside.
        if (definitions[key] !== undefined) {
          expect(referenceScripts.fieldPreimageCertificateMint).toBe(
            references.fieldPreimageCertificateMint,
          );
        }
      } else {
        // The contract-name-keyed shape: each role's reference sits under the
        // contract the roster names for it.
        for (const [role, contractName] of Object.entries(roster)) {
          expect(referenceScripts[contractName]).toBe(references[role]);
        }
      }
      expect(roles).toContain("computationThreadMint");
    },
  );

  it.each(definedCategories)(
    "covers the %s definition's step, witness, certificate and auxiliary roles exactly",
    (category) => {
      const definition = definitions[category]!;
      const roster = registry[category]!.roster;
      const steps = familyStepContractNames(definition as never);
      expect(
        steps.map((_, index) => roster[familyStepRole(index + 1)]),
      ).toEqual(steps);
      expect(roster[familyStepRole(steps.length + 1)]).toBeUndefined();
      for (const role of definition.witnessRoles) {
        expect(roster[role]).toBe(role);
      }
      expect(roster.fieldPreimageCertificateMint).toBe(
        definition.fieldPreimageCertificate
          ? "fieldPreimageCertificateMint"
          : undefined,
      );
      const auxiliary = definition.auxiliaryReferenceScripts ?? {};
      for (const [role, contractName] of Object.entries(auxiliary)) {
        expect(roster[role]).toBe(contractName);
      }
      expect(Object.keys(roster).length).toBe(
        steps.length +
          definition.witnessRoles.length +
          (definition.fieldPreimageCertificate ? 1 : 0) +
          Object.keys(auxiliary).length,
      );
    },
  );
});

describe("family application records bind the admitted decision digest", () => {
  it.each(Object.keys(registry))(
    "lays the digest into %s exactly when its record binds it",
    async (key) => {
      const record = registry[key]!;
      const references = referenceRoles(record.roster);
      const config = await bind(record, { infrastructure, references });
      expect(config.decisionDigest).toBe(
        record.bindsDecisionDigest ? DECISION_DIGEST : undefined,
      );
      if (record.bindsDecisionDigest) {
        await expect(
          bind(record, { infrastructure: undecidedInfrastructure, references }),
        ).rejects.toThrow(`${key} binds the admitted decision digest`);
      } else {
        expect(
          (
            await bind(record, {
              infrastructure: undecidedInfrastructure,
              references,
            })
          ).decisionDigest,
        ).toBeUndefined();
      }
    },
  );

  it("binds the digest on every registered non-linear family except the ones whose config has no digest field", () => {
    const bound = Object.values(registry)
      .filter((record) => record.bindsDecisionDigest)
      .map((record) => record.category)
      .sort();
    const unbound = new Set<string>([
      ...LINEAR_FAMILY_CATEGORIES,
      "doubleSpend",
      ...NO_DIGEST_FAMILIES,
    ]);
    expect(bound).toEqual(
      Object.keys(registry)
        .filter((category) => !unbound.has(category))
        .sort(),
    );
  });

  it("carries the state-queue removal set exactly on the families that declare it", () => {
    const removalNames = [...REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES];
    for (const [category, record] of Object.entries(registry)) {
      const declared = HAND_WRITTEN_REMOVAL_FAMILIES.includes(category)
        ? removalNames
        : Object.keys(definitions[category]?.auxiliaryReferenceScripts ?? {});
      const roster = Object.keys(record.roster);
      const declaresRemoval = removalNames.every((name) =>
        declared.includes(name),
      );
      if (!declaresRemoval) {
        for (const name of removalNames) {
          expect(roster, category).not.toContain(name);
        }
      } else {
        expect([...declared].sort(), category).toEqual(
          [...removalNames].sort(),
        );
        expect(roster).toEqual(expect.arrayContaining(removalNames));
      }
    }
  });
});

describe("family application records declare the infrastructure they read", () => {
  it.each(Object.keys(registry))(
    "binds into %s exactly the optional infrastructure its record requires",
    async (key) => {
      const record = registry[key]!;
      const references = referenceRoles(record.roster);
      const config = await bind(record, { infrastructure, references });
      const sentinels = boundSentinels(config);
      const requiresReplay = record.requires.includes("replayContext");
      const requiresAuthority = record.requires.includes(
        "historicalNativeScriptAuthority",
      );
      const requiresChallenge = record.requires.includes("validationChallenge");
      expect(sentinels.has("replayContext")).toBe(requiresReplay);
      expect(sentinels.has("validationChallenge")).toBe(requiresChallenge);
      if (requiresChallenge) {
        // The challenge is the port's answer for this header and the admitted
        // decision, laid under the family's own `challenge` key.
        expect(config.challenge).toEqual({
          sentinel: "validationChallenge",
          headerHash: infrastructure.headerHash,
          decisionDigest: DECISION_DIGEST,
        });
      }
      expect(
        sentinels.has("historySource") && sentinels.has("checkpointStore"),
      ).toBe(requiresAuthority);
      if (!requiresAuthority) {
        expect(sentinels.has("l1SourceRoster")).toBe(false);
        expect(sentinels.has("providerRoster")).toBe(false);
      }
      // The same record binds without the optional parts when it requires
      // none of them; a record requiring the authority refuses without it. A
      // record requiring the challenge port binds challenge-free without one:
      // the shared loop refuses the missing port before binding, and the
      // family's own execution fail-closes on a challenge-free workflow.
      if (requiresAuthority) {
        await expect(
          bind(record, { infrastructure: plainInfrastructure, references }),
        ).rejects.toThrow(`${key} reconstructs historical native scripts`);
      } else {
        const plain = await bind(record, {
          infrastructure: plainInfrastructure,
          references,
        });
        expect(boundSentinels(plain).size).toBe(0);
        expect(plain.challenge).toBeUndefined();
      }
    },
  );

  it("requires the validation challenge on exactly the interactive family", () => {
    expect(
      Object.values(registry)
        .filter((record) => record.requires.includes("validationChallenge"))
        .map((record) => record.category),
    ).toEqual(VALIDATION_CHALLENGE_FAMILIES);
  });

  it("requires the replay context on exactly the families that replay the predecessor", () => {
    expect(
      Object.values(registry)
        .filter((record) => record.requires.includes("replayContext"))
        .map((record) => record.category)
        .sort(),
    ).toEqual([...REPLAY_CONTEXT_FAMILIES].sort());
  });

  it("requires the historical authority on exactly the families that reconstruct native scripts", () => {
    expect(
      Object.values(registry)
        .filter((record) =>
          record.requires.includes("historicalNativeScriptAuthority"),
        )
        .map((record) => record.category)
        .sort(),
    ).toEqual([...HISTORICAL_AUTHORITY_FAMILIES].sort());
  });

  it("lays the authority's history source and checkpoint store into every historical family", async () => {
    for (const category of HISTORICAL_AUTHORITY_FAMILIES) {
      const record = registry[category]!;
      const config = await bind(record, {
        infrastructure,
        references: referenceRoles(record.roster),
      });
      const values = Object.values(config);
      expect(values, category).toContain(HISTORICAL_AUTHORITY.historySource);
      expect(values, category).toContain(HISTORICAL_AUTHORITY.checkpointStore);
    }
  });
});
