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
  NOT_YET_REGISTERED_FAMILY_CATEGORIES,
  REGISTERED_FAMILY_CATEGORIES,
} from "../src/workflow/family-application-registry.js";
import { familyStepContractNames } from "../src/workflow/family-definition.js";
import { FAMILY_DEFINITIONS } from "../src/workflow/family-definitions.js";
import { LINEAR_FAMILY_CATEGORIES } from "../src/workflow/linear-family-spec.js";

/**
 * The two reference-script shapes a bound config carries: the bundle shape
 * (`steps` tuple, `witnesses`, optional certificate and removal set) and the
 * authenticated certificate shape, which keys each step at the top level
 * beside the certificate and the witnesses.
 */
type BoundReferenceScripts = Readonly<{
  steps?: readonly unknown[];
  witnesses: Readonly<Record<string, unknown>>;
  fieldPreimageCertificateMint?: unknown;
  removal?: Readonly<Record<string, unknown>>;
}> &
  Readonly<Record<string, unknown>>;

const FLAT_SHAPE_NON_STEP_ROLES = new Set([
  "fieldPreimageCertificateMint",
  "witnesses",
  "removal",
]);

/**
 * The step references a bound config carries, in step order: the `steps`
 * tuple of the bundle shape, or on the flat certificate shape every key
 * beginning `step`, which must be exactly the keys the shape has no other
 * name for. Each step must be a resolved reference, never absent.
 */
const boundSteps = (
  referenceScripts: BoundReferenceScripts,
): readonly unknown[] => {
  if (referenceScripts.steps !== undefined) return referenceScripts.steps;
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
      bindsDecisionDigest: boolean;
      bindConfig: (input: {
        readonly infrastructure: FamilyCommonInfrastructure;
        readonly references: Readonly<Record<string, UTxO>>;
      }) => Readonly<{
        decisionDigest?: string;
        referenceScripts: BoundReferenceScripts;
      }>;
    }>
  >
> = FAMILY_APPLICATION_REGISTRY;

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
} satisfies FamilyCommonInfrastructure;

const { decisionDigest: _undecided, ...undecidedInfrastructure } =
  infrastructure;

describe("family application registry table", () => {
  it.each(Object.keys(registry))(
    "keys %s by the record's own category",
    (key) => {
      expect(registry[key]!.category).toBe(key);
    },
  );

  it("covers the catalogue order exactly once, together with the allow-list", () => {
    const registered = new Set<string>(REGISTERED_FAMILY_CATEGORIES);
    const allowed = new Set<string>(NOT_YET_REGISTERED_FAMILY_CATEGORIES);
    expect(registered.size).toBe(REGISTERED_FAMILY_CATEGORIES.length);
    expect(allowed.size).toBe(NOT_YET_REGISTERED_FAMILY_CATEGORIES.length);
    expect([...REGISTERED_FAMILY_CATEGORIES].sort()).toEqual(
      Object.keys(registry).sort(),
    );
    expect(REGISTERED_FAMILY_CATEGORIES).toEqual(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter((category) =>
        registered.has(category),
      ),
    );
    expect([...registered, ...allowed].sort()).toEqual(
      [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER].sort(),
    );
    for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      expect(registered.has(category) !== allowed.has(category)).toBe(true);
    }
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
    (key) => {
      const roster = registry[key]!.roster;
      const roles = Object.keys(roster);
      const references = referenceRoles(roster);
      const { referenceScripts } = registry[key]!.bindConfig({
        infrastructure,
        references,
      });
      const stepRoles = roles.filter((role) => /^step[0-9]{2}$/u.test(role));
      const removalRoles = Object.keys(
        definitions[key]?.auxiliaryReferenceScripts ?? {},
      );
      const witnessRoles = roles.filter(
        (role) =>
          !stepRoles.includes(role) &&
          !removalRoles.includes(role) &&
          role !== "fieldPreimageCertificateMint",
      );
      expect(boundSteps(referenceScripts)).toEqual(
        stepRoles.map((role) => references[role]),
      );
      expect(Object.keys(referenceScripts.witnesses).sort()).toEqual(
        [...witnessRoles].sort(),
      );
      // Double-spend alone carries the certificate as a bare reference script
      // beside its bundle; every definition-assembled family binds it inside.
      if (definitions[key] !== undefined) {
        expect(referenceScripts.fieldPreimageCertificateMint).toBe(
          references.fieldPreimageCertificateMint,
        );
      }
      if (removalRoles.length === 0) {
        expect(referenceScripts.removal).toBeUndefined();
      } else {
        expect(referenceScripts.removal).toEqual(
          Object.fromEntries(
            removalRoles.map((role) => [role, references[role]]),
          ),
        );
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
    (key) => {
      const record = registry[key]!;
      const references = referenceRoles(record.roster);
      const config = record.bindConfig({ infrastructure, references });
      expect(config.decisionDigest).toBe(
        record.bindsDecisionDigest ? DECISION_DIGEST : undefined,
      );
      if (record.bindsDecisionDigest) {
        expect(() =>
          record.bindConfig({
            infrastructure: undecidedInfrastructure,
            references,
          }),
        ).toThrow(`${key} binds the admitted decision digest`);
      } else {
        expect(
          record.bindConfig({
            infrastructure: undecidedInfrastructure,
            references,
          }).decisionDigest,
        ).toBeUndefined();
      }
    },
  );

  it("binds the digest on every registered cursor family and on no linear family", () => {
    const bound = Object.values(registry)
      .filter((record) => record.bindsDecisionDigest)
      .map((record) => record.category)
      .sort();
    const linear = new Set<string>(LINEAR_FAMILY_CATEGORIES);
    expect(bound).toEqual(
      Object.keys(registry)
        .filter(
          (category) => !linear.has(category) && category !== "doubleSpend",
        )
        .sort(),
    );
  });

  it("carries the state-queue removal set exactly on the families whose definition declares it", () => {
    for (const [category, record] of Object.entries(registry)) {
      const declared = Object.keys(
        definitions[category]?.auxiliaryReferenceScripts ?? {},
      );
      const roster = Object.keys(record.roster);
      if (declared.length === 0) {
        for (const name of REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES) {
          expect(roster).not.toContain(name);
        }
      } else {
        expect(declared.sort()).toEqual(
          [...REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES].sort(),
        );
        expect(roster).toEqual(
          expect.arrayContaining([
            ...REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES,
          ]),
        );
      }
    }
  });
});
