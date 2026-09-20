import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type FamilyCommonInfrastructure,
  familyStepRole,
} from "../src/workflow/family-application.js";
import {
  FAMILY_APPLICATION_REGISTRY,
  NOT_YET_REGISTERED_FAMILY_CATEGORIES,
  REGISTERED_FAMILY_CATEGORIES,
} from "../src/workflow/family-application-registry.js";
import { LINEAR_FAMILY_DEFINITIONS } from "../src/workflow/linear-family-definitions.js";
import { linearFamilySpec } from "../src/workflow/linear-family-spec.js";

type BoundReferenceScripts = Readonly<{
  steps: readonly unknown[];
  witnesses: Readonly<Record<string, unknown>>;
  fieldPreimageCertificateMint?: unknown;
}>;

const registry: Readonly<
  Record<
    string,
    Readonly<{
      category: string;
      roster: Readonly<Record<string, string>>;
      bindConfig: (input: {
        readonly infrastructure: FamilyCommonInfrastructure;
        readonly references: Readonly<Record<string, UTxO>>;
      }) => Readonly<{ referenceScripts: BoundReferenceScripts }>;
    }>
  >
> = FAMILY_APPLICATION_REGISTRY;

const reference = (outputIndex: number): UTxO => ({
  txHash: "22".repeat(32),
  outputIndex,
  address: credentialToAddress("Preprod", {
    type: "Key",
    hash: "11".repeat(28),
  }),
  assets: { lovelace: 2_000_000n },
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
} satisfies FamilyCommonInfrastructure;

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
      const references = Object.fromEntries(
        roles.map((role, index) => [role, reference(index)]),
      );
      const { referenceScripts } = registry[key]!.bindConfig({
        infrastructure,
        references,
      });
      const stepRoles = roles.filter((role) => /^step[0-9]{2}$/u.test(role));
      const witnessRoles = roles.filter(
        (role) =>
          !stepRoles.includes(role) && role !== "fieldPreimageCertificateMint",
      );
      expect(referenceScripts.steps).toEqual(
        stepRoles.map((role) => references[role]),
      );
      expect(Object.keys(referenceScripts.witnesses).sort()).toEqual(
        [...witnessRoles].sort(),
      );
      expect(roles).toContain("computationThreadMint");
    },
  );

  it.each(Object.keys(LINEAR_FAMILY_DEFINITIONS))(
    "covers the %s definition's witness roles and step roles",
    (category) => {
      const spec = linearFamilySpec(
        category as keyof typeof LINEAR_FAMILY_DEFINITIONS,
      );
      const roster = registry[category]!.roster;
      const steps = spec.steps.map((step) => step.manifestContractName);
      expect(
        steps.map((_, index) => roster[familyStepRole(index + 1)]),
      ).toEqual(steps);
      expect(roster[familyStepRole(steps.length + 1)]).toBeUndefined();
      const definition =
        LINEAR_FAMILY_DEFINITIONS[
          category as keyof typeof LINEAR_FAMILY_DEFINITIONS
        ];
      for (const role of definition.witnessRoles) {
        expect(roster[role]).toBe(role);
      }
      expect(roster.fieldPreimageCertificateMint).toBe(
        definition.fieldPreimageCertificate
          ? "fieldPreimageCertificateMint"
          : undefined,
      );
    },
  );
});
