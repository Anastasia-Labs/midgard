import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES } from "../src/remove-fraudulent-block.js";
import { familyStepRole } from "../src/workflow/family-application.js";
import { familyStepContractNames } from "../src/workflow/family-definition.js";
import { LINEAR_FAMILY_CATEGORIES } from "../src/workflow/linear-family-spec.js";
import {
  bind,
  boundReferenceLeaves,
  boundReferenceScripts,
  boundSentinels,
  boundSteps,
  DECISION_DIGEST,
  definedCategories,
  definitions,
  HAND_WRITTEN_REMOVAL_FAMILIES,
  HISTORICAL_AUTHORITY,
  HISTORICAL_AUTHORITY_FAMILIES,
  infrastructure,
  NO_DIGEST_FAMILIES,
  OPTIONAL_REPLAY_CONTEXT_FAMILIES,
  plainInfrastructure,
  referenceKey,
  referenceRoles,
  registry,
  REPLAY_CONTEXT_FAMILIES,
  undecidedInfrastructure,
  VALIDATION_CHALLENGE_FAMILIES,
  WITNESS_ROLES,
  witnessesOf,
} from "./family-application-registry.bound-steps.js";

describe("family application registry table", () => {
  it.each(Object.keys(registry))(
    "keys %s by the record's own category",
    (key) => {
      expect(registry[key]!.category).toBe(key);
    },
  );

  it("covers the whole catalogue exactly once", () => {
    expect(Object.keys(registry).sort()).toEqual(
      [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER].sort(),
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
      const witnessRoles = roles.filter(
        (role) =>
          WITNESS_ROLES.has(role) ||
          ((key === "fabricatedDeposit" || key === "fabricatedWithdrawal") &&
            role === "stateQueueSpend"),
      );
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
          if (
            (category === "fabricatedDeposit" ||
              category === "fabricatedWithdrawal") &&
            name === "stateQueueSpend"
          ) {
            // Terminal history proofs mark the queue; this is not the full removal set.
            expect(roster, category).toContain(name);
            expect(definitions[category]?.witnessRoles).toContain(name);
          } else if (
            category === "transitionTrace" &&
            name === "stateQueueSpend"
          ) {
            expect(roster, category).toContain(name);
            expect(
              definitions[category]?.auxiliaryReferenceScripts?.stateQueueSpend,
            ).toBe("stateQueueSpend");
          } else {
            expect(roster, category).not.toContain(name);
          }
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
      expect(sentinels.has("replayContext")).toBe(
        requiresReplay ||
          OPTIONAL_REPLAY_CONTEXT_FAMILIES.includes(record.category),
      );
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
      expect(config.historicalNativeScriptHistorySource, category).toBe(
        HISTORICAL_AUTHORITY.historySource,
      );
      expect(config.historicalNativeScriptCheckpointStore, category).toBe(
        HISTORICAL_AUTHORITY.checkpointStore,
      );
      for (const obsoleteField of [
        "historicalSource",
        "historicalCheckpointStore",
        "historySource",
        "checkpointStore",
      ]) {
        expect(config, category).not.toHaveProperty(obsoleteField);
      }
      if (category === "missingNativeScriptTx") {
        expect(config.historicalNativeScriptL1Roster, category).toBe(
          HISTORICAL_AUTHORITY.l1SourceRoster,
        );
      } else {
        expect(config, category).not.toHaveProperty(
          "historicalNativeScriptL1Roster",
        );
      }
    }
  });
});
