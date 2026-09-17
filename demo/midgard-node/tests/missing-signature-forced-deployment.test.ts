/**
 * Every missing-signature forced script survives deployment publication.
 *
 * CI: `midgard-node-ci.yml` runs `aiken build --env testnet` before
 * `pnpm --dir demo/midgard-node test`, so the blueprint is a prerequisite of
 * this lane. A missing or pre-forced-signer blueprint FAILS here instead of
 * skipping — a skipped deployment-wiring check is a gate that cannot fail.
 */
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import type * as SDK from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import {
  auxiliaryManifestRoles,
  expectBlueprintCarriesValidators,
} from "./helpers/forced-script-deployment.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repositoryRoot, "onchain/aiken/plutus.json");

/** Derived from the production role table, not transcribed from it. */
const AUXILIARY_ROLES = auxiliaryManifestRoles(
  "fraudProofMissingSignatureForced",
);

/** The linear chain length is part of the contract these scripts sit beside. */
const LINEAR_STEP_COUNT = 4;

describe("missing-signature auxiliary-script deployment wiring", () => {
  it("names every missing-signature forced script under its own distinct manifest role", () => {
    expect(AUXILIARY_ROLES.map(({ field }) => field).sort()).toEqual([
      "forcedSigner",
      "forcedStep",
      "forcedWitness",
    ]);
    expect(new Set(AUXILIARY_ROLES.map(({ role }) => role)).size).toEqual(
      AUXILIARY_ROLES.length,
    );
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toEqual(expect.arrayContaining(AUXILIARY_ROLES.map(({ role }) => role)));
  });

  describe("applied against the real blueprint", () => {
    let contracts: SDK.MidgardValidators;

    beforeAll(async () => {
      expect(
        existsSync(blueprintPath),
        `Missing Aiken blueprint at ${blueprintPath}. Run \`aiken build --env testnet\` in onchain/aiken, or point MIDGARD_REAL_BLUEPRINT_PATH at one.`,
      ).toBe(true);
      expectBlueprintCarriesValidators(
        JSON.parse(readFileSync(blueprintPath, "utf8")),
        blueprintPath,
        [
          "fraud_proofs/missing_signature/forced_step.main.spend",
          "fraud_proofs/missing_signature/forced_signer.main.spend",
          "fraud_proofs/missing_signature/forced_witness.main.spend",
        ],
      );
      contracts = await loadRealMidgardContractsForTest({
        txHash: "00".repeat(32),
        outputIndex: 0,
      });
    }, 600_000);

    it("exposes exactly the forced fields the manifest roles name", () => {
      const chain = contracts.fraudProofContracts.missingSignature;
      expect(
        Object.keys(chain)
          .filter((key) => key.startsWith("forced"))
          .sort(),
      ).toEqual(AUXILIARY_ROLES.map(({ field }) => field).sort());
    });

    it("applies auxiliary scripts distinct from both linear steps and each other", () => {
      const chain = contracts.fraudProofContracts.missingSignature;
      const stepHashes = chain.steps.map(
        ({ spendingScriptHash }) => spendingScriptHash,
      );
      expect(stepHashes).toHaveLength(LINEAR_STEP_COUNT);
      const auxiliaryHashes = AUXILIARY_ROLES.map(
        ({ field }) =>
          (chain as unknown as Record<string, SDK.SpendingValidator>)[field]!
            .spendingScriptHash,
      );
      for (const hash of auxiliaryHashes) {
        expect(hash).toMatch(/^[0-9a-f]{56}$/u);
        expect(stepHashes).not.toContain(hash);
      }
      expect(new Set(auxiliaryHashes).size).toEqual(auxiliaryHashes.length);
    });

    it("publishes each auxiliary script as its own reference script", () => {
      const chain = contracts.fraudProofContracts.missingSignature;
      const targets = nodeRuntimeReferenceScriptTargets(contracts);
      for (const { field, role } of AUXILIARY_ROLES) {
        const roleTargets = targets.filter(({ name }) => name === role);
        // One role, one script: an auxiliary script must not be published
        // under a linear step's role, nor a linear step under an auxiliary one.
        expect(roleTargets).toHaveLength(1);
        expect(roleTargets[0]!.script).toEqual(
          (chain as unknown as Record<string, SDK.SpendingValidator>)[field]!
            .spendingScript,
        );
      }
    });
  });
});
