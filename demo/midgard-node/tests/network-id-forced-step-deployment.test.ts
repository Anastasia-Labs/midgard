/**
 * The network-id family's auxiliary scripts — the forced (wrongful-rejection)
 * door and the resumable output scan it hands off to — have to survive the
 * whole deployment path: the SDK applies them, the deployment manifest names
 * them, and the node publishes a reference script for each.
 *
 * `buildNetworkIdChain` deliberately keeps `forcedStep` and `forcedScan` OUT of
 * `steps` — they are side entrances into step 02, not third and fourth links —
 * so every deployment surface that walks a chain by step index skips them. That
 * made the forced door invisible: it was applied on every deployment and
 * published nowhere, and the forced leg spends from its own script address, so
 * the wrongful-rejection proof could not be submitted against a real deployment
 * at all. The scan sits between the door and step 02 and fails the same way.
 *
 * This asserts against the REAL blueprint rather than the AlwaysSucceeds
 * stand-in, because under the stand-in every fault-proof step shares one script
 * and an auxiliary script wired to step 02 would pass unnoticed. The manifest
 * round trip itself is covered against the stand-in in
 * `contract-deployment-info.test.ts`.
 *
 * CI: `midgard-node-ci.yml` runs `aiken build --env testnet` before
 * `pnpm --dir demo/midgard-node test`, so the blueprint is a prerequisite of
 * this lane, not an optional extra. A missing or pre-forced-scan blueprint
 * therefore FAILS here instead of skipping — a skipped deployment-wiring check
 * is a gate that cannot fail.
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

/**
 * Derived from the production role table, not transcribed from it: every
 * reference-script role whose contract is a network-id forced script, paired
 * with the chain field that contract name denotes.
 */
const AUXILIARY_ROLES = auxiliaryManifestRoles("fraudProofNetworkIdForced");

/** The linear chain length is part of the contract these scripts sit beside. */
const LINEAR_STEP_COUNT = 2;

describe("network-id auxiliary-script deployment wiring", () => {
  it("names every network-id forced script under its own distinct manifest role", () => {
    expect(AUXILIARY_ROLES.map(({ field }) => field).sort()).toEqual([
      "forcedScan",
      "forcedStep",
    ]);
    expect(new Set(AUXILIARY_ROLES.map(({ role }) => role)).size).toEqual(
      AUXILIARY_ROLES.length,
    );
    // No auxiliary role may collide with any other manifest role.
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toEqual(expect.arrayContaining(AUXILIARY_ROLES.map(({ role }) => role)));
  });

  describe("applied against the real blueprint", () => {
    let contracts: SDK.MidgardValidators;

    beforeAll(async () => {
      // Fail closed, loudly, before the parameter application error would
      // otherwise be reported as an unrelated SDK failure.
      expect(
        existsSync(blueprintPath),
        `Missing Aiken blueprint at ${blueprintPath}. Run \`aiken build --env testnet\` in onchain/aiken, or point MIDGARD_REAL_BLUEPRINT_PATH at one.`,
      ).toBe(true);
      expectBlueprintCarriesValidators(
        JSON.parse(readFileSync(blueprintPath, "utf8")),
        blueprintPath,
        [
          "fraud_proofs/network_id/forced_step.main.spend",
          "fraud_proofs/network_id/forced_scan.main.spend",
        ],
      );
      contracts = await loadRealMidgardContractsForTest({
        txHash: "00".repeat(32),
        outputIndex: 0,
      });
    }, 600_000);

    it("exposes exactly the forced fields the manifest roles name", () => {
      const chain = contracts.fraudProofContracts.networkId;
      expect(
        Object.keys(chain)
          .filter((key) => key.startsWith("forced"))
          .sort(),
      ).toEqual(AUXILIARY_ROLES.map(({ field }) => field).sort());
    });

    it("applies auxiliary scripts distinct from both linear steps and each other", () => {
      const chain = contracts.fraudProofContracts.networkId;
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
      const chain = contracts.fraudProofContracts.networkId;
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
