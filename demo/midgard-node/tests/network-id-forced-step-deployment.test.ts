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
 */
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import type * as SDK from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");

/**
 * `onchain/aiken/plutus.json` is gitignored, so a clean checkout has no
 * blueprint and cannot apply real parameters at all. Skipping is honest.
 */
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repositoryRoot, "onchain/aiken/plutus.json");
const blueprintPresent = existsSync(blueprintPath);

const FORCED_SCAN_TITLE = "fraud_proofs/network_id/forced_scan.main.spend";

/**
 * A blueprint compiled before the resumable forced scan existed cannot apply
 * either auxiliary script: the forced door is parameterized by the scan's
 * script hash, so `buildNetworkIdChain` fails outright against it. Reporting
 * that as a skip is honest; reporting it as a failure would only say the
 * blueprint is stale.
 */
const blueprintHasForcedScan =
  blueprintPresent &&
  (
    JSON.parse(readFileSync(blueprintPath, "utf8")) as {
      readonly validators?: readonly { readonly title?: string }[];
    }
  ).validators?.some(({ title }) => title === FORCED_SCAN_TITLE) === true;

const AUXILIARY_CONTRACTS = [
  [
    "forcedStep",
    "V1 fraud-proof network-id forced step",
    "fraudProofNetworkIdForcedStep",
  ],
  [
    "forcedScan",
    "V1 fraud-proof network-id forced scan",
    "fraudProofNetworkIdForcedScan",
  ],
] as const;

describe("network-id auxiliary-script deployment wiring", () => {
  it("names each auxiliary script under its own canonical manifest role", () => {
    for (const [, role, contractName] of AUXILIARY_CONTRACTS) {
      expect(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role],
      ).toEqual(contractName);
    }
  });

  // Everything below needs a blueprint that carries the forced scan; until the
  // on-chain side lands it, these skip rather than pretend.
  describe.skipIf(!blueprintHasForcedScan)(
    "applied against the real blueprint",
    () => {
      let contracts: SDK.MidgardValidators;

      beforeAll(async () => {
        contracts = await loadRealMidgardContractsForTest({
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }, 600_000);

      it("applies auxiliary scripts distinct from both linear steps and each other", () => {
        const chain = contracts.fraudProofContracts.networkId;
        const stepHashes = chain.steps.map(
          ({ spendingScriptHash }) => spendingScriptHash,
        );
        expect(stepHashes).toHaveLength(2);
        const auxiliaryHashes = AUXILIARY_CONTRACTS.map(
          ([field]) => chain[field].spendingScriptHash,
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
        for (const [field, role] of AUXILIARY_CONTRACTS) {
          const roleTargets = targets.filter(({ name }) => name === role);
          // One role, one script: an auxiliary script must not be published
          // under a linear step's role, nor a linear step under an auxiliary one.
          expect(roleTargets).toHaveLength(1);
          expect(roleTargets[0]!.script).toEqual(chain[field].spendingScript);
        }
      });
    },
  );
});
