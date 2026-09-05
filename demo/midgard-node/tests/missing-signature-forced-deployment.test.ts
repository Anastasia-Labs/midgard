/** Every missing-signature forced script survives deployment publication. */
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

const FORCED_SIGNER_TITLE =
  "fraud_proofs/missing_signature/forced_signer.main.spend";

/** Real parameter application requires the forced signer blueprint. */
const blueprintHasForcedSigner =
  blueprintPresent &&
  (
    JSON.parse(readFileSync(blueprintPath, "utf8")) as {
      readonly validators?: readonly { readonly title?: string }[];
    }
  ).validators?.some(({ title }) => title === FORCED_SIGNER_TITLE) === true;

const AUXILIARY_CONTRACTS = [
  [
    "forcedStep",
    "V1 fraud-proof missing-signature forced step",
    "fraudProofMissingSignatureForcedStep",
  ],
  [
    "forcedSigner",
    "V1 fraud-proof missing-signature forced signer",
    "fraudProofMissingSignatureForcedSigner",
  ],
  [
    "forcedWitness",
    "V1 fraud-proof missing-signature forced witness",
    "fraudProofMissingSignatureForcedWitness",
  ],
] as const;

describe("missing-signature auxiliary-script deployment wiring", () => {
  it("names each auxiliary script under its own canonical manifest role", () => {
    for (const [, role, contractName] of AUXILIARY_CONTRACTS) {
      expect(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role],
      ).toEqual(contractName);
    }
  });

  // Everything below needs a blueprint that carries the forced signer; until the
  // on-chain side lands it, these skip rather than pretend.
  describe.skipIf(!blueprintHasForcedSigner)(
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
        const chain = contracts.fraudProofContracts.missingSignature;
        const stepHashes = chain.steps.map(
          ({ spendingScriptHash }) => spendingScriptHash,
        );
        expect(stepHashes).toHaveLength(4);
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
        const chain = contracts.fraudProofContracts.missingSignature;
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
