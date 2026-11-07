import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  type DeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildDeploymentManifest,
  deploymentDaTransportProfile,
} from "../../src/commands/contract-deployment-info.js";
import { isPublicDeploymentProfile } from "../../src/commands/listen-public-http.js";
import { loadPhasMembershipWithdrawalScript } from "../../src/phas-membership.js";
import {
  cachedRealBlueprint,
  loadRealBlueprint,
} from "../../src/services/midgard-contracts.load-reference-script-auth-validator.js";
import {
  buildFinalizedContractDeploymentInfo,
  TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
  testReferenceScriptAuthPolicy,
} from "../contract-deployment-info.validator-hashes-by-path.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

/**
 * A public profile's deployment tooling reads the unused-validator census
 * from MIDGARD_BLUEPRINT_CLASSIFICATION_FILE; mount this probe's census there
 * while the manifest is built. Other profiles need none.
 */
const withPublicClassification = async <T>(
  classifications: Readonly<Record<string, string>>,
  build: () => Promise<T>,
): Promise<T> => {
  if (!isPublicDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name))
    return build();
  const directory = mkdtempSync(join(tmpdir(), "midgard-classification-"));
  const path = join(directory, "blueprint-classification.json");
  writeFileSync(path, JSON.stringify(classifications));
  const previous = process.env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE;
  process.env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE = path;
  try {
    return await build();
  } finally {
    if (previous === undefined)
      delete process.env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE;
    else process.env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE = previous;
    rmSync(directory, { recursive: true, force: true });
  }
};

/** Genuine current applied scripts/parameters; publication references remain
 * deterministic codec fixtures, never evidence of a live deployment. */
export const makeGenuineDeploymentRegistryForTest = async (published?: {
  readonly contracts: SDK.MidgardValidators;
  readonly manifest: DeploymentManifest;
}) => {
  const mintingScript = { type: "Native" as const, script: "820500" };
  const contracts =
    published?.contracts ??
    (await loadRealMidgardContractsForTest(
      { txHash: "ab".repeat(32), outputIndex: 0 },
      {
        mintingScript,
        mintingScriptCBOR: mintingScript.script,
        policyId: validatorToScriptHash(mintingScript),
      },
    ));
  const blueprint = await Effect.runPromise(loadRealBlueprint());
  // The members alone: a public profile's deployment tooling also checks them
  // against the census below, which this probe has not built yet.
  const scripts = SDK.manifestDeploymentScripts(contracts, {
    phasMembership: loadPhasMembershipWithdrawalScript,
  }).map((entry) => {
    if (entry.script.type !== "PlutusV3" && entry.script.type !== "Native")
      throw new Error("Unexpected script language in real registry");
    return {
      contract: entry.contract,
      purpose: entry.purpose,
      script: { type: entry.script.type, script: entry.script.script },
      scriptHash: entry.scriptHash,
      referenceScriptRole: entry.role ?? null,
    };
  });
  // The PHAS observer has its own loader. Reconstruct its zero-parameter door
  // from the genuine raw program, without manufacturing parameter records.
  const phas = scripts.find(
    (entry) => entry.contract === "phasMembershipWithdraw",
  )!;
  const phasEntry = blueprint.validators.filter(
    (entry) =>
      entry.title.endsWith(".withdraw") &&
      entry.parameters.length === 0 &&
      applyDoubleCborEncoding(entry.compiledCode) ===
        applyDoubleCborEncoding(phas.script.script),
  );
  expect(phasEntry).toHaveLength(1);
  SDK.getUnappliedScript(blueprint, phasEntry[0]!.title);
  const records = SDK.blueprintApplicationRecords(blueprint);
  const elseTitles: Readonly<Record<string, string>> = {
    fraudProofCatalogueSpend: "fraud_proof_catalogue.spend.else",
    fraudProofSpend: "fraud_proof.spend.else",
    reserveWithdraw: "reserve.withdraw.else",
    depositMint: "user_events/history.history.spend",
    withdrawalMint: "user_events/history.history.spend",
    validationTraceDisputeProofItem:
      "fraud_proofs/validation_trace/proof_item_v1.main.else",
  };
  const used = new Set(
    scripts.flatMap((entry) =>
      records
        .filter(
          (record) =>
            record.appliedScriptCborHex ===
              applyDoubleCborEncoding(entry.script.script) &&
            (record.validatorTitle.endsWith(`.${entry.purpose}`) ||
              record.validatorTitle === elseTitles[entry.contract]),
        )
        .map((record) => record.validatorTitle),
    ),
  );
  // This probe's explicit census is retained when the injected raw validator
  // is tested. It is a constructor control, not a public release allow-list.
  const classifications = Object.fromEntries(
    blueprint.validators
      .filter((entry) => !used.has(entry.title))
      .map((entry) => [
        entry.title,
        "Not used by this complete constructor probe",
      ]),
  );
  const registry = SDK.buildBlueprintDeploymentRegistry(
    blueprint,
    scripts,
    classifications,
  );

  if (cachedRealBlueprint === undefined)
    throw new Error("Missing authenticated blueprint context");
  const blueprintJson = readFileSync(cachedRealBlueprint.path, "utf8");
  const manifest =
    published?.manifest ??
    (await withPublicClassification(classifications, async () =>
      buildDeploymentManifest(
        await Effect.runPromise(
          buildFinalizedContractDeploymentInfo(
            contracts,
            testReferenceScriptAuthPolicy(
              contracts.referenceScriptAuth.policyId,
              contracts.referenceScriptAuth.mintingScriptCBOR,
            ),
          ),
        ),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          economics:
            DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[
              SELECTED_DEPLOYMENT_PROFILE.economics.profile
            ],
          da: {
            ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT.da,
            transportProfile: deploymentDaTransportProfile({
              MIDGARD_DA_PAYLOAD_ENVELOPE: "identity",
              MIDGARD_DA_ZSTD_LEVEL: 3,
            }),
          },
          artifacts: {
            blueprintHash: createHash("sha256")
              .update(blueprintJson)
              .digest("hex"),
          },
        },
      ),
    ));
  const retained = {
    schemaVersion: "midgard-deployment-parameter-registry-v1",
    manifestId: manifest.manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
    deployment: registry,
    referenceScripts: manifest.referenceScripts,
  };
  return { blueprint, registry, contracts, manifest, retained, blueprintJson };
};
