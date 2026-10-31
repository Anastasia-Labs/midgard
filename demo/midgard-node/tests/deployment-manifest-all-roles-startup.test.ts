import "./utils.js";

import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Layer } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
} from "../src/commands/contract-deployment-info.js";
import { publishedDeployableScripts } from "../src/deployable-scripts.js";
import type { L1Access } from "../src/l1-access.js";
import { L1Adapter } from "../src/services/l1-adapter.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
  MidgardContractServices,
} from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import {
  ONE_SHOT_TX_HASH,
  TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
  testFraudProofCatalogue,
  testReferenceScriptAuthPolicy,
} from "./contract-deployment-info.validator-hashes-by-path.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

// A finalized manifest never opens L1; the port is only here because the
// service layer requires one.
const preprodPort = Layer.succeed(L1Adapter, {
  role: "tool",
  open: () =>
    Effect.succeed({
      slotConfig: async () => ({ ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod }),
    } as unknown as L1Access),
});

const directories: string[] = [];
const savedEnvironment = new Map<string, string | undefined>();
const setEnvironment = (values: Record<string, string>) => {
  for (const [key, value] of Object.entries(values)) {
    if (!savedEnvironment.has(key)) savedEnvironment.set(key, process.env[key]);
    process.env[key] = value;
  }
};
afterEach(() => {
  for (const [key, value] of savedEnvironment) {
    if (value === undefined) delete process.env[key];
    else process.env[key] = value;
  }
  savedEnvironment.clear();
  for (const directory of directories.splice(0)) {
    rmSync(directory, { recursive: true, force: true });
  }
});

describe("a deployment whose initialization publishes every reference script", () => {
  it("finalizes its manifest and starts the node from it", async () => {
    const contracts = await loadRealMidgardContractsForTest({
      txHash: ONE_SHOT_TX_HASH,
      outputIndex: 0,
    });
    const authPolicy = testReferenceScriptAuthPolicy(
      contracts.referenceScriptAuth.policyId,
      contracts.referenceScriptAuth.mintingScriptCBOR,
    );
    // Initialization publishes exactly these scripts, each with its own
    // confirmed reference output.
    const published = publishedDeployableScripts(contracts);
    const outRefs = new Map(
      published.map(({ contract }, outputIndex) => [
        contract,
        { txHash: "33".repeat(32), outputIndex },
      ]),
    );
    const deploymentInfo = buildContractDeploymentInfoFromContracts(
      contracts,
      authPolicy,
      outRefs,
      await Effect.runPromise(testFraudProofCatalogue(contracts)),
    );

    const manifest = buildDeploymentManifest(deploymentInfo, {
      ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
      now: new Date("2026-10-18T00:00:00.000Z"),
    });

    const directory = mkdtempSync(join(tmpdir(), "midgard-e4-manifest-"));
    directories.push(directory);
    const manifestPath = join(directory, "contract-deployment-info.json");
    writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));
    setEnvironment({
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: manifestPath,
      NETWORK: TEST_FINALIZED_MANIFEST_BUILD_CONTEXT.network,
      L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS:
        TEST_FINALIZED_MANIFEST_BUILD_CONTEXT.referenceScriptDeployAddress,
      HUB_ORACLE_ONE_SHOT_TX_HASH: ONE_SHOT_TX_HASH,
      HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
      DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
    });
    const [identity, started] = await Effect.runPromise(
      Effect.all([ContractDeploymentIdentity, MidgardContracts]).pipe(
        Effect.provide(
          MidgardContractServices.pipe(Layer.provide(preprodPort)),
        ),
      ),
    );
    expect(identity).toMatchObject({
      kind: "manifest",
      manifestId: manifest.manifestId,
      path: manifestPath,
    });
    expect(started.stateQueue.policyId).toBe(contracts.stateQueue.policyId);
  });
});
