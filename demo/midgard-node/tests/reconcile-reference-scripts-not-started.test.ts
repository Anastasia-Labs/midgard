/**
 * `reconcile reference-scripts-complete --scope node-runtime` before the
 * deployment records a reference-script auth policy reports the publication
 * as not started, in the result the full stack reads, instead of failing;
 * once a policy is recorded it defers to the observation under it.
 */
import { join } from "node:path";

import {
  createReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  parseReconciliationResult,
  reconcileReferenceScriptsBeforeAuthPolicy,
} from "../src/commands/reconcile.js";
import {
  createDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "../src/e2e/run-state.js";

const makeTempDir = createTrackedTempDirFactory("midgard-reconcile-ref-");

const ADDRESS =
  "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz";

afterEach(() => {
  vi.unstubAllEnvs();
});

describe("node-runtime reference-scripts-complete before an auth policy", () => {
  it("reports the publication blocked and not started, in a result the stack parses", async () => {
    vi.stubEnv("MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH", "");
    const runStatePath = join(await makeTempDir(), "run-state.json");

    const result = await reconcileReferenceScriptsBeforeAuthPolicy({
      runStatePath,
      address: ADDRESS,
    });

    expect(result).not.toBeNull();
    const parsed = parseReconciliationResult(
      JSON.parse(JSON.stringify(result)),
    );
    expect(parsed.milestone).toBe("reference-scripts-complete");
    expect(parsed.status).toBe("blocked");
    expect(parsed.target).toEqual({
      scope: "node-runtime",
      address: ADDRESS,
      authPolicyId: null,
    });
    expect(parsed.nextAction).toContain("deploy-reference-script-node-runtime");
  });

  it("defers to the observation once the run state records a policy", async () => {
    vi.stubEnv("MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH", "");
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const account = generateEmulatorAccount({ lovelace: 10_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(
      await createReferenceScriptAuthPolicy(lucid),
    );
    await writeDeploymentRunStateAtomic(
      runStatePath,
      createDeploymentRunState({
        mode: "resume",
        identity: {
          network: "Preprod",
          hubOracleOneShot: { txHash: "aa".repeat(32), outputIndex: 0 },
          manifestPath: join(dir, "contract-deployment-info.json"),
          referenceScriptAuthPolicyId: policyInfo.policyId,
          referenceScriptAuthPolicy: {
            policyId: policyInfo.policyId,
            nativeScript: policyInfo.nativeScript,
          },
        },
      }),
    );

    expect(
      await reconcileReferenceScriptsBeforeAuthPolicy({
        runStatePath,
        address: ADDRESS,
      }),
    ).toBeNull();
  });
});
