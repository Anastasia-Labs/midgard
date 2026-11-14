import "./deployment-run-state.deployment-run-state.js";

import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  createReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as ContractDeploymentInfo from "../src/commands/contract-deployment-info.js";
import {
  type DeploymentRunCliOptions,
  resolveReferenceScriptAuthPolicyProgram,
} from "../src/commands/deployment-run-state.js";
import {
  createDeploymentRunState,
  loadDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "../src/e2e/run-state.js";
import { lucid, makeTempDir } from "./deployment-run-state.lucid.js";

describe("deployment run-state command identity guards", () => {
  const manifestWithIdentity = ({
    network = "Preprod",
    oneShotTxHash,
    policyInfo,
  }: {
    readonly network?: string;
    readonly oneShotTxHash: string;
    readonly policyInfo: ReturnType<
      typeof referenceScriptAuthPolicyDeploymentInfo
    >;
  }): ContractDeploymentInfo.DeploymentManifest =>
    ({
      network,
      hubOracleOneShot: {
        txHash: oneShotTxHash,
        outputIndex: 0,
      },
      referenceScriptAuthPolicy: policyInfo,
    }) as ContractDeploymentInfo.DeploymentManifest;

  const resolveAuthPolicy = ({
    runStatePath,
    manifestPath,
    hubOracleOneShotTxHash,
    persistRunState,
  }: {
    readonly runStatePath: string;
    readonly manifestPath: string;
    readonly hubOracleOneShotTxHash: string;
    readonly persistRunState?: boolean;
  }) => {
    const options: DeploymentRunCliOptions = {
      runStatePath,
      freshRedeploy: false,
    };
    return Effect.runPromise(
      resolveReferenceScriptAuthPolicyProgram({
        options,
        lucid,
        network: "Preprod",
        hubOracleOneShotTxHash,
        hubOracleOneShotOutputIndex: 0,
        timelockDurationMs: 10_000,
        manifestOutputPath: manifestPath,
        persistRunState,
      }),
    );
  };

  it("refuses to reuse a run-state auth policy for a different one-shot identity", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const policy = await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(policy);
    await writeDeploymentRunStateAtomic(
      runStatePath,
      createDeploymentRunState({
        mode: "resume",
        runId: "run-mismatch",
        now: new Date("2026-01-01T00:00:00.000Z"),
        identity: {
          network: "Preprod",
          hubOracleOneShot: {
            txHash: "11".repeat(32),
            outputIndex: 0,
          },
          manifestPath,
          referenceScriptAuthPolicyId: policyInfo.policyId,
          referenceScriptAuthPolicy: {
            policyId: policyInfo.policyId,
            nativeScript: policyInfo.nativeScript,
          },
        },
      }),
    );

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: "22".repeat(32),
      }),
    ).rejects.toThrow(
      "deployment run state deployment identity does not match",
    );
  });

  it("restores the exact manifest auth policy when the run state is missing", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const oneShotTxHash = "33".repeat(32);
    const policy = await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(policy);
    await writeFile(manifestPath, "{}\n", "utf8");
    const readManifest = vi
      .spyOn(ContractDeploymentInfo, "readDeploymentManifestFile")
      .mockReturnValue(manifestWithIdentity({ oneShotTxHash, policyInfo }));

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: oneShotTxHash,
      }),
    ).resolves.toEqual(policy);
    expect(readManifest).toHaveBeenCalledWith(manifestPath);
    await expect(loadDeploymentRunState(runStatePath)).resolves.toMatchObject({
      identity: {
        network: "Preprod",
        hubOracleOneShot: {
          txHash: oneShotTxHash,
          outputIndex: 0,
        },
        manifestPath,
        referenceScriptAuthPolicyId: policyInfo.policyId,
        referenceScriptAuthPolicy: {
          policyId: policyInfo.policyId,
          nativeScript: policyInfo.nativeScript,
        },
      },
      steps: {
        referenceScriptAuthPolicy: {
          status: "complete",
          message: "imported from deployment manifest",
        },
      },
    });
  });

  it("refuses a manifest auth policy bound to a different deployment identity", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(
      await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000),
    );
    await writeFile(manifestPath, "{}\n", "utf8");
    vi.spyOn(
      ContractDeploymentInfo,
      "readDeploymentManifestFile",
    ).mockReturnValue(
      manifestWithIdentity({
        network: "Preview",
        oneShotTxHash: "44".repeat(32),
        policyInfo,
      }),
    );

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: "55".repeat(32),
      }),
    ).rejects.toThrow("deployment manifest deployment identity does not match");
    await expect(loadDeploymentRunState(runStatePath)).resolves.toBeNull();
  });

  it("fails closed on a malformed deployment manifest without creating run state", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    await writeFile(
      manifestPath,
      JSON.stringify({
        schemaVersion: "midgard-deployment-manifest-v1",
        referenceScriptAuthPolicy: {
          policyId: "00".repeat(28),
          nativeScript: {
            type: "Native",
            cborHex: "not-cbor",
            expiresAtSlot: 1,
            expiresAtUnixTime: 1,
            timelockDurationMs: 1,
          },
        },
      }),
      "utf8",
    );

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: "66".repeat(32),
      }),
    ).rejects.toThrow(
      `Deployment manifest at "${manifestPath}" cannot be reused because it is invalid`,
    );
    await expect(loadDeploymentRunState(runStatePath)).resolves.toBeNull();
  });

  it("refuses conflicting manifest and run-state auth policies", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const oneShotTxHash = "77".repeat(32);
    const runStatePolicyInfo = referenceScriptAuthPolicyDeploymentInfo(
      await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000),
    );
    const manifestPolicyInfo = referenceScriptAuthPolicyDeploymentInfo(
      await createReferenceScriptAuthPolicy(lucid, 2_000, 10_000),
    );
    await writeDeploymentRunStateAtomic(
      runStatePath,
      createDeploymentRunState({
        mode: "resume",
        runId: "run-policy-mismatch",
        now: new Date("2026-01-01T00:00:00.000Z"),
        identity: {
          network: "Preprod",
          hubOracleOneShot: { txHash: oneShotTxHash, outputIndex: 0 },
          manifestPath,
          referenceScriptAuthPolicyId: runStatePolicyInfo.policyId,
          // Run state stores only the two fields
          // `resolveReferenceScriptAuthPolicyProgram` persists; the wider
          // deployment-info record (`tokenNames`, `postTimelockAudit`) is
          // manifest-only and `parseDeploymentRunIdentity` fails closed on it.
          referenceScriptAuthPolicy: {
            policyId: runStatePolicyInfo.policyId,
            nativeScript: runStatePolicyInfo.nativeScript,
          },
        },
      }),
    );
    await writeFile(manifestPath, "{}\n", "utf8");
    vi.spyOn(
      ContractDeploymentInfo,
      "readDeploymentManifestFile",
    ).mockReturnValue(
      manifestWithIdentity({
        oneShotTxHash,
        policyInfo: manifestPolicyInfo,
      }),
    );

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: oneShotTxHash,
      }),
    ).rejects.toThrow(
      "deployment manifest and run state reference-script auth policy mismatch",
    );
  });

  it("resolves a run-state auth policy without persisting diagnostic transitions", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const oneShotTxHash = "66".repeat(32);
    const policy = await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(policy);
    await writeDeploymentRunStateAtomic(
      runStatePath,
      createDeploymentRunState({
        mode: "resume",
        runId: "run-read-only",
        now: new Date("2026-01-01T00:00:00.000Z"),
        identity: {
          network: "Preprod",
          hubOracleOneShot: { txHash: oneShotTxHash, outputIndex: 0 },
          manifestPath,
          referenceScriptAuthPolicyId: policyInfo.policyId,
          referenceScriptAuthPolicy: {
            policyId: policyInfo.policyId,
            nativeScript: policyInfo.nativeScript,
          },
        },
      }),
    );
    const beforeBytes = await readFile(runStatePath, "utf8");
    const beforeState = await loadDeploymentRunState(runStatePath);
    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: oneShotTxHash,
        persistRunState: false,
      }),
    ).resolves.toMatchObject({ policyId: policyInfo.policyId });
    await expect(readFile(runStatePath, "utf8")).resolves.toBe(beforeBytes);
    await expect(loadDeploymentRunState(runStatePath)).resolves.toEqual(
      beforeState,
    );
  });

  it("fails closed without creating run state during read-only resolution", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");

    await expect(
      resolveAuthPolicy({
        runStatePath,
        manifestPath,
        hubOracleOneShotTxHash: "77".repeat(32),
        persistRunState: false,
      }),
    ).rejects.toThrow(
      "Read-only reference-script capture requires an existing deployment run state",
    );
    await expect(loadDeploymentRunState(runStatePath)).resolves.toBeNull();
  });

  it("replaces an expired auth policy over an existing run state on a fresh redeploy", async () => {
    const dir = await makeTempDir();
    const runStatePath = join(dir, "run-state.json");
    const manifestPath = join(dir, "contract-deployment-info.json");
    const oneShotTxHash = "88".repeat(32);
    const expired = await createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const expiredInfo = referenceScriptAuthPolicyDeploymentInfo(expired);
    await writeDeploymentRunStateAtomic(
      runStatePath,
      createDeploymentRunState({
        mode: "resume",
        runId: "run-expired-policy",
        now: new Date("2026-01-01T00:00:00.000Z"),
        identity: {
          network: "Preprod",
          hubOracleOneShot: { txHash: oneShotTxHash, outputIndex: 0 },
          manifestPath,
          referenceScriptAuthPolicyId: expiredInfo.policyId,
          referenceScriptAuthPolicy: {
            policyId: expiredInfo.policyId,
            nativeScript: expiredInfo.nativeScript,
          },
        },
      }),
    );

    const replacement = await Effect.runPromise(
      resolveReferenceScriptAuthPolicyProgram({
        options: {
          runStatePath,
          freshRedeploy: true,
          freshRedeployReason:
            "auth policy expired before publication completed",
        },
        lucid,
        network: "Preprod",
        hubOracleOneShotTxHash: oneShotTxHash,
        hubOracleOneShotOutputIndex: 0,
        timelockDurationMs: 10_000,
        manifestOutputPath: manifestPath,
      }),
    );

    expect(replacement.policyId).not.toBe(expiredInfo.policyId);
    const state = await loadDeploymentRunState(runStatePath);
    expect(state?.mode).toBe("resume");
    expect(state?.identity).toMatchObject({
      hubOracleOneShot: { txHash: oneShotTxHash, outputIndex: 0 },
      referenceScriptAuthPolicyId: replacement.policyId,
    });
    expect(state?.steps.referenceScriptAuthPolicy?.message).toContain(
      "fresh_redeploy_reason=auth policy expired before publication completed",
    );
  });
});
