import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  createReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import {
  type DeploymentRunCliOptions,
  loadPendingHubOracleNonceAttempt,
  recordHubOracleNonceSubmitted,
  recordHubOracleNonceTxHashConfirmed,
  resolveReferenceScriptAuthPolicyProgram,
} from "@/commands/deployment-run-state.js";
import {
  createDeploymentRunState,
  defaultDeploymentRunStatePath,
  DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  parseDeploymentRunState,
  RunStateError,
  sha256File,
  transitionDeploymentStep,
  withDeploymentRunStateLock,
  writeDeploymentRunStateAtomic,
} from "@/e2e/run-state.js";

import { createTrackedTempDirFactory } from "./helpers/temp-files.js";

const lucid = {
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
} as unknown as LucidEvolution;

const makeTempDir = createTrackedTempDirFactory("midgard-run-state-");

afterEach(() => {
  vi.restoreAllMocks();
});

describe("deployment run state", () => {
  it("creates and parses a versioned deployment run state", () => {
    const state = createDeploymentRunState({
      mode: "fresh",
      runId: "run-1",
      now: new Date("2026-01-01T00:00:00.000Z"),
      identity: {
        network: "Preprod",
        hubOracleOneShot: {
          txHash: "aa".repeat(32),
          outputIndex: 1,
        },
      },
    });

    expect(parseDeploymentRunState(state)).toEqual(state);
    expect(state).toMatchObject({
      schemaVersion: DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
      runId: "run-1",
      mode: "fresh",
      identity: {
        network: "Preprod",
      },
    });
  });

  it("rejects corrupt or unsupported state", () => {
    expect(() => parseDeploymentRunState({})).toThrow(RunStateError);
    expect(() =>
      parseDeploymentRunState({
        schemaVersion: "old",
      }),
    ).toThrow("Unsupported run-state schemaVersion");
  });

  it("transitions steps without dropping prior evidence", () => {
    const state = createDeploymentRunState({
      mode: "resume",
      runId: "run-2",
      now: new Date("2026-01-01T00:00:00.000Z"),
    });

    const submitted = transitionDeploymentStep(
      state,
      "initProtocol",
      "submitted",
      {
        txHashes: ["11".repeat(32)],
        evidence: ["logs/init.log"],
        details: {
          confirmationStatus: "submitted_confirmation_unknown",
        },
      },
      new Date("2026-01-01T00:01:00.000Z"),
    );
    const complete = transitionDeploymentStep(
      submitted,
      "initProtocol",
      "complete",
      {
        message: "deployment-status complete",
      },
      new Date("2026-01-01T00:02:00.000Z"),
    );

    expect(complete.steps.initProtocol).toMatchObject({
      status: "complete",
      txHashes: ["11".repeat(32)],
      evidence: ["logs/init.log"],
      details: {
        confirmationStatus: "submitted_confirmation_unknown",
      },
      message: "deployment-status complete",
    });
    expect(complete.events.map((event) => event.kind)).toEqual([
      "created",
      "step_transition",
      "step_transition",
    ]);
  });

  it("writes and reads state atomically", async () => {
    const dir = await makeTempDir();
    const path = join(dir, "state", "run-state.json");
    const state = transitionDeploymentStep(
      createDeploymentRunState({
        mode: "attach",
        runId: "run-3",
        now: new Date("2026-01-01T00:00:00.000Z"),
      }),
      "referenceScripts",
      "complete",
      {
        outRefs: ["aa".repeat(32) + "#0"],
      },
      new Date("2026-01-01T00:03:00.000Z"),
    );

    await writeDeploymentRunStateAtomic(path, state);

    await expect(loadDeploymentRunState(path)).resolves.toEqual(state);
    await expect(readFile(path, "utf8")).resolves.toContain(
      '"schemaVersion": "midgard-deployment-run-state-v1"',
    );
  });

  it("surfaces corrupt JSON as a run-state error", async () => {
    const dir = await makeTempDir();
    const path = join(dir, "run-state.json");
    await writeFile(path, "{not json", "utf8");

    await expect(loadDeploymentRunState(path)).rejects.toThrow(RunStateError);
  });

  it("locks mutations and refuses a concurrent holder", async () => {
    const dir = await makeTempDir();
    const path = join(dir, "run-state.json");

    await expect(
      withDeploymentRunStateLock(path, async () => {
        await expect(
          withDeploymentRunStateLock(path, async () => "unexpected"),
        ).rejects.toThrow("Run state is locked");
        return "ok";
      }),
    ).resolves.toBe("ok");
  });

  it("mutates under the lock and persists the next state", async () => {
    const dir = await makeTempDir();
    const path = join(dir, "run-state.json");

    const next = await mutateDeploymentRunState(
      path,
      () =>
        createDeploymentRunState({
          mode: "fresh",
          runId: "run-4",
          now: new Date("2026-01-01T00:00:00.000Z"),
        }),
      (state) =>
        transitionDeploymentStep(
          state,
          "hubOracleNonce",
          "confirmed",
          { outRefs: ["bb".repeat(32) + "#1"] },
          new Date("2026-01-01T00:05:00.000Z"),
        ),
    );

    expect(next.steps.hubOracleNonce?.status).toBe("confirmed");
    await expect(loadDeploymentRunState(path)).resolves.toEqual(next);
  });

  it("records and loads a pending submitted hub-oracle nonce attempt", async () => {
    const dir = await makeTempDir();
    const path = join(dir, "run-state.json");
    const options: DeploymentRunCliOptions = {
      runStatePath: path,
      freshRedeploy: false,
    };
    const txHash = "cc".repeat(32);

    const state = await recordHubOracleNonceSubmitted({
      options,
      network: "Preprod",
      txHash,
      address: "addr_test1operatornonce",
      lovelace: "5000000",
      inlineDatum: "d8799f00",
    });

    expect(state.steps.hubOracleNonce).toMatchObject({
      status: "submitted",
      txHashes: [txHash],
      message: "submitted_confirmation_unknown",
      details: {
        address: "addr_test1operatornonce",
        lovelace: "5000000",
        inlineDatum: "d8799f00",
        confirmationStatus: "submitted_confirmation_unknown",
        outputStatus: "unknown",
      },
    });
    await expect(
      loadPendingHubOracleNonceAttempt({ options }),
    ).resolves.toEqual({
      txHash,
      address: "addr_test1operatornonce",
      lovelace: "5000000",
      inlineDatum: "d8799f00",
    });

    const confirmed = await recordHubOracleNonceTxHashConfirmed({
      options,
      network: "Preprod",
      txHash,
      address: "addr_test1operatornonce",
      lovelace: "5000000",
      inlineDatum: "d8799f00",
      confirmationStatus: "reconciled_after_timeout",
    });

    expect(confirmed.steps.hubOracleNonce).toMatchObject({
      status: "submitted",
      txHashes: [txHash],
      message: "confirmed_output_pending",
      details: {
        address: "addr_test1operatornonce",
        lovelace: "5000000",
        inlineDatum: "d8799f00",
        confirmationStatus: "reconciled_after_timeout",
        outputStatus: "pending",
      },
    });
    await expect(
      loadPendingHubOracleNonceAttempt({ options }),
    ).resolves.toEqual({
      txHash,
      address: "addr_test1operatornonce",
      lovelace: "5000000",
      inlineDatum: "d8799f00",
    });
  });

  it("hashes manifest files and resolves env override paths", async () => {
    const dir = await makeTempDir();
    const manifestPath = join(dir, "contract-deployment-info.json");
    await writeFile(manifestPath, '{"contracts":{}}\n', "utf8");

    await expect(sha256File(manifestPath)).resolves.toBe(
      "4f68b856c756173b67a298783e963bdddc490623c904f6ad807c0e97d248f390",
    );
    expect(
      defaultDeploymentRunStatePath({
        MIDGARD_RUN_STATE_PATH: join(dir, "custom-state.json"),
      }),
    ).toBe(join(dir, "custom-state.json"));
  });
});

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
  }): ContractDeploymentInfo.DeploymentManifestV1 =>
    ({
      network,
      hubOracleOneShot: {
        txHash: oneShotTxHash,
        outputIndex: 0,
      },
      referenceScriptAuthPolicy: policyInfo,
    }) as ContractDeploymentInfo.DeploymentManifestV1;

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
    const policy = createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
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
    const policy = createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const policyInfo = referenceScriptAuthPolicyDeploymentInfo(policy);
    await writeFile(manifestPath, "{}\n", "utf8");
    const readManifest = vi
      .spyOn(ContractDeploymentInfo, "readDeploymentManifestV1File")
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
      createReferenceScriptAuthPolicy(lucid, 1_000, 10_000),
    );
    await writeFile(manifestPath, "{}\n", "utf8");
    vi.spyOn(
      ContractDeploymentInfo,
      "readDeploymentManifestV1File",
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
      createReferenceScriptAuthPolicy(lucid, 1_000, 10_000),
    );
    const manifestPolicyInfo = referenceScriptAuthPolicyDeploymentInfo(
      createReferenceScriptAuthPolicy(lucid, 2_000, 10_000),
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
          referenceScriptAuthPolicy: runStatePolicyInfo,
        },
      }),
    );
    await writeFile(manifestPath, "{}\n", "utf8");
    vi.spyOn(
      ContractDeploymentInfo,
      "readDeploymentManifestV1File",
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
    const policy = createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
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
});
