import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  type DeploymentRunCliOptions,
  loadPendingHubOracleNonceAttempt,
  recordHubOracleNonceSubmitted,
  recordHubOracleNonceTxHashConfirmed,
} from "../src/commands/deployment-run-state.js";
import {
  bindDeploymentRunStateToMarker,
  createDeploymentRunState,
  defaultDeploymentRunStatePath,
  DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  parseDeploymentRunEvent,
  parseDeploymentRunIdentity,
  parseDeploymentRunState,
  parseDeploymentStepState,
  RunStateError,
  sha256File,
  transitionDeploymentStep,
  withDeploymentRunStateLock,
  writeDeploymentRunStateAtomic,
} from "../src/e2e/run-state.js";
import { makeTempDir } from "./deployment-run-state.lucid.js";

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
    const state = createDeploymentRunState({
      mode: "fresh",
      runId: "wrong-version",
      now: new Date("2026-01-01T00:00:00.000Z"),
    });
    expect(() =>
      parseDeploymentRunState({
        ...state,
        schemaVersion: "old",
      }),
    ).toThrow("Unsupported run-state schemaVersion");
  });

  it("rejects missing and extra fields at every run-state boundary", () => {
    const state = transitionDeploymentStep(
      createDeploymentRunState({
        mode: "fresh",
        runId: "run-exact",
        now: new Date("2026-01-01T00:00:00.000Z"),
        identity: { network: "Preprod" },
      }),
      "init",
      "complete",
      {},
      new Date("2026-01-01T00:00:01.000Z"),
    );
    expect(parseDeploymentRunState(state)).toEqual(state);
    const { runId: _runId, ...missingRunId } = state;
    expect(() => parseDeploymentRunState(missingRunId)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseDeploymentRunState({ ...state, unexpected: true }),
    ).toThrow("unknown field");
    expect(() =>
      parseDeploymentRunState({ ...state, schemaVersion: "run-state-v0" }),
    ).toThrow("Unsupported run-state schemaVersion");
    expect(() =>
      parseDeploymentRunState({
        ...state,
        updatedAt: "2026-01-01T00:00:00Z",
      }),
    ).toThrow("canonical ISO timestamp");
    expect(() =>
      parseDeploymentRunState({
        ...state,
        updatedAt: "2026-01-01T00:00:02.000Z",
      }),
    ).toThrow("creation event are inconsistent");
    expect(() =>
      parseDeploymentRunState({
        ...state,
        steps: {
          ...state.steps,
          init: { ...state.steps.init, status: "failed" },
        },
      }),
    ).toThrow("not bound to its latest transition event");
    expect(() =>
      parseDeploymentRunState({
        ...state,
        events: [
          state.events[0],
          { ...state.events[1], kind: "legacy_transition" },
        ],
      }),
    ).toThrow("must be created or step_transition");

    expect(() =>
      parseDeploymentRunIdentity({
        ...state.identity,
        unexpected: true,
      }),
    ).toThrow("unknown field");
    expect(() =>
      parseDeploymentRunIdentity({
        hubOracleOneShot: { txHash: "AA".repeat(32), outputIndex: 0 },
      }),
    ).toThrow("lowercase hexadecimal");
    const step = state.steps.init!;
    expect(parseDeploymentStepState(step)).toEqual(step);
    expect(() =>
      parseDeploymentStepState({ ...step, unexpected: true }),
    ).toThrow("unknown field");
    const event = state.events[0]!;
    expect(parseDeploymentRunEvent(event)).toEqual(event);
    expect(() =>
      parseDeploymentRunEvent({ ...event, unexpected: true }),
    ).toThrow("unknown field");
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

  it("binds the run state exactly once to the final deployment marker", () => {
    const initial = createDeploymentRunState({
      mode: "fresh",
      runId: "run-marker",
      now: new Date("2026-01-01T00:00:00.000Z"),
      identity: { network: "Preprod" },
    });
    const marker = makeDeploymentMarker("ab".repeat(32));
    const bound = bindDeploymentRunStateToMarker(initial, {
      marker,
      manifestPath: "/deployment/contract-deployment-info.json",
      manifestSha256: "cd".repeat(32),
      now: new Date("2026-01-01T00:00:01.000Z"),
    });
    expect(parseDeploymentRunState(bound).identity).toMatchObject({
      deploymentMarker: marker,
      manifestSha256: "cd".repeat(32),
    });
    expect(() =>
      bindDeploymentRunStateToMarker(bound, {
        marker: makeDeploymentMarker("ef".repeat(32)),
        manifestPath: "/deployment/contract-deployment-info.json",
        manifestSha256: "01".repeat(32),
        now: new Date("2026-01-01T00:00:02.000Z"),
      }),
    ).toThrow("Deployment run state marker mismatch");
    expect(() =>
      parseDeploymentRunIdentity({
        deploymentMarker: {
          schemaVersion: "midgard-deployment-marker-v1",
          manifestId: "ab".repeat(32),
          legacyFingerprint: "ab".repeat(32),
        },
      }),
    ).toThrow("must contain exactly schemaVersion and manifestId");
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
