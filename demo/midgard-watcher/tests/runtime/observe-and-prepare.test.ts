import {
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherPreparedOpenReporter,
  openWatcherPreparedProofStore,
  watcherObserveAndPrepare,
  watcherPreparedOpenAlertSubject,
  watcherSubmissionModes,
} from "../../src/runtime/observe-and-prepare.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

const CURATED = DEPLOYMENT_PROFILES["preprod-emulator-curated"];
const TESTING = DEPLOYMENT_PROFILES["preprod-testing"];
const LISTED_PROVER = CURATED.allow_lists.fraud_provers![0]!;
const LISTED_CHALLENGER = CURATED.allow_lists.da_challengers![0]!;
const OUTSIDER = "cd".repeat(28);
const HEADER = "ef".repeat(28);
const FINGERPRINT = "11".repeat(32);

const directories: string[] = [];
const scratch = () => {
  const directory = mkdtempSync(join(tmpdir(), "watcher-observe-prepare-"));
  directories.push(directory);
  return directory;
};
afterEach(() => {
  for (const directory of directories.splice(0))
    rmSync(directory, { recursive: true, force: true });
});

const address = (hash: string) =>
  credentialToAddress("Preprod", { type: "Key", hash });

describe("watcher submission modes", () => {
  it("submits only with keys the deployment's allow-lists admit", () => {
    expect(
      watcherSubmissionModes(
        { prover: LISTED_PROVER, availability: LISTED_CHALLENGER },
        CURATED,
      ),
    ).toEqual({ faultProofs: "submit", availability: "submit" });
    expect(
      watcherSubmissionModes(
        { prover: OUTSIDER, availability: LISTED_CHALLENGER },
        CURATED,
      ),
    ).toEqual({ faultProofs: "observe_and_prepare", availability: "submit" });
    expect(
      watcherSubmissionModes(
        { prover: LISTED_PROVER, availability: OUTSIDER },
        CURATED,
      ),
    ).toEqual({ faultProofs: "submit", availability: "observe_and_prepare" });
    // An open deployment (no lists) admits every key.
    expect(
      watcherSubmissionModes(
        { prover: OUTSIDER, availability: OUTSIDER },
        TESTING,
      ),
    ).toEqual({ faultProofs: "submit", availability: "submit" });
  });

  it("installs a preparation hook only for the key a list omits", () => {
    const wiring = (prover: string, availability: string) =>
      watcherObserveAndPrepare({
        proverWalletAddress: address(prover),
        availabilityWalletAddress: address(availability),
        storagePath: join(scratch(), "watcher.sqlite"),
        deploymentFingerprint: FINGERPRINT,
        operationsSink: () => ({ setAlert: vi.fn() }),
        profile: CURATED,
      });
    const listed = wiring(LISTED_PROVER, LISTED_CHALLENGER);
    expect(listed.preparation).toBeUndefined();
    expect(listed.onOpensPrepared).toBeUndefined();
    const outsider = wiring(OUTSIDER, OUTSIDER);
    expect(outsider.modes).toEqual({
      faultProofs: "observe_and_prepare",
      availability: "observe_and_prepare",
    });
    expect(outsider.preparation).toBeDefined();
    expect(outsider.onOpensPrepared).toBeDefined();
  });
});

describe("prepared-proof store", () => {
  it("writes owner-only bundles atomically, keeps the first preparation time, and removes them", () => {
    const store = openWatcherPreparedProofStore(join(scratch(), "bundles"));
    const bundle = {
      format: "midgard-watcher-prepared-proof-v1",
      kind: "availability_open",
      deploymentFingerprint: FINGERPRINT,
      headerHash: HEADER,
      openDeadlineEpochMs: "5000",
      observerKeyHash: OUTSIDER,
      preparedAtMs: "1",
    } as const;
    const path = store.write(bundle);
    expect(path).toBe(
      join(store.directory, `availability_open-${HEADER}.json`),
    );
    expect(statSync(path).mode & 0o777).toBe(0o600);
    expect(statSync(store.directory).mode & 0o777).toBe(0o700);
    store.write({ ...bundle, preparedAtMs: "2" });
    expect(JSON.parse(readFileSync(path, "utf8")).preparedAtMs).toBe("1");
    store.write({ ...bundle, openDeadlineEpochMs: "6000", preparedAtMs: "3" });
    expect(JSON.parse(readFileSync(path, "utf8"))).toMatchObject({
      openDeadlineEpochMs: "6000",
      preparedAtMs: "3",
    });
    expect(readdirSync(store.directory)).toEqual([
      `availability_open-${HEADER}.json`,
    ]);
    store.remove({ kind: "availability_open", headerHash: HEADER });
    expect(readdirSync(store.directory)).toEqual([]);
    expect(() => store.write({ ...bundle, headerHash: "../x" })).toThrow(
      /header hash/u,
    );
  });
});

describe("prepared availability Opens", () => {
  it("pages once per due Open and clears it once the Open is no longer due", () => {
    const store = openWatcherPreparedProofStore(join(scratch(), "bundles"));
    const setAlert = vi.fn();
    const report = createWatcherPreparedOpenReporter({
      store,
      observerKeyHash: OUTSIDER,
      deploymentFingerprint: FINGERPRINT,
      operationsSink: () => ({ setAlert }),
      nowMs: () => 7,
    });
    const alert = (active: boolean) => ({
      code: "prepared_proof_awaiting_submission",
      subjectDigest: watcherPreparedOpenAlertSubject(HEADER),
      active,
      observedAtMs: "7",
    });
    report([{ headerHash: HEADER, openDeadlineEpochMs: 9_000 }]);
    report([{ headerHash: HEADER, openDeadlineEpochMs: 9_000 }]);
    expect(setAlert).toHaveBeenCalledExactlyOnceWith(alert(true));
    expect(
      JSON.parse(
        readFileSync(
          join(store.directory, `availability_open-${HEADER}.json`),
          "utf8",
        ),
      ),
    ).toEqual({
      format: "midgard-watcher-prepared-proof-v1",
      kind: "availability_open",
      deploymentFingerprint: FINGERPRINT,
      headerHash: HEADER,
      openDeadlineEpochMs: "9000",
      observerKeyHash: OUTSIDER,
      preparedAtMs: "7",
    });
    report([]);
    expect(setAlert).toHaveBeenLastCalledWith(alert(false));
    expect(readdirSync(store.directory)).toEqual([]);
  });
});

describe("observe-and-prepare operations", () => {
  const observability = (modes?: boolean) =>
    createWatcherOperationsObservability({
      deploymentFingerprint: FINGERPRINT,
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 32,
        requiredCategoryCount: 32,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "99000",
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      nowMs: () => 100_000n,
      ...(modes === true
        ? {
            submissionModes: () =>
              watcherSubmissionModes(
                { prover: OUTSIDER, availability: LISTED_CHALLENGER },
                CURATED,
              ),
          }
        : {}),
    });

  it("reports each key's mode, and a prepared proof as an alert that keeps the watcher ready", () => {
    expect(observability().api.metrics().submissionModes).toBeNull();
    const operations = observability(true);
    operations.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "100000",
    });
    expect(operations.api.metrics().submissionModes).toEqual({
      faultProofs: "observe_and_prepare",
      availability: "submit",
    });
    operations.sink.setAlert({
      code: "prepared_proof_awaiting_submission",
      subjectDigest: "44".repeat(32),
      active: true,
      observedAtMs: "100000",
    });
    expect(operations.api.status()).toMatchObject({
      readiness: "ready",
      activeAlerts: [{ code: "prepared_proof_awaiting_submission" }],
    });
    operations.sink.setAlert({
      code: "prepared_proof_awaiting_submission",
      subjectDigest: "44".repeat(32),
      active: false,
      observedAtMs: "100001",
    });
    expect(operations.api.status().activeAlerts).toEqual([]);
  });
});
