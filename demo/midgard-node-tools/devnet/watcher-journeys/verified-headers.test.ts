import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  createWatcherOperationsObservability,
  type WatcherFaultProofSupervisor,
  type WatcherVerificationDiagnostic,
} from "midgard-watcher";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock } from "./fixture.js";
import {
  createJourneyVerifiedHeaders,
  JOURNEY_VERIFIED_HEADERS_ARTIFACT,
  verifyJourneyVerifiedHeaders,
} from "./verified-headers.js";

const block = (byte: string): JourneyBlock =>
  ({
    headerHash: byte.repeat(28),
    payloadEnvelopeCbor: Buffer.from(`envelope-${byte}`),
  }) as unknown as JourneyBlock;

const envelopeSha256 = (value: JourneyBlock) =>
  createHash("sha256").update(value.payloadEnvelopeCbor).digest("hex");

const PREDECESSOR = block("a1");
const SUCCESSOR = block("b2");
const ADOPTED_HEAD = block("c3");

/** The production observability served through its own HTTP handler. */
const watcher = () => {
  const observability = createWatcherOperationsObservability({
    deploymentFingerprint: "11".repeat(32),
    supervisor: {
      status: () => ({
        phase: "accepting",
        recovered: true,
        queuedJobCount: 0,
        activeJob: null,
        blockedJob: null,
        deadlineHealth: "safe",
        earliestDeadlineJob: null,
        remainingSafeStartMs: "1000",
      }),
    } as unknown as WatcherFaultProofSupervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
  });
  let sequence = 0;
  const record = (
    value: JourneyBlock,
    outcome: WatcherVerificationDiagnostic["outcome"],
    payloadEnvelopeSha256: string | undefined = envelopeSha256(value),
  ) => {
    sequence += 1;
    observability.sink.recordVerification({
      subjectDigest: sequence.toString(16).padStart(64, "0"),
      headerHash: value.headerHash,
      ...(payloadEnvelopeSha256 === undefined ? {} : { payloadEnvelopeSha256 }),
      queuedAtMs: "1",
      startedAtMs: "2",
      completedAtMs: "3",
      elapsedMs: "1",
      outcome,
    });
  };
  const paths: string[] = [];
  return {
    record,
    paths,
    operations: async (path: string) => {
      paths.push(path);
      const response = await observability.handleHttpRequest(
        new Request(`http://127.0.0.1${path}`),
      );
      expect(response.status).toBe(200);
      return await response.json();
    },
    observe: () => ({ pid: 4242 as number | null }),
  };
};

let directory: string;
beforeEach(async () => {
  directory = await mkdtemp(join(tmpdir(), "journey-verified-headers-"));
});
afterEach(async () => {
  await rm(directory, { recursive: true, force: true });
});

describe("journey verified-header evidence", () => {
  it("waits for every healthy block, then retains records the verifier accepts", async () => {
    const source = watcher();
    const headers = createJourneyVerifiedHeaders(source, directory);
    source.record(PREDECESSOR, "verified");
    expect(await headers.collect([PREDECESSOR])).toHaveLength(1);
    expect(
      await headers.collectHealthy(PREDECESSOR, SUCCESSOR),
    ).toBeUndefined();
    // Deferral and failure records never satisfy a healthy block.
    source.record(SUCCESSOR, "pending_da", undefined);
    source.record(SUCCESSOR, "failed", undefined);
    expect(
      await headers.collectHealthy(PREDECESSOR, SUCCESSOR),
    ).toBeUndefined();
    source.record(SUCCESSOR, "verified");
    const healthy = await headers.collectHealthy(PREDECESSOR, SUCCESSOR);
    expect(healthy?.map(({ headerHash }) => headerHash)).toEqual([
      PREDECESSOR.headerHash,
      PREDECESSOR.headerHash,
      SUCCESSOR.headerHash,
    ]);
    // Later polls read only records after the last sequence seen.
    expect(source.paths.at(-1)).toMatch(/&cursor=[1-9][0-9]*$/u);
    await headers.retain();
    await verifyJourneyVerifiedHeaders(directory, [PREDECESSOR, SUCCESSOR]);
  });

  it("binds the successor's parent when a resumed proof adopted a later head", async () => {
    const source = watcher();
    await writeJourneyArtifact(
      join(directory, "successor-predecessor.json"),
      ADOPTED_HEAD,
    );
    const headers = createJourneyVerifiedHeaders(source, directory);
    source.record(PREDECESSOR, "verified");
    source.record(SUCCESSOR, "verified");
    expect(
      await headers.collectHealthy(PREDECESSOR, SUCCESSOR),
    ).toBeUndefined();
    source.record(ADOPTED_HEAD, "verified");
    await headers.collectHealthy(PREDECESSOR, SUCCESSOR);
    await headers.retain();
    await verifyJourneyVerifiedHeaders(directory, [
      PREDECESSOR,
      ADOPTED_HEAD,
      SUCCESSOR,
    ]);
  });

  it("rereads from the start after the watcher restarts", async () => {
    const first = watcher();
    let current = first;
    const headers = createJourneyVerifiedHeaders(
      {
        operations: (path) => current.operations(path),
        observe: () => current.observe(),
      },
      directory,
    );
    first.record(block("d4"), "verified");
    first.record(block("d5"), "verified");
    expect(await headers.collect([PREDECESSOR])).toBeUndefined();
    const restarted = watcher();
    restarted.observe = () => ({ pid: 4343 });
    current = restarted;
    restarted.record(PREDECESSOR, "verified");
    expect(await headers.collect([PREDECESSOR])).toHaveLength(1);
    expect(restarted.paths[0]).toMatch(/&cursor=0$/u);
  });

  it("resumes from the records an earlier attempt retained", async () => {
    const earlier = watcher();
    const first = createJourneyVerifiedHeaders(earlier, directory);
    earlier.record(PREDECESSOR, "verified");
    earlier.record(SUCCESSOR, "verified");
    await first.collectHealthy(PREDECESSOR, SUCCESSOR);
    await first.retain();
    // The restarted watcher never re-verifies headers it decided before.
    const resumed = createJourneyVerifiedHeaders(watcher(), directory);
    expect(await resumed.collect([PREDECESSOR])).toHaveLength(1);
    expect(await resumed.collectHealthy(PREDECESSOR, SUCCESSOR)).toHaveLength(
      3,
    );
  });

  it("still binds a retained record to its block's payload", async () => {
    await writeJourneyArtifact(
      join(directory, JOURNEY_VERIFIED_HEADERS_ARTIFACT),
      [
        {
          kind: "verification",
          sequence: "1",
          headerHash: PREDECESSOR.headerHash,
          payloadEnvelopeSha256: "ee".repeat(32),
          outcome: "verified",
        },
      ],
    );
    const headers = createJourneyVerifiedHeaders(watcher(), directory);
    await expect(headers.collect([PREDECESSOR])).rejects.toThrow(
      /another payload envelope/u,
    );
  });

  it("pages through more records than one diagnostics page holds", async () => {
    const source = watcher();
    const headers = createJourneyVerifiedHeaders(source, directory);
    for (let index = 0; index < 150; index += 1)
      source.record(block("e6"), "verified");
    source.record(PREDECESSOR, "verified");
    expect(await headers.collect([PREDECESSOR])).toHaveLength(1);
    expect(source.paths).toHaveLength(2);
  });

  it.each([
    ["a fault decision", "fault_detected" as const, undefined],
    ["an unprovable gap", "unprovable_gap" as const, undefined],
    ["another payload envelope", "verified" as const, "ee".repeat(32)],
  ])(
    "fails a healthy block the watcher reports with %s",
    async (_label, outcome, digest) => {
      const source = watcher();
      const headers = createJourneyVerifiedHeaders(source, directory);
      source.record(
        PREDECESSOR,
        outcome,
        digest ?? envelopeSha256(PREDECESSOR),
      );
      await expect(headers.collect([PREDECESSOR])).rejects.toThrow(
        /not verified healthy|another payload envelope/u,
      );
    },
  );

  it.each([
    ["unverified_merged", "merged"],
    ["unverified_removed", "removed"],
  ])(
    "fails a healthy block the watcher reports as %s",
    async (outcome, release) => {
      // Served as the watcher's diagnostics page carries it.
      const headers = createJourneyVerifiedHeaders(
        {
          operations: async () => ({
            records: [
              {
                kind: "verification",
                sequence: "1",
                headerHash: PREDECESSOR.headerHash,
                outcome,
              },
            ],
            nextCursor: null,
          }),
          observe: () => ({ pid: 4242 }),
        },
        directory,
      );
      await expect(headers.collect([PREDECESSOR])).rejects.toThrow(
        `Header ${PREDECESSOR.headerHash} was ${release} on L1 before the watcher verified it (${outcome})`,
      );
    },
  );

  it("keeps a verified header verified when a restarted watcher later reports it released", async () => {
    const source = watcher();
    const headers = createJourneyVerifiedHeaders(source, directory);
    source.record(PREDECESSOR, "verified");
    expect(await headers.collect([PREDECESSOR])).toHaveLength(1);
    // A stale observation replayed after a restart lists the header L1 has
    // merged since it was verified.
    source.record(PREDECESSOR, "unverified_merged", undefined);
    expect(
      (await headers.collect([PREDECESSOR]))?.map(({ outcome }) => outcome),
    ).toEqual(["verified"]);
  });

  it("refuses to retain evidence before every healthy block was verified", async () => {
    const headers = createJourneyVerifiedHeaders(watcher(), directory);
    await expect(headers.retain()).rejects.toThrow(
      /Healthy blocks were not verified/u,
    );
  });
});

describe("journey verified-header verifier", () => {
  const retained = async () => {
    const source = watcher();
    const headers = createJourneyVerifiedHeaders(source, directory);
    source.record(PREDECESSOR, "verified");
    source.record(SUCCESSOR, "verified");
    await headers.collectHealthy(PREDECESSOR, SUCCESSOR);
    await headers.retain();
    const path = join(directory, JOURNEY_VERIFIED_HEADERS_ARTIFACT);
    return {
      path,
      records: JSON.parse(
        await readFile(path, "utf8"),
      ) as WatcherVerificationDiagnostic[],
    };
  };

  it("refuses a run directory without verified-header evidence", async () => {
    await expect(
      verifyJourneyVerifiedHeaders(directory, [PREDECESSOR]),
    ).rejects.toThrow(/Healthy predecessor\/successor decision is missing/u);
  });

  it("refuses a healthy block with no retained record", async () => {
    await retained();
    await expect(
      verifyJourneyVerifiedHeaders(directory, [PREDECESSOR, ADOPTED_HEAD]),
    ).rejects.toThrow(/Healthy predecessor\/successor decision is missing/u);
  });

  it.each([
    [
      "a tampered payload envelope digest",
      { payloadEnvelopeSha256: "ff".repeat(32) },
      /another payload envelope/u,
    ],
    [
      "a missing payload envelope digest",
      { payloadEnvelopeSha256: undefined },
      /another payload envelope/u,
    ],
    ["a non-verified outcome", { outcome: "failed" }, /not verified healthy/u],
  ])("refuses %s", async (_label, override, message) => {
    const { path, records } = await retained();
    await writeFile(
      path,
      JSON.stringify(
        records.map((record) =>
          record.headerHash === SUCCESSOR.headerHash
            ? { ...record, ...override }
            : record,
        ),
      ),
    );
    await expect(
      verifyJourneyVerifiedHeaders(directory, [PREDECESSOR, SUCCESSOR]),
    ).rejects.toThrow(message);
  });
});
