import { createHash, createHmac, timingSafeEqual } from "node:crypto";
import { mkdir, open, readdir, readFile, realpath } from "node:fs/promises";
import { isAbsolute, join, normalize } from "node:path";

import { watcherCanonicalJsonV1 } from "../storage/durable-store.js";

export const WATCHER_PRODUCTION_FAULT_PROOF_QUEUE_JOURNAL_V1 =
  "midgard-watcher-production-fault-proof-queue-journal-v1" as const;
export const WATCHER_PRODUCTION_FAULT_PROOF_QUEUE_RECORD_V1 =
  "midgard-watcher-production-fault-proof-queue-record-v1" as const;

const RECORD_FILE = /^([0-9]{20})\.json$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAXIMUM_RECORDS = 65_536;
const MAXIMUM_RECORD_BYTES = 64 * 1024;
const MAC_DOMAIN = "midgard-watcher-fault-proof-edf-queue-v1";

export type WatcherProductionFaultProofQueueIdentityV1 = Readonly<{
  category: string;
  headerHash: string;
  decisionDigest: string;
  rollbackGeneration: string;
}>;

type QueueEventV1 =
  | Readonly<{
      kind: "enqueued";
      identity: WatcherProductionFaultProofQueueIdentityV1;
      queuedAtMs: string;
    }>
  | Readonly<{
      kind: "requeued" | "started" | "finished";
      jobIdentityDigest: string;
      observedAtMs: string;
    }>;

type QueueRecordV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_FAULT_PROOF_QUEUE_RECORD_V1;
  revision: string;
  priorRecordSha256: string | null;
  deploymentFingerprint: string;
  event: QueueEventV1;
  authenticationKeyId: string;
  authenticationMac: string;
}>;

type QueueStateV1 = Readonly<{
  queuedAtMs: string;
  state: "queued" | "active" | "finished";
}>;

export type WatcherProductionFaultProofQueueJournalV1 = Readonly<{
  register(
    identity: WatcherProductionFaultProofQueueIdentityV1,
    observedAtMs: string,
  ): Promise<Readonly<{ queuedAtMs: string; finished: boolean }>>;
  markStarted(jobIdentityDigest: string, observedAtMs: string): Promise<void>;
  markFinished(jobIdentityDigest: string, observedAtMs: string): Promise<void>;
  status(): Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
}>;

const sha256 = (value: Uint8Array | string): string =>
  createHash("sha256").update(value).digest("hex");

const exactDirectory = async (path: string): Promise<string> => {
  if (
    !isAbsolute(path) ||
    normalize(path) !== path ||
    path === "/" ||
    path === "/tmp" ||
    path.startsWith("/tmp/")
  ) {
    throw new Error("fault-proof queue journal directory is invalid");
  }
  await mkdir(path, { recursive: true, mode: 0o700 });
  if ((await realpath(path)) !== path) {
    throw new Error("fault-proof queue journal traverses a symlink");
  }
  return path;
};

const identityDigest = (
  deploymentFingerprint: string,
  identity: WatcherProductionFaultProofQueueIdentityV1,
): string => {
  if (
    identity.category.length === 0 ||
    identity.category.length > 128 ||
    !HEX_28.test(identity.headerHash) ||
    !HEX_32.test(identity.decisionDigest) ||
    !NATURAL.test(identity.rollbackGeneration)
  ) {
    throw new Error("fault-proof queue identity is invalid");
  }
  return sha256(watcherCanonicalJsonV1({ deploymentFingerprint, ...identity }));
};

const recordBody = (record: QueueRecordV1) => ({
  schemaVersion: record.schemaVersion,
  revision: record.revision,
  priorRecordSha256: record.priorRecordSha256,
  deploymentFingerprint: record.deploymentFingerprint,
  event: record.event,
  authenticationKeyId: record.authenticationKeyId,
});

export const openWatcherProductionFaultProofQueueJournalV1 = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly authenticationKey: Uint8Array;
}): Promise<WatcherProductionFaultProofQueueJournalV1> => {
  if (
    !HEX_32.test(input.deploymentFingerprint) ||
    input.authenticationKey.byteLength !== 32
  ) {
    throw new Error("fault-proof queue journal authority is invalid");
  }
  const directory = await exactDirectory(
    join(input.journalRoot, "fault-proof-queue-v1"),
  );
  const key = createHmac("sha256", input.authenticationKey)
    .update(MAC_DOMAIN)
    .digest();
  const authenticationKeyId = sha256(key);
  const mac = (body: unknown): string =>
    createHmac("sha256", key)
      .update(watcherCanonicalJsonV1(body))
      .digest("hex");
  const states = new Map<string, QueueStateV1>();
  let lastRecordSha256: string | null = null;
  let nextRevision = 0n;
  const entries = await readdir(directory, { withFileTypes: true });
  entries.sort((left, right) => left.name.localeCompare(right.name));
  if (entries.length > MAXIMUM_RECORDS) {
    throw new Error("fault-proof queue journal exceeds its recovery bound");
  }
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index]!;
    const match = RECORD_FILE.exec(entry.name);
    if (
      !entry.isFile() ||
      match === null ||
      BigInt(match[1]!) !== BigInt(index)
    ) {
      throw new Error("fault-proof queue journal contains an invalid record");
    }
    const bytes = await readFile(join(directory, entry.name));
    if (bytes.byteLength === 0 || bytes.byteLength > MAXIMUM_RECORD_BYTES) {
      throw new Error("fault-proof queue journal record size is invalid");
    }
    const parsed = JSON.parse(bytes.toString("utf8")) as QueueRecordV1;
    const body = recordBody(parsed);
    const expectedMac = Buffer.from(mac(body), "hex");
    const claimedMac = Buffer.from(parsed.authenticationMac ?? "", "hex");
    if (
      parsed.schemaVersion !== WATCHER_PRODUCTION_FAULT_PROOF_QUEUE_RECORD_V1 ||
      parsed.revision !== index.toString() ||
      parsed.deploymentFingerprint !== input.deploymentFingerprint ||
      parsed.priorRecordSha256 !== lastRecordSha256 ||
      parsed.authenticationKeyId !== authenticationKeyId ||
      claimedMac.byteLength !== expectedMac.byteLength ||
      !timingSafeEqual(claimedMac, expectedMac)
    ) {
      throw new Error("fault-proof queue journal authentication failed");
    }
    const event = parsed.event;
    if (event.kind === "enqueued") {
      if (!NATURAL.test(event.queuedAtMs)) {
        throw new Error("fault-proof queue enqueue time is invalid");
      }
      const digest = identityDigest(
        input.deploymentFingerprint,
        event.identity,
      );
      if (states.has(digest)) {
        throw new Error("fault-proof queue repeats its initial enqueue");
      }
      states.set(
        digest,
        Object.freeze({ queuedAtMs: event.queuedAtMs, state: "queued" }),
      );
    } else {
      if (
        !HEX_32.test(event.jobIdentityDigest) ||
        !NATURAL.test(event.observedAtMs)
      ) {
        throw new Error("fault-proof queue transition is invalid");
      }
      const prior = states.get(event.jobIdentityDigest);
      if (
        prior === undefined ||
        BigInt(event.observedAtMs) < BigInt(prior.queuedAtMs)
      ) {
        throw new Error(
          "fault-proof queue transition has no admitted predecessor",
        );
      }
      const state =
        event.kind === "started"
          ? "active"
          : event.kind === "finished"
            ? "finished"
            : "queued";
      states.set(
        event.jobIdentityDigest,
        Object.freeze({ queuedAtMs: prior.queuedAtMs, state }),
      );
    }
    lastRecordSha256 = sha256(bytes);
    nextRevision += 1n;
  }

  let serial = Promise.resolve();
  const append = async (event: QueueEventV1): Promise<void> => {
    const operation = serial.then(async () => {
      if (nextRevision >= BigInt(MAXIMUM_RECORDS)) {
        throw new Error("fault-proof queue journal exceeds its append bound");
      }
      const body = {
        schemaVersion: WATCHER_PRODUCTION_FAULT_PROOF_QUEUE_RECORD_V1,
        revision: nextRevision.toString(),
        priorRecordSha256: lastRecordSha256,
        deploymentFingerprint: input.deploymentFingerprint,
        event,
        authenticationKeyId,
      } as const;
      const record = Object.freeze({ ...body, authenticationMac: mac(body) });
      const bytes = Buffer.from(`${watcherCanonicalJsonV1(record)}\n`, "utf8");
      const name = `${nextRevision.toString().padStart(20, "0")}.json`;
      const handle = await open(join(directory, name), "wx", 0o600);
      try {
        await handle.writeFile(bytes);
        await handle.sync();
      } finally {
        await handle.close();
      }
      const directoryHandle = await open(directory, "r");
      try {
        await directoryHandle.sync();
      } finally {
        await directoryHandle.close();
      }
      lastRecordSha256 = sha256(bytes);
      nextRevision += 1n;
    });
    serial = operation.then(
      () => undefined,
      () => undefined,
    );
    await operation;
  };

  const transition = async (
    jobIdentityDigest: string,
    observedAtMs: string,
    kind: "started" | "finished",
  ): Promise<void> => {
    const prior = states.get(jobIdentityDigest);
    if (
      prior === undefined ||
      !NATURAL.test(observedAtMs) ||
      BigInt(observedAtMs) < BigInt(prior.queuedAtMs)
    ) {
      throw new Error("fault-proof queue transition authority is invalid");
    }
    await append(Object.freeze({ kind, jobIdentityDigest, observedAtMs }));
    states.set(
      jobIdentityDigest,
      Object.freeze({
        queuedAtMs: prior.queuedAtMs,
        state: kind === "started" ? "active" : "finished",
      }),
    );
  };

  return Object.freeze({
    register: async (identity, observedAtMs) => {
      if (!NATURAL.test(observedAtMs)) {
        throw new Error("fault-proof queue enqueue time is invalid");
      }
      const digest = identityDigest(input.deploymentFingerprint, identity);
      const prior = states.get(digest);
      if (prior?.state === "finished") {
        return Object.freeze({ queuedAtMs: prior.queuedAtMs, finished: true });
      }
      if (prior?.state === "queued") {
        return Object.freeze({ queuedAtMs: prior.queuedAtMs, finished: false });
      }
      if (prior?.state === "active") {
        await append(
          Object.freeze({
            kind: "requeued",
            jobIdentityDigest: digest,
            observedAtMs,
          }),
        );
        states.set(
          digest,
          Object.freeze({ queuedAtMs: prior.queuedAtMs, state: "queued" }),
        );
        return Object.freeze({ queuedAtMs: prior.queuedAtMs, finished: false });
      }
      await append(
        Object.freeze({
          kind: "enqueued",
          identity: Object.freeze({ ...identity }),
          queuedAtMs: observedAtMs,
        }),
      );
      states.set(
        digest,
        Object.freeze({ queuedAtMs: observedAtMs, state: "queued" }),
      );
      return Object.freeze({ queuedAtMs: observedAtMs, finished: false });
    },
    markStarted: async (digest, observedAtMs) =>
      await transition(digest, observedAtMs, "started"),
    markFinished: async (digest, observedAtMs) =>
      await transition(digest, observedAtMs, "finished"),
    status: () => {
      const queued = [...states.values()].filter(
        ({ state }) => state === "queued",
      );
      const oldest = queued
        .map(({ queuedAtMs }) => BigInt(queuedAtMs))
        .sort((left, right) => (left < right ? -1 : left > right ? 1 : 0))[0];
      return Object.freeze({
        queuedJobCount: queued.length,
        oldestQueuedAtMs: oldest?.toString() ?? null,
      });
    },
  });
};

export const watcherProductionFaultProofQueueIdentityDigestV1 = (input: {
  readonly deploymentFingerprint: string;
  readonly identity: WatcherProductionFaultProofQueueIdentityV1;
}): string => identityDigest(input.deploymentFingerprint, input.identity);
