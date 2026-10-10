import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import { openWatcherProofObjective } from "./fault-proof-objective-table.js";
import {
  openWatcherJournalDatabase,
  type WatcherJournalDatabase,
  WatcherJournalIntegrityError,
  type WatcherJournalRow,
} from "./watcher-journal-database.js";
import {
  type WatcherJournalName,
  watcherObjectiveScope,
} from "./watcher-journal-schema.js";

/**
 * The fault-proof queue journal (ticket W2): one row per scheduled job
 * identity, updated in place as the job moves through queued, active and
 * finished. A newer identity of the same objective replaces the older row,
 * so retries never grow the table. Registering a job also records its
 * objective as open, in the same commit, before any workflow directory
 * exists.
 */
const JOURNAL = "fault_proof_queue" satisfies WatcherJournalName;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherFaultProofQueueIdentity = Readonly<{
  category: string;
  headerHash: string;
  decisionDigest: string;
  rollbackGeneration: string;
}>;

type QueueState = "queued" | "active" | "finished";

type QueueBody = Readonly<{
  identity: WatcherFaultProofQueueIdentity;
  queuedAtMs: string;
  observedAtMs: string;
}>;

export type WatcherFaultProofQueueJournal = Readonly<{
  /** Queues the job, or requeues it when it is active or finished; the
   * original queue time is kept. */
  register(
    identity: WatcherFaultProofQueueIdentity,
    observedAtMs: string,
  ): Promise<Readonly<{ queuedAtMs: string }>>;
  markStarted(jobIdentityDigest: string, observedAtMs: string): Promise<void>;
  markFinished(jobIdentityDigest: string, observedAtMs: string): Promise<void>;
  status(): Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
}>;

const identityDigest = (
  deploymentFingerprint: string,
  identity: WatcherFaultProofQueueIdentity,
): string => {
  if (
    typeof identity.category !== "string" ||
    identity.category.length === 0 ||
    identity.category.length > 128 ||
    !HEX_28.test(identity.headerHash) ||
    !HEX_32.test(identity.decisionDigest) ||
    !NATURAL.test(identity.rollbackGeneration)
  ) {
    throw new Error("fault-proof queue identity is invalid");
  }
  return watcherSha256CanonicalJson({ deploymentFingerprint, ...identity });
};

/** A row that differs from its job identity refuses the journals. */
const parseRow = (
  database: WatcherJournalDatabase,
  deploymentFingerprint: string,
  row: WatcherJournalRow,
): QueueBody & Readonly<{ state: QueueState }> => {
  const body = row.body as QueueBody | null;
  const fail = (): never =>
    database.refuse(JOURNAL, `row ${row.key} differs from its job identity`);
  if (
    (row.state !== "queued" &&
      row.state !== "active" &&
      row.state !== "finished") ||
    typeof body !== "object" ||
    body === null ||
    typeof body.queuedAtMs !== "string" ||
    !NATURAL.test(body.queuedAtMs) ||
    typeof body.identity !== "object" ||
    body.identity === null
  )
    fail();
  try {
    if (
      identityDigest(deploymentFingerprint, body!.identity) !== row.key ||
      watcherObjectiveScope(
        body!.identity.category,
        body!.identity.headerHash,
      ) !== row.scope
    )
      fail();
  } catch (error) {
    if (error instanceof WatcherJournalIntegrityError) throw error;
    fail();
  }
  return { ...body!, state: row.state as QueueState };
};

export const openWatcherFaultProofQueueJournal = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly authenticationKey: Uint8Array;
}): Promise<WatcherFaultProofQueueJournal> => {
  if (!HEX_32.test(input.deploymentFingerprint)) {
    throw new Error("fault-proof queue journal authority is invalid");
  }
  const database = openWatcherJournalDatabase({
    journalRoot: input.journalRoot,
    authenticationKey: input.authenticationKey,
  });
  // Startup admits every row once; a foreign or altered row fails closed.
  for (const row of database.rows(JOURNAL))
    parseRow(database, input.deploymentFingerprint, row);

  // Authenticated revisions establish event order. Wall time can move backward
  // during clock synchronization, so observation times are recorded but never
  // compared to authorize a transition.
  const transition = async (
    jobIdentityDigest: string,
    observedAtMs: string,
    from: QueueState,
    to: QueueState,
  ): Promise<void> => {
    if (!HEX_32.test(jobIdentityDigest))
      throw new Error(
        "fault-proof queue transition identity digest is invalid",
      );
    if (!NATURAL.test(observedAtMs))
      throw new Error(
        "fault-proof queue transition observation time is invalid",
      );
    database.transaction((tx) => {
      const row = tx.row(JOURNAL, jobIdentityDigest);
      if (row === undefined)
        throw new Error(
          `fault-proof queue transition has no admitted predecessor: ${jobIdentityDigest}`,
        );
      const prior = parseRow(database, input.deploymentFingerprint, row);
      if (prior.state !== from)
        throw new Error(
          `fault-proof queue transition requires ${from} predecessor, found ${prior.state}: ${jobIdentityDigest}`,
        );
      tx.put(JOURNAL, {
        key: jobIdentityDigest,
        scope: row.scope,
        state: to,
        body: {
          identity: prior.identity,
          queuedAtMs: prior.queuedAtMs,
          observedAtMs,
        },
      });
    });
  };

  return Object.freeze({
    register: async (identity, observedAtMs) => {
      if (!NATURAL.test(observedAtMs)) {
        throw new Error("fault-proof queue enqueue time is invalid");
      }
      const digest = identityDigest(input.deploymentFingerprint, identity);
      const scope = watcherObjectiveScope(
        identity.category,
        identity.headerHash,
      );
      return database.transaction((tx) => {
        openWatcherProofObjective(tx, {
          category: identity.category as WatcherInstalledWorkflowCategory,
          headerHash: identity.headerHash,
        });
        let queuedAtMs = observedAtMs;
        for (const row of tx.rows(JOURNAL, { scope })) {
          if (row.key !== digest) {
            tx.delete(JOURNAL, row.key);
            continue;
          }
          const prior = parseRow(database, input.deploymentFingerprint, row);
          if (prior.state === "queued")
            return Object.freeze({ queuedAtMs: prior.queuedAtMs });
          queuedAtMs = prior.queuedAtMs;
        }
        tx.put(JOURNAL, {
          key: digest,
          scope,
          state: "queued",
          body: {
            identity: {
              category: identity.category,
              headerHash: identity.headerHash,
              decisionDigest: identity.decisionDigest,
              rollbackGeneration: identity.rollbackGeneration,
            },
            queuedAtMs,
            observedAtMs,
          },
        });
        return Object.freeze({ queuedAtMs });
      });
    },
    markStarted: (digest, observedAtMs) =>
      transition(digest, observedAtMs, "queued", "active"),
    markFinished: (digest, observedAtMs) =>
      transition(digest, observedAtMs, "active", "finished"),
    status: () => {
      const queued = database
        .rows(JOURNAL, { state: "queued" })
        .map(({ body }) => BigInt((body as QueueBody).queuedAtMs));
      const oldest = queued.sort((left, right) =>
        left < right ? -1 : left > right ? 1 : 0,
      )[0];
      return Object.freeze({
        queuedJobCount: queued.length,
        oldestQueuedAtMs: oldest?.toString() ?? null,
      });
    },
  });
};

export const watcherFaultProofQueueIdentityDigest = (input: {
  readonly deploymentFingerprint: string;
  readonly identity: WatcherFaultProofQueueIdentity;
}): string => identityDigest(input.deploymentFingerprint, input.identity);
