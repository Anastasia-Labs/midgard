import { createHash, randomUUID } from "node:crypto";
import {
  appendFile,
  mkdir,
  open,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { dirname, resolve } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Kupmios,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { LoadedCommitteeConfig } from "../config.js";
import type {
  ChainPoint,
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../domain.js";
import { canonicalJson } from "./canonical-json.js";
import { L1SourceIntegrityError } from "./source-integrity.js";
import {
  createLocalKupmiosStateQueueReplayProvider,
  fetchOgmiosTipBlockNo,
  kupoHoldsChainPoint,
} from "./state-queue-replay-provider.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

type CardanoNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export type CanonicalChainPoint = ChainPoint & {
  readonly network: string;
  readonly slot: number;
  readonly blockHash: string;
  readonly providerSource: string;
  readonly observedAt: string;
};

export type ChainSyncCursor = {
  readonly sequence: number;
  readonly point: CanonicalChainPoint;
  readonly rollbackGeneration: number;
};

export type ChainSyncEvent =
  | {
      readonly direction: "roll_forward";
      readonly point: CanonicalChainPoint;
    }
  | {
      readonly direction: "roll_backward";
      readonly point: CanonicalChainPoint;
    };

export type ChainSyncEventBatch = {
  readonly event?: ChainSyncEvent;
  readonly tip: CanonicalChainPoint;
};

export interface ChainSyncEventSource {
  next(
    cursor: ChainSyncCursor | undefined,
    intersectionCandidates?: readonly CanonicalChainPoint[],
  ): Promise<ChainSyncEventBatch>;
}

export interface ChainSyncCursorStore {
  load(): Promise<ChainSyncCursor | undefined>;
  append(event: ChainSyncEvent, cursor: ChainSyncCursor): Promise<void>;
  replay(afterSequence: number): Promise<readonly ChainSyncEvent[]>;
  intersectionPoints?(limit: number): Promise<readonly CanonicalChainPoint[]>;
  prune?(consumedSequence: number): Promise<void>;
}

/**
 * Distinct recent points a chain-sync resumption offers the node to intersect
 * with: the node rolls back at most k = 2160 blocks. The journal keeps the
 * entries that hold them, and older entries are pruned once consumed.
 */
export const CHAIN_SYNC_INTERSECTION_POINTS = 2160;

/**
 * Prunable entries a chain-sync journal accumulates before it is rewritten,
 * so the rewrite is amortized over many appends instead of every tick.
 */
export const CHAIN_SYNC_JOURNAL_PRUNE_SLACK = 1080;

export interface ChainSyncReplayProvider {
  currentChainSyncCursor(): Promise<ChainSyncCursor>;
  replayChainSyncEvents(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]>;
  loadConsumedChainSyncCursor(): Promise<ChainSyncCursor | undefined>;
  acknowledgeChainSyncCursor(cursor: ChainSyncCursor): Promise<void>;
}

type PersistedChainSyncState = {
  readonly schemaVersion: 2;
  readonly authorityFingerprint: string;
  readonly cursor?: ChainSyncCursor;
};

type PersistedChainSyncConsumerState = {
  readonly schemaVersion: 1;
  readonly authorityFingerprint: string;
  readonly cursor: ChainSyncCursor;
};

type PersistedChainSyncJournalEntry = {
  readonly sequence: number;
  readonly event: ChainSyncEvent;
  readonly cursor: ChainSyncCursor;
};

/**
 * The journal as it stands on disk. A line is committed only once its
 * terminating newline is written, so `entries` holds the complete lines and
 * `committedBytes` is where they end; anything past that is an interrupted
 * append that nothing references.
 */
type CommittedChainSyncJournal = {
  readonly entries: readonly PersistedChainSyncJournalEntry[];
  readonly committedBytes: number;
  readonly totalBytes: number;
};

export class FileChainSyncCursorStore implements ChainSyncCursorStore {
  private readonly journalPath: string;
  private cachedState: PersistedChainSyncState | undefined;
  // The journal holds the contiguous sequences [first, next).
  private cachedJournalFirstSequence = 0;
  private cachedJournalNextSequence = 0;
  private initialized = false;

  constructor(
    private readonly path: string,
    private readonly authorityFingerprint: string,
  ) {
    if (!/^[0-9a-f]{64}$/u.test(authorityFingerprint)) {
      throw new Error(
        "chain-sync authority fingerprint must be lowercase sha256 hex",
      );
    }
    this.journalPath = `${path}.events.jsonl`;
  }

  async load(): Promise<ChainSyncCursor | undefined> {
    const state = await this.initialize();
    return state.cursor;
  }

  async append(event: ChainSyncEvent, cursor: ChainSyncCursor): Promise<void> {
    const previous = await this.initialize();
    if (
      previous.cursor !== undefined &&
      cursor.sequence !== previous.cursor.sequence + 1
    ) {
      throw new L1SourceIntegrityError(
        `chain-sync cursor sequence is not contiguous: persisted=${previous.cursor.sequence.toString()}, next=${cursor.sequence.toString()}`,
      );
    }
    if (this.cachedJournalNextSequence !== cursor.sequence) {
      throw new L1SourceIntegrityError(
        "chain-sync event journal does not match its durable cursor; refusing unsafe recovery",
      );
    }
    await mkdir(dirname(this.path), { recursive: true });
    const next: PersistedChainSyncState = {
      schemaVersion: 2,
      authorityFingerprint: this.authorityFingerprint,
      cursor,
    };
    try {
      // A failure part-way through leaves an unterminated line; the next
      // initialize discards it, since no cursor metadata can reference it.
      await appendFile(
        this.journalPath,
        `${JSON.stringify({ sequence: cursor.sequence, event, cursor })}\n`,
        { encoding: "utf8", mode: 0o600 },
      );
      // The cursor metadata below commits this line, so the line must be
      // durable first: metadata that outlives its journal line is refused as an
      // integrity failure on the next start.
      await syncFileData(this.journalPath);
      const temporaryPath = `${this.path}.${randomUUID()}.tmp`;
      await writeFile(temporaryPath, `${JSON.stringify(next)}\n`, {
        encoding: "utf8",
        mode: 0o600,
      });
      await rename(temporaryPath, this.path);
    } catch (error) {
      this.initialized = false;
      this.cachedState = undefined;
      this.cachedJournalFirstSequence = 0;
      this.cachedJournalNextSequence = 0;
      throw error;
    }
    this.cachedState = next;
    this.cachedJournalNextSequence += 1;
  }

  async replay(afterSequence: number): Promise<readonly ChainSyncEvent[]> {
    if (!Number.isSafeInteger(afterSequence) || afterSequence < -1) {
      throw new Error("chain-sync replay sequence must be an integer >= -1");
    }
    const state = await this.initialize();
    const journal = await this.readJournal();
    await this.assertJournalMatchesCursor(state.cursor, journal);
    return journal
      .filter(({ sequence }) => sequence > afterSequence)
      .map(({ event }) => event);
  }

  async intersectionPoints(
    limit: number,
  ): Promise<readonly CanonicalChainPoint[]> {
    if (!Number.isSafeInteger(limit) || limit < 1) {
      throw new Error("chain-sync intersection limit must be positive");
    }
    await this.initialize();
    const journal = await this.readJournal();
    const seen = new Set<string>();
    const points: CanonicalChainPoint[] = [];
    for (let index = journal.length - 1; index >= 0; index -= 1) {
      const point = journal[index]!.cursor.point;
      const key = `${point.slot.toString()}:${point.blockHash}`;
      if (!seen.has(key)) {
        seen.add(key);
        points.push(point);
        if (points.length === limit) {
          break;
        }
      }
    }
    return points;
  }

  /**
   * Drops the entries that no reader needs any more: those older than both
   * the newest `CHAIN_SYNC_INTERSECTION_POINTS` distinct points (what a
   * resumption intersects with) and every event after `consumedSequence`
   * (what the durable consumer has yet to replay). The journal is rewritten
   * atomically, and only once `CHAIN_SYNC_JOURNAL_PRUNE_SLACK` such entries
   * have accumulated, so it stays bounded without a rewrite per append.
   */
  async prune(consumedSequence: number): Promise<void> {
    if (!Number.isSafeInteger(consumedSequence) || consumedSequence < -1) {
      throw new Error("chain-sync consumed sequence must be an integer >= -1");
    }
    const state = await this.initialize();
    if (
      this.cachedJournalNextSequence - this.cachedJournalFirstSequence <
      CHAIN_SYNC_INTERSECTION_POINTS + CHAIN_SYNC_JOURNAL_PRUNE_SLACK
    ) {
      return;
    }
    const committed = await this.readCommittedJournal();
    const journal = committed.entries;
    await this.assertJournalMatchesCursor(state.cursor, journal);
    const seen = new Set<string>();
    let oldestIntersectionIndex: number | undefined;
    for (let index = journal.length - 1; index >= 0; index -= 1) {
      const point = journal[index]!.cursor.point;
      seen.add(`${point.slot.toString()}:${point.blockHash}`);
      if (seen.size === CHAIN_SYNC_INTERSECTION_POINTS) {
        oldestIntersectionIndex = index;
        break;
      }
    }
    if (oldestIntersectionIndex === undefined) {
      return;
    }
    const firstSequence = journal[0]!.sequence;
    const retainFrom = Math.min(
      journal[oldestIntersectionIndex]!.sequence,
      consumedSequence + 1,
    );
    if (retainFrom - firstSequence < CHAIN_SYNC_JOURNAL_PRUNE_SLACK) {
      return;
    }
    const retained = journal.slice(retainFrom - firstSequence);
    // The rewrite is durable before it replaces the journal, so a crash leaves
    // either journal, and each is contiguous up to the durable cursor.
    const temporaryPath = `${this.journalPath}.${randomUUID()}.tmp`;
    try {
      await writeFile(
        temporaryPath,
        retained.map((entry) => `${JSON.stringify(entry)}\n`).join(""),
        { encoding: "utf8", mode: 0o600 },
      );
      await syncFileData(temporaryPath);
      await rename(temporaryPath, this.journalPath);
    } catch (error) {
      await rm(temporaryPath, { force: true });
      throw error;
    }
    this.cachedJournalFirstSequence = retainFrom;
  }

  private async initialize(): Promise<PersistedChainSyncState> {
    if (this.initialized) {
      return this.cachedState!;
    }
    let state = await this.readState();
    const committedJournal = await this.readCommittedJournal();
    const journal = committedJournal.entries;
    const firstSequence = journal[0]?.sequence ?? 0;
    const entryAt = (sequence: number) => journal[sequence - firstSequence];
    if (
      state.cursor !== undefined &&
      (entryAt(state.cursor.sequence) === undefined ||
        !samePersistedCursor(
          entryAt(state.cursor.sequence)!.cursor,
          state.cursor,
        ))
    ) {
      throw new L1SourceIntegrityError(
        "persisted chain-sync cursor does not match its durable event journal",
      );
    }
    const journalCursor = journal.at(-1)?.cursor;
    const rollsForward =
      journalCursor !== undefined &&
      (state.cursor === undefined ||
        journalCursor.sequence > state.cursor.sequence);
    if (rollsForward && state.cursor !== undefined) {
      const expectedNext = state.cursor.sequence + 1;
      if (entryAt(expectedNext)?.sequence !== expectedNext) {
        throw new L1SourceIntegrityError(
          "persisted chain-sync journal tail is not contiguous with its cursor",
        );
      }
    }
    if (committedJournal.totalBytes > committedJournal.committedBytes) {
      // An append that never completed: the metadata is written only after
      // its line returns, so the checks above prove nothing references it.
      // Drop it before the next append would extend it into a garbage line;
      // the chain-sync source re-delivers the event from the recovered cursor.
      await truncateFileDurably(
        this.journalPath,
        committedJournal.committedBytes,
      );
    }
    if (rollsForward) {
      state = {
        schemaVersion: 2,
        authorityFingerprint: this.authorityFingerprint,
        cursor: journalCursor,
      };
      await this.writeState(state);
    }
    await this.assertJournalMatchesCursor(state.cursor, journal);
    this.cachedState = state;
    this.cachedJournalFirstSequence = firstSequence;
    this.cachedJournalNextSequence = firstSequence + journal.length;
    this.initialized = true;
    return state;
  }

  private async readState(): Promise<PersistedChainSyncState> {
    let raw: string;
    try {
      raw = await readFile(this.path, "utf8");
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return {
          schemaVersion: 2,
          authorityFingerprint: this.authorityFingerprint,
        };
      }
      throw error;
    }
    const parsed = JSON.parse(raw) as unknown;
    return parsePersistedChainSyncState(parsed, this.authorityFingerprint);
  }

  private async readJournal(): Promise<
    readonly PersistedChainSyncJournalEntry[]
  > {
    return (await this.readCommittedJournal()).entries;
  }

  private async readCommittedJournal(): Promise<CommittedChainSyncJournal> {
    let raw: Buffer;
    try {
      raw = await readFile(this.journalPath);
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return { entries: [], committedBytes: 0, totalBytes: 0 };
      }
      throw error;
    }
    // Every complete line ends in a newline byte, which never occurs inside a
    // multi-byte UTF-8 sequence. Only a final unterminated segment can be an
    // interrupted append; an unparseable complete line is still refused.
    const committedBytes = raw.lastIndexOf(0x0a) + 1;
    // Pruning drops a prefix, so the sequences run contiguously from the
    // first entry kept.
    let firstSequence: number | undefined;
    const entries = raw
      .subarray(0, committedBytes)
      .toString("utf8")
      .split("\n")
      .filter((line) => line.length > 0)
      .map((line, index) => {
        const record = getRecord(
          JSON.parse(line) as unknown,
          `persisted chain-sync event ${index.toString()}`,
        );
        const sequence = safeSlot(
          record.sequence,
          `persisted chain-sync event ${index.toString()} sequence`,
        );
        firstSequence ??= sequence;
        if (sequence !== firstSequence + index) {
          throw new L1SourceIntegrityError(
            "persisted chain-sync event sequences must be contiguous",
          );
        }
        const event = parsePersistedChainSyncEvent(record.event);
        const cursor = parsePersistedChainSyncCursor(record.cursor);
        if (
          cursor.sequence !== sequence ||
          !samePersistedEventPoint(event, cursor.point)
        ) {
          throw new L1SourceIntegrityError(
            "persisted chain-sync journal cursor does not match its event",
          );
        }
        return {
          sequence,
          event,
          cursor,
        };
      });
    return { entries, committedBytes, totalBytes: raw.length };
  }

  private async writeState(state: PersistedChainSyncState): Promise<void> {
    await mkdir(dirname(this.path), { recursive: true });
    const temporaryPath = `${this.path}.${randomUUID()}.tmp`;
    await writeFile(temporaryPath, `${JSON.stringify(state)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporaryPath, this.path);
  }

  private async assertJournalMatchesCursor(
    cursor: ChainSyncCursor | undefined,
    suppliedJournal?: readonly PersistedChainSyncJournalEntry[],
  ): Promise<void> {
    const journal = suppliedJournal ?? (await this.readJournal());
    if (
      (cursor === undefined && journal.length !== 0) ||
      (cursor !== undefined &&
        (journal.at(-1)?.sequence !== cursor.sequence ||
          !samePersistedCursor(journal.at(-1)!.cursor, cursor)))
    ) {
      throw new L1SourceIntegrityError(
        "persisted chain-sync cursor does not match its durable event journal",
      );
    }
  }
}

export class FileChainSyncConsumerCursorStore {
  constructor(
    private readonly path: string,
    private readonly authorityFingerprint: string,
  ) {
    if (!/^[0-9a-f]{64}$/u.test(authorityFingerprint)) {
      throw new Error(
        "chain-sync consumer authority fingerprint must be lowercase sha256 hex",
      );
    }
  }

  async load(): Promise<ChainSyncCursor | undefined> {
    let raw: string;
    try {
      raw = await readFile(this.path, "utf8");
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return undefined;
      }
      throw error;
    }
    const state = parsePersistedChainSyncConsumerState(
      JSON.parse(raw) as unknown,
      this.authorityFingerprint,
    );
    return state.cursor;
  }

  async save(cursor: ChainSyncCursor): Promise<void> {
    const previous = await this.load();
    if (
      previous !== undefined &&
      (cursor.sequence < previous.sequence ||
        cursor.rollbackGeneration < previous.rollbackGeneration)
    ) {
      throw new Error("chain-sync consumer cursor cannot move backwards");
    }
    if (
      previous !== undefined &&
      cursor.sequence === previous.sequence &&
      !samePersistedCursor(previous, cursor)
    ) {
      throw new Error(
        "chain-sync consumer cursor cannot change at the same sequence",
      );
    }
    const state: PersistedChainSyncConsumerState = {
      schemaVersion: 1,
      authorityFingerprint: this.authorityFingerprint,
      cursor,
    };
    await mkdir(dirname(this.path), { recursive: true });
    const temporaryPath = `${this.path}.${randomUUID()}.tmp`;
    await writeFile(temporaryPath, `${JSON.stringify(state)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporaryPath, this.path);
  }
}

export class LocalNodeChainAuthority {
  private cursor: ChainSyncCursor | undefined;
  private loaded = false;
  private operation = Promise.resolve();

  constructor(
    readonly authorityNodeId: string,
    readonly network: string,
    private readonly source: ChainSyncEventSource,
    private readonly store: ChainSyncCursorStore,
  ) {}

  async synchronizeToTip(maxEvents = 4096): Promise<CanonicalChainPoint> {
    let result: CanonicalChainPoint | undefined;
    const run = this.operation.then(async () => {
      await this.loadCursor();
      const intersectionCandidates =
        this.cursor === undefined
          ? undefined
          : await this.store.intersectionPoints?.(
              CHAIN_SYNC_INTERSECTION_POINTS,
            );
      for (let count = 0; count < maxEvents; count += 1) {
        const batch = await this.source.next(
          this.cursor,
          intersectionCandidates,
        );
        this.assertSourcePoint(batch.tip, "chain-sync tip");
        if (batch.event === undefined) {
          if (
            this.cursor === undefined ||
            !sameCanonicalPoint(this.cursor.point, batch.tip)
          ) {
            throw new Error(
              "chain-sync source reported no event before the canonical tip was reached",
            );
          }
          result = this.cursor.point;
          return;
        }
        this.assertSourcePoint(batch.event.point, "chain-sync event");
        const sequence = (this.cursor?.sequence ?? -1) + 1;
        const rollbackGeneration =
          (this.cursor?.rollbackGeneration ?? 0) +
          (batch.event.direction === "roll_backward" ? 1 : 0);
        const cursor: ChainSyncCursor = {
          sequence,
          point: batch.event.point,
          rollbackGeneration,
        };
        try {
          await this.store.append(batch.event, cursor);
        } catch (error) {
          // The append may or may not have reached the durable journal (a
          // failed write is transient, not an integrity fault). Forget the
          // cached cursor so the next sync resumes from whatever the store
          // recovers; the event source re-intersects from that cursor.
          this.cursor = undefined;
          this.loaded = false;
          throw error;
        }
        this.cursor = cursor;
        if (sameCanonicalPoint(batch.event.point, batch.tip)) {
          result = cursor.point;
          return;
        }
      }
      throw new Error(
        `local node chain-sync did not reach its advertised tip within ${maxEvents.toString()} events`,
      );
    });
    this.operation = run.catch(() => undefined);
    await run;
    return result!;
  }

  async currentPoint(): Promise<CanonicalChainPoint> {
    await this.loadCursor();
    if (this.cursor === undefined) {
      throw new Error(
        "local node chain authority has no synchronized canonical point",
      );
    }
    return this.cursor.point;
  }

  async currentCursor(): Promise<ChainSyncCursor> {
    await this.loadCursor();
    if (this.cursor === undefined) {
      throw new Error("local node chain authority has no durable cursor");
    }
    return this.cursor;
  }

  async replay(afterSequence: number): Promise<readonly ChainSyncEvent[]> {
    await this.loadCursor();
    return this.store.replay(afterSequence);
  }

  /**
   * Prunes journal entries the durable consumer has replayed through
   * `consumedSequence` and no resumption would intersect with. Serialized with
   * synchronization, so it never races an append.
   */
  async pruneConsumed(consumedSequence: number): Promise<void> {
    const run = this.operation.then(async () => {
      await this.store.prune?.(consumedSequence);
    });
    this.operation = run.catch(() => undefined);
    await run;
  }

  assertAligned(point: CanonicalChainPoint, sourceLabel: string): void {
    if (this.cursor === undefined) {
      throw new Error("local node chain authority has not been synchronized");
    }
    const canonical = this.cursor.point;
    if (!sameCanonicalPoint(point, canonical)) {
      throw new Error(
        `${sourceLabel} is stale or on a mismatched chain point: query=${point.network}:${point.slot.toString()}:${point.blockHash}, authority=${canonical.network}:${canonical.slot.toString()}:${canonical.blockHash}`,
      );
    }
  }

  private async loadCursor(): Promise<void> {
    if (!this.loaded) {
      this.cursor = await this.store.load();
      if (this.cursor !== undefined) {
        this.assertSourcePoint(
          this.cursor.point,
          "persisted chain-sync cursor",
        );
      }
      this.loaded = true;
    }
  }

  private assertSourcePoint(point: CanonicalChainPoint, label: string): void {
    if (point.network !== this.network) {
      throw new L1SourceIntegrityError(
        `${label} network ${point.network} does not match configured network ${this.network}`,
      );
    }
    if (point.providerSource !== `chain-sync:${this.authorityNodeId}`) {
      throw new L1SourceIntegrityError(
        `${label} provider source is not bound to local authority ${this.authorityNodeId}`,
      );
    }
  }
}

export class OgmiosChainSyncEventSource implements ChainSyncEventSource {
  private readonly request: OgmiosChainSyncRequest;

  constructor(
    private readonly ogmiosUrl: string,
    private readonly network: string,
    private readonly authorityNodeId: string,
    request?: OgmiosChainSyncRequest,
  ) {
    this.request = request ?? createOgmiosChainSyncRequest();
  }

  async next(
    cursor: ChainSyncCursor | undefined,
    intersectionCandidates?: readonly CanonicalChainPoint[],
  ): Promise<ChainSyncEventBatch> {
    const response = await this.request(
      this.ogmiosUrl,
      cursor?.point,
      intersectionCandidates,
      this.network,
      this.authorityNodeId,
    );
    return response;
  }
}

export class FixtureChainSyncEventSource implements ChainSyncEventSource {
  constructor(
    private readonly path: string,
    private readonly network: string,
    private readonly authorityNodeId: string,
  ) {}

  async next(
    cursor: ChainSyncCursor | undefined,
  ): Promise<ChainSyncEventBatch> {
    const parsed = JSON.parse(await readFile(this.path, "utf8")) as unknown;
    const events = parseFixtureChainSyncEvents(
      parsed,
      this.network,
      this.authorityNodeId,
    );
    const event = events[(cursor?.sequence ?? -1) + 1];
    if (event === undefined) {
      if (cursor === undefined) {
        throw new Error("chain-sync fixture contains no events");
      }
      return { tip: cursor.point };
    }
    return { event, tip: events.at(-1)!.point };
  }
}

export class FixtureStateQueueProvider implements StateQueueProvider {
  private readonly path: string;
  private readonly network: string;

  constructor(path: string, network: string) {
    this.path = path;
    this.network = network;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const raw = await readFile(this.path, "utf8");
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) {
      throw new Error("fixture provider file must contain an array");
    }
    return parsed as readonly ObservedStateQueueNode[];
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    const nodes = await this.fetchStateQueueNodes();
    const point = nodes[0]?.chainPoint;
    if (
      point?.slot === undefined ||
      point.blockHash === undefined ||
      point.providerSource === undefined
    ) {
      throw new Error(
        "fixture query provider requires node-derived slot, blockHash, and providerSource provenance",
      );
    }
    return {
      ...point,
      network: this.network,
      slot: point.slot,
      blockHash: point.blockHash,
      providerSource: point.providerSource,
      observedAt: point.observedAt ?? new Date().toISOString(),
    };
  }
}

export class LucidStateQueueProvider implements StateQueueProvider {
  private readonly lucid: LucidEvolution;
  private readonly stateQueueAddress: string;
  private readonly stateQueuePolicyId: string;
  private readonly providerSource: string;
  private readonly chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>;
  private readonly currentChainPointResolver: () => Promise<CanonicalChainPoint>;
  private readonly tipBlockNoResolver: () => Promise<number>;
  private readonly replayCheckpoints: NonNullable<
    StateQueueProvider["fetchStateQueueReplayCheckpoints"]
  >;
  private readonly chainPointHeldResolver?: (
    point: CanonicalChainPoint,
  ) => Promise<boolean>;

  constructor({
    lucid,
    stateQueueAddress,
    stateQueuePolicyId,
    providerSource,
    chainPointResolver,
    currentChainPointResolver,
    tipBlockNoResolver,
    replayCheckpoints,
    chainPointHeldResolver,
  }: {
    readonly lucid: LucidEvolution;
    readonly stateQueueAddress: string;
    readonly stateQueuePolicyId: string;
    readonly providerSource: string;
    readonly chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>;
    readonly currentChainPointResolver: () => Promise<CanonicalChainPoint>;
    /**
     * Reads the tip block height. A snapshot reads it right after its
     * outputs, inside the caller's check that the chain point held still.
     */
    readonly tipBlockNoResolver: () => Promise<number>;
    readonly replayCheckpoints: NonNullable<
      StateQueueProvider["fetchStateQueueReplayCheckpoints"]
    >;
    /** Whether the chain still holds a point it reported earlier. */
    readonly chainPointHeldResolver?: (
      point: CanonicalChainPoint,
    ) => Promise<boolean>;
  }) {
    this.lucid = lucid;
    this.stateQueueAddress = stateQueueAddress;
    this.stateQueuePolicyId = stateQueuePolicyId;
    this.providerSource = providerSource;
    this.chainPointResolver = chainPointResolver;
    this.currentChainPointResolver = currentChainPointResolver;
    this.tipBlockNoResolver = tipBlockNoResolver;
    this.replayCheckpoints = replayCheckpoints;
    this.chainPointHeldResolver = chainPointHeldResolver;
  }

  /** Whether the chain still holds `point`; undefined when it cannot tell. */
  async holdsChainPoint(
    point: CanonicalChainPoint,
  ): Promise<boolean | undefined> {
    return this.chainPointHeldResolver?.(point);
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    return (await this.fetchStateQueueSnapshot()).nodes;
  }

  async fetchStateQueueSnapshot(): Promise<ObservedStateQueueSnapshot> {
    const stateQueueUtxos = await SDK.fetchSortedStateQueueUTxOs(this.lucid, {
      stateQueueAddress: this.stateQueueAddress,
      stateQueuePolicyId: this.stateQueuePolicyId,
    });
    const snapshot = await stateQueueUtxosToObservedSnapshot(
      stateQueueUtxos,
      this.providerSource,
      this.chainPointResolver,
    );
    return { ...snapshot, tipBlockNo: await this.tipBlockNoResolver() };
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    return this.currentChainPointResolver();
  }

  async fetchStateQueueReplayCheckpoints(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> {
    return this.replayCheckpoints(anchor, current, tipBlockNo, limit);
  }
}

/**
 * The chain moved while a read that must observe one chain point was in
 * progress. An observation failure: the read is retaken, or the tick fails
 * and the next one retries.
 */
class ChainMovedDuringSnapshotError extends Error {}

/** Replay attempts one tick makes while the chain keeps moving under them. */
export const STATE_QUEUE_REPLAY_ATTEMPTS = 3;

/** Whether the chain held still since the watch was opened. */
type ChainWatch = () => Promise<boolean>;

/**
 * State-queue history is replayed across many queries that share no pinned
 * chain point, so a block or rollback landing mid-replay can make honest
 * history look inconsistent. An integrity failure from `replay` stands only
 * when the chain is shown to have held still while it ran. When the chain
 * moved, the replay is retaken, up to `STATE_QUEUE_REPLAY_ATTEMPTS` attempts
 * in all, before the failure becomes an observation failure that the next
 * tick replays.
 *
 * The first attempt is watched from the snapshot's own point. A retaken
 * attempt is watched from the point it starts at when `watchFromNow` can
 * show the snapshot is still on the chain (the chain only moved forward
 * since); otherwise it too is watched from the snapshot. So a failure is not
 * excused merely because blocks keep arriving while history is replayed.
 */
const replayWithIntegrityOnHeldChain = async <T>(
  replay: () => Promise<T>,
  heldSinceSnapshot: ChainWatch,
  watchFromNow?: () => Promise<ChainWatch | undefined>,
): Promise<T> => {
  let watch = heldSinceSnapshot;
  for (let attempt = 1; ; attempt += 1) {
    try {
      return await replay();
    } catch (failure) {
      if (!(failure instanceof L1SourceIntegrityError)) {
        throw failure;
      }
      let held: boolean;
      try {
        held = await watch();
      } catch (cause) {
        throw new Error(
          `state-queue replay failed and the chain could not be re-read to confirm it held still: ${failure.message}`,
          { cause },
        );
      }
      if (held) {
        throw failure;
      }
      if (attempt >= STATE_QUEUE_REPLAY_ATTEMPTS) {
        throw new ChainMovedDuringSnapshotError(
          `chain moved while state-queue history was replayed, ${attempt.toString()} times: ${failure.message}`,
          { cause: failure },
        );
      }
      watch = (await watchFromNow?.()) ?? heldSinceSnapshot;
    }
  }
};

/**
 * The tip height every surface read its snapshot at, when all report one.
 * Surfaces at different heights were not read at one point.
 */
const agreedTipBlockNo = (
  snapshots: readonly ObservedStateQueueSnapshot[],
  surfaces: string,
): number | undefined => {
  const heights = snapshots.map(({ tipBlockNo }) => tipBlockNo);
  if (heights.some((height) => height === undefined)) {
    return undefined;
  }
  if (new Set(heights).size !== 1) {
    throw new ChainMovedDuringSnapshotError(
      `${surfaces} read their state-queue snapshots at different tip heights`,
    );
  }
  return heights[0];
};

type ChainPointAwareStateQueueProvider = StateQueueProvider & {
  currentChainPoint(): Promise<CanonicalChainPoint>;
};

type StateQueueProviderWithOptionalPoint = StateQueueProvider & {
  currentChainPoint?: () => Promise<CanonicalChainPoint>;
  holdsChainPoint?: (
    point: CanonicalChainPoint,
  ) => Promise<boolean | undefined>;
};

export class MultiStateQueueProvider implements StateQueueProvider {
  private readonly providers: readonly StateQueueProviderWithOptionalPoint[];
  private readonly identities: readonly string[];
  private readonly mergedIdentities?: readonly string[];
  private readonly sourceMode: "local_node" | "external_providers";
  /** Each provider's chain point when the last snapshot was accepted. */
  private snapshotPoints: readonly (CanonicalChainPoint | undefined)[] = [];

  constructor(
    providers: readonly StateQueueProviderWithOptionalPoint[],
    options: {
      readonly sourceMode: "local_node" | "external_providers";
      readonly identities?: readonly string[];
    },
  ) {
    if (providers.length === 0) {
      throw new Error("at least one state-queue provider is required");
    }
    if (
      options.sourceMode !== "local_node" &&
      options.sourceMode !== "external_providers"
    ) {
      throw new Error(
        "state-queue provider sourceMode must be local_node or external_providers",
      );
    }
    const sourceMode = options.sourceMode;
    if (sourceMode === "external_providers" && providers.length < 2) {
      throw new Error(
        "external_providers mode requires at least two state-queue providers",
      );
    }
    const identities =
      options.identities ??
      providers.map((_, index) => `provider-${index.toString()}`);
    if (
      identities.length !== providers.length ||
      new Set(identities).size !== identities.length
    ) {
      throw new Error(
        "state-queue provider identities must be complete and distinct",
      );
    }
    this.providers = providers;
    this.identities = identities;
    this.mergedIdentities = options.identities;
    this.sourceMode = sourceMode;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const snapshots = await Promise.all(
      this.providers.map(async (provider, index) => {
        if (
          this.sourceMode === "external_providers" &&
          typeof provider.currentChainPoint !== "function"
        ) {
          throw new Error(
            `external provider ${this.identities[index]!} cannot prove its current chain point`,
          );
        }
        if (typeof provider.currentChainPoint === "function") {
          const pointBefore = await (
            provider as ChainPointAwareStateQueueProvider
          ).currentChainPoint();
          const nodes = await provider.fetchStateQueueNodes();
          const pointAfter = await (
            provider as ChainPointAwareStateQueueProvider
          ).currentChainPoint();
          if (!sameCanonicalPoint(pointBefore, pointAfter)) {
            throw new Error(
              `provider ${this.identities[index]!} chain point changed while its state-queue snapshot was read`,
            );
          }
          return { nodes, point: pointAfter };
        }
        return {
          nodes: await provider.fetchStateQueueNodes(),
          point: undefined,
        };
      }),
    );
    if (this.sourceMode === "external_providers") {
      const baselinePoint = snapshots[0]!.point!;
      for (const [index, { point }] of snapshots.entries()) {
        if (point === undefined || !sameCanonicalPoint(point, baselinePoint)) {
          throw new Error(
            `external provider current chain-point disagreement between ${this.identities[0]!} and ${this.identities[index]!}`,
          );
        }
      }
    }
    const results = snapshots.map(({ nodes }) => nodes);
    const sortedResults = results.map(sortObservedNodes);
    const baseline = canonicalObservedNodes(sortedResults[0]!);
    for (const [index, nodes] of sortedResults.entries()) {
      const candidate = canonicalObservedNodes(nodes);
      if (!canonicalArraysEqual(candidate, baseline)) {
        throw new L1SourceIntegrityError(
          `state queue provider disagreement in ${this.sourceMode} mode between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    assertCompatibleChainPoints(sortedResults);
    return mergeAgreedObservedNodes(sortedResults, this.mergedIdentities);
  }

  async fetchStateQueueSnapshot(): Promise<ObservedStateQueueSnapshot> {
    const snapshots = await Promise.all(
      this.providers.map(async (provider, index) => {
        if (provider.fetchStateQueueSnapshot === undefined) {
          throw new Error(
            `state-queue provider ${this.identities[index]!} cannot authenticate the confirmed root`,
          );
        }
        const pointBefore =
          provider.currentChainPoint === undefined
            ? undefined
            : await provider.currentChainPoint();
        if (
          this.sourceMode === "external_providers" &&
          pointBefore === undefined
        ) {
          throw new Error(
            `external provider ${this.identities[index]!} cannot prove its current chain point`,
          );
        }
        const snapshot = await provider.fetchStateQueueSnapshot();
        const pointAfter =
          provider.currentChainPoint === undefined
            ? undefined
            : await provider.currentChainPoint();
        if (
          pointBefore !== undefined &&
          (pointAfter === undefined ||
            !sameCanonicalPoint(pointBefore, pointAfter))
        ) {
          throw new Error(
            `provider ${this.identities[index]!} chain point changed while its state-queue root snapshot was read`,
          );
        }
        return { snapshot, point: pointAfter };
      }),
    );
    if (this.sourceMode === "external_providers") {
      const baselinePoint = snapshots[0]!.point!;
      for (const [index, { point }] of snapshots.entries()) {
        if (point === undefined || !sameCanonicalPoint(point, baselinePoint)) {
          throw new Error(
            `external provider current chain-point disagreement between ${this.identities[0]!} and ${this.identities[index]!}`,
          );
        }
      }
    }
    const baseline = snapshots[0]!.snapshot;
    const sortedResults = snapshots.map(({ snapshot }) =>
      sortObservedNodes(snapshot.nodes),
    );
    for (const [index, { snapshot }] of snapshots.entries()) {
      if (
        snapshot.confirmedHeaderHash !== baseline.confirmedHeaderHash ||
        snapshot.confirmedStateOutRef !== baseline.confirmedStateOutRef ||
        !canonicalArraysEqual(
          canonicalObservedNodes(sortedResults[index]!),
          canonicalObservedNodes(sortedResults[0]!),
        ) ||
        !compatibleChainPoint(
          snapshot.observedChainPoint,
          baseline.observedChainPoint,
        )
      ) {
        throw new L1SourceIntegrityError(
          `state queue root provider disagreement in ${this.sourceMode} mode between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    assertCompatibleChainPoints(sortedResults);
    const tipBlockNo = agreedTipBlockNo(
      snapshots.map(({ snapshot }) => snapshot),
      `${this.sourceMode} providers`,
    );
    this.snapshotPoints = snapshots.map(({ point }) => point);
    return {
      nodes: mergeAgreedObservedNodes(sortedResults, this.mergedIdentities),
      confirmedHeaderHash: baseline.confirmedHeaderHash,
      confirmedStateOutRef: baseline.confirmedStateOutRef,
      ...(tipBlockNo === undefined ? {} : { tipBlockNo }),
      observedChainPoint: mergeChainPoints(
        snapshots.map(({ snapshot }, index) => ({
          ...snapshot.observedChainPoint,
          providerSource:
            this.mergedIdentities?.[index] ??
            snapshot.observedChainPoint.providerSource,
        })),
      ),
    };
  }

  async fetchStateQueueReplayCheckpoints(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> {
    const snapshotPoints = this.snapshotPoints;
    const currentPoints = () =>
      Promise.all(
        this.providers.map((provider) => provider.currentChainPoint?.()),
      );
    const heldAt =
      (points: readonly (CanonicalChainPoint | undefined)[]) => async () => {
        if (
          points.length !== this.providers.length ||
          points.some((point) => point === undefined)
        ) {
          // Without per-provider points no movement can be shown.
          return true;
        }
        const now = await currentPoints();
        return now.every(
          (point, index) =>
            point !== undefined && sameCanonicalPoint(point, points[index]!),
        );
      };
    return replayWithIntegrityOnHeldChain(
      () => this.replayOnEveryProvider(anchor, current, tipBlockNo, limit),
      heldAt(snapshotPoints),
      async () => {
        if (
          snapshotPoints.length !== this.providers.length ||
          snapshotPoints.some((point) => point === undefined)
        ) {
          return undefined;
        }
        // The points are read first, and every provider must then still
        // hold its snapshot's point: the chain only moved forward since the
        // snapshot, so its queue is still on the chain, and a retaken replay
        // is watched from here.
        const points = await currentPoints();
        const held = await Promise.all(
          this.providers.map((provider, index) =>
            provider.holdsChainPoint?.(snapshotPoints[index]!),
          ),
        );
        return held.every((holds) => holds === true)
          ? heldAt(points)
          : undefined;
      },
    );
  }

  private async replayOnEveryProvider(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> {
    const histories = await Promise.all(
      this.providers.map(async (provider, index) => {
        if (provider.fetchStateQueueReplayCheckpoints === undefined) {
          throw new Error(
            `state-queue provider ${this.identities[index]!} has no authenticated ordered history source`,
          );
        }
        return provider.fetchStateQueueReplayCheckpoints(
          anchor,
          current,
          tipBlockNo,
          limit,
        );
      }),
    );
    const baseline = canonicalJson(histories[0]);
    for (const [index, history] of histories.entries()) {
      if (canonicalJson(history) !== baseline) {
        throw new L1SourceIntegrityError(
          `state-queue replay disagreement between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    return histories[0]!;
  }
}

export const LOCAL_NODE_SNAPSHOT_ATTEMPTS = 8;
export const LOCAL_NODE_SNAPSHOT_RETRY_MS = 250;

export class LocalNodeStateQueueProvider
  implements StateQueueProvider, ChainSyncReplayProvider
{
  constructor(
    private readonly authority: LocalNodeChainAuthority,
    private readonly queryProviders: readonly ChainPointAwareStateQueueProvider[],
    private readonly queryIdentities: readonly string[],
    private readonly consumerCursorStore: FileChainSyncConsumerCursorStore,
  ) {
    if (queryProviders.length === 0) {
      throw new Error(
        "local_node mode requires at least one same-node query surface",
      );
    }
    if (
      queryIdentities.length !== queryProviders.length ||
      new Set(queryIdentities).size !== queryIdentities.length
    ) {
      throw new Error(
        "local_node query identities must be complete and distinct",
      );
    }
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    return (await this.fetchStateQueueSnapshot()).nodes;
  }

  /**
   * A snapshot is taken at one chain point: every query surface must sit at
   * the authority's tip before and after its read. A block arriving inside
   * that window is expected on a live chain, so the whole snapshot is retaken
   * a bounded number of times; a surface that never settles on the authority's
   * point is refused.
   */
  async fetchStateQueueSnapshot(): Promise<ObservedStateQueueSnapshot> {
    for (let attempt = 1; ; attempt += 1) {
      try {
        return await this.readStateQueueSnapshotAtOnePoint();
      } catch (error) {
        if (
          !(error instanceof ChainMovedDuringSnapshotError) ||
          attempt >= LOCAL_NODE_SNAPSHOT_ATTEMPTS
        ) {
          throw error;
        }
      }
      await new Promise((resolve) =>
        setTimeout(resolve, LOCAL_NODE_SNAPSHOT_RETRY_MS),
      );
    }
  }

  private async readStateQueueSnapshotAtOnePoint(): Promise<ObservedStateQueueSnapshot> {
    const canonicalBefore = await this.authority.synchronizeToTip();
    const assertAtAuthorityPoint = (
      point: CanonicalChainPoint,
      index: number,
    ) => {
      try {
        this.authority.assertAligned(point, this.queryIdentities[index]!);
      } catch (error) {
        throw new ChainMovedDuringSnapshotError(
          error instanceof Error ? error.message : String(error),
        );
      }
    };
    const results = await Promise.all(
      this.queryProviders.map(async (provider, index) => {
        if (provider.fetchStateQueueSnapshot === undefined) {
          throw new Error(
            `local_node query surface ${this.queryIdentities[index]!} cannot authenticate the confirmed root`,
          );
        }
        const before = await provider.currentChainPoint();
        assertAtAuthorityPoint(before, index);
        const snapshot = await provider.fetchStateQueueSnapshot();
        const after = await provider.currentChainPoint();
        if (!sameCanonicalPoint(before, after)) {
          throw new ChainMovedDuringSnapshotError(
            `local_node query surface ${this.queryIdentities[index]!} changed chain point while its snapshot was read`,
          );
        }
        assertAtAuthorityPoint(after, index);
        return { snapshot, queryPoint: after };
      }),
    );
    const canonicalAfter = await this.authority.currentPoint();
    if (!sameCanonicalPoint(canonicalBefore, canonicalAfter)) {
      throw new ChainMovedDuringSnapshotError(
        "local node chain authority changed while query snapshots were being collected",
      );
    }
    const baselineSnapshot = results[0]!.snapshot;
    const sortedResults = results.map(({ snapshot }) =>
      sortObservedNodes(snapshot.nodes),
    );
    const baseline = canonicalObservedNodes(sortedResults[0]!);
    for (const [index, { snapshot }] of results.entries()) {
      if (
        snapshot.confirmedHeaderHash !== baselineSnapshot.confirmedHeaderHash ||
        snapshot.confirmedStateOutRef !==
          baselineSnapshot.confirmedStateOutRef ||
        !canonicalArraysEqual(
          canonicalObservedNodes(sortedResults[index]!),
          baseline,
        ) ||
        !compatibleChainPoint(
          snapshot.observedChainPoint,
          baselineSnapshot.observedChainPoint,
        )
      ) {
        throw new L1SourceIntegrityError(
          `local_node state-queue root disagreement between ${this.queryIdentities[0]!} and ${this.queryIdentities[index]!}`,
        );
      }
    }
    const merged = mergeAgreedObservedNodes(
      sortedResults,
      this.queryIdentities,
    );
    const tipBlockNo = agreedTipBlockNo(
      results.map(({ snapshot }) => snapshot),
      "local_node query surfaces",
    );
    const nodes = merged.map((node) => ({
      ...node,
      chainPoint: {
        ...declaredChainPoint(node.chainPoint),
        providerSource: [
          canonicalAfter.providerSource,
          ...this.queryIdentities,
        ].join(","),
        observedAt: new Date().toISOString(),
      } satisfies ChainPoint,
    }));
    return {
      nodes,
      confirmedHeaderHash: baselineSnapshot.confirmedHeaderHash,
      confirmedStateOutRef: baselineSnapshot.confirmedStateOutRef,
      ...(tipBlockNo === undefined ? {} : { tipBlockNo }),
      observedChainPoint: {
        ...mergeChainPoints(
          results.map(({ snapshot }, index) => ({
            ...snapshot.observedChainPoint,
            providerSource: this.queryIdentities[index],
          })),
        ),
        providerSource: [
          canonicalAfter.providerSource,
          ...this.queryIdentities,
        ].join(","),
        observedAt: new Date().toISOString(),
      } satisfies ChainPoint,
    };
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    return this.authority.currentPoint();
  }

  async currentChainSyncCursor(): Promise<ChainSyncCursor> {
    return this.authority.currentCursor();
  }

  async replayChainSyncEvents(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]> {
    return this.authority.replay(afterSequence);
  }

  async fetchStateQueueReplayCheckpoints(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> {
    // The authority cursor still marks the point the snapshot was read at.
    const snapshotCursor = await this.authority.currentCursor();
    const heldSince = (cursor: ChainSyncCursor) => async () => {
      await this.authority.synchronizeToTip();
      return samePersistedCursor(cursor, await this.authority.currentCursor());
    };
    return replayWithIntegrityOnHeldChain(
      () => this.replayOnEverySurface(anchor, current, tipBlockNo, limit),
      heldSince(snapshotCursor),
      async () => {
        await this.authority.synchronizeToTip();
        const cursor = await this.authority.currentCursor();
        // Without a rollback since the snapshot, its queue is still on the
        // chain, and a retaken replay is watched from here.
        return cursor.rollbackGeneration === snapshotCursor.rollbackGeneration
          ? heldSince(cursor)
          : undefined;
      },
    );
  }

  private async replayOnEverySurface(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> {
    const histories = await Promise.all(
      this.queryProviders.map(async (provider, index) => {
        if (provider.fetchStateQueueReplayCheckpoints === undefined) {
          throw new Error(
            `local-node query surface ${this.queryIdentities[index]!} has no authenticated ordered history source`,
          );
        }
        return provider.fetchStateQueueReplayCheckpoints(
          anchor,
          current,
          tipBlockNo,
          limit,
        );
      }),
    );
    const baseline = canonicalJson(histories[0]);
    for (const [index, history] of histories.entries()) {
      if (canonicalJson(history) !== baseline) {
        throw new L1SourceIntegrityError(
          `local-node state-queue replay disagreement between ${this.queryIdentities[0]!} and ${this.queryIdentities[index]!}`,
        );
      }
    }
    return histories[0]!;
  }

  async loadConsumedChainSyncCursor(): Promise<ChainSyncCursor | undefined> {
    return this.consumerCursorStore.load();
  }

  async acknowledgeChainSyncCursor(cursor: ChainSyncCursor): Promise<void> {
    const current = await this.authority.currentCursor();
    if (!samePersistedCursor(current, cursor)) {
      throw new Error(
        "refusing to acknowledge a stale local-node chain-sync cursor",
      );
    }
    await this.consumerCursorStore.save(cursor);
    // The consumer has replayed every event through this cursor: only what a
    // resumption intersects with is still needed, and the journal stays
    // bounded instead of holding the whole history since it was created.

  }
}

export const stateQueueUtxosToObservedNodes = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>,
): Promise<readonly ObservedStateQueueNode[]> => {
  const observed: ObservedStateQueueNode[] = [];
  for (const stateQueueUtxo of stateQueueUtxos) {
    if (stateQueueUtxo.datum.key === "Empty") {
      continue;
    }
    const stateQueueNode = await Effect.runPromise(
      SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
    );
    const chainPoint = {
      providerSource,
      observedAt: new Date().toISOString(),
      ...(chainPointResolver === undefined
        ? {}
        : declaredChainPoint(await chainPointResolver(stateQueueUtxo.utxo))),
    } satisfies ChainPoint;
    observed.push({
      outRef: outRefLabel(stateQueueUtxo.utxo),
      assetName: stateQueueUtxo.assetName,
      linkedListKey: stateQueueUtxo.datum.key.Key.key,
      rawDatumCbor: SDK.encodeLinkedListNodeView(stateQueueUtxo.datum),
      header: stateQueueNode.header,
      daAttestation: stateQueueNode.da_attestation,
      chainPoint,
    });
  }
  return observed;
};

export const stateQueueUtxosToObservedSnapshot = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>,
): Promise<ObservedStateQueueSnapshot> => {
  const confirmed = stateQueueUtxos[0];
  if (confirmed === undefined || confirmed.datum.key !== "Empty") {
    throw new Error("state queue snapshot has no confirmed root node");
  }
  const [{ data }, nodes] = await Promise.all([
    Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(confirmed.datum),
    ),
    stateQueueUtxosToObservedNodes(
      stateQueueUtxos,
      providerSource,
      chainPointResolver,
    ),
  ]);
  const observedChainPoint = {
    providerSource,
    observedAt: new Date().toISOString(),
    ...(chainPointResolver === undefined
      ? {}
      : declaredChainPoint(await chainPointResolver(confirmed.utxo))),
  } satisfies ChainPoint;
  return {
    nodes,
    confirmedHeaderHash: data.headerHash,
    confirmedStateOutRef: outRefLabel(confirmed.utxo),
    observedChainPoint,
  };
};

export const providerFromConfig = async (
  config: LoadedCommitteeConfig,
): Promise<StateQueueProvider> => {
  const urls = config.cardanoProviderUrls;
  if (config.l1Source.sourceMode === "local_node") {
    const localSource = config.l1Source;
    const authority = localNodeChainAuthorityFromConfig(config);
    const cursorPath = localNodeChainCursorPath(localSource, config.localState);
    const authorityFingerprint = localAuthorityFingerprint(
      config.network,
      localSource.authorityNodeId,
      localSource.chainSyncProviderUrl,
    );
    const queryProviders = await Promise.all(
      urls.map(async (url) =>
        requireStateQueueReplaySource(url, await providerFromUrl(url, config)),
      ),
    );
    const pointAware = queryProviders.map((provider, index) => {
      if (!("currentChainPoint" in provider)) {
        throw new Error(
          `local_node query surface ${index.toString()} cannot prove its current chain point`,
        );
      }
      return provider as ChainPointAwareStateQueueProvider;
    });
    return new LocalNodeStateQueueProvider(
      authority,
      pointAware,
      urls.map(
        (_, index) =>
          `query:${localSource.authorityNodeId}:${index.toString()}`,
      ),
      new FileChainSyncConsumerCursorStore(
        `${cursorPath}.watcher-consumer-v1`,
        authorityFingerprint,
      ),
    );
  }
  if (
    config.cardanoL1Source.sourceMode === "external_providers" &&
    (urls.length < 2 ||
      config.cardanoL1Source.providerAuthorityIds.length !== urls.length)
  ) {
    throw new Error(
      "external_providers mode requires at least two matched provider authority identities",
    );
  }
  const providers = await Promise.all(
    urls.map(async (url, index) =>
      requireStateQueueReplaySource(
        url,
        await providerFromUrl(url, config, index),
      ),
    ),
  );
  return new MultiStateQueueProvider(providers, {
    sourceMode: "external_providers",
    identities: config.l1Source.providers.map(({ identity }) => identity),
  });
};

/**
 * Refuses, at startup, a live state-queue provider that cannot replay the
 * queue's authenticated ordered history: it could read a snapshot but never
 * follow a change to it. Deterministic fixtures, admitted only in explicit
 * L1 test mode, read no live queue. Every live provider `providerFromUrl`
 * builds today has both, so this guards the provider kinds added later.
 */
export const requireStateQueueReplaySource = (
  url: string,
  provider: StateQueueProvider,
): StateQueueProvider => {
  if (
    !url.startsWith("fixture:") &&
    !url.startsWith("file:") &&
    (provider.fetchStateQueueSnapshot === undefined ||
      provider.fetchStateQueueReplayCheckpoints === undefined)
  ) {
    throw new Error(
      `state-queue provider ${url.split(/[#|]/u)[0]!} has no authenticated ordered history source`,
    );
  }
  return provider;
};

const localAuthorityRegistry = new Map<string, LocalNodeChainAuthority>();

export const localNodeChainAuthorityFromConfig = (
  config: LoadedCommitteeConfig,
): LocalNodeChainAuthority => {
  if (config.l1Source.sourceMode !== "local_node") {
    throw new Error(
      "local chain authority is only available in local_node mode",
    );
  }
  const source = config.l1Source;
  const cursorPath = localNodeChainCursorPath(source, config.localState);
  const registryKey = [
    config.network,
    source.authorityNodeId,
    localAuthorityFingerprint(
      config.network,
      source.authorityNodeId,
      source.chainSyncProviderUrl,
    ),
    cursorPath,
  ].join("\u0000");
  const existing = localAuthorityRegistry.get(registryKey);
  if (existing !== undefined) {
    return existing;
  }
  const chainSyncUrl = source.chainSyncProviderUrl.slice("chain-sync:".length);
  let eventSource: ChainSyncEventSource;
  if (chainSyncUrl.startsWith("ogmios:")) {
    eventSource = new OgmiosChainSyncEventSource(
      chainSyncUrl.slice("ogmios:".length),
      config.network,
      source.authorityNodeId,
    );
  } else if (chainSyncUrl.startsWith("kupmios:")) {
    const { ogmiosUrl } = parseKupmiosUrl(chainSyncUrl);
    eventSource = new OgmiosChainSyncEventSource(
      ogmiosUrl,
      config.network,
      source.authorityNodeId,
    );
  } else if (chainSyncUrl.startsWith("fixture:")) {
    eventSource = new FixtureChainSyncEventSource(
      chainSyncUrl.slice("fixture:".length),
      config.network,
      source.authorityNodeId,
    );
  } else if (chainSyncUrl.startsWith("file:")) {
    eventSource = new FixtureChainSyncEventSource(
      new URL(chainSyncUrl).pathname,
      config.network,
      source.authorityNodeId,
    );
  } else {
    throw new Error(`unsupported local-node chain-sync source ${chainSyncUrl}`);
  }
  const authority = new LocalNodeChainAuthority(
    source.authorityNodeId,
    config.network,
    eventSource,
    new FileChainSyncCursorStore(
      cursorPath,
      localAuthorityFingerprint(
        config.network,
        source.authorityNodeId,
        source.chainSyncProviderUrl,
      ),
    ),
  );
  localAuthorityRegistry.set(registryKey, authority);
  return authority;
};

const localNodeChainCursorPath = (
  source: Extract<
    LoadedCommitteeConfig["l1Source"],
    { readonly sourceMode: "local_node" }
  >,
  localState: LoadedCommitteeConfig["localState"],
): string => {
  const cursorPath =
    source.chainSyncCursorPath ??
    (localState.kind === "file"
      ? `${localState.path}.chain-sync-cursor`
      : undefined);
  if (cursorPath === undefined) {
    throw new Error(
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH is required for durable local-node chain sync",
    );
  }
  return cursorPath;
};

export const localAuthorityFingerprint = (
  network: string,
  authorityNodeId: string,
  chainSyncProviderUrl: string,
): string => {
  const source = chainSyncProviderUrl.slice("chain-sync:".length);
  let canonicalSource: string;
  if (source.startsWith("ogmios:")) {
    canonicalSource = `ogmios:${normalizeAuthorityEndpoint(source.slice("ogmios:".length))}`;
  } else if (source.startsWith("kupmios:")) {
    canonicalSource = `ogmios:${normalizeAuthorityEndpoint(parseKupmiosUrl(source).ogmiosUrl)}`;
  } else if (source.startsWith("fixture:")) {
    canonicalSource = `fixture:${resolve(source.slice("fixture:".length))}`;
  } else if (source.startsWith("file:")) {
    canonicalSource = `fixture:${resolve(new URL(source).pathname)}`;
  } else {
    throw new Error("unsupported local chain authority source");
  }
  return createHash("sha256")
    .update(
      canonicalJson({
        network,
        authorityNodeId,
        canonicalSource,
      }),
    )
    .digest("hex");
};

export const providerFromUrl = async (
  url: string,
  config: Pick<
    LoadedCommitteeConfig,
    "network" | "cardanoL1Source" | "stateQueueAddress" | "stateQueuePolicyId"
  > & {
    readonly deploymentFingerprint?: string;
    readonly finalityDepth?: number;
    readonly hubOraclePolicyId?: string;
    readonly correctionLockAddress?: string;
    readonly fraudProofPolicyId?: string;
    readonly fraudProofAddress?: string;
  },
  providerIndex = 0,
): Promise<StateQueueProvider> => {
  if (url.startsWith("fixture:")) {
    return new FixtureStateQueueProvider(
      url.slice("fixture:".length),
      config.network,
    );
  }
  if (url.startsWith("file:")) {
    return new FixtureStateQueueProvider(new URL(url).pathname, config.network);
  }
  if (url.startsWith("blockfrost:")) {
    // Blockfrost serves no authenticated ordered state-queue history, so a
    // committee reading the queue through it could never follow a change.
    throw new Error(
      "blockfrost: cannot serve the state queue: it has no authenticated ordered history source; use kupmios:<kupo-url>|<ogmios-url>",
    );
  }
  if (url.startsWith("kupmios:")) {
    if (config.deploymentFingerprint === undefined) {
      throw new Error(
        "Kupmios state-queue replay requires a deployment fingerprint",
      );
    }
    if (config.finalityDepth === undefined) {
      throw new Error("Kupmios state-queue replay requires the finality depth");
    }
    if (
      config.hubOraclePolicyId === undefined ||
      config.correctionLockAddress === undefined ||
      config.fraudProofPolicyId === undefined ||
      config.fraudProofAddress === undefined
    ) {
      throw new Error(
        "Kupmios state-queue replay requires deployment-bound CorrectionLock and fraud-proof identities",
      );
    }
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    await assertOgmiosNetworkMagic(
      ogmiosUrl,
      config.cardanoL1Source.networkMagic,
    );
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl, headers),
      normalizeNetwork(config.network),
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: l1AuthorityProviderSource(
        config,
        providerIndex,
        `kupmios:${kupoUrl}|${ogmiosUrl}`,
      ),
      chainPointResolver: kupmiosChainPointResolver(
        lucid,
        kupoUrl,
        fetch,
        ogmiosUrl,
        config.network,
        Math.max(1, config.finalityDepth),
      ),
      currentChainPointResolver: kupmiosCurrentChainPointResolver(
        config.network,
        kupoUrl,
        ogmiosUrl,
      ),
      tipBlockNoResolver: () => fetchOgmiosTipBlockNo(ogmiosUrl, fetch),
      chainPointHeldResolver: (point) =>
        kupoHoldsChainPoint(kupoUrl, point, fetch),
      replayCheckpoints: createLocalKupmiosStateQueueReplayProvider({
        deploymentIdentityDigest: config.deploymentFingerprint,
        stateQueuePolicyId: config.stateQueuePolicyId,
        stateQueueAddress: config.stateQueueAddress,
        hubOraclePolicyId: config.hubOraclePolicyId,
        correctionLockAddress: config.correctionLockAddress,
        fraudProofPolicyId: config.fraudProofPolicyId,
        fraudProofAddress: config.fraudProofAddress,
        kupoUrl,
        ogmiosUrl,
      }),
    });
  }
  throw new Error(
    `unsupported CARDANO_PROVIDER_URLS entry ${url}; supported forms are fixture:<path>, file:<path>, and kupmios:<kupo-url>|<ogmios-url>`,
  );
};

export const l1AuthorityProviderSource = (
  config: Pick<LoadedCommitteeConfig, "cardanoL1Source">,
  providerIndex: number,
  surfaceIdentity: string,
): string => {
  const authority = config.cardanoL1Source;
  const surfaceDigest = createHash("sha256")
    .update(surfaceIdentity)
    .digest("hex");
  if (authority.sourceMode === "local_node") {
    return [
      "local_node",
      authority.authorityNodeId,
      authority.authorityDigest,
      surfaceDigest,
    ].join(":");
  }
  const providerAuthorityId = authority.providerAuthorityIds[providerIndex];
  if (providerAuthorityId === undefined) {
    throw new Error(
      "external provider is missing its configured authority identity",
    );
  }
  return [
    "external_providers",
    providerAuthorityId,
    authority.authorityDigest,
    surfaceDigest,
  ].join(":");
};

export const parseBlockfrostUrl = (
  value: string,
): { readonly apiUrl: string; readonly projectId: string } => {
  const raw = value.slice("blockfrost:".length);
  const hashIndex = raw.lastIndexOf("#");
  if (hashIndex <= 0 || hashIndex === raw.length - 1) {
    throw new Error(
      "blockfrost provider URL must be blockfrost:<api-url>#<project-id>",
    );
  }
  return {
    apiUrl: raw.slice(0, hashIndex),
    projectId: raw.slice(hashIndex + 1),
  };
};

export const parseKupmiosUrl = (
  value: string,
): {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly headers?: Record<string, string>;
} => {
  const raw = value.slice("kupmios:".length);
  const [kupoUrl, ogmiosUrl] = raw.split("|");
  if (kupoUrl === undefined || ogmiosUrl === undefined) {
    throw new Error(
      "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
    );
  }
  return { kupoUrl, ogmiosUrl };
};

export const assertOgmiosNetworkMagic = async (
  ogmiosUrl: string,
  expectedNetworkMagic: number,
  fetchFn: typeof fetch = fetch,
): Promise<void> => {
  const endpoint = ogmiosHttpEndpoint(ogmiosUrl);
  let response: Response;
  try {
    response = await fetchFn(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryNetwork/genesisConfiguration",
        params: { era: "shelley" },
        id: "midgard-network-magic-preflight",
      }),
      signal: AbortSignal.timeout(10_000),
    });
  } catch {
    throw new Error("Ogmios network-magic preflight failed");
  }
  if (!response.ok) {
    throw new Error("Ogmios network-magic preflight failed");
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    throw new Error("Ogmios network-magic preflight returned invalid JSON");
  }
  const actualNetworkMagic = ogmiosNetworkMagic(body);
  if (actualNetworkMagic !== expectedNetworkMagic) {
    throw new Error(
      "Ogmios network magic does not match configured Cardano network authority",
    );
  }
};

const ogmiosHttpEndpoint = (ogmiosUrl: string): string => {
  let parsed: URL;
  try {
    parsed = new URL(ogmiosUrl);
  } catch {
    throw new Error("Kupmios Ogmios URL is invalid");
  }
  if (parsed.protocol === "ws:") {
    parsed.protocol = "http:";
  } else if (parsed.protocol === "wss:") {
    parsed.protocol = "https:";
  } else if (parsed.protocol !== "http:" && parsed.protocol !== "https:") {
    throw new Error("Kupmios Ogmios URL must use http, https, ws, or wss");
  }
  return parsed.toString();
};

const ogmiosNetworkMagic = (body: unknown): number => {
  if (typeof body !== "object" || body === null || Array.isArray(body)) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  const result = (body as Record<string, unknown>).result;
  if (typeof result !== "object" || result === null || Array.isArray(result)) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  const networkMagic = (result as Record<string, unknown>).networkMagic;
  if (
    !Number.isSafeInteger(networkMagic) ||
    (networkMagic as number) < 0 ||
    (networkMagic as number) > 4_294_967_295
  ) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  return networkMagic as number;
};

const normalizeAuthorityEndpoint = (value: string): string => {
  const endpoint = new URL(value);
  if (endpoint.username !== "" || endpoint.password !== "") {
    throw new Error("local authority endpoint must not embed credentials");
  }
  const protocol =
    endpoint.protocol === "wss:"
      ? "https:"
      : endpoint.protocol === "ws:"
        ? "http:"
        : endpoint.protocol;
  if (protocol !== "http:" && protocol !== "https:") {
    throw new Error("local authority endpoint must use HTTP(S) or WS(S)");
  }
  const port =
    (protocol === "http:" && endpoint.port === "80") ||
    (protocol === "https:" && endpoint.port === "443")
      ? ""
      : endpoint.port;
  const path = endpoint.pathname.replace(/\/+$/u, "");
  const hostname = endpoint.hostname.toLowerCase().replace(/\.$/u, "");
  return `${protocol}//${hostname}${port === "" ? "" : `:${port}`}${path}`;
};

export const lucidChainPointResolver = (
  lucid: LucidEvolution,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  return async (utxo) => {
    const status = getRecord(
      (await lucid.transactionStatus(utxo.txHash)) as unknown,
      "Cardano transaction status",
    );
    if (status.status !== "confirmed") {
      throw new Error(
        `state-queue transaction ${utxo.txHash} is not confirmed: ${String(status.status)}`,
      );
    }
    const confirmation = getRecord(
      status.confirmation,
      "confirmed Cardano transaction provenance",
    );
    const slot =
      confirmation.slot === undefined
        ? undefined
        : safeSlot(confirmation.slot, "transaction inclusion slot");
    const blockHash =
      confirmation.blockHash === undefined
        ? undefined
        : safeBlockHash(
            confirmation.blockHash,
            "transaction inclusion block hash",
          );
    const blockHeight =
      confirmation.blockHeight === undefined
        ? undefined
        : safeSlot(
            confirmation.blockHeight,
            "transaction inclusion block height",
          );
    const confirmations =
      confirmation.confirmations === undefined
        ? undefined
        : safeSlot(
            confirmation.confirmations,
            "transaction confirmation count",
          );
    // Lucid counts the inclusion block; Midgard depth counts descendants.
    return {
      ...(slot === undefined ? {} : { slot }),
      ...(blockHash === undefined ? {} : { blockHash }),
      ...(blockHeight === undefined ? {} : { blockHeight }),
      ...(confirmations === undefined
        ? {}
        : { depth: Math.max(0, confirmations - 1) }),
    };
  };
};

export const kupmiosChainPointResolver = (
  lucid: LucidEvolution,
  _kupoUrl: string,
  _fetchFn: typeof fetch = fetch,
  ogmiosUrl?: string,
  network?: string,
  requiredDepth = 2160,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  const resolveInclusion = lucidChainPointResolver(lucid);
  return async (utxo) => {
    const inclusion = await resolveInclusion(utxo);
    if (inclusion.depth !== undefined) {
      return inclusion;
    }
    if (
      ogmiosUrl === undefined ||
      network === undefined ||
      inclusion.slot === undefined ||
      inclusion.blockHash === undefined
    ) {
      // Empty slots are not confirmations. Keep depth unknown unless the
      // aligned node can count actual descendant blocks.
      return inclusion;
    }
    const before = await alignedKupmiosTip(
      network,
      _kupoUrl,
      ogmiosUrl,
      _fetchFn,
    );
    const depth = await requestOgmiosDescendantDepth({
      ogmiosUrl,
      network,
      inclusion: {
        network,
        slot: inclusion.slot,
        blockHash: inclusion.blockHash,
        providerSource: `kupmios:${_kupoUrl}|${ogmiosUrl}`,
        observedAt: new Date().toISOString(),
      },
      expectedTip: before,
      requiredDepth,
    });
    const after = await alignedKupmiosTip(
      network,
      _kupoUrl,
      ogmiosUrl,
      _fetchFn,
    );
    if (!sameCanonicalPoint(before, after)) {
      throw new ChainMovedDuringSnapshotError(
        "Kupmios chain point changed while deriving block confirmations",
      );
    }
    return { ...inclusion, depth };
  };
};

type KupoCheckpoint = {
  readonly slot: number;
  readonly blockHash: string;
};

export const fetchKupoCheckpoint = async (
  kupoUrl: string,
  fetchFn: typeof fetch,
): Promise<KupoCheckpoint> => {
  const response = await fetchFn(`${kupoUrl.replace(/\/+$/, "")}/health`, {
    headers: { accept: "text/plain" },
  });
  if (!response.ok) {
    throw new Error(
      `Kupo health lookup failed: ${response.status.toString()} ${await response.text()}`,
    );
  }
  const body = await response.text();
  const match =
    body.match(/^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/mu) ??
    body.match(/^kupo_most_recent_node_tip\s+([0-9]+(?:\.[0-9]+)?)/mu);
  if (match === null) {
    throw new Error("Kupo health omitted its current checkpoint slot");
  }
  const slot = Number(match[1]);
  if (!Number.isSafeInteger(slot) || slot < 0) {
    throw new Error("Kupo health returned an invalid checkpoint slot");
  }
  const rawEtag = response.headers.get("etag");
  const blockHash = rawEtag
    ?.replace(/^W\//u, "")
    .replace(/^"|"$/gu, "")
    .toLowerCase();
  return {
    slot,
    blockHash: safeBlockHash(blockHash, "Kupo checkpoint ETag"),
  };
};

export const blockfrostCurrentChainPointResolver =
  (network: string, apiUrl: string, projectId: string) =>
  async (): Promise<CanonicalChainPoint> => {
    const [latest, liveNetwork] = await Promise.all([
      blockfrostJson(
        apiUrl,
        projectId,
        "/blocks/latest",
        parseBlockfrostLatestBlock,
      ),
      blockfrostJson(apiUrl, projectId, "/genesis", parseBlockfrostNetwork),
    ]);
    assertNetworkMagic(network, liveNetwork.networkMagic, "Blockfrost");
    return {
      network,
      slot: latest.slot,
      blockHash: latest.hash,
      blockHeight: latest.height,
      providerSource: `blockfrost:${apiUrl}`,
      observedAt: new Date().toISOString(),
    };
  };

export const kupmiosCurrentChainPointResolver =
  (network: string, kupoUrl: string, ogmiosUrl: string) =>
  async (): Promise<CanonicalChainPoint> =>
    alignedKupmiosTip(network, kupoUrl, ogmiosUrl, fetch);

/**
 * Kupo indexes each block shortly after the node adopts it, so one read of
 * both tips can straddle a block arrival. Re-read briefly until they agree;
 * surfaces that still disagree after the window are not following one chain.
 */
export const KUPMIOS_TIP_ALIGNMENT_ATTEMPTS = 8;
export const KUPMIOS_TIP_ALIGNMENT_RETRY_MS = 250;

const alignedKupmiosTip = async (
  network: string,
  kupoUrl: string,
  ogmiosUrl: string,
  fetchFn: typeof fetch,
): Promise<CanonicalChainPoint> => {
  for (let attempt = 1; ; attempt += 1) {
    const [kupoPoint, ogmiosTip] = await Promise.all([
      fetchKupoCheckpoint(kupoUrl, fetchFn),
      requestOgmiosTip(ogmiosUrl),
    ]);
    assertNetworkMagic(network, ogmiosTip.networkMagic, "Ogmios");
    if (
      kupoPoint.slot === ogmiosTip.slot &&
      kupoPoint.blockHash === ogmiosTip.blockHash
    ) {
      return alignedTipPoint(network, kupoUrl, ogmiosUrl, ogmiosTip);
    }
    if (attempt >= KUPMIOS_TIP_ALIGNMENT_ATTEMPTS) {
      throw new Error(
        `Kupmios query surfaces are not aligned after ${attempt.toString()} reads: Kupo=${kupoPoint.slot.toString()}:${kupoPoint.blockHash}, Ogmios=${ogmiosTip.slot.toString()}:${ogmiosTip.blockHash}`,
      );
    }
    await new Promise((resolve) =>
      setTimeout(resolve, KUPMIOS_TIP_ALIGNMENT_RETRY_MS),
    );
  }
};

const alignedTipPoint = (
  network: string,
  kupoUrl: string,
  ogmiosUrl: string,
  ogmiosTip: Awaited<ReturnType<typeof requestOgmiosTip>>,
): CanonicalChainPoint => {
  return {
    network,
    slot: ogmiosTip.slot,
    blockHash: ogmiosTip.blockHash,
    ...(ogmiosTip.blockHeight === undefined
      ? {}
      : { blockHeight: ogmiosTip.blockHeight }),
    providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
    observedAt: new Date().toISOString(),
  };
};

const blockfrostJson = async <T>(
  apiUrl: string,
  projectId: string,
  path: string,
  parse: (value: unknown) => T,
): Promise<T> => {
  const response = await fetch(`${apiUrl.replace(/\/$/, "")}${path}`, {
    headers: { project_id: projectId },
  });
  if (!response.ok) {
    throw new Error(
      `Blockfrost ${path} returned ${response.status.toString()} ${response.statusText}`,
    );
  }
  return parse(await response.json());
};

type BlockfrostLatestBlock = {
  readonly slot: number;
  readonly hash: string;
  readonly height: number;
};

const parseBlockfrostLatestBlock = (value: unknown): BlockfrostLatestBlock => {
  const block = getRecord(value, "Blockfrost latest block");
  return {
    slot: safeSlot(block.slot, "Blockfrost latest block slot"),
    hash: safeBlockHash(block.hash, "Blockfrost latest block hash"),
    height: safeSlot(block.height, "Blockfrost latest block height"),
  };
};

const parseBlockfrostNetwork = (
  value: unknown,
): { readonly networkMagic: number } => {
  const result = getRecord(value, "Blockfrost genesis");
  return {
    networkMagic: safeSlot(
      result.network_magic,
      "Blockfrost genesis network magic",
    ),
  };
};

const sortObservedNodes = (
  nodes: readonly ObservedStateQueueNode[],
): readonly ObservedStateQueueNode[] =>
  [...nodes].sort((left, right) =>
    canonicalObservedNode(left).localeCompare(canonicalObservedNode(right)),
  );

const canonicalObservedNodes = (
  nodes: readonly ObservedStateQueueNode[],
): readonly string[] => nodes.map(canonicalObservedNode);

const canonicalObservedNode = (node: ObservedStateQueueNode): string =>
  canonicalJson({
    outRef: node.outRef,
    assetName: node.assetName,
    linkedListKey: node.linkedListKey,
    rawDatumCbor: node.rawDatumCbor ?? null,
    header: node.header,
    daAttestation: node.daAttestation,
    chainPoint: {
      slot: node.chainPoint.slot ?? null,
      blockHash: node.chainPoint.blockHash ?? null,
      blockHeight: node.chainPoint.blockHeight ?? null,
    },
  });

const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const assertCompatibleChainPoints = (
  sortedResults: readonly (readonly ObservedStateQueueNode[])[],
): void => {
  const baseline = sortedResults[0] ?? [];
  for (const [providerIndex, nodes] of sortedResults.entries()) {
    for (const [nodeIndex, node] of nodes.entries()) {
      const expected = baseline[nodeIndex]?.chainPoint;
      if (
        expected !== undefined &&
        !compatibleChainPoint(expected, node.chainPoint)
      ) {
        throw new L1SourceIntegrityError(
          `state queue provider chain-point disagreement between provider 0 and provider ${providerIndex.toString()}`,
        );
      }
    }
  }
};

const compatibleChainPoint = (left: ChainPoint, right: ChainPoint): boolean =>
  (
    [
      ["slot", left.slot, right.slot],
      ["blockHash", left.blockHash, right.blockHash],
      ["blockHeight", left.blockHeight, right.blockHeight],
    ] as const
  ).every(
    ([, leftValue, rightValue]) =>
      leftValue === undefined ||
      rightValue === undefined ||
      leftValue === rightValue,
  );

const mergeAgreedObservedNodes = (
  sortedResults: readonly (readonly ObservedStateQueueNode[])[],
  identities?: readonly string[],
): readonly ObservedStateQueueNode[] =>
  sortedResults[0]!.map((node, index) => ({
    ...node,
    chainPoint: mergeChainPoints(
      sortedResults.map((nodes, providerIndex) => ({
        ...nodes[index]!.chainPoint,
        providerSource:
          identities?.[providerIndex] ??
          nodes[index]!.chainPoint.providerSource,
      })),
    ),
  }));

/**
 * Copies exactly the fields `ChainPoint` declares, leaving out undefined ones.
 * A point typed as a wider type, such as a `CanonicalChainPoint`, still
 * satisfies `ChainPoint`, so spreading it would carry fields the stored
 * records' exact-keys parser rejects.
 */
const declaredChainPoint = (point: ChainPoint): ChainPoint => {
  const declared: ChainPoint = {
    slot: point.slot,
    blockHash: point.blockHash,
    blockHeight: point.blockHeight,
    observedAt: point.observedAt,
    depth: point.depth,
    finalized: point.finalized,
    providerSource: point.providerSource,
  };
  return Object.fromEntries(
    Object.entries(declared).filter(([, value]) => value !== undefined),
  );
};

const mergeChainPoints = (points: readonly ChainPoint[]): ChainPoint => {
  const primary = declaredChainPoint(points[0] ?? {});
  const sources = points
    .map((point) => point.providerSource)
    .filter((source): source is string => source !== undefined);
  const depths = points
    .map((point) => point.depth)
    .filter((depth): depth is number => depth !== undefined);
  const allDepthsKnown = depths.length === points.length;
  const finalized =
    points.length > 0 && points.every((point) => point.finalized === true)
      ? true
      : points.some((point) => point.finalized === false)
        ? false
        : undefined;
  return {
    ...primary,
    providerSource:
      sources.length === 0 ? primary.providerSource : sources.join(","),
    observedAt: new Date().toISOString(),
    depth: allDepthsKnown ? Math.min(...depths) : undefined,
    finalized,
  } satisfies ChainPoint;
};

type OgmiosChainSyncRequest = (
  ogmiosUrl: string,
  cursor: CanonicalChainPoint | undefined,
  intersectionCandidates: readonly CanonicalChainPoint[] | undefined,
  network: string,
  authorityNodeId: string,
) => Promise<ChainSyncEventBatch>;

type RuntimeWebSocket = {
  onopen: ((event: unknown) => void) | null;
  onmessage: ((event: { readonly data: unknown }) => void) | null;
  onerror: ((event: unknown) => void) | null;
  onclose: ((event: unknown) => void) | null;
  send(data: string): void;
  close(): void;
};

type RuntimeWebSocketConstructor = new (url: string) => RuntimeWebSocket;

class OgmiosRpcSession {
  private requestId = 0;
  private pending:
    | {
        readonly id: string;
        readonly resolve: (value: unknown) => void;
        readonly reject: (error: Error) => void;
        readonly timeout: ReturnType<typeof setTimeout>;
      }
    | undefined;
  private closed = false;

  private constructor(private readonly socket: RuntimeWebSocket) {
    socket.onmessage = ({ data }) => {
      const pending = this.pending;
      if (pending === undefined) {
        this.fail(new Error("Ogmios sent an unsolicited JSON-RPC response"));
        return;
      }
      try {
        if (typeof data !== "string") {
          throw new Error("Ogmios returned a non-text WebSocket message");
        }
        const envelope = getRecord(
          JSON.parse(data) as unknown,
          "Ogmios JSON-RPC response",
        );
        if (envelope.id !== pending.id) {
          throw new Error(
            `Ogmios JSON-RPC response id ${String(envelope.id)} does not match ${pending.id}`,
          );
        }
        if (envelope.error !== undefined) {
          throw new Error(
            `Ogmios JSON-RPC error: ${JSON.stringify(envelope.error)}`,
          );
        }
        clearTimeout(pending.timeout);
        this.pending = undefined;
        pending.resolve(envelope.result);
      } catch (error) {
        this.fail(error instanceof Error ? error : new Error(String(error)));
      }
    };
    socket.onerror = () => {
      this.fail(new Error("Ogmios WebSocket failed"));
    };
    socket.onclose = () => {
      if (!this.closed) {
        this.fail(
          new Error("Ogmios WebSocket closed while chain sync was active"),
        );
      }
    };
  }

  static async open(ogmiosUrl: string): Promise<OgmiosRpcSession> {
    const constructor = (
      globalThis as unknown as {
        readonly WebSocket?: RuntimeWebSocketConstructor;
      }
    ).WebSocket;
    if (constructor === undefined) {
      throw new Error("Node.js WebSocket support is required for Ogmios");
    }
    const socketUrl = ogmiosWebSocketUrl(ogmiosUrl);
    const socket = new constructor(socketUrl.toString());
    await new Promise<void>((resolveOpen, rejectOpen) => {
      const timeout = setTimeout(() => {
        socket.close();
        rejectOpen(new Error("Ogmios WebSocket connection timed out"));
      }, 15_000);
      socket.onopen = () => {
        clearTimeout(timeout);
        resolveOpen();
      };
      socket.onerror = () => {
        clearTimeout(timeout);
        rejectOpen(
          new Error(`Ogmios WebSocket failed for ${socketUrl.origin}`),
        );
      };
      socket.onclose = () => {
        clearTimeout(timeout);
        rejectOpen(new Error("Ogmios WebSocket closed before opening"));
      };
    });
    return new OgmiosRpcSession(socket);
  }

  async request(
    method: string,
    params: Record<string, unknown>,
  ): Promise<unknown> {
    if (this.closed) {
      throw new Error("Ogmios JSON-RPC session is closed");
    }
    if (this.pending !== undefined) {
      throw new Error("Ogmios JSON-RPC session already has an active request");
    }
    const id = `midgard-${this.requestId.toString()}`;
    this.requestId += 1;
    return new Promise((resolveRequest, rejectRequest) => {
      const timeout = setTimeout(() => {
        this.fail(new Error(`Ogmios ${method} request timed out`));
      }, 15_000);
      this.pending = {
        id,
        resolve: resolveRequest,
        reject: rejectRequest,
        timeout,
      };
      this.socket.send(JSON.stringify({ jsonrpc: "2.0", id, method, params }));
    });
  }

  close(): void {
    if (!this.closed) {
      this.closed = true;
      const pending = this.pending;
      this.pending = undefined;
      if (pending !== undefined) {
        clearTimeout(pending.timeout);
        pending.reject(new Error("Ogmios JSON-RPC session closed"));
      }
      this.socket.close();
    }
  }

  private fail(error: Error): void {
    const pending = this.pending;
    this.pending = undefined;
    if (pending !== undefined) {
      clearTimeout(pending.timeout);
      pending.reject(error);
    }
    if (!this.closed) {
      this.closed = true;
      this.socket.close();
    }
  }
}

const ogmiosWebSocketUrl = (ogmiosUrl: string): URL => {
  const socketUrl = new URL(ogmiosUrl);
  if (socketUrl.protocol === "http:") {
    socketUrl.protocol = "ws:";
  } else if (socketUrl.protocol === "https:") {
    socketUrl.protocol = "wss:";
  } else if (socketUrl.protocol !== "ws:" && socketUrl.protocol !== "wss:") {
    throw new Error("Ogmios chain-sync endpoint must use HTTP(S) or WS(S)");
  }
  return socketUrl;
};

const createOgmiosChainSyncRequest = (): OgmiosChainSyncRequest => {
  let session: OgmiosRpcSession | undefined;
  let sessionUrl: string | undefined;
  let intersection: CanonicalChainPoint | undefined;
  // The caller position this session is synchronized with: the cursor it
  // intersected from, then each event point it hands back. A caller that did
  // not record a delivered event (a failed durable append, say) passes an older
  // cursor; continuing the session would then skip the events it lost.
  let delivered: CanonicalChainPoint | undefined;
  let pendingRollback: ChainSyncEventBatch | undefined;
  let suppressHandshakeRollback = false;
  // The node tip the session last reported. Once the session has delivered it,
  // the caller is at the tip, and Ogmios answers a nextBlock there only when
  // the node adopts another block.
  let reportedTip: CanonicalChainPoint | undefined;

  const disconnect = (): void => {
    session?.close();
    session = undefined;
    sessionUrl = undefined;
    intersection = undefined;
    delivered = undefined;
    pendingRollback = undefined;
    suppressHandshakeRollback = false;
    reportedTip = undefined;
  };

  const deliver = (batch: ChainSyncEventBatch): ChainSyncEventBatch => {
    reportedTip = batch.tip;
    if (batch.event !== undefined) {
      delivered = batch.event.point;
    }
    return batch;
  };

  return async (
    ogmiosUrl,
    cursor,
    intersectionCandidates,
    network,
    authorityNodeId,
  ) => {
    const source = `chain-sync:${authorityNodeId}`;
    for (let attempt = 0; attempt < 2; attempt += 1) {
      try {
        if (
          session === undefined ||
          sessionUrl !== ogmiosUrl ||
          cursor === undefined ||
          delivered === undefined ||
          !sameCanonicalPoint(cursor, delivered)
        ) {
          // Only a caller at the session's delivered point may continue it;
          // any other cursor re-intersects so no event is skipped or repeated.
          disconnect();
          session = await OgmiosRpcSession.open(ogmiosUrl);
          sessionUrl = ogmiosUrl;
          const genesis = getRecord(
            await session.request("queryNetwork/genesisConfiguration", {
              era: "shelley",
            }),
            "Ogmios genesis configuration",
          );
          assertNetworkMagic(
            network,
            safeSlot(
              genesis.networkMagic ?? genesis.network_magic,
              "Ogmios network magic",
            ),
            "Ogmios",
          );
          const bootstrapTip =
            cursor === undefined
              ? parseOgmiosPoint(
                  await session.request("queryNetwork/tip", {}),
                  network,
                  source,
                  "Ogmios bootstrap tip",
                )
              : undefined;
          const durableCandidates =
            cursor === undefined
              ? []
              : [
                  cursor,
                  ...(intersectionCandidates ?? []).filter(
                    (point) => !sameCanonicalPoint(point, cursor),
                  ),
                ].slice(0, CHAIN_SYNC_INTERSECTION_POINTS);
          const found = getRecord(
            await session.request("findIntersection", {
              points:
                cursor === undefined
                  ? [
                      {
                        slot: bootstrapTip!.slot,
                        id: bootstrapTip!.blockHash,
                      },
                      "origin",
                    ]
                  : [
                      ...durableCandidates.map((point) => ({
                        slot: point.slot,
                        id: point.blockHash,
                      })),
                      "origin",
                    ],
            }),
            "findIntersection result",
          );
          const tip = parseOgmiosPoint(
            found.tip,
            network,
            source,
            "findIntersection tip",
          );
          intersection = parseOgmiosPointOrOrigin(
            found.intersection,
            network,
            source,
            "findIntersection intersection",
          );
          delivered = cursor;
          suppressHandshakeRollback = true;
          if (cursor === undefined) {
            if (
              bootstrapTip === undefined ||
              intersection === undefined ||
              !sameCanonicalPoint(intersection, bootstrapTip)
            ) {
              throw new Error(
                "Ogmios bootstrap tip left the canonical chain before intersection; retrying from a fresh node-derived tip",
              );
            }
            return deliver({
              event: { direction: "roll_forward", point: bootstrapTip },
              tip,
            });
          }
          if (cursor !== undefined && intersection === undefined) {
            throw new L1SourceIntegrityError(
              "Ogmios rolled the durable chain-sync cursor back to origin; explicit state reset is required",
            );
          }
          if (
            cursor !== undefined &&
            intersection !== undefined &&
            !sameCanonicalPoint(intersection, cursor)
          ) {
            pendingRollback = {
              event: { direction: "roll_backward", point: intersection },
              tip,
            };
          }
          if (pendingRollback !== undefined) {
            const result = pendingRollback;
            pendingRollback = undefined;
            return deliver(result);
          }
          if (cursor !== undefined && sameCanonicalPoint(cursor, tip)) {
            return deliver({ tip });
          }
        } else if (
          reportedTip !== undefined &&
          sameCanonicalPoint(cursor, reportedTip)
        ) {
          // At the tip a nextBlock waits for the node's next block, up to the
          // request timeout, and the timeout then drops the session. Asking
          // for the tip first answers at once: while it is still the caller's
          // point there is nothing to deliver. Anything the node did since,
          // rollbacks included, stays queued on the session in order for the
          // nextBlock below or a later call.
          const tip = parseOgmiosPoint(
            await session.request("queryNetwork/tip", {}),
            network,
            source,
            "Ogmios tip",
          );
          if (sameCanonicalPoint(cursor, tip)) {
            return deliver({ tip });
          }
        }

        // Ogmios may echo the negotiated intersection as the first backward
        // response. It is a handshake acknowledgement, not a second rollback.
        for (
          let handshakeResponses = 0;
          handshakeResponses < 2;
          handshakeResponses += 1
        ) {
          const nextResult = getRecord(
            await session.request("nextBlock", {}),
            "nextBlock result",
          );
          const direction = nextResult.direction;
          const tip = parseOgmiosPoint(
            nextResult.tip,
            network,
            source,
            "nextBlock tip",
          );
          if (direction === "forward") {
            suppressHandshakeRollback = false;
            const block = getRecord(nextResult.block, "nextBlock block");
            return deliver({
              event: {
                direction: "roll_forward",
                point: parseOgmiosPoint(
                  block,
                  network,
                  source,
                  "roll-forward block",
                ),
              },
              tip,
            });
          }
          if (direction === "backward") {
            const point = parseOgmiosPointOrOrigin(
              nextResult.point,
              network,
              source,
              "roll-backward point",
            );
            if (
              suppressHandshakeRollback &&
              point === undefined &&
              intersection === undefined &&
              cursor === undefined
            ) {
              suppressHandshakeRollback = false;
              continue;
            }
            if (point === undefined) {
              throw new L1SourceIntegrityError(
                "Ogmios rolled chain sync back to origin; explicit state reset is required",
              );
            }
            if (
              suppressHandshakeRollback &&
              intersection !== undefined &&
              sameCanonicalPoint(point, intersection)
            ) {
              suppressHandshakeRollback = false;
              if (cursor !== undefined && sameCanonicalPoint(cursor, tip)) {
                return deliver({ tip });
              }
              continue;
            }
            suppressHandshakeRollback = false;
            return deliver({
              event: { direction: "roll_backward", point },
              tip,
            });
          }
          throw new Error("Ogmios nextBlock returned an unsupported direction");
        }
        throw new Error(
          "Ogmios repeated its chain-sync handshake rollback response",
        );
      } catch (error) {
        disconnect();
        if (attempt === 1) {
          throw error;
        }
      }
    }
    throw new Error("Ogmios chain-sync reconnect exhausted");
  };
};

const requestOgmiosDescendantDepth = async ({
  ogmiosUrl,
  network,
  inclusion,
  expectedTip,
  requiredDepth,
}: {
  readonly ogmiosUrl: string;
  readonly network: string;
  readonly inclusion: CanonicalChainPoint;
  readonly expectedTip: CanonicalChainPoint;
  readonly requiredDepth: number;
}): Promise<number> => {
  const source = `confirmation-depth:${ogmiosUrl}`;
  const session = await OgmiosRpcSession.open(ogmiosUrl);
  try {
    const genesis = getRecord(
      await session.request("queryNetwork/genesisConfiguration", {
        era: "shelley",
      }),
      "Ogmios genesis configuration",
    );
    assertNetworkMagic(
      network,
      safeSlot(
        genesis.networkMagic ?? genesis.network_magic,
        "Ogmios network magic",
      ),
      "Ogmios",
    );
    const found = getRecord(
      await session.request("findIntersection", {
        points: [{ slot: inclusion.slot, id: inclusion.blockHash }, "origin"],
      }),
      "confirmation-depth findIntersection result",
    );
    const intersection = parseOgmiosPointOrOrigin(
      found.intersection,
      network,
      source,
      "confirmation-depth intersection",
    );
    const tip = parseOgmiosPoint(
      found.tip,
      network,
      source,
      "confirmation-depth tip",
    );
    if (
      intersection === undefined ||
      !sameCanonicalPoint(intersection, inclusion)
    ) {
      throw new ChainMovedDuringSnapshotError(
        "state-queue inclusion point is not on the canonical local-node chain",
      );
    }
    if (!sameCanonicalPoint(tip, expectedTip)) {
      throw new ChainMovedDuringSnapshotError(
        "local-node tip changed before confirmation depth derivation",
      );
    }
    if (sameCanonicalPoint(inclusion, expectedTip)) {
      return 0;
    }
    let depth = 0;
    let suppressIntersection = true;
    while (depth < requiredDepth) {
      const next = getRecord(
        await session.request("nextBlock", {}),
        "confirmation-depth nextBlock result",
      );
      const responseTip = parseOgmiosPoint(
        next.tip,
        network,
        source,
        "confirmation-depth response tip",
      );
      if (!sameCanonicalPoint(responseTip, expectedTip)) {
        throw new ChainMovedDuringSnapshotError(
          "local-node tip changed while deriving confirmation depth",
        );
      }
      if (next.direction === "backward") {
        const point = parseOgmiosPointOrOrigin(
          next.point,
          network,
          source,
          "confirmation-depth rollback point",
        );
        if (
          suppressIntersection &&
          point !== undefined &&
          sameCanonicalPoint(point, inclusion)
        ) {
          suppressIntersection = false;
          continue;
        }
        throw new ChainMovedDuringSnapshotError(
          "local node rolled back while deriving confirmation depth",
        );
      }
      if (next.direction !== "forward") {
        throw new Error(
          "Ogmios confirmation-depth chain sync returned an invalid direction",
        );
      }
      suppressIntersection = false;
      const block = parseOgmiosPoint(
        next.block,
        network,
        source,
        "confirmation-depth block",
      );
      depth += 1;
      if (sameCanonicalPoint(block, expectedTip)) {
        return depth;
      }
    }
    // This is a conservative lower bound derived from real roll-forward
    // blocks, and is sufficient to prove the configured finality threshold.
    return requiredDepth;
  } finally {
    session.close();
  }
};

const requestOgmiosTip = async (
  ogmiosUrl: string,
): Promise<{
  readonly slot: number;
  readonly blockHash: string;
  readonly blockHeight?: number;
  readonly networkMagic: number;
}> => {
  const response = await runOgmiosSession(ogmiosUrl, [
    { id: "query-tip", method: "queryNetwork/tip", params: {} },
    {
      id: "query-genesis",
      method: "queryNetwork/genesisConfiguration",
      params: { era: "shelley" },
    },
  ]);
  const point = getRecord(response.get("query-tip"), "Ogmios network tip");
  const genesis = getRecord(
    response.get("query-genesis"),
    "Ogmios genesis configuration",
  );
  const height =
    point.height === undefined
      ? undefined
      : safeSlot(point.height, "Ogmios network tip height");
  return {
    slot: safeSlot(point.slot, "Ogmios network tip slot"),
    blockHash: safeBlockHash(point.id, "Ogmios network tip block hash"),
    ...(height === undefined ? {} : { blockHeight: height }),
    networkMagic: safeSlot(
      genesis.networkMagic ?? genesis.network_magic,
      "Ogmios network magic",
    ),
  };
};

const runOgmiosSession = async (
  ogmiosUrl: string,
  requests: readonly {
    readonly id: string;
    readonly method: string;
    readonly params: Record<string, unknown>;
  }[],
): Promise<ReadonlyMap<string, unknown>> => {
  const constructor = (
    globalThis as unknown as {
      readonly WebSocket?: RuntimeWebSocketConstructor;
    }
  ).WebSocket;
  if (constructor === undefined) {
    throw new Error("Node.js WebSocket support is required for Ogmios");
  }
  const socketUrl = new URL(ogmiosUrl);
  if (socketUrl.protocol === "http:") {
    socketUrl.protocol = "ws:";
  } else if (socketUrl.protocol === "https:") {
    socketUrl.protocol = "wss:";
  } else if (socketUrl.protocol !== "ws:" && socketUrl.protocol !== "wss:") {
    throw new Error("Ogmios chain-sync endpoint must use HTTP(S) or WS(S)");
  }
  return new Promise((resolve, reject) => {
    const socket = new constructor(socketUrl.toString());
    const results = new Map<string, unknown>();
    let requestIndex = 0;
    let settled = false;
    const timeout = setTimeout(() => {
      fail(new Error("Ogmios chain-sync request timed out"));
    }, 15_000);
    const finish = (): void => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      socket.close();
      resolve(results);
    };
    const fail = (error: Error): void => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      socket.close();
      reject(error);
    };
    const sendNext = (): void => {
      const request = requests[requestIndex];
      if (request === undefined) {
        finish();
        return;
      }
      socket.send(
        JSON.stringify({
          jsonrpc: "2.0",
          id: request.id,
          method: request.method,
          params: request.params,
        }),
      );
    };
    socket.onopen = sendNext;
    socket.onmessage = ({ data }) => {
      try {
        if (typeof data !== "string") {
          throw new Error("Ogmios returned a non-text WebSocket message");
        }
        const envelope = getRecord(
          JSON.parse(data) as unknown,
          "Ogmios JSON-RPC response",
        );
        if (envelope.error !== undefined) {
          throw new Error(
            `Ogmios JSON-RPC error: ${JSON.stringify(envelope.error)}`,
          );
        }
        const id = envelope.id;
        if (typeof id !== "string") {
          throw new Error("Ogmios JSON-RPC response omitted request id");
        }
        const expected = requests[requestIndex];
        if (expected === undefined || id !== expected.id) {
          throw new Error(
            `Ogmios JSON-RPC response id ${id} does not match the active request`,
          );
        }
        results.set(id, envelope.result);
        requestIndex += 1;
        sendNext();
      } catch (error) {
        fail(error instanceof Error ? error : new Error(String(error)));
      }
    };
    socket.onerror = () => {
      fail(new Error(`Ogmios WebSocket failed for ${socketUrl.origin}`));
    };
    socket.onclose = () => {
      if (!settled) {
        fail(
          new Error("Ogmios WebSocket closed before the response completed"),
        );
      }
    };
  });
};

const parseOgmiosPoint = (
  value: unknown,
  network: string,
  providerSource: string,
  label: string,
): CanonicalChainPoint => {
  const point = getRecord(value, label);
  return {
    network,
    slot: safeSlot(point.slot, `${label} slot`),
    blockHash: safeBlockHash(point.id, `${label} block hash`),
    providerSource,
    observedAt: new Date().toISOString(),
  };
};

const parseOgmiosPointOrOrigin = (
  value: unknown,
  network: string,
  providerSource: string,
  label: string,
): CanonicalChainPoint | undefined =>
  value === "origin"
    ? undefined
    : parseOgmiosPoint(value, network, providerSource, label);

const sameCanonicalPoint = (
  left: Pick<CanonicalChainPoint, "network" | "slot" | "blockHash">,
  right: Pick<CanonicalChainPoint, "network" | "slot" | "blockHash">,
): boolean =>
  left.network === right.network &&
  left.slot === right.slot &&
  left.blockHash === right.blockHash;

const assertNetworkMagic = (
  configuredNetwork: string,
  liveNetworkMagic: number,
  provider: string,
): void => {
  const expected =
    configuredNetwork === "Mainnet"
      ? 764_824_073
      : configuredNetwork === "Preprod"
        ? 1
        : configuredNetwork === "Preview"
          ? 2
          : undefined;
  if (expected === undefined) {
    throw new Error(
      `${provider} cannot prove custom-network identity without configured network magic`,
    );
  }
  if (liveNetworkMagic !== expected) {
    throw new L1SourceIntegrityError(
      `${provider} network magic ${liveNetworkMagic.toString()} does not match configured ${configuredNetwork} magic ${expected.toString()}`,
    );
  }
};

const syncFileData = async (path: string): Promise<void> => {
  const handle = await open(path, "r+");
  try {
    await handle.datasync();
  } finally {
    await handle.close();
  }
};

const truncateFileDurably = async (
  path: string,
  length: number,
): Promise<void> => {
  const handle = await open(path, "r+");
  try {
    await handle.truncate(length);
    await handle.datasync();
  } finally {
    await handle.close();
  }
};

const safeSlot = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const safeBlockHash = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be a lowercase 32-byte hex value`);
  }
  return value;
};

const getRecord = (value: unknown, label: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
};

const parsePersistedChainSyncState = (
  value: unknown,
  expectedAuthorityFingerprint: string,
): PersistedChainSyncState => {
  const record = getRecord(value, "persisted chain-sync state");
  if (
    record.schemaVersion !== 2 ||
    typeof record.authorityFingerprint !== "string"
  ) {
    throw new Error("persisted chain-sync state has an unsupported schema");
  }
  if (record.authorityFingerprint !== expectedAuthorityFingerprint) {
    throw new Error(
      "persisted chain-sync cursor authority fingerprint does not match the configured local node endpoint",
    );
  }
  const cursor =
    record.cursor === undefined
      ? undefined
      : parsePersistedChainSyncCursor(record.cursor);
  return {
    schemaVersion: 2,
    authorityFingerprint: record.authorityFingerprint,
    ...(cursor === undefined ? {} : { cursor }),
  };
};

const parsePersistedChainSyncConsumerState = (
  value: unknown,
  expectedAuthorityFingerprint: string,
): PersistedChainSyncConsumerState => {
  const record = getRecord(value, "persisted chain-sync consumer state");
  if (
    Object.keys(record).some(
      (key) =>
        key !== "schemaVersion" &&
        key !== "authorityFingerprint" &&
        key !== "cursor",
    ) ||
    record.schemaVersion !== 1 ||
    typeof record.authorityFingerprint !== "string" ||
    record.cursor === undefined
  ) {
    throw new Error(
      "persisted chain-sync consumer state has an unsupported schema",
    );
  }
  if (record.authorityFingerprint !== expectedAuthorityFingerprint) {
    throw new Error(
      "persisted chain-sync consumer authority fingerprint does not match the configured local node endpoint",
    );
  }
  return {
    schemaVersion: 1,
    authorityFingerprint: record.authorityFingerprint,
    cursor: parsePersistedChainSyncCursor(record.cursor),
  };
};

const parsePersistedChainSyncCursor = (value: unknown): ChainSyncCursor => {
  const cursor = getRecord(value, "persisted chain-sync cursor");
  return {
    sequence: safeSlot(cursor.sequence, "persisted chain-sync sequence"),
    rollbackGeneration: safeSlot(
      cursor.rollbackGeneration,
      "persisted chain-sync rollback generation",
    ),
    point: parsePersistedCanonicalPoint(
      cursor.point,
      "persisted chain-sync point",
    ),
  };
};

const parsePersistedChainSyncEvent = (value: unknown): ChainSyncEvent => {
  const event = getRecord(value, "persisted chain-sync event");
  if (
    event.direction !== "roll_forward" &&
    event.direction !== "roll_backward"
  ) {
    throw new Error("persisted chain-sync event has an invalid direction");
  }
  return {
    direction: event.direction,
    point: parsePersistedCanonicalPoint(
      event.point,
      "persisted chain-sync event point",
    ),
  };
};

const parsePersistedCanonicalPoint = (
  value: unknown,
  label: string,
): CanonicalChainPoint => {
  const point = getRecord(value, label);
  if (
    typeof point.network !== "string" ||
    point.network.length === 0 ||
    typeof point.providerSource !== "string" ||
    point.providerSource.length === 0 ||
    typeof point.observedAt !== "string" ||
    !Number.isFinite(Date.parse(point.observedAt))
  ) {
    throw new Error(`${label} has invalid provenance`);
  }
  return {
    network: point.network,
    slot: safeSlot(point.slot, `${label} slot`),
    blockHash: safeBlockHash(point.blockHash, `${label} block hash`),
    providerSource: point.providerSource,
    observedAt: point.observedAt,
  };
};

const samePersistedEventPoint = (
  event: ChainSyncEvent,
  point: CanonicalChainPoint,
): boolean => samePersistedCanonicalPoint(event.point, point);

const samePersistedCursor = (
  left: ChainSyncCursor,
  right: ChainSyncCursor,
): boolean =>
  left.sequence === right.sequence &&
  left.rollbackGeneration === right.rollbackGeneration &&
  samePersistedCanonicalPoint(left.point, right.point);

const samePersistedCanonicalPoint = (
  left: CanonicalChainPoint,
  right: CanonicalChainPoint,
): boolean =>
  sameCanonicalPoint(left, right) &&
  left.providerSource === right.providerSource &&
  left.observedAt === right.observedAt;

const parseFixtureChainSyncEvents = (
  value: unknown,
  network: string,
  authorityNodeId: string,
): readonly ChainSyncEvent[] => {
  if (!Array.isArray(value)) {
    throw new Error("chain-sync fixture must contain an event array");
  }
  return value.map((entry, index) => {
    const event = getRecord(
      entry,
      `chain-sync fixture event ${index.toString()}`,
    );
    if (
      event.direction !== "roll_forward" &&
      event.direction !== "roll_backward"
    ) {
      throw new Error(
        `chain-sync fixture event ${index.toString()} has an invalid direction`,
      );
    }
    return {
      direction: event.direction,
      point: {
        network,
        slot: safeSlot(
          event.slot,
          `chain-sync fixture event ${index.toString()} slot`,
        ),
        blockHash: safeBlockHash(
          event.blockHash,
          `chain-sync fixture event ${index.toString()} block hash`,
        ),
        providerSource: `chain-sync:${authorityNodeId}`,
        observedAt:
          typeof event.observedAt === "string"
            ? event.observedAt
            : new Date().toISOString(),
      },
    };
  });
};

const normalizeNetwork = (network: string): CardanoNetwork => {
  if (
    network === "Mainnet" ||
    network === "Preprod" ||
    network === "Preview" ||
    network === "Custom"
  ) {
    return network;
  }
  throw new Error(
    `unsupported Lucid network ${network}; expected Mainnet, Preprod, Preview, or Custom`,
  );
};

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
