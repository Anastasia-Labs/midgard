import { randomUUID } from "node:crypto";
import {
  appendFile,
  mkdir,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { dirname } from "node:path";

import {
  type CanonicalChainPoint,
  CHAIN_SYNC_INTERSECTION_POINTS,
  CHAIN_SYNC_JOURNAL_PRUNE_SLACK,
  type ChainSyncCursor,
  type ChainSyncCursorStore,
  type ChainSyncEvent,
  type CommittedChainSyncJournal,
  getRecord,
  parsePersistedChainSyncCursor,
  parsePersistedChainSyncEvent,
  parsePersistedChainSyncState,
  type PersistedChainSyncJournalEntry,
  type PersistedChainSyncState,
  safeSlot,
  samePersistedEventPoint,
  syncFileData,
  truncateFileDurably,
} from "./provider.parse-persisted-chain-sync-state.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";
import { L1SourceIntegrityError } from "./source-integrity.js";

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

  async cursorAt(sequence: number): Promise<ChainSyncCursor | undefined> {
    if (!Number.isSafeInteger(sequence) || sequence < 0) {
      throw new Error("chain-sync journal sequence must be an integer >= 0");
    }
    const state = await this.initialize();
    if (
      sequence < this.cachedJournalFirstSequence ||
      sequence >= this.cachedJournalNextSequence
    ) {
      return undefined;
    }
    const journal = await this.readJournal();
    await this.assertJournalMatchesCursor(state.cursor, journal);
    return journal.find((entry) => entry.sequence === sequence)?.cursor;
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
