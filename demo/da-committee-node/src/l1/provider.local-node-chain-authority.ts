import {
  type AvailabilityCursorRefresh,
  type CanonicalChainPoint,
  CHAIN_SYNC_CHUNK_EVENTS,
  CHAIN_SYNC_INTERSECTION_POINTS,
  type ChainSyncAcknowledgement,
  type ChainSyncCatchUpProgress,
  type ChainSyncConsumerCursorStore,
  type ChainSyncCursor,
  type ChainSyncCursorStore,
  type ChainSyncEvent,
  type ChainSyncEventBatch,
  type ChainSyncEventSource,
  ChainSyncNoProgressError,
  type ChainSyncReadBudget,
  sameCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";
import { L1SourceIntegrityError } from "./source-integrity.js";

export class LocalNodeChainAuthority {
  private cursor: ChainSyncCursor | undefined;
  private loaded = false;
  private operation = Promise.resolve();
  private catchUp: ChainSyncCatchUpProgress | undefined;

  constructor(
    readonly authorityNodeId: string,
    readonly network: string,
    private readonly source: ChainSyncEventSource,
    private readonly store: ChainSyncCursorStore,
    private readonly onCatchUpProgress?: (
      progress: ChainSyncCatchUpProgress & { readonly caughtUp: boolean },
    ) => void,
  ) {}

  /**
   * Synchronizes the durable cursor to the node's tip, `maxEvents` at a time.
   * However far behind the member is, it keeps going for as long as each chunk
   * moves the cursor forward on the chain; a chunk that does not fails with
   * the retryable `ChainSyncNoProgressError`. It resolves only at the tip, so
   * nothing is decided on a view that is not synchronized.
   */
  async synchronizeToTip(
    maxEvents = CHAIN_SYNC_CHUNK_EVENTS,
  ): Promise<CanonicalChainPoint> {
    let result: CanonicalChainPoint | undefined;
    const run = this.operation.then(async () => {
      await this.loadCursor();
      const intersectionCandidates =
        this.cursor === undefined
          ? undefined
          : await this.store.intersectionPoints?.(
              CHAIN_SYNC_INTERSECTION_POINTS,
            );
      let events = 0;
      try {
        for (;;) {
          const startSlot = this.cursor?.point.slot;
          const chunk = await this.synchronizeChunk(
            maxEvents,
            intersectionCandidates,
          );
          if (chunk.reachedTip) {
            result = this.cursor!.point;
            if (this.catchUp !== undefined) {
              this.onCatchUpProgress?.({
                events: events + chunk.events,
                cursorSlot: result.slot,
                tipSlot: result.slot,
                caughtUp: true,
              });
            }
            return;
          }
          events += chunk.events;
          const cursorSlot = this.cursor!.point.slot;
          if (startSlot !== undefined && cursorSlot <= startSlot) {
            throw new ChainSyncNoProgressError(
              `local node chain-sync appended ${chunk.events.toString()} events without moving past slot ${startSlot.toString()} toward tip slot ${chunk.tip.slot.toString()}`,
            );
          }
          this.catchUp = { events, cursorSlot, tipSlot: chunk.tip.slot };
          this.onCatchUpProgress?.({ ...this.catchUp, caughtUp: false });
        }
      } finally {
        this.catchUp = undefined;
      }
    });
    this.operation = run.catch(() => undefined);
    await run;
    return result!;
  }

  /** One bounded refresh on the same serialized journal writer as full scans. */
  async refreshToTip(
    budget: AvailabilityCursorRefresh,
  ): Promise<ChainSyncCursor> {
    budget.scope.assertCurrent();
    if (!Number.isSafeInteger(budget.maxEvents) || budget.maxEvents <= 0)
      throw new Error("Availability cursor event limit must be positive");
    let began = false;
    let signalStarted!: () => void;
    const started = new Promise<void>((resolve) => {
      signalStarted = resolve;
    });
    const run = this.operation.then(async () => {
      // A queued expired caller never starts a read or a durable append later.
      budget.scope.assertCurrent();
      began = true;
      signalStarted();
      await this.loadCursor();
      budget.scope.assertCurrent();
      const candidates =
        this.cursor === undefined
          ? undefined
          : await this.store.intersectionPoints?.(
              CHAIN_SYNC_INTERSECTION_POINTS,
            );
      budget.scope.assertCurrent();
      const chunk = await this.synchronizeChunk(
        budget.maxEvents,
        candidates,
        budget,
      );
      budget.scope.assertCurrent();
      if (!chunk.reachedTip)
        throw new Error(
          "Availability cursor refresh exceeded its event limit before reaching the tip",
        );
      return this.cursor!;
    });
    this.operation = run.then(
      () => undefined,
      () => undefined,
    );
    // Also wake the waiter when the scope expired before this callback began.
    void run.catch(() => {
      signalStarted();
    });
    try {
      await budget.scope.read(() => started);
    } catch (error) {
      // Once started, await any owned durable append before returning. Only
      // queue waiting is raced; an append never outlives this method's result.
      if (began) await run.catch(() => undefined);
      throw error;
    }
    return run;
  }

  /** Set while a synchronization has not reached the tip yet. */
  catchUpProgress(): ChainSyncCatchUpProgress | undefined {
    return this.catchUp;
  }

  private async synchronizeChunk(
    maxEvents: number,
    intersectionCandidates: readonly CanonicalChainPoint[] | undefined,
    readBudget?: ChainSyncReadBudget,
  ): Promise<
    | { readonly reachedTip: true; readonly events: number }
    | {
        readonly reachedTip: false;
        readonly events: number;
        readonly tip: CanonicalChainPoint;
      }
  > {
    let tip: CanonicalChainPoint | undefined;
    for (let count = 0; count < maxEvents; count += 1) {
      readBudget?.scope.assertCurrent();
      const read = () =>
        this.source.next(this.cursor, intersectionCandidates, readBudget);
      const batch =
        readBudget === undefined
          ? await read()
          : await readBudget.scope.read(read);
      readBudget?.scope.assertCurrent();
      this.assertSourcePoint(batch.tip, "chain-sync tip");
      tip = batch.tip;
      if (batch.event === undefined) {
        if (
          this.cursor === undefined ||
          !sameCanonicalPoint(this.cursor.point, batch.tip)
        ) {
          throw new Error(
            "chain-sync source reported no event before the canonical tip was reached",
          );
        }
        return { reachedTip: true, events: count };
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
        readBudget?.scope.assertCurrent();
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
      readBudget?.scope.assertCurrent();
      if (sameCanonicalPoint(batch.event.point, batch.tip)) {
        return { reachedTip: true, events: count + 1 };
      }
    }
    if (tip === undefined) {
      throw new Error("local node chain-sync chunk must allow an event");
    }
    return { reachedTip: false, events: maxEvents, tip };
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
   * Records in `consumer` that it has replayed every journal event through
   * `cursor`, then prunes the entries it no longer needs.
   *
   * The consumer captures `cursor` before it acts on the chain view and
   * acknowledges it afterwards, so the authority may have synchronized
   * further in between, rollbacks included. That is expected: those events
   * follow the cursor, stay journaled, and the consumer's next replay
   * delivers them. Anything else (a cursor ahead of the authority, one the
   * journal does not hold, or one behind the durable consumer) is refused.
   *
   * Serialized with synchronization, so no append lands between the check
   * and the prune.
   */
  async acknowledgeConsumed(
    cursor: ChainSyncCursor,
    consumer: ChainSyncConsumerCursorStore,
  ): Promise<ChainSyncAcknowledgement> {
    let outcome: ChainSyncAcknowledgement | undefined;
    const run = this.operation.then(async () => {
      await this.loadCursor();
      const current = this.cursor;
      if (current === undefined) {
        throw new L1SourceIntegrityError(
          "refusing to acknowledge a chain-sync cursor before the authority has one",
        );
      }
      if (
        cursor.sequence > current.sequence ||
        cursor.rollbackGeneration > current.rollbackGeneration
      ) {
        throw new L1SourceIntegrityError(
          "refusing to acknowledge a chain-sync cursor ahead of the authority cursor",
        );
      }
      const consumed = await consumer.load();
      if (
        consumed !== undefined &&
        (cursor.sequence < consumed.sequence ||
          cursor.rollbackGeneration < consumed.rollbackGeneration ||
          (cursor.sequence === consumed.sequence &&
            !samePersistedCursor(cursor, consumed)))
      ) {
        throw new L1SourceIntegrityError(
          "refusing to acknowledge a chain-sync cursor behind or conflicting with the durable consumer cursor",
        );
      }
      // The consumed cursor itself may already be pruned from the journal;
      // every event after it is still journaled.
      const alreadyConsumed =
        consumed !== undefined && samePersistedCursor(cursor, consumed);
      if (!alreadyConsumed) {
        const journaled = await this.store.cursorAt(cursor.sequence);
        if (
          journaled === undefined ||
          !samePersistedCursor(journaled, cursor)
        ) {
          throw new L1SourceIntegrityError(
            "refusing to acknowledge a chain-sync cursor the durable event journal does not hold",
          );
        }
      }
      if (!alreadyConsumed) {
        await consumer.save(cursor);
      }
      // Only what a resumption intersects with and what the consumer has not
      // replayed yet is still needed, so the journal stays bounded.
      await this.store.prune?.(cursor.sequence);
      outcome = {
        rollbackSinceCapture:
          cursor.rollbackGeneration !== current.rollbackGeneration,
      };
    });
    this.operation = run.catch(() => undefined);
    await run;
    return outcome!;
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

/**
 * The chain moved while a read that must observe one chain point was in
 * progress. An observation failure: the read is retaken, or the tick fails
 * and the next one retries.
 */
export class ChainMovedDuringSnapshotError extends Error {}

/** Replay attempts one tick makes while the chain keeps moving under them. */
export const STATE_QUEUE_REPLAY_ATTEMPTS = 3;

export const LOCAL_NODE_SNAPSHOT_ATTEMPTS = 8;

export const LOCAL_NODE_SNAPSHOT_RETRY_MS = 250;

export type OgmiosChainSyncRequest = (
  ogmiosUrl: string,
  cursor: CanonicalChainPoint | undefined,
  intersectionCandidates: readonly CanonicalChainPoint[] | undefined,
  network: string,
  authorityNodeId: string,
  networkMagic: number | undefined,
  readBudget?: ChainSyncReadBudget,
) => Promise<ChainSyncEventBatch>;
