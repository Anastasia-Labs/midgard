import { createHash, randomUUID } from "node:crypto";
import {
  appendFile,
  mkdir,
  readFile,
  rename,
  writeFile,
} from "node:fs/promises";
import { dirname, resolve } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Blockfrost,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { WatcherConfig } from "../config.js";
import type { ChainPoint, ObservedStateQueueNode } from "../domain.js";
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
}

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

export class FileChainSyncCursorStore implements ChainSyncCursorStore {
  private readonly journalPath: string;
  private cachedState: PersistedChainSyncState | undefined;
  private cachedJournalLength = 0;
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
      throw new Error(
        `chain-sync cursor sequence is not contiguous: persisted=${previous.cursor.sequence.toString()}, next=${cursor.sequence.toString()}`,
      );
    }
    if (this.cachedJournalLength !== cursor.sequence) {
      throw new Error(
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
      await appendFile(
        this.journalPath,
        `${JSON.stringify({ sequence: cursor.sequence, event, cursor })}\n`,
        { encoding: "utf8", mode: 0o600 },
      );
      const temporaryPath = `${this.path}.${randomUUID()}.tmp`;
      await writeFile(temporaryPath, `${JSON.stringify(next)}\n`, {
        encoding: "utf8",
        mode: 0o600,
      });
      await rename(temporaryPath, this.path);
    } catch (error) {
      this.initialized = false;
      this.cachedState = undefined;
      this.cachedJournalLength = 0;
      throw error;
    }
    this.cachedState = next;
    this.cachedJournalLength += 1;
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

  private async initialize(): Promise<PersistedChainSyncState> {
    if (this.initialized) {
      return this.cachedState!;
    }
    let state = await this.readState();
    const journal = await this.readJournal();
    if (
      state.cursor !== undefined &&
      (journal[state.cursor.sequence] === undefined ||
        !samePersistedCursor(
          journal[state.cursor.sequence]!.cursor,
          state.cursor,
        ))
    ) {
      throw new Error(
        "persisted chain-sync cursor does not match its durable event journal",
      );
    }
    const journalCursor = journal.at(-1)?.cursor;
    if (
      journalCursor !== undefined &&
      (state.cursor === undefined ||
        journalCursor.sequence > state.cursor.sequence)
    ) {
      const expectedNext = (state.cursor?.sequence ?? -1) + 1;
      if (journal[expectedNext]?.sequence !== expectedNext) {
        throw new Error(
          "persisted chain-sync journal tail is not contiguous with its cursor",
        );
      }
      state = {
        schemaVersion: 2,
        authorityFingerprint: this.authorityFingerprint,
        cursor: journalCursor,
      };
      await this.writeState(state);
    }
    await this.assertJournalMatchesCursor(state.cursor, journal);
    this.cachedState = state;
    this.cachedJournalLength = journal.length;
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
    let raw: string;
    try {
      raw = await readFile(this.journalPath, "utf8");
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return [];
      }
      throw error;
    }
    return raw
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
        if (sequence !== index) {
          throw new Error(
            "persisted chain-sync event sequences must be contiguous from zero",
          );
        }
        const event = parsePersistedChainSyncEvent(record.event);
        const cursor = parsePersistedChainSyncCursor(record.cursor);
        if (
          cursor.sequence !== sequence ||
          !samePersistedEventPoint(event, cursor.point)
        ) {
          throw new Error(
            "persisted chain-sync journal cursor does not match its event",
          );
        }
        return {
          sequence,
          event,
          cursor,
        };
      });
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
      throw new Error(
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
          : await this.store.intersectionPoints?.(2160);
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
        await this.store.append(batch.event, cursor);
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
      throw new Error(
        `${label} network ${point.network} does not match configured network ${this.network}`,
      );
    }
    if (point.providerSource !== `chain-sync:${this.authorityNodeId}`) {
      throw new Error(
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

  constructor({
    lucid,
    stateQueueAddress,
    stateQueuePolicyId,
    providerSource,
    chainPointResolver,
    currentChainPointResolver,
  }: {
    readonly lucid: LucidEvolution;
    readonly stateQueueAddress: string;
    readonly stateQueuePolicyId: string;
    readonly providerSource: string;
    readonly chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>;
    readonly currentChainPointResolver: () => Promise<CanonicalChainPoint>;
  }) {
    this.lucid = lucid;
    this.stateQueueAddress = stateQueueAddress;
    this.stateQueuePolicyId = stateQueuePolicyId;
    this.providerSource = providerSource;
    this.chainPointResolver = chainPointResolver;
    this.currentChainPointResolver = currentChainPointResolver;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const stateQueueUtxos = await SDK.fetchSortedStateQueueUTxOs(this.lucid, {
      stateQueueAddress: this.stateQueueAddress,
      stateQueuePolicyId: this.stateQueuePolicyId,
    });
    return stateQueueUtxosToObservedNodes(
      stateQueueUtxos,
      this.providerSource,
      this.chainPointResolver,
    );
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    return this.currentChainPointResolver();
  }
}

type ChainPointAwareStateQueueProvider = StateQueueProvider & {
  currentChainPoint(): Promise<CanonicalChainPoint>;
};

type StateQueueProviderWithOptionalPoint = StateQueueProvider & {
  currentChainPoint?: () => Promise<CanonicalChainPoint>;
};

export class MultiStateQueueProvider implements StateQueueProvider {
  private readonly providers: readonly StateQueueProviderWithOptionalPoint[];
  private readonly identities: readonly string[];
  private readonly mergedIdentities?: readonly string[];
  private readonly sourceMode: "local_node" | "external_providers";

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
        throw new Error(
          `state queue provider disagreement in ${this.sourceMode} mode between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    return mergeAgreedObservedNodes(sortedResults, this.mergedIdentities);
  }
}

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
    const canonicalBefore = await this.authority.synchronizeToTip();
    const results = await Promise.all(
      this.queryProviders.map(async (provider, index) => {
        const before = await provider.currentChainPoint();
        this.authority.assertAligned(before, this.queryIdentities[index]!);
        const nodes = await provider.fetchStateQueueNodes();
        const after = await provider.currentChainPoint();
        if (!sameCanonicalPoint(before, after)) {
          throw new Error(
            `local_node query surface ${this.queryIdentities[index]!} changed chain point while its snapshot was read`,
          );
        }
        this.authority.assertAligned(after, this.queryIdentities[index]!);
        return { nodes, queryPoint: after };
      }),
    );
    const canonicalAfter = await this.authority.currentPoint();
    if (!sameCanonicalPoint(canonicalBefore, canonicalAfter)) {
      throw new Error(
        "local node chain authority changed while query snapshots were being collected",
      );
    }
    const sortedResults = results.map(({ nodes }) => sortObservedNodes(nodes));
    const baseline = canonicalObservedNodes(sortedResults[0]!);
    for (const [index, nodes] of sortedResults.entries()) {
      if (!canonicalArraysEqual(canonicalObservedNodes(nodes), baseline)) {
        throw new Error(
          `local_node query surface disagreement between ${this.queryIdentities[0]!} and ${this.queryIdentities[index]!}`,
        );
      }
    }
    const merged = mergeAgreedObservedNodes(
      sortedResults,
      this.queryIdentities,
    );
    const cursor = await this.authority.currentCursor();
    return merged.map((node) => ({
      ...node,
      chainPoint: {
        ...node.chainPoint,
        network: canonicalAfter.network,
        providerSource: [
          canonicalAfter.providerSource,
          ...this.queryIdentities,
        ].join(","),
        observedAt: new Date().toISOString(),
        canonicalSlot: canonicalAfter.slot,
        canonicalBlockHash: canonicalAfter.blockHash,
        chainSyncSequence: cursor.sequence,
        rollbackGeneration: cursor.rollbackGeneration,
      },
    }));
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
      SDK.getStateQueueNodeV1FromStateQueueDatum(stateQueueUtxo.datum),
    );
    const chainPoint = {
      providerSource,
      observedAt: new Date().toISOString(),
      ...(chainPointResolver === undefined
        ? {}
        : await chainPointResolver(stateQueueUtxo.utxo)),
    };
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

export const providerFromConfig = async (
  config: WatcherConfig,
): Promise<StateQueueProvider> => {
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
      localSource.queryProviderUrls.map((url) => providerFromUrl(url, config)),
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
      localSource.queryProviderUrls.map(
        (_, index) =>
          `query:${localSource.authorityNodeId}:${index.toString()}`,
      ),
      new FileChainSyncConsumerCursorStore(
        `${cursorPath}.watcher-consumer-v1`,
        authorityFingerprint,
      ),
    );
  }
  const providers = await Promise.all(
    config.l1Source.providers.map(({ url }) => providerFromUrl(url, config)),
  );
  return new MultiStateQueueProvider(providers, {
    sourceMode: "external_providers",
    identities: config.l1Source.providers.map(({ identity }) => identity),
  });
};

const localAuthorityRegistry = new Map<string, LocalNodeChainAuthority>();

export const localNodeChainAuthorityFromConfig = (
  config: WatcherConfig,
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
    WatcherConfig["l1Source"],
    { readonly sourceMode: "local_node" }
  >,
  localState: WatcherConfig["localState"],
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
    WatcherConfig,
    "network" | "stateQueueAddress" | "stateQueuePolicyId"
  > & { readonly finalityDepth?: number },
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
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    const lucid = await Lucid(
      new Blockfrost(apiUrl, projectId),
      normalizeNetwork(config.network),
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `blockfrost:${apiUrl}`,
      chainPointResolver: blockfrostChainPointResolver(
        lucid,
        apiUrl,
        projectId,
      ),
      currentChainPointResolver: blockfrostCurrentChainPointResolver(
        config.network,
        apiUrl,
        projectId,
      ),
    });
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl, headers),
      normalizeNetwork(config.network),
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
      chainPointResolver: kupmiosChainPointResolver(
        lucid,
        kupoUrl,
        fetch,
        ogmiosUrl,
        config.network,
        Math.max(1, config.finalityDepth ?? 2160),
      ),
      currentChainPointResolver: kupmiosCurrentChainPointResolver(
        config.network,
        kupoUrl,
        ogmiosUrl,
      ),
    });
  }
  throw new Error(
    `unsupported CARDANO_PROVIDER_URLS entry ${url}; supported forms are fixture:<path>, file:<path>, blockfrost:<api-url>#<project-id>, and kupmios:<kupo-url>|<ogmios-url>`,
  );
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
      throw new Error(
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

const blockfrostChainPointResolver =
  (lucid: LucidEvolution, apiUrl: string, projectId: string) =>
  async (utxo: UTxO): Promise<ChainPoint> => {
    const [inclusion, latest] = await Promise.all([
      lucidChainPointResolver(lucid)(utxo),
      blockfrostJson(
        apiUrl,
        projectId,
        "/blocks/latest",
        parseBlockfrostLatestBlock,
      ),
    ]);
    const blockHeight = inclusion.blockHeight;
    const latestHeight = latest.height;
    return {
      ...inclusion,
      depth:
        typeof blockHeight === "number" && typeof latestHeight === "number"
          ? Math.max(0, latestHeight - blockHeight)
          : undefined,
      finalized: undefined,
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

const alignedKupmiosTip = async (
  network: string,
  kupoUrl: string,
  ogmiosUrl: string,
  fetchFn: typeof fetch,
): Promise<CanonicalChainPoint> => {
  const [kupoPoint, ogmiosTip] = await Promise.all([
    fetchKupoCheckpoint(kupoUrl, fetchFn),
    requestOgmiosTip(ogmiosUrl),
  ]);
  assertNetworkMagic(network, ogmiosTip.networkMagic, "Ogmios");
  if (
    kupoPoint.slot !== ogmiosTip.slot ||
    kupoPoint.blockHash !== ogmiosTip.blockHash
  ) {
    throw new Error(
      `Kupmios query surfaces are not aligned: Kupo=${kupoPoint.slot.toString()}:${kupoPoint.blockHash}, Ogmios=${ogmiosTip.slot.toString()}:${ogmiosTip.blockHash}`,
    );
  }
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

const mergeChainPoints = (points: readonly ChainPoint[]): ChainPoint => {
  const primary = points[0] ?? {};
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
  };
};

const canonicalJson = (value: unknown): string =>
  JSON.stringify(canonicalValue(value));

const canonicalValue = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString();
  }
  if (Array.isArray(value)) {
    return value.map(canonicalValue);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([key, entry]) => [key, canonicalValue(entry)]),
    );
  }
  return value;
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
  let pendingRollback: ChainSyncEventBatch | undefined;
  let suppressHandshakeRollback = false;

  const disconnect = (): void => {
    session?.close();
    session = undefined;
    sessionUrl = undefined;
    intersection = undefined;
    pendingRollback = undefined;
    suppressHandshakeRollback = false;
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
        if (session === undefined || sessionUrl !== ogmiosUrl) {
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
                ].slice(0, 2160);
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
            return {
              event: { direction: "roll_forward", point: bootstrapTip },
              tip,
            };
          }
          if (cursor !== undefined && intersection === undefined) {
            throw new Error(
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
            return result;
          }
          if (cursor !== undefined && sameCanonicalPoint(cursor, tip)) {
            return { tip };
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
            return {
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
            };
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
              throw new Error(
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
                return { tip };
              }
              continue;
            }
            suppressHandshakeRollback = false;
            return {
              event: { direction: "roll_backward", point },
              tip,
            };
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
      throw new Error(
        "state-queue inclusion point is not on the canonical local-node chain",
      );
    }
    if (!sameCanonicalPoint(tip, expectedTip)) {
      throw new Error(
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
        throw new Error(
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
        throw new Error(
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
    throw new Error(
      `${provider} network magic ${liveNetworkMagic.toString()} does not match configured ${configuredNetwork} magic ${expected.toString()}`,
    );
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
