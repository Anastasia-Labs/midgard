import * as SDK from "@al-ft/midgard-sdk";

import type { LoadedCommitteeConfig } from "../config.js";
import type {
  ChainPoint,
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../domain.js";
import { readAvailabilityCursor } from "./availability-cursor.js";
import { canonicalJson } from "./canonical-json.js";
import { joinNativeReads } from "./provider.join-native-reads.js";
import {
  ChainMovedDuringSnapshotError,
  LOCAL_NODE_SNAPSHOT_ATTEMPTS,
  LOCAL_NODE_SNAPSHOT_RETRY_MS,
  LocalNodeChainAuthority,
} from "./provider.local-node-chain-authority.js";
import {
  agreedTipBlockNo,
  canonicalArraysEqual,
  canonicalObservedNodes,
  type ChainPointAwareStateQueueProvider,
  compatibleChainPoint,
  mergeAgreedObservedNodes,
  mergeChainPoints,
  replayWithIntegrityOnHeldChain,
  sortObservedNodes,
} from "./provider.lucid-state-queue-provider.js";
import { declaredChainPoint } from "./provider.parse-fixture-chain-sync-events.js";
import {
  type AvailabilityCursorRefresh,
  type CanonicalChainPoint,
  type ChainSyncAcknowledgement,
  type ChainSyncCatchUpProgress,
  type ChainSyncConsumerCursorStore,
  type ChainSyncCursor,
  type ChainSyncEvent,
  type ChainSyncReplayProvider,
  sameCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";
import { L1SourceIntegrityError } from "./source-integrity.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

export class LocalNodeStateQueueProvider
  implements StateQueueProvider, ChainSyncReplayProvider
{
  constructor(
    private readonly authority: LocalNodeChainAuthority,
    private readonly queryProviders: readonly ChainPointAwareStateQueueProvider[],
    private readonly queryIdentities: readonly string[],
    private readonly consumerCursorStore: ChainSyncConsumerCursorStore,
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
    const cursorBefore = await this.authority.currentCursor();
    if (!sameCanonicalPoint(canonicalBefore, cursorBefore.point)) {
      throw new ChainMovedDuringSnapshotError(
        "local node chain authority moved on before query snapshots were collected",
      );
    }
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
    const results = await joinNativeReads(
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
    // The same cursor, not only the same point: a rollback and a re-adoption
    // of the block in between would leave the point unchanged.
    const cursorAfter = await this.authority.currentCursor();
    const canonicalAfter = cursorAfter.point;
    if (!samePersistedCursor(cursorBefore, cursorAfter)) {
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
      baselineSnapshot.nodes,
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
      chainSyncCursor: cursorAfter,
    };
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    return this.authority.currentPoint();
  }

  async refreshAvailabilityCursor(
    budget: AvailabilityCursorRefresh,
  ): Promise<ChainSyncCursor> {
    await this.authority.refreshToTip(budget);
    return readAvailabilityCursor(this, budget.scope);
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
    const histories = await joinNativeReads(
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

  chainSyncCatchUpProgress(): ChainSyncCatchUpProgress | undefined {
    return this.authority.catchUpProgress();
  }

  async acknowledgeChainSyncCursor(
    cursor: ChainSyncCursor,
  ): Promise<ChainSyncAcknowledgement> {
    return this.authority.acknowledgeConsumed(cursor, this.consumerCursorStore);
  }
}

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

export const localNodeChainCursorPath = (
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
