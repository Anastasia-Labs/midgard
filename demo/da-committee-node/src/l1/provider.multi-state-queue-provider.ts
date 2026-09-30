import * as SDK from "@al-ft/midgard-sdk";

import type {
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../domain.js";
import { canonicalJson } from "./canonical-json.js";
import {
  agreedTipBlockNo,
  assertCompatibleChainPoints,
  canonicalArraysEqual,
  canonicalObservedNodes,
  type ChainPointAwareStateQueueProvider,
  compatibleChainPoint,
  mergeAgreedObservedNodes,
  mergeChainPoints,
  replayWithIntegrityOnHeldChain,
  sortObservedNodes,
  type StateQueueProviderWithOptionalPoint,
} from "./provider.lucid-state-queue-provider.js";
import {
  type CanonicalChainPoint,
  sameCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";
import { L1SourceIntegrityError } from "./source-integrity.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

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
    return mergeAgreedObservedNodes(
      results[0]!,
      sortedResults,
      this.mergedIdentities,
    );
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
      nodes: mergeAgreedObservedNodes(
        baseline.nodes,
        sortedResults,
        this.mergedIdentities,
      ),
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
