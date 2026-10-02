import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import type {
  ChainPoint,
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../domain.js";
import { canonicalJson } from "./canonical-json.js";
import type { ChainPointResolver } from "./provider.chain-point-batch.js";
import {
  ChainMovedDuringSnapshotError,
  STATE_QUEUE_REPLAY_ATTEMPTS,
} from "./provider.local-node-chain-authority.js";
import {
  declaredChainPoint,
  stateQueueUtxosToObservedSnapshot,
} from "./provider.parse-fixture-chain-sync-events.js";
import { type CanonicalChainPoint } from "./provider.parse-persisted-chain-sync-state.js";
import { L1SourceIntegrityError } from "./source-integrity.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

export class LucidStateQueueProvider implements StateQueueProvider {
  private readonly lucid: LucidEvolution;
  private readonly stateQueueAddress: string;
  private readonly stateQueuePolicyId: string;
  private readonly providerSource: string;
  private readonly chainPointResolver?: ChainPointResolver;
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
    /**
     * Its `resolveAll`, when present, must reach the snapshot builder, which
     * resolves a whole snapshot against one aligned tip through it.
     */
    readonly chainPointResolver?: ChainPointResolver;
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
export const replayWithIntegrityOnHeldChain = async <T>(
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
export const agreedTipBlockNo = (
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

export type ChainPointAwareStateQueueProvider = StateQueueProvider & {
  currentChainPoint(): Promise<CanonicalChainPoint>;
};

export type StateQueueProviderWithOptionalPoint = StateQueueProvider & {
  currentChainPoint?: () => Promise<CanonicalChainPoint>;
  holdsChainPoint?: (
    point: CanonicalChainPoint,
  ) => Promise<boolean | undefined>;
};

export const sortObservedNodes = (
  nodes: readonly ObservedStateQueueNode[],
): readonly ObservedStateQueueNode[] =>
  [...nodes].sort((left, right) =>
    canonicalObservedNode(left).localeCompare(canonicalObservedNode(right)),
  );

export const canonicalObservedNodes = (
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

export const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

export const assertCompatibleChainPoints = (
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

export const compatibleChainPoint = (
  left: ChainPoint,
  right: ChainPoint,
): boolean =>
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

// Surfaces are compared in one canonical sort, which orders nodes by asset
// name. The merged nodes go back into the first surface's own order, the
// linked-list order: replay walks the list, and the scanner's final queue must
// match it node for node.
export const mergeAgreedObservedNodes = (
  firstSurfaceNodes: readonly ObservedStateQueueNode[],
  sortedResults: readonly (readonly ObservedStateQueueNode[])[],
  identities?: readonly string[],
): readonly ObservedStateQueueNode[] =>
  firstSurfaceNodes.map((surfaceNode) => {
    const index = sortedResults[0]!.indexOf(surfaceNode);
    return {
      ...surfaceNode,
      chainPoint: mergeChainPoints(
        sortedResults.map((nodes, providerIndex) => ({
          ...nodes[index]!.chainPoint,
          providerSource:
            identities?.[providerIndex] ??
            nodes[index]!.chainPoint.providerSource,
        })),
      ),
    };
  });

export const mergeChainPoints = (points: readonly ChainPoint[]): ChainPoint => {
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
