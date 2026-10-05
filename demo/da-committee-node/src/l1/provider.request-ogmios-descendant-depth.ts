import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ChainPoint } from "../domain.js";
import {
  CHAIN_POINT_BATCH_DEADLINE_MS,
  CHAIN_POINT_RESOLUTION_CONCURRENCY,
  ChainPointBatchDeadlineError,
  type ChainPointResolver,
  mapWithConcurrency,
} from "./provider.chain-point-batch.js";
import { ChainMovedDuringSnapshotError } from "./provider.local-node-chain-authority.js";
import { lucidChainPointResolver } from "./provider.local-node-chain-authority-from-config.js";
import {
  assertNetworkMagic,
  OgmiosRpcSession,
  parseOgmiosPoint,
  parseOgmiosPointOrOrigin,
} from "./provider.ogmios-rpc-session.js";
import {
  type CanonicalChainPoint,
  type CardanoNetwork,
  getRecord,
  safeSlot,
  sameCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";
import { alignedKupmiosTip } from "./provider.run-ogmios-session.js";

/**
 * Resolves state-queue chain points over Kupmios. A point whose inclusion
 * lookup carries no depth gets one counted from real descendant blocks,
 * pinned to an aligned Kupo/Ogmios tip read before the walk and read again
 * after it: a tip that moved in between fails the resolution.
 *
 * Called per UTxO, each resolution brackets its own walk. `resolveAll`
 * resolves a whole snapshot inside ONE bracket: one aligned tip read before
 * the first walk, every walk pinned to it, and one read after the last, at
 * most `CHAIN_POINT_RESOLUTION_CONCURRENCY` UTxOs at a time and within
 * `batchDeadlineMs`; past the deadline no further UTxO starts and the pass
 * fails as an observation failure.
 */
export const kupmiosChainPointResolver = (
  lucid: LucidEvolution,
  _kupoUrl: string,
  _fetchFn: typeof fetch = fetch,
  ogmiosUrl?: string,
  network?: string,
  requiredDepth = 2160,
  networkMagic?: number,
  batch: {
    readonly batchDeadlineMs?: number;
    readonly nowMs?: () => number;
    /** Owning scoped consumer supplies both transports; default callers are unchanged. */
    readonly openSession?: (
      url: string,
    ) => Promise<Pick<OgmiosRpcSession, "request" | "close">>;
    readonly readAlignedTip?: () => Promise<CanonicalChainPoint>;
  } = {},
): ChainPointResolver => {
  const resolveInclusion = lucidChainPointResolver(lucid);
  const alignedTip = (walk: {
    readonly network: string;
    readonly ogmiosUrl: string;
  }): Promise<CanonicalChainPoint> =>
    batch.readAlignedTip?.() ??
    alignedKupmiosTip(
      walk.network,
      _kupoUrl,
      walk.ogmiosUrl,
      _fetchFn,
      networkMagic,
    );
  /** The walk's input when the inclusion needs a counted depth. */
  const walkFor = (inclusion: ChainPoint) =>
    inclusion.depth !== undefined ||
    ogmiosUrl === undefined ||
    network === undefined ||
    inclusion.slot === undefined ||
    inclusion.blockHash === undefined
      ? undefined
      : {
          ogmiosUrl,
          network,
          networkMagic,
          // Empty slots are not confirmations. Keep depth unknown unless the
          // aligned node can count actual descendant blocks.
          inclusion: {
            network,
            slot: inclusion.slot,
            blockHash: inclusion.blockHash,
            providerSource: `kupmios:${_kupoUrl}|${ogmiosUrl}`,
            observedAt: new Date().toISOString(),
          },
          requiredDepth,
          openSession: batch.openSession,
        };
  const assertTipHeld = async (
    walk: Parameters<typeof alignedTip>[0],
    before: CanonicalChainPoint,
  ): Promise<void> => {
    if (!sameCanonicalPoint(before, await alignedTip(walk))) {
      throw new ChainMovedDuringSnapshotError(
        "Kupmios chain point changed while deriving block confirmations",
      );
    }
  };
  const resolveOne = async (utxo: UTxO): Promise<ChainPoint> => {
    const inclusion = await resolveInclusion(utxo);
    const walk = walkFor(inclusion);
    if (walk === undefined) return inclusion;
    const before = await alignedTip(walk);
    const depth = await requestOgmiosDescendantDepth({
      ...walk,
      expectedTip: before,
    });
    await assertTipHeld(walk, before);
    return { ...inclusion, depth };
  };
  const resolveAll = async (
    utxos: readonly UTxO[],
  ): Promise<readonly ChainPoint[]> => {
    const nowMs = batch.nowMs ?? Date.now;
    const deadlineMs = batch.batchDeadlineMs ?? CHAIN_POINT_BATCH_DEADLINE_MS;
    const deadline = nowMs() + deadlineMs;
    const assertWithinDeadline = (): void => {
      if (nowMs() > deadline) {
        throw new ChainPointBatchDeadlineError(deadlineMs);
      }
    };
    // Read once, by the first UTxO that needs a walk.
    let pinned:
      | {
          readonly walk: Parameters<typeof alignedTip>[0];
          readonly before: Promise<CanonicalChainPoint>;
        }
      | undefined;
    const points = await mapWithConcurrency(
      utxos,
      CHAIN_POINT_RESOLUTION_CONCURRENCY,
      async (utxo) => {
        assertWithinDeadline();
        const inclusion = await resolveInclusion(utxo);
        const walk = walkFor(inclusion);
        if (walk === undefined) return inclusion;
        pinned ??= { walk, before: alignedTip(walk) };
        const expectedTip = await pinned.before;
        assertWithinDeadline();
        const depth = await requestOgmiosDescendantDepth({
          ...walk,
          expectedTip,
        });
        return { ...inclusion, depth };
      },
    );
    if (pinned !== undefined) {
      await assertTipHeld(pinned.walk, await pinned.before);
    }
    return points;
  };
  return Object.assign(resolveOne, { resolveAll });
};

export const kupmiosCurrentChainPointResolver =
  (
    network: string,
    kupoUrl: string,
    ogmiosUrl: string,
    networkMagic?: number,
  ) =>
  async (): Promise<CanonicalChainPoint> =>
    alignedKupmiosTip(network, kupoUrl, ogmiosUrl, fetch, networkMagic);

const requestOgmiosDescendantDepth = async ({
  ogmiosUrl,
  network,
  networkMagic,
  inclusion,
  expectedTip,
  requiredDepth,
  openSession,
}: {
  readonly ogmiosUrl: string;
  readonly network: string;
  readonly networkMagic: number | undefined;
  readonly inclusion: CanonicalChainPoint;
  readonly expectedTip: CanonicalChainPoint;
  readonly requiredDepth: number;
  readonly openSession?: (
    url: string,
  ) => Promise<Pick<OgmiosRpcSession, "request" | "close">>;
}): Promise<number> => {
  const source = `confirmation-depth:${ogmiosUrl}`;
  const session = await (openSession ?? OgmiosRpcSession.open)(ogmiosUrl);
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
      networkMagic,
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

export const normalizeNetwork = (network: string): CardanoNetwork => {
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
