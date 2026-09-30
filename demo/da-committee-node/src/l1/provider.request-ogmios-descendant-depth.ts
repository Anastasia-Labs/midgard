import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ChainPoint } from "../domain.js";
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

export const kupmiosChainPointResolver = (
  lucid: LucidEvolution,
  _kupoUrl: string,
  _fetchFn: typeof fetch = fetch,
  ogmiosUrl?: string,
  network?: string,
  requiredDepth = 2160,
  networkMagic?: number,
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
      networkMagic,
    );
    const depth = await requestOgmiosDescendantDepth({
      ogmiosUrl,
      network,
      networkMagic,
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
      networkMagic,
    );
    if (!sameCanonicalPoint(before, after)) {
      throw new ChainMovedDuringSnapshotError(
        "Kupmios chain point changed while deriving block confirmations",
      );
    }
    return { ...inclusion, depth };
  };
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
}: {
  readonly ogmiosUrl: string;
  readonly network: string;
  readonly networkMagic: number | undefined;
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
