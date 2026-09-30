import {
  Blockfrost,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { DaAttestationCandidateRecord } from "../domain.js";
import {
  canonicalArraysEqual,
  canonicalCandidates,
  canonicalOnChainDaParams,
  sortCandidates,
} from "./da-attestation-reader.lucid-da-attestation-chain-reader.js";
import {
  type DaAttestationChainReader,
  type DaObservationChainPoint,
  type OnChainDaParams,
  provenanceResolver,
} from "./da-attestation-reader.provenance-resolver.js";
import { committeeLucidSlotOptions } from "./lucid-network.js";
import {
  blockfrostCurrentChainPointResolver,
  type CanonicalChainPoint,
  kupmiosCurrentChainPointResolver,
  parseBlockfrostUrl,
  parseKupmiosUrl,
} from "./provider.js";

export class MultiDaAttestationChainReader implements DaAttestationChainReader {
  private readonly readers: readonly DaAttestationChainReader[];

  constructor(readers: readonly DaAttestationChainReader[]) {
    if (readers.length === 0) {
      throw new Error("at least one DA attestation chain reader is required");
    }
    this.readers = readers;
  }

  async fetchDaParams(): Promise<OnChainDaParams> {
    const results = await Promise.all(
      this.readers.map((reader) => reader.fetchDaParams()),
    );
    assertReaderQueryPointsCompatible(this.readers);
    const baseline = canonicalOnChainDaParams(results[0]!);
    for (const [index, result] of results.entries()) {
      if (canonicalOnChainDaParams(result) !== baseline) {
        throw new Error(
          `DA params provider disagreement between provider 0 and provider ${index.toString()}`,
        );
      }
    }
    return {
      ...results[0]!,
      observedChainPoint: mergeOptionalObservationPoints(
        results.map(({ observedChainPoint }) => observedChainPoint),
      ),
    };
  }

  async fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const results = await Promise.all(
      this.readers.map((reader) =>
        reader.fetchDaAttestationCandidates(headerHash),
      ),
    );
    assertReaderQueryPointsCompatible(this.readers);
    const sortedResults = results.map(sortCandidates);
    const baseline = canonicalCandidates(sortedResults[0]!);
    for (const [index, candidates] of sortedResults.entries()) {
      const current = canonicalCandidates(candidates);
      if (!canonicalArraysEqual(current, baseline)) {
        throw new Error(
          `DA attestation candidate provider disagreement between provider 0 and provider ${index.toString()}`,
        );
      }
    }
    return mergeAgreedCandidates(sortedResults);
  }
}

export const lucidFromProviderUrl = async (
  url: string,
  network: string,
  networkMagic: number,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly inclusionPointResolver: (utxo: UTxO) => Promise<CanonicalChainPoint>;
  readonly queryPointResolver: () => Promise<CanonicalChainPoint>;
}> => {
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    const cardanoNetwork = normalizeNetwork(network);
    const slotOptions = await committeeLucidSlotOptions({
      network: cardanoNetwork,
      route: { provider: "blockfrost", apiUrl },
      networkMagic,
    });
    const lucid = await Lucid(
      new Blockfrost(apiUrl, projectId),
      cardanoNetwork,
      slotOptions,
    );
    const providerSource = `blockfrost:${apiUrl}`;
    return {
      lucid,
      inclusionPointResolver: provenanceResolver(
        lucid,
        network,
        providerSource,
      ),
      queryPointResolver: blockfrostCurrentChainPointResolver(
        network,
        apiUrl,
        projectId,
        undefined,
        networkMagic,
      ),
    };
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl } = parseKupmiosUrl(url);
    const cardanoNetwork = normalizeNetwork(network);
    const slotOptions = await committeeLucidSlotOptions({
      network: cardanoNetwork,
      route: { provider: "kupmios", ogmiosUrl },
      networkMagic,
    });
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl),
      cardanoNetwork,
      slotOptions,
    );
    const providerSource = `kupmios:${kupoUrl}|${ogmiosUrl}`;
    return {
      lucid,
      inclusionPointResolver: provenanceResolver(
        lucid,
        network,
        providerSource,
      ),
      queryPointResolver: kupmiosCurrentChainPointResolver(
        network,
        kupoUrl,
        ogmiosUrl,
        networkMagic,
      ),
    };
  }
  throw new Error(`unsupported Cardano provider for DA reader: ${url}`);
};

const normalizeNetwork = (network: string) => {
  if (
    network === "Mainnet" ||
    network === "Preprod" ||
    network === "Preview" ||
    network === "Custom"
  ) {
    return network;
  }
  throw new Error(`unsupported Lucid network ${network}`);
};

const mergeAgreedCandidates = (
  sortedResults: readonly (readonly DaAttestationCandidateRecord[])[],
): readonly DaAttestationCandidateRecord[] =>
  sortedResults[0]!.map((candidate, index) => ({
    ...candidate,
    observedChainPoint: mergeOptionalObservationPoints(
      sortedResults.map(
        (candidates) =>
          candidates[index]!.observedChainPoint as
            | DaObservationChainPoint
            | undefined,
      ),
    )!,
  }));

const mergeObservationPoints = (
  points: readonly DaObservationChainPoint[],
): DaObservationChainPoint | undefined => {
  const first = points[0];
  if (first === undefined) {
    return undefined;
  }
  for (const [index, point] of points.entries()) {
    if (
      typeof point.network !== "string" ||
      !Number.isSafeInteger(point.slot) ||
      typeof point.blockHash !== "string" ||
      !/^[0-9a-f]{64}$/u.test(point.blockHash)
    ) {
      throw new Error(
        `DA observation provider ${index.toString()} omitted canonical network, slot, or block hash provenance`,
      );
    }
    if (
      point.network !== first.network ||
      point.slot !== first.slot ||
      point.blockHash !== first.blockHash ||
      point.blockHeight !== first.blockHeight
    ) {
      throw new Error(
        `DA observation provenance disagreement between provider 0 and provider ${index.toString()}`,
      );
    }
  }
  const providerSource = points.map((point) => point.providerSource).join(",");
  const depths = points
    .map((point) => point.depth)
    .filter((depth): depth is number => depth !== undefined);
  const finalized = points.every((point) => point.finalized === true)
    ? true
    : points.some((point) => point.finalized === false)
      ? false
      : undefined;
  return {
    ...first,
    providerSource,
    observedAt: new Date().toISOString(),
    depth: depths.length === points.length ? Math.min(...depths) : undefined,
    finalized,
  };
};

const mergeOptionalObservationPoints = (
  points: readonly (DaObservationChainPoint | undefined)[],
): DaObservationChainPoint | undefined => {
  const observed = points.filter(
    (point): point is DaObservationChainPoint => point !== undefined,
  );
  if (observed.length === 0) {
    return undefined;
  }
  if (observed.length !== points.length) {
    throw new Error(
      "DA readers must all expose observation chain-point provenance",
    );
  }
  return mergeObservationPoints(observed);
};

const assertReaderQueryPointsCompatible = (
  readers: readonly DaAttestationChainReader[],
): void => {
  const points = readers.map((reader) => reader.currentQueryPoint?.());
  const observedPoints = points.filter(
    (point): point is CanonicalChainPoint => point !== undefined,
  );
  if (observedPoints.length === 0) {
    return;
  }
  if (observedPoints.length !== readers.length) {
    throw new Error(
      "DA readers must all expose current chain-point provenance",
    );
  }
  const baseline = observedPoints[0]!;
  for (const [index, point] of observedPoints.entries()) {
    if (
      point.network !== baseline.network ||
      point.slot !== baseline.slot ||
      point.blockHash !== baseline.blockHash
    ) {
      throw new Error(
        `DA reader chain-point disagreement between provider 0 and provider ${index.toString()}`,
      );
    }
  }
};
