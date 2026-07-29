import * as SDK from "@al-ft/midgard-sdk";
import {
  Blockfrost,
  Data,
  Kupmios,
  Lucid,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { WatcherConfig } from "../config.js";
import type { DaAttestationCandidateRecord } from "../domain.js";

export type OnChainDaParams = {
  readonly outRef: string;
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
  readonly ownerCount: number;
  readonly updateThreshold: number;
  readonly rawDatum: SDK.DaParamsDatum;
};

export interface DaAttestationChainReader {
  fetchDaParams(): Promise<OnChainDaParams>;
  fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]>;
}

export class LucidDaAttestationChainReader implements DaAttestationChainReader {
  private readonly lucid: LucidEvolution;
  private readonly config: WatcherConfig;
  private readonly providerSource: string;

  constructor({
    lucid,
    config,
    providerSource,
  }: {
    readonly lucid: LucidEvolution;
    readonly config: WatcherConfig;
    readonly providerSource: string;
  }) {
    this.lucid = lucid;
    this.config = config;
    this.providerSource = providerSource;
  }

  async fetchDaParams(): Promise<OnChainDaParams> {
    const unit = toUnit(
      this.config.daParamsGovernorPolicyId,
      SDK.DA_PARAMS_ASSET_NAME,
    );
    const utxos = await this.lucid.utxosAtWithUnit(
      this.config.daParamsGovernorAddress,
      unit,
    );
    if (utxos.length !== 1) {
      throw new Error(
        `expected exactly one DA params UTxO, found ${utxos.length.toString()}`,
      );
    }
    const utxo = utxos[0]!;
    const datum = decodeInlineDatum<SDK.DaParamsDatum>(
      utxo,
      SDK.DaParamsDatum as never,
      "DA params",
    );
    return {
      outRef: outRefLabel(utxo),
      committeeHex: datum.committee,
      committeeSignersHash: datum.committee_signers_hash,
      threshold: safeNumber(datum.da_threshold, "DA threshold"),
      ownerCount: datum.owners.length,
      updateThreshold: safeNumber(
        datum.update_threshold,
        "DA update threshold",
      ),
      rawDatum: datum,
    };
  }

  async fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const unit = toUnit(
      this.config.daAttestationPolicyId,
      SDK.daAttestationAssetName(headerHash),
    );
    const utxos = await this.lucid.utxosAtWithUnit(
      this.config.daAttestationAddress,
      unit,
    );
    const records: DaAttestationCandidateRecord[] = [];
    for (const utxo of utxos) {
      let datum: SDK.DaAttestationDatum;
      try {
        datum = decodeInlineDatum<SDK.DaAttestationDatum>(
          utxo,
          SDK.DaAttestationDatum as never,
          "DA attestation",
        );
      } catch {
        continue;
      }
      if (datum.header_hash !== headerHash) {
        continue;
      }
      const attestationCount = safeNumber(
        datum.attestation_count,
        "DA attestation count",
      );
      const threshold = safeNumber(
        datum.da_threshold,
        "DA attestation threshold",
      );
      records.push({
        deploymentFingerprint: this.config.deploymentFingerprint,
        headerHash,
        outRef: outRefLabel(utxo),
        datumCbor: utxo.datum!,
        attestationCount,
        threshold,
        committeeSignersHash: datum.committee_signers_hash,
        bitmap: datum.attested_signers,
        observedChainPoint: {
          providerSource: this.providerSource,
          observedAt: new Date().toISOString(),
        },
        status:
          attestationCount >= threshold
            ? "threshold"
            : attestationCount > 0
              ? "signed"
              : "initialized",
      });
    }
    return records.sort((left, right) =>
      left.outRef.localeCompare(right.outRef),
    );
  }
}

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
    const baseline = canonicalJson(results[0]!);
    for (const [index, result] of results.entries()) {
      if (canonicalJson(result) !== baseline) {
        throw new Error(
          `DA params provider disagreement between provider 0 and provider ${index.toString()}`,
        );
      }
    }
    return results[0]!;
  }

  async fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const results = await Promise.all(
      this.readers.map((reader) =>
        reader.fetchDaAttestationCandidates(headerHash),
      ),
    );
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

export const daAttestationReaderFromConfig = async (
  config: WatcherConfig,
): Promise<DaAttestationChainReader | undefined> => {
  const providerUrls =
    config.l1Source.sourceMode === "local_node"
      ? config.l1Source.queryProviderUrls
      : config.l1Source.providers.map(({ url }) => url);
  if (
    providerUrls[0]?.startsWith("fixture:") === true ||
    providerUrls[0]?.startsWith("file:") === true
  ) {
    return undefined;
  }
  const readers = await Promise.all(
    providerUrls.map(async (url) => {
      const { lucid, providerSource } = await lucidFromProviderUrl(
        url,
        config.network,
      );
      return new LucidDaAttestationChainReader({
        lucid,
        config,
        providerSource,
      });
    }),
  );
  if (
    config.l1Source.sourceMode === "external_providers" &&
    readers.length < 2
  ) {
    throw new Error(
      "external_providers mode requires at least two DA attestation readers",
    );
  }
  // Local query surfaces share one chain authority and are not a provider
  // quorum, but conflicting views must still fail closed.
  return readers.length === 1
    ? readers[0]!
    : new MultiDaAttestationChainReader(readers);
};

const lucidFromProviderUrl = async (
  url: string,
  network: string,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly providerSource: string;
}> => {
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    return {
      lucid: await Lucid(
        new Blockfrost(apiUrl, projectId),
        normalizeNetwork(network),
      ),
      providerSource: `blockfrost:${apiUrl}`,
    };
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl } = parseKupmiosUrl(url);
    return {
      lucid: await Lucid(
        new Kupmios(kupoUrl, ogmiosUrl),
        normalizeNetwork(network),
      ),
      providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
    };
  }
  throw new Error(`unsupported Cardano provider for DA reader: ${url}`);
};

const decodeInlineDatum = <T>(
  utxo: UTxO,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): T => {
  if (utxo.datum == null) {
    throw new Error(`${label} UTxO ${outRefLabel(utxo)} has no inline datum`);
  }
  return Data.from(utxo.datum, schema) as T;
};

const safeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} is outside safe integer range`);
  }
  return Number(value);
};

const parseBlockfrostUrl = (
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

const parseKupmiosUrl = (
  value: string,
): { readonly kupoUrl: string; readonly ogmiosUrl: string } => {
  const raw = value.slice("kupmios:".length);
  const [kupoUrl, ogmiosUrl] = raw.split("|");
  if (kupoUrl === undefined || ogmiosUrl === undefined) {
    throw new Error(
      "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
    );
  }
  return { kupoUrl, ogmiosUrl };
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

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const sortCandidates = (
  candidates: readonly DaAttestationCandidateRecord[],
): readonly DaAttestationCandidateRecord[] =>
  [...candidates].sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );

const canonicalCandidates = (
  candidates: readonly DaAttestationCandidateRecord[],
): readonly string[] => candidates.map(canonicalCandidate);

const canonicalCandidate = (candidate: DaAttestationCandidateRecord): string =>
  canonicalJson({
    deploymentFingerprint: candidate.deploymentFingerprint,
    headerHash: candidate.headerHash,
    outRef: candidate.outRef,
    datumCbor: candidate.datumCbor,
    attestationCount: candidate.attestationCount,
    threshold: candidate.threshold,
    committeeSignersHash: candidate.committeeSignersHash,
    bitmap: candidate.bitmap,
    status: candidate.status,
  });

const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const mergeAgreedCandidates = (
  sortedResults: readonly (readonly DaAttestationCandidateRecord[])[],
): readonly DaAttestationCandidateRecord[] =>
  sortedResults[0]!.map((candidate, index) => ({
    ...candidate,
    observedChainPoint: {
      providerSource: sortedResults
        .map(
          (candidates) => candidates[index]!.observedChainPoint.providerSource,
        )
        .filter((source): source is string => source !== undefined)
        .join(","),
      observedAt: new Date().toISOString(),
    },
  }));

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
