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

import type { LoadedWatcherConfig, WatcherConfig } from "../config.js";
import type { DaAttestationCandidateRecord } from "../domain.js";
import {
  blockfrostCurrentChainPointResolver,
  type CanonicalChainPoint,
  kupmiosCurrentChainPointResolver,
  type LocalNodeChainAuthority,
  localNodeChainAuthorityFromConfig,
  lucidChainPointResolver,
  parseBlockfrostUrl,
  parseKupmiosUrl,
} from "./provider.js";

export type DaObservationChainPoint = CanonicalChainPoint & {
  readonly authorityNodeId?: string;
  readonly canonicalSlot?: number;
  readonly canonicalBlockHash?: string;
  readonly chainSyncSequence?: number;
  readonly rollbackGeneration?: number;
};

export type OnChainDaParams = {
  readonly outRef: string;
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
  readonly ownerCount: number;
  readonly updateThreshold: number;
  readonly rawDatum: SDK.DaParamsDatum;
  readonly observedChainPoint?: DaObservationChainPoint;
};

export interface DaAttestationChainReader {
  fetchDaParams(): Promise<OnChainDaParams>;
  fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]>;
  currentQueryPoint?(): CanonicalChainPoint | undefined;
}

export class LucidDaAttestationChainReader implements DaAttestationChainReader {
  private readonly lucid: LucidEvolution;
  private readonly config: WatcherConfig;
  private readonly providerSource: string;
  private readonly inclusionPointResolver: (
    utxo: UTxO,
  ) => Promise<CanonicalChainPoint>;
  private readonly queryPointResolver: () => Promise<CanonicalChainPoint>;
  private readonly localAuthority?: LocalNodeChainAuthority;
  private lastQueryPoint: CanonicalChainPoint | undefined;

  constructor({
    lucid,
    config,
    providerSource,
    inclusionPointResolver,
    queryPointResolver,
    localAuthority,
  }: {
    readonly lucid: LucidEvolution;
    readonly config: WatcherConfig;
    readonly providerSource: string;
    readonly inclusionPointResolver?: (
      utxo: UTxO,
    ) => Promise<CanonicalChainPoint>;
    readonly queryPointResolver?: () => Promise<CanonicalChainPoint>;
    readonly localAuthority?: LocalNodeChainAuthority;
  }) {
    this.lucid = lucid;
    this.config = config;
    this.providerSource = providerSource;
    this.inclusionPointResolver =
      inclusionPointResolver ??
      provenanceResolver(lucid, config.network, providerSource);
    this.queryPointResolver =
      queryPointResolver ??
      (async () => {
        throw new Error(
          "DA lifecycle reads require an explicit current-chain-point resolver",
        );
      });
    this.localAuthority = localAuthority;
  }

  async fetchDaParams(): Promise<OnChainDaParams> {
    const authorityBefore = await this.synchronizeLocalAuthority();
    const queryBefore = await this.proveQuerySnapshot(authorityBefore);
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
    const queryPoint = await this.proveQuerySnapshot(authorityBefore);
    assertSameQueryPoint(queryBefore, queryPoint, "DA params");
    const observedChainPoint = await this.proveObservationPoint(
      utxo,
      authorityBefore,
      queryPoint,
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
      observedChainPoint,
    };
  }

  async fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const authorityBefore = await this.synchronizeLocalAuthority();
    const queryBefore = await this.proveQuerySnapshot(authorityBefore);
    const unit = toUnit(
      this.config.daAttestationPolicyId,
      SDK.daAttestationAssetName(headerHash),
    );
    const utxos = await this.lucid.utxosAtWithUnit(
      this.config.daAttestationAddress,
      unit,
    );
    const queryPoint = await this.proveQuerySnapshot(authorityBefore);
    assertSameQueryPoint(queryBefore, queryPoint, "DA attestation");
    const records: DaAttestationCandidateRecord[] = [];
    for (const utxo of utxos) {
      const datum = decodeInlineDatum<SDK.DaAttestationDatum>(
        utxo,
        SDK.DaAttestationDatum as never,
        "DA attestation",
      );
      if (datum.header_hash !== headerHash) {
        throw new Error(
          `DA attestation UTxO ${outRefLabel(utxo)} has header hash ${datum.header_hash}, expected ${headerHash}`,
        );
      }
      const attestationCount = safeNumber(
        datum.attestation_count,
        "DA attestation count",
      );
      const threshold = safeNumber(
        datum.da_threshold,
        "DA attestation threshold",
      );
      const observedChainPoint = await this.proveObservationPoint(
        utxo,
        authorityBefore,
        queryPoint,
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
        observedChainPoint,
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

  private async synchronizeLocalAuthority(): Promise<
    CanonicalChainPoint | undefined
  > {
    return this.localAuthority?.synchronizeToTip();
  }

  private async proveObservationPoint(
    utxo: UTxO,
    authorityBefore: CanonicalChainPoint | undefined,
    queryPoint: CanonicalChainPoint,
  ): Promise<DaObservationChainPoint> {
    const inclusionPoint = await this.inclusionPointResolver(utxo);
    if (inclusionPoint.network !== this.config.network) {
      throw new Error(
        `L1 observation network ${inclusionPoint.network} does not match configured network ${this.config.network}`,
      );
    }
    if (this.localAuthority === undefined) {
      return {
        ...inclusionPoint,
        canonicalSlot: queryPoint.slot,
        canonicalBlockHash: queryPoint.blockHash,
      };
    }
    if (authorityBefore === undefined) {
      throw new Error("local authority point was not synchronized");
    }
    const authorityAfter = await this.localAuthority.currentPoint();
    if (
      authorityAfter.network !== authorityBefore.network ||
      authorityAfter.slot !== authorityBefore.slot ||
      authorityAfter.blockHash !== authorityBefore.blockHash
    ) {
      throw new Error(
        "local chain authority changed while DA datum query was in flight",
      );
    }
    if (inclusionPoint.slot > authorityAfter.slot) {
      throw new Error(
        `DA datum inclusion slot ${inclusionPoint.slot.toString()} is ahead of local authority slot ${authorityAfter.slot.toString()}`,
      );
    }
    if (
      inclusionPoint.slot === authorityAfter.slot &&
      inclusionPoint.blockHash !== authorityAfter.blockHash
    ) {
      throw new Error(
        "DA datum inclusion point is on a rolled-back block at the local authority slot",
      );
    }
    const cursor = await this.localAuthority.currentCursor();
    return {
      ...inclusionPoint,
      authorityNodeId: this.localAuthority.authorityNodeId,
      canonicalSlot: authorityAfter.slot,
      canonicalBlockHash: authorityAfter.blockHash,
      chainSyncSequence: cursor.sequence,
      rollbackGeneration: cursor.rollbackGeneration,
    };
  }

  private async proveQuerySnapshot(
    authorityBefore: CanonicalChainPoint | undefined,
  ): Promise<CanonicalChainPoint> {
    const queryPoint = await this.queryPointResolver();
    if (queryPoint.network !== this.config.network) {
      throw new Error(
        `L1 query network ${queryPoint.network} does not match configured network ${this.config.network}`,
      );
    }
    if (this.localAuthority !== undefined) {
      if (authorityBefore === undefined) {
        throw new Error("local authority point was not synchronized");
      }
      this.localAuthority.assertAligned(queryPoint, this.providerSource);
      const authorityAfter = await this.localAuthority.currentPoint();
      if (
        authorityAfter.network !== authorityBefore.network ||
        authorityAfter.slot !== authorityBefore.slot ||
        authorityAfter.blockHash !== authorityBefore.blockHash
      ) {
        throw new Error(
          "local chain authority changed while DA datum query was in flight",
        );
      }
    }
    this.lastQueryPoint = queryPoint;
    return queryPoint;
  }

  currentQueryPoint(): CanonicalChainPoint | undefined {
    return this.lastQueryPoint;
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

export const daAttestationReaderFromConfig = async (
  config: LoadedWatcherConfig,
): Promise<DaAttestationChainReader | undefined> => {
  const l1Source = config.l1Source;
  const providerDescriptors =
    l1Source.sourceMode === "local_node"
      ? l1Source.queryProviderUrls.map((url, index) => ({
          url,
          providerSource: `query:${l1Source.authorityNodeId}:${index.toString()}`,
        }))
      : l1Source.providers.map(({ url, identity, operationalIdentity }) => ({
          url,
          providerSource: [
            identity,
            `operator=${operationalIdentity.operatorId}`,
            `transport=${operationalIdentity.transport}`,
            `backend=${operationalIdentity.backendKey}`,
          ].join(";"),
        }));
  if (
    providerDescriptors[0]?.url.startsWith("fixture:") === true ||
    providerDescriptors[0]?.url.startsWith("file:") === true
  ) {
    return undefined;
  }
  const localAuthority =
    config.l1Source.sourceMode === "local_node"
      ? localNodeChainAuthorityFromConfig(config)
      : undefined;
  const readers = await Promise.all(
    providerDescriptors.map(async ({ url, providerSource }) => {
      const provider = await lucidFromProviderUrl(url, config.network);
      return new LucidDaAttestationChainReader({
        lucid: provider.lucid,
        config,
        providerSource,
        inclusionPointResolver: provider.inclusionPointResolver,
        queryPointResolver: provider.queryPointResolver,
        localAuthority,
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
  readonly inclusionPointResolver: (utxo: UTxO) => Promise<CanonicalChainPoint>;
  readonly queryPointResolver: () => Promise<CanonicalChainPoint>;
}> => {
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    const lucid = await Lucid(
      new Blockfrost(apiUrl, projectId),
      normalizeNetwork(network),
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
      ),
    };
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl } = parseKupmiosUrl(url);
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl),
      normalizeNetwork(network),
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
      ),
    };
  }
  throw new Error(`unsupported Cardano provider for DA reader: ${url}`);
};

const provenanceResolver =
  (lucid: LucidEvolution, network: string, providerSource: string) =>
  async (utxo: UTxO): Promise<CanonicalChainPoint> => {
    const point = await lucidChainPointResolver(lucid)(utxo);
    if (point.slot === undefined || point.blockHash === undefined) {
      throw new Error(
        `Cardano provider ${providerSource} omitted node-derived slot or block hash for ${outRefLabel(utxo)}`,
      );
    }
    return {
      ...point,
      network,
      slot: point.slot,
      blockHash: point.blockHash,
      providerSource,
      observedAt: new Date().toISOString(),
    };
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

const canonicalOnChainDaParams = (params: OnChainDaParams): string =>
  canonicalJson({
    outRef: params.outRef,
    committeeHex: params.committeeHex,
    committeeSignersHash: params.committeeSignersHash,
    threshold: params.threshold,
    ownerCount: params.ownerCount,
    updateThreshold: params.updateThreshold,
    rawDatum: params.rawDatum,
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

const assertSameQueryPoint = (
  before: CanonicalChainPoint,
  after: CanonicalChainPoint,
  label: string,
): void => {
  if (
    before.network !== after.network ||
    before.slot !== after.slot ||
    before.blockHash !== after.blockHash
  ) {
    throw new Error(
      `${label} query chain point changed while its UTxO snapshot was read`,
    );
  }
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
