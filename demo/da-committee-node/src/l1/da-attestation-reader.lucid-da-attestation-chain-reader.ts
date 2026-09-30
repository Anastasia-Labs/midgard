import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";

import type { CommitteeConfig } from "../config.js";
import type { DaAttestationCandidateRecord } from "../domain.js";
import { canonicalJson } from "./canonical-json.js";
import {
  assertSameQueryPoint,
  type DaAttestationChainReader,
  type DaObservationChainPoint,
  decodeInlineDatum,
  type OnChainDaParams,
  outRefLabel,
  provenanceResolver,
  safeNumber,
} from "./da-attestation-reader.provenance-resolver.js";
import {
  type CanonicalChainPoint,
  type LocalNodeChainAuthority,
} from "./provider.js";

export class LucidDaAttestationChainReader implements DaAttestationChainReader {
  private readonly lucid: LucidEvolution;
  private readonly config: CommitteeConfig;
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
    readonly config: CommitteeConfig;
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

export const sortCandidates = (
  candidates: readonly DaAttestationCandidateRecord[],
): readonly DaAttestationCandidateRecord[] =>
  [...candidates].sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );

export const canonicalCandidates = (
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

export const canonicalOnChainDaParams = (params: OnChainDaParams): string =>
  canonicalJson({
    outRef: params.outRef,
    committeeHex: params.committeeHex,
    committeeSignersHash: params.committeeSignersHash,
    threshold: params.threshold,
    ownerCount: params.ownerCount,
    updateThreshold: params.updateThreshold,
    rawDatum: params.rawDatum,
  });

export const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);
