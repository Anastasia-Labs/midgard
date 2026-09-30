import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { DaAttestationCandidateRecord } from "../domain.js";
import {
  type CanonicalChainPoint,
  lucidChainPointResolver,
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

export const provenanceResolver =
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

export const decodeInlineDatum = <T>(
  utxo: UTxO,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): T => {
  if (utxo.datum == null) {
    throw new Error(`${label} UTxO ${outRefLabel(utxo)} has no inline datum`);
  }
  return Data.from(utxo.datum, schema) as T;
};

export const safeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} is outside safe integer range`);
  }
  return Number(value);
};

export const outRefLabel = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const assertSameQueryPoint = (
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
