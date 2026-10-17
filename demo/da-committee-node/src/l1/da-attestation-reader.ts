import type { FactStore, StoredOutput } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";

import type { CommitteeConfig } from "../config.js";
import type { ChainPoint, DaAttestationCandidateRecord } from "../domain.js";

export type OnChainDaParams = {
  readonly outRef: string;
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
  readonly ownerCount: number;
  readonly updateThreshold: number;
  readonly rawDatum: SDK.DaParamsDatum;
  readonly observedChainPoint?: ChainPoint;
};

export interface DaAttestationChainReader {
  fetchDaParams(): Promise<OnChainDaParams>;
  fetchDaAttestationCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]>;
}

/** Where the committee's follower recorded an output. */
const FOLLOWER_SOURCE = "l1_follower";

const safeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} is outside safe integer range`);
  }
  return Number(value);
};

const outRefLabel = (row: StoredOutput): string =>
  `${row.outRef.txHash.toString("hex")}#${row.outRef.index.toString()}`;

const inlineDatum = <T>(
  row: StoredOutput,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): T => {
  if (row.output.datum === null) {
    throw new Error(`${label} UTxO ${outRefLabel(row)} has no inline datum`);
  }
  return Data.from(row.output.datum.toString("hex"), schema) as T;
};

/**
 * The DA params and attestation outputs, read from the committee follower's
 * facts at its cursor: the live outputs holding the asset, at the configured
 * address. Both addresses and policies are in the committee's tracked set,
 * so the facts hold every such output; nothing is read from Kupo or Ogmios.
 */
export const followerDaAttestationReader = (
  store: Pick<FactStore, "liveUtxos" | "blockAtOrBeforeSlot">,
  config: Pick<
    CommitteeConfig,
    | "deploymentFingerprint"
    | "daParamsGovernorPolicyId"
    | "daParamsGovernorAddress"
    | "daAttestationPolicyId"
    | "daAttestationAddress"
  >,
): DaAttestationChainReader => {
  const liveWithUnit = async (
    policyId: string,
    assetName: string,
    address: string,
    label: string,
  ): Promise<StoredOutput[]> => {
    const read = await store.liveUtxos({
      by: "unit",
      policyId: Buffer.from(policyId, "hex"),
      assetName: Buffer.from(assetName, "hex"),
    });
    if (read.kind !== "ok") {
      throw new Error(`${label} read refused: ${read.kind}: ${read.detail}`);
    }
    const at = getAddressDetails(address).address.hex;
    return read.utxos.filter(
      (row) => row.output.address.toString("hex") === at,
    );
  };
  const chainPoint = async (row: StoredOutput): Promise<ChainPoint> => {
    const slot = row.created?.slot ?? row.seedSlot ?? undefined;
    const block =
      row.created === null
        ? null
        : await store.blockAtOrBeforeSlot(row.created.slot);
    return {
      ...(slot === undefined ? {} : { slot }),
      ...(block === null || block.slot !== slot
        ? {}
        : { blockHash: block.hash.toString("hex"), blockHeight: block.height }),
      providerSource: FOLLOWER_SOURCE,
      observedAt: new Date().toISOString(),
    };
  };
  return {
    fetchDaParams: async () => {
      const rows = await liveWithUnit(
        config.daParamsGovernorPolicyId,
        SDK.DA_PARAMS_ASSET_NAME,
        config.daParamsGovernorAddress,
        "DA params",
      );
      if (rows.length !== 1) {
        throw new Error(
          `expected exactly one DA params UTxO, found ${rows.length.toString()}`,
        );
      }
      const row = rows[0]!;
      const datum = inlineDatum<SDK.DaParamsDatum>(
        row,
        SDK.DaParamsDatum as never,
        "DA params",
      );
      return {
        outRef: outRefLabel(row),
        committeeHex: datum.committee,
        committeeSignersHash: datum.committee_signers_hash,
        threshold: safeNumber(datum.da_threshold, "DA threshold"),
        ownerCount: datum.owners.length,
        updateThreshold: safeNumber(
          datum.update_threshold,
          "DA update threshold",
        ),
        rawDatum: datum,
        observedChainPoint: await chainPoint(row),
      };
    },
    fetchDaAttestationCandidates: async (headerHash) => {
      const rows = await liveWithUnit(
        config.daAttestationPolicyId,
        SDK.daAttestationAssetName(headerHash),
        config.daAttestationAddress,
        "DA attestation",
      );
      const records: DaAttestationCandidateRecord[] = [];
      for (const row of rows) {
        const datum = inlineDatum<SDK.DaAttestationDatum>(
          row,
          SDK.DaAttestationDatum as never,
          "DA attestation",
        );
        if (datum.header_hash !== headerHash) {
          throw new Error(
            `DA attestation UTxO ${outRefLabel(row)} has header hash ${datum.header_hash}, expected ${headerHash}`,
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
        records.push({
          deploymentFingerprint: config.deploymentFingerprint,
          headerHash,
          outRef: outRefLabel(row),
          datumCbor: row.output.datum!.toString("hex"),
          attestationCount,
          threshold,
          committeeSignersHash: datum.committee_signers_hash,
          bitmap: datum.attested_signers,
          observedChainPoint: await chainPoint(row),
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
    },
  };
};
