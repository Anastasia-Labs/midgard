import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  castStateQueueNodeToData,
  DaAttestationDatum,
  type DaAttestationReferenceScripts,
  type DaAttestationStateQueueTarget,
  daAttestationUnit,
  type DaAttestationUtxo,
  type DaAvailabilityParameters,
  type DaBondPoolDatum,
  daBondPoolUnit,
  type DaParamsDatum,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  encodeDaBondPoolDatum,
  encodeLinkedListNodeView,
  type LinkedListNodeView,
  type MidgardValidators,
  NO_DA_ATTESTATION,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  StateQueueNode,
  type StateQueueUTxO,
} from "../src/index.js";

export const signature = (byte: string): string => byte.repeat(64);

const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitment({
    deploymentIdentity: h28(0x71),
    headerHash,
    payload: Uint8Array.of(1),
    responseGeometry: availabilityResponseGeometry({
      chunkByteLength: 4096,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

type RecordedPayment = {
  readonly address: string;
  /** Absent for `pay.ToAddress`. */
  readonly datum?: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

export type Recording = {
  readonly withdrawals: {
    readonly address: string;
    readonly amount: bigint;
    readonly redeemer: unknown;
  }[];
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer: unknown }[];
  readonly mints: { readonly assets: Assets; readonly redeemer: unknown }[];
  readonly payments: RecordedPayment[];
  readonly signerKeys: string[];
  readonly validityRanges: {
    readonly validFrom: number;
    readonly validTo: number;
  }[];
  /** Every `utxosAtWithUnit` query, in order: the apply's pool fetches. */
  readonly unitQueries: { readonly address: string; readonly unit: string }[];
};

/**
 * `chain` is what `utxosAtWithUnit` answers from, and a test may swap its
 * contents between builds: the apply builder must query it on every build.
 */
export const makeRecordingLucid = (
  chain: { utxos: readonly UTxO[] } = { utxos: [] },
): {
  readonly lucid: LucidEvolution;
  readonly record: Recording;
} => {
  const record: Recording = {
    withdrawals: [],
    reads: [],
    collects: [],
    mints: [],
    payments: [],
    signerKeys: [],
    validityRanges: [],
    unitQueries: [],
  };
  const lucid = {
    config: () => ({ network: "Custom" }),
    utxosAtWithUnit: async (address: string, unit: string) => {
      record.unitQueries.push({ address, unit });
      return chain.utxos.filter(
        (utxo) => utxo.address === address && (utxo.assets[unit] ?? 0n) > 0n,
      );
    },
    newTx: () => {
      const tx = {
        validFrom: (validFrom: number) => {
          record.validityRanges.push({ validFrom, validTo: Number.NaN });
          return tx;
        },
        validTo: (validTo: number) => {
          const latest = record.validityRanges.at(-1);
          if (latest !== undefined) {
            record.validityRanges[record.validityRanges.length - 1] = {
              validFrom: latest.validFrom,
              validTo,
            };
          }
          return tx;
        },
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer: unknown) => {
          record.collects.push({ inputs, redeemer });
          return tx;
        },
        withdraw: (address: string, amount: bigint, redeemer: unknown) => {
          record.withdrawals.push({ address, amount, redeemer });
          return tx;
        },
        mintAssets: (assets: Assets, redeemer: unknown) => {
          record.mints.push({ assets, redeemer });
          return tx;
        },
        pay: {
          ToContract: (
            address: string,
            datum: RecordedPayment["datum"],
            assets: Assets,
          ) => {
            record.payments.push({ address, datum, assets });
            return tx;
          },
          ToAddress: (address: string, assets: Assets) => {
            record.payments.push({ address, assets });
            return tx;
          },
        },
        addSignerKey: (keyHash: string) => {
          record.signerKeys.push(keyHash);
          return tx;
        },
      };
      return tx;
    },
  } as unknown as LucidEvolution;
  return { lucid, record };
};

const makeUtxo = (
  outputIndex: number,
  assets: Assets = { lovelace: 1n },
  datum: string | null = null,
  address = `addr_test_${outputIndex.toString()}`,
): UTxO =>
  ({
    txHash: outputIndex.toString(16).padStart(64, "0"),
    outputIndex,
    address,
    assets,
    datum,
  }) as UTxO;

const validator = (policyByte: number, address: string) =>
  ({
    policyId: h28(policyByte),
    spendingScriptAddress: address,
    spendingScriptHash: h28(policyByte),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as unknown as MidgardValidators["daAttestation"];

export const makeFixture = () => {
  const contracts = {
    daAttestation: validator(0xaa, "addr_da_attestation"),
    stateQueue: validator(0xbb, "addr_state_queue"),
    daBondPool: validator(0xdd, "addr_da_bond_pool"),
  } as Pick<MidgardValidators, "daAttestation" | "daBondPool" | "stateQueue">;
  const headerHash = h28(0x10);
  const stateQueueNode: StateQueueNode = {
    proven_fraud: null,
    header: {
      prevUtxosRoot: h32(0x01),
      utxosRoot: h32(0x02),
      withdrawalsRoot: h32(0x05),
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      transactionsRoot: h32(0x03),
      depositsRoot: h32(0x04),
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28(0x06),
      operatorVkey: h28(0x07),
      protocolVersion: 0n,
    },
    da_attestation: NO_DA_ATTESTATION,
  };
  const linkedListNode: LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: castStateQueueNodeToData(
      stateQueueNode,
    ) as LinkedListNodeView["data"],
  };
  const stateQueueUnit =
    contracts.stateQueue.policyId +
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    headerHash;
  const stateQueueUtxo: StateQueueUTxO = {
    utxo: makeUtxo(
      1,
      { lovelace: 3_000_000n, [stateQueueUnit]: 1n },
      encodeLinkedListNodeView(linkedListNode),
      contracts.stateQueue.spendingScriptAddress,
    ),
    datum: linkedListNode,
    assetName: STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  };
  const target: DaAttestationStateQueueTarget = {
    stateQueueUtxo,
    stateQueueNode,
    headerHash,
  };
  // Q63 (F04 §4) floors both governed thresholds at two, so the fixture is a
  // 2-of-2 committee over a 2-of-2 owner set. Both sets are sorted-unique.
  const daParamsDatum: DaParamsDatum = {
    committee: h32(0x11) + h32(0x22),
    committee_signers_hash: h32(0x33),
    da_threshold: 2n,
    owners: [h28(0x44), h28(0x55)],
    update_threshold: 2n,
  };
  const daParamsUtxo = makeUtxo(2, { lovelace: 2_000_000n });
  const attestationUnit = daAttestationUnit(
    contracts.daAttestation,
    headerHash,
  );
  const attestationDatum: DaAttestationDatum = {
    header_hash: headerHash,
    availability_commitment: availabilityCommitment(headerHash),
    da_threshold: 2n,
    committee_signers_hash: daParamsDatum.committee_signers_hash,
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: [h28(0x66)] },
      stakeCredential: null,
    },
    attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
    attestation_count: 0n,
  };
  const attestation: DaAttestationUtxo = {
    utxo: makeUtxo(
      3,
      { lovelace: 5_000_000n, [attestationUnit]: 1n },
      Data.to(attestationDatum, DaAttestationDatum),
      contracts.daAttestation.spendingScriptAddress,
    ),
    datum: attestationDatum,
  };
  const referenceScripts: DaAttestationReferenceScripts = {
    daAttestationMinting: makeUtxo(4),
    daAttestationSpending: makeUtxo(5),
    stateQueueMinting: makeUtxo(6),
    stateQueueSpending: makeUtxo(7),
  };
  return {
    contracts,
    headerHash,
    daParamsDatum,
    daParamsUtxo,
    target,
    attestation,
    attestationUnit,
    referenceScripts,
    availabilityParameters: AVAILABILITY_PARAMETERS,
    applyValidityRange: { validFrom: 1_000n, validTo: 2_000n },
    // Output index 0 of the all-zero tx hash: it sorts before every other
    // reference input although the builder reads it second, so a pool index
    // taken from the `readFrom` order instead of the ledger's sorted order is
    // off by one.
    pool: (datum: DaBondPoolDatum, lovelace: bigint): UTxO =>
      makeUtxo(
        0,
        {
          lovelace,
          [daBondPoolUnit(contracts.daBondPool.policyId)]: 1n,
        },
        encodeDaBondPoolDatum(datum),
        contracts.daBondPool.spendingScriptAddress,
      ),
  };
};

/**
 * Only `da_bond_lovelace` and `da_bond_pool_floor_lovelace` matter to the
 * apply pre-check; the rest is a well-formed filler.
 */
const AVAILABILITY_PARAMETERS: DaAvailabilityParameters = {
  response_geometry: availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  }),
  da_bond_lovelace: 100_000_000n,
  challenger_bond_lovelace: 50_000_000n,
  max_open_fee_lovelace: 1_000_000n,
  max_publication_fee_lovelace: 1_000_000n,
  max_settlement_fee_lovelace: 1_000_000n,
  max_close_fee_lovelace: 1_000_000n,
  max_timeout_fee_lovelace: 1_000_000n,
  da_slash_penalty_lovelace: 10_000_000n,
  da_bond_min_top_up_lovelace: 10_000_000n,
  da_bond_pool_floor_lovelace: 5_000_000n,
  challenge_record_lovelace: 27_000_000n,
};

/** A pool backing exactly one DA bond above its floor: the apply boundary. */
export const EXACTLY_BONDED_POOL_LOVELACE =
  AVAILABILITY_PARAMETERS.da_bond_pool_floor_lovelace +
  AVAILABILITY_PARAMETERS.da_bond_lovelace;
