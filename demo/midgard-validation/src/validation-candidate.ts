import {
  decodeMidgardAddressBytes,
  encodeMidgardAddressText,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";

import { LedgerColumns, type LedgerEntry, type ProcessedTx } from "./ledger.js";
import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
  MidgardOutRef,
} from "./ledger-tx/types.js";
import type { PhaseAValidatedTx } from "./types.js";
import { mintToValueDelta, sumMidgardValues } from "./value-accounting.js";

const DEFAULT_ADDRESS_CACHE_MAX_ENTRIES = 4_096;
type AddressProjection = {
  readonly text: string;
  readonly protected: boolean;
};
const addressProjectionCache = new Map<string, AddressProjection>();
let addressCacheMaxEntries = DEFAULT_ADDRESS_CACHE_MAX_ENTRIES;
let addressCacheHits = 0;
let addressCacheMisses = 0;
let addressCacheEvictions = 0;

export const phaseAAddressCacheStats = (): {
  readonly size: number;
  readonly maxEntries: number;
  readonly hits: number;
  readonly misses: number;
  readonly evictions: number;
} => ({
  size: addressProjectionCache.size,
  maxEntries: addressCacheMaxEntries,
  hits: addressCacheHits,
  misses: addressCacheMisses,
  evictions: addressCacheEvictions,
});

export const resetPhaseAAddressCache = (
  maxEntries = DEFAULT_ADDRESS_CACHE_MAX_ENTRIES,
): void => {
  addressProjectionCache.clear();
  addressCacheMaxEntries = Math.max(1, Math.floor(maxEntries));
  addressCacheHits = 0;
  addressCacheMisses = 0;
  addressCacheEvictions = 0;
};

const projectAddress = (address: Buffer): AddressProjection => {
  const cacheKey = address.toString("hex");
  const cached = addressProjectionCache.get(cacheKey);
  if (cached !== undefined) {
    addressProjectionCache.delete(cacheKey);
    addressProjectionCache.set(cacheKey, cached);
    addressCacheHits += 1;
    return cached;
  }

  // Foreign-network bytes still have to reach the staged output scan so it can
  // classify E_NETWORK_ID_MISMATCH at the consensus-defined instruction.
  // They cannot be rendered under addr/addr_test, so retain a deterministic
  // non-persisted diagnostic spelling; Phase B rejects them before mutation.
  const decoded = decodeMidgardAddressBytes(address);
  const projection = {
    text:
      decoded.networkId === 0 || decoded.networkId === 1
        ? encodeMidgardAddressText(address)
        : `foreign-network:${cacheKey}`,
    protected: decoded.protected,
  } satisfies AddressProjection;
  addressCacheMisses += 1;
  if (addressProjectionCache.size >= addressCacheMaxEntries) {
    const oldestKey = addressProjectionCache.keys().next().value as
      | string
      | undefined;
    if (oldestKey !== undefined) {
      addressProjectionCache.delete(oldestKey);
      addressCacheEvictions += 1;
    }
  }
  addressProjectionCache.set(cacheKey, projection);
  return projection;
};

/**
 * The out-ref's canonical Midgard bytes, and therefore the ledger MPF trie key
 * and the ledger DB `outref` column: the §5.3 field-0/1 item encoding
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes.
 *
 * On-chain `ledger_outref_key` derives the same trie key through
 * `encode_midgard_tx_input`, so this must stay the §5.3 encoder rather than
 * CML's minimal-index `TransactionInput` CBOR — that spells indices 0–23 in one
 * byte and would produce a 36-byte key the on-chain side never computes. See
 * `docs/spec/midgard-tx.md` §5.3.
 */
export const midgardOutRefToCbor = (outRef: MidgardOutRef): Buffer =>
  encodeMidgardSpendInputItem({
    txId: outRef.txId,
    outputIndex: Number(outRef.index),
  });

export const midgardOutRefToCborHex = (outRef: MidgardOutRef): string =>
  midgardOutRefToCbor(outRef).toString("hex");

const ledgerOutputToTxOutput = (
  output: MidgardLedgerOutput,
): MidgardTxOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.scriptRef === undefined ? {} : { script_ref: output.scriptRef }),
});

export const ledgerOutputToCbor = (output: MidgardLedgerOutput): Buffer =>
  encodeMidgardTxOutput(ledgerOutputToTxOutput(output));

const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.map((hash) => hash.toString("hex"));

const mintPolicyHashHexes = (tx: MidgardLedgerTx): readonly string[] => {
  if (tx.mint.assets.length === 0) {
    return [];
  }
  const seen = new Set<string>();
  const policyIds: string[] = [];
  for (const asset of tx.mint.assets) {
    const policyId = asset.policyId.toString("hex");
    if (!seen.has(policyId)) {
      seen.add(policyId);
      policyIds.push(policyId);
    }
  }
  return policyIds;
};

type BuildPhaseAValidatedTxArgs = {
  readonly ledgerTx: MidgardLedgerTx;
  readonly expectedNetworkId: bigint;
  readonly txCbor: Buffer;
  readonly programMaterialSidecarCbor?: Buffer | null;
  readonly arrivalSeq: bigint;
  readonly createdAt: Date;
  readonly redeemerWitnessHash: Buffer;
};

export const buildPhaseAValidatedTx = ({
  ledgerTx,
  expectedNetworkId,
  txCbor,
  programMaterialSidecarCbor,
  arrivalSeq,
  createdAt,
  redeemerWitnessHash,
}: BuildPhaseAValidatedTxArgs): PhaseAValidatedTx => {
  const outputAddresses = ledgerTx.outputs.map((output) =>
    projectAddress(output.address),
  );
  const produced: LedgerEntry[] = ledgerTx.outputs.map((output, index) => {
    const outputCbor = ledgerOutputToCbor(output);
    return {
      [LedgerColumns.TX_ID]: Buffer.from(ledgerTx.txId),
      [LedgerColumns.OUTREF]: midgardOutRefToCbor({
        txId: ledgerTx.txId,
        index: BigInt(index),
      }),
      [LedgerColumns.OUTPUT]: outputCbor,
      [LedgerColumns.ADDRESS]: outputAddresses[index]!.text,
    };
  });

  return {
    ledgerTx,
    submission: {
      txCbor: Buffer.from(txCbor),
      programMaterialSidecarCbor:
        programMaterialSidecarCbor == null
          ? null
          : Buffer.from(programMaterialSidecarCbor),
      arrivalSeq,
      createdAt: new Date(createdAt.getTime()),
    },
    graph: {
      spentOutRefHexes: ledgerTx.spendInputs.map(midgardOutRefToCborHex),
      referenceOutRefHexes: ledgerTx.referenceInputs.map(
        midgardOutRefToCborHex,
      ),
      produced,
    },
    derived: {
      expectedNetworkId,
      outputSum: sumMidgardValues(
        ledgerTx.outputs.map((output) => output.value),
      ),
      mintDelta: mintToValueDelta(ledgerTx.mint),
      witnessKeyHashHexes: hashHexes(ledgerTx.witnessKeyHashes),
      nativeScriptHashHexes: hashHexes(ledgerTx.nativeScriptHashes),
      plutusScriptHashHexes: hashHexes(ledgerTx.plutusScriptHashes),
      requiredObserverHashHexes: hashHexes(ledgerTx.requiredObserverHashes),
      mintPolicyHashHexes: mintPolicyHashHexes(ledgerTx),
      redeemerWitnessHash: Buffer.from(redeemerWitnessHash),
      requiresLocalScriptDiscovery:
        ledgerTx.scriptWitnesses.length > 0 ||
        ledgerTx.redeemers.length > 0 ||
        ledgerTx.requiredObserverHashes.length > 0 ||
        ledgerTx.mint.assets.length > 0 ||
        outputAddresses.some((address) => address.protected),
    },
  };
};

export const processedTxFromValidatedTx = (
  candidate: PhaseAValidatedTx,
): ProcessedTx => ({
  txId: Buffer.from(candidate.ledgerTx.txId),
  txCbor: Buffer.from(candidate.submission.txCbor),
  spent: candidate.graph.spentOutRefHexes.map((outRefHex) =>
    Buffer.from(outRefHex, "hex"),
  ),
  produced: candidate.graph.produced.map((entry) => ({
    [LedgerColumns.TX_ID]: Buffer.from(entry[LedgerColumns.TX_ID]),
    [LedgerColumns.OUTREF]: Buffer.from(entry[LedgerColumns.OUTREF]),
    [LedgerColumns.OUTPUT]: Buffer.from(entry[LedgerColumns.OUTPUT]),
    [LedgerColumns.ADDRESS]: entry[LedgerColumns.ADDRESS],
  })),
});
