import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-snapshot.js";

// ---------------------------------------------------------------------------
// Fixture construction (mirrors demo/midgard-fault-proofs/tests/helpers/
// canonical-block-evidence-fixture.ts, using only package exports)
// ---------------------------------------------------------------------------

/** The canonical header hash: blake2b-224 over the header's CBOR, exactly as
 * `hashBlockHeader` (demo/midgard-sdk/src/ledger-state.ts:467) and the
 * state-queue datum parser (state-queue-snapshot.ts) derive it. */
export const headerHashOf = (header: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(header, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

export const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const address = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

export const depositInfo = (byte: number): SDK.DepositInfo => ({
  l2_address: address(byte),
  l2_network_id: 0n,
  l2_datum: null,
});

export const withdrawalInfo = (byte: number): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(byte),
    l2_owner: h28(byte + 1),
    l2_value: new Map(),
    l1_address: address(byte + 2),
    l1_datum: "NoDatum",
  },
  signature: [h32(byte + 3), h32(byte + 4)],
  validity: "IncorrectWithdrawalSignature",
});

/**
 * The cross-language boundary corpus. Its entries are the exact canonical
 * transaction bytes the Aiken/TypeScript boundary corpus is generated from:
 * demo/midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json
 * (checked by tests/fixtures/verify-cardano-capability-p2-retained-da-v1.mjs).
 */
const CORPUS_PATH = fileURLToPath(
  new URL(
    "../../../midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json",
    import.meta.url,
  ),
);

type CorpusEntry = {
  readonly label: string;
  readonly transactionIdHex: string;
  readonly canonicalCborHex: string;
};

export const corpus = JSON.parse(readFileSync(CORPUS_PATH, "utf8")) as {
  readonly schema: string;
  readonly entries: readonly CorpusEntry[];
};

export type FixtureTransaction = {
  readonly txId: string;
  readonly canonicalCbor: Buffer;
  readonly source: SDK.L2TransactionSource;
  readonly sourceValueBytes: Buffer;
};

/** Builds a payload transaction from canonical bytes, the way the node does. */
const fixtureTransactionFromCanonicalCbor = (
  canonicalCbor: Buffer,
): FixtureTransaction => {
  const full = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
  const proofSource =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSource = {
    tx_id: computeMidgardNativeTxId(full).toString("hex"),
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId: source.tx_id,
    canonicalCbor,
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceSchema),
  };
};

export const corpusTransaction = (index: number): FixtureTransaction =>
  fixtureTransactionFromCanonicalCbor(
    Buffer.from(corpus.entries[index]!.canonicalCborHex, "hex"),
  );

export const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const bufferEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export const hex = <A>(
  value: A,
  schema: Parameters<typeof Data.to>[1],
): string => encodeData(value, schema as never).toString("hex");

export type Fixture = {
  readonly payload: SDK.DaPayload;
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly envelope: Buffer;
  readonly record: WatcherStateQueueHeader;
  readonly transactions: readonly FixtureTransaction[];
};

export const watcherHeaderRecord = (
  header: SDK.Header,
  headerHash: string,
): WatcherStateQueueHeader => ({
  headerHash,
  headerCborHex: Data.to(header, SDK.Header),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: header.prevUtxosRoot,
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
  withdrawalCount: header.withdrawalCount.toString(),
  forcedTransactionCount: header.forcedTransactionCount.toString(),
  l2TransactionCount: header.l2TransactionCount.toString(),
  depositCount: header.depositCount.toString(),
  totalEventCount: header.totalEventCount.toString(),
  transitionStepCount: header.transitionStepCount.toString(),
  validationTraceCount: header.validationTraceCount.toString(),
  startTime: header.startTime.toString(),
  endTime: header.endTime.toString(),
  blockSlot: header.blockSlot.toString(),
  expectedNetworkId: header.expectedNetworkId.toString(),
  minFeeA: header.minFeeA.toString(),
  minFeeB: header.minFeeB.toString(),
  prevHeaderHash: header.prevHeaderHash,
  operatorVkey: header.operatorVkey,
  protocolVersion: header.protocolVersion.toString(),
  daAttestationPolicyId: null,
});
