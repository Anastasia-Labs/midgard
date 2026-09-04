/**
 * W22 header/root reconstruction tests.
 *
 * PROVENANCE OF EVERY INPUT USED BY THESE TESTS (the non-circularity argument
 * these cases exist to demonstrate):
 *
 * - The expected root/count set: `WatcherStateQueueHeader`, the W14 record
 *   decoded from the L1 state-queue node datum
 *   (demo/midgard-watcher/src/indexers/state-queue-indexer.ts). In these tests
 *   the record is derived from the fixture's `Header` by re-encoding it the
 *   way the indexer does, so no test ever feeds a header field that did not
 *   come from a committed header.
 * - The header hash: never taken from the caller. It is re-derived from the
 *   header struct by `admitAuthenticatedStateQueueHeaderObservation`.
 * - The payload bytes: an argument, standing in for the exact bytes the W21
 *   canonical block store persisted from a public DA peer.
 * - The reconstruction: `reconstructDaPayload` in
 *   `@al-ft/midgard-fault-proofs`, reached through the Q03 evidence core.
 *   Nothing in the watcher recomputes a root.
 *
 * The payload's embedded header is only ever a claim. Cases in the
 * "fail-closed / non-circularity" block show it is never promoted to the
 * expected set.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  buildCountedRoot,
  encodeData,
  reconstructDaPayload,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-indexer.js";
import {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherAuthenticatedHeaderObservation,
  makeWatcherHeaderRootReconstructedState,
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
  WatcherHeaderRootReconstructionError,
} from "../../src/verification/header-root-reconstruction.js";

// ---------------------------------------------------------------------------
// Fixture construction (mirrors demo/midgard-fault-proofs/tests/helpers/
// canonical-block-evidence-fixture.ts, using only package exports)
// ---------------------------------------------------------------------------

/** The canonical header hash: blake2b-224 over the header's CBOR, exactly as
 * `hashBlockHeader` (demo/midgard-sdk/src/ledger-state.ts:467) and the W14
 * indexer (state-queue-indexer.ts:566) derive it. */
const headerHashOf = (header: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(header, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const address = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

const depositInfo = (byte: number): SDK.DepositInfo => ({
  l2_address: address(byte),
  l2_network_id: 0n,
  l2_datum: null,
});

const withdrawalInfo = (byte: number): SDK.WithdrawalInfo => ({
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

const corpus = JSON.parse(readFileSync(CORPUS_PATH, "utf8")) as {
  readonly schema: string;
  readonly entries: readonly CorpusEntry[];
};

type FixtureTransaction = {
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

const corpusTransaction = (index: number): FixtureTransaction =>
  fixtureTransactionFromCanonicalCbor(
    Buffer.from(corpus.entries[index]!.canonicalCborHex, "hex"),
  );

const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const bufferEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

const hex = <A>(value: A, schema: Parameters<typeof Data.to>[1]): string =>
  encodeData(value, schema as never).toString("hex");

type Fixture = {
  readonly payload: SDK.DaPayload;
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly envelope: Buffer;
  readonly record: WatcherStateQueueHeader;
  readonly transactions: readonly FixtureTransaction[];
};

const watcherHeaderRecord = (
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

const buildFixture = async ({
  transactions = [],
  depositBytes = [],
  withdrawalBytes = [],
}: {
  readonly transactions?: readonly FixtureTransaction[];
  readonly depositBytes?: readonly number[];
  readonly withdrawalBytes?: readonly number[];
} = {}): Promise<Fixture> => {
  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.sourceValueBytes.toString("hex"),
  ]);
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.canonicalCbor.toString("hex"),
  ]);
  const depositEntries: SDK.DaPayloadEntry[] = depositBytes.map((byte) => [
    hex(outRef(byte), SDK.OutputReferenceSchema),
    hex(depositInfo(byte), SDK.DepositInfoSchema),
  ]);
  const withdrawalEntries: SDK.DaPayloadEntry[] = withdrawalBytes.map(
    (byte) => [
      hex(outRef(byte), SDK.OutputReferenceSchema),
      hex(withdrawalInfo(byte), SDK.WithdrawalInfoSchema),
    ],
  );

  const eventKeyHex = (eventKey: SDK.EventKey): string =>
    hex(eventKey, SDK.EventKeySchema);
  const stepValueHex = (index: number, phase: SDK.TransitionPhase): string =>
    hex(
      { step_index: BigInt(index), phase } satisfies SDK.EventToStepValue,
      SDK.EventToStepValueSchema,
    );

  let stepIndex = 0;
  const eventToStepEntries: SDK.DaPayloadEntry[] = [];
  for (const byte of withdrawalBytes) {
    eventToStepEntries.push([
      eventKeyHex({
        WithdrawalEventKey: { withdrawal_id: outRef(byte) },
      }),
      stepValueHex(stepIndex++, "Withdrawal"),
    ]);
  }
  for (const tx of transactions) {
    eventToStepEntries.push([
      eventKeyHex({ L2TransactionEventKey: { tx_id: tx.txId } }),
      stepValueHex(stepIndex++, "L2Transaction"),
    ]);
  }
  for (const byte of depositBytes) {
    eventToStepEntries.push([
      eventKeyHex({ DepositEventKey: { deposit_id: outRef(byte) } }),
      stepValueHex(stepIndex++, "Deposit"),
    ]);
  }

  const validationTraceEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      eventKeyHex({ L2TransactionEventKey: { tx_id: tx.txId } }),
      hex(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptor,
        SDK.ValidationTraceDescriptorSchema,
      ),
    ],
  );

  const countedRoot = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(entries))).root;

  const counts = {
    withdrawalCount: BigInt(withdrawalEntries.length),
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactionEntries.length),
    depositCount: BigInt(depositEntries.length),
    totalEventCount: BigInt(
      withdrawalEntries.length +
        transactionEntries.length +
        depositEntries.length,
    ),
    transitionStepCount: 0n,
    validationTraceCount: BigInt(transactionEntries.length),
  };

  const header: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEntries,
    ),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      [],
    ),
    transactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactionEntries,
    ),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      [],
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepEntries,
    ),
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraceEntries,
    ),
    ...counts,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: 1n,
  };
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: sortEntries(withdrawalEntries),
      forced_transactions: [],
      transactions: sortEntries(transactionEntries),
      deposits: sortEntries(depositEntries),
      transition_trace: [],
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: sortEntries(preimageEntries),
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: sortEntries(validationTraceEntries),
      validation_trace_witnesses: [],
      counts,
    },
  };
  return {
    payload,
    header,
    headerHash,
    envelope: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    record: watcherHeaderRecord(header, headerHash),
    transactions,
  };
};

/** Re-encodes a mutated payload into a fresh envelope. */
const reencode = async (payload: SDK.DaPayload): Promise<Buffer> =>
  await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

const clonePayload = (payload: SDK.DaPayload): SDK.DaPayload => ({
  version: payload.version,
  block_body: {
    ...payload.block_body,
    counts: { ...payload.block_body.counts },
    header: { ...payload.block_body.header },
    withdrawals: [...payload.block_body.withdrawals],
    forced_transactions: [...payload.block_body.forced_transactions],
    transactions: [...payload.block_body.transactions],
    deposits: [...payload.block_body.deposits],
    transition_trace: [...payload.block_body.transition_trace],
    event_to_step: [...payload.block_body.event_to_step],
    transaction_preimages: [...payload.block_body.transaction_preimages],
    forced_transaction_preimages: [
      ...payload.block_body.forced_transaction_preimages,
    ],
    cek_program_material: [...payload.block_body.cek_program_material],
    validation_traces: [...payload.block_body.validation_traces],
    utxos: [...payload.block_body.utxos],
  },
});

const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

const observationFor = async (
  fixture: Fixture,
  overrides: {
    readonly confirmationDepth?: number;
    readonly provenance?: SDK.EvidenceProvenance;
    readonly record?: WatcherStateQueueHeader;
    readonly minimumConfirmationDepth?: number;
  } = {},
): Promise<SDK.AuthenticatedStateQueueHeaderObservation> =>
  await makeWatcherAuthenticatedHeaderObservation({
    header: overrides.record ?? fixture.record,
    chainPoint: CHAIN_POINT,
    confirmationDepth: overrides.confirmationDepth ?? 12,
    sourceMode: "local_node",
    provenance: overrides.provenance ?? L1_PROVENANCE,
    ...(overrides.minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth: overrides.minimumConfirmationDepth }),
  });

/**
 * Commits a mutated header on L1 and inside the payload at once: the operator's
 * committed header and the payload's embedded copy stay byte-identical (so the
 * evaluation reaches the root/count comparison) while exactly one header field
 * diverges from what the payload body actually contains.
 */
const commitMutatedHeader = async (
  fixture: Fixture,
  mutate: (header: SDK.Header) => SDK.Header,
): Promise<Fixture> => {
  const header = mutate(fixture.header);
  const headerHash = headerHashOf(header);
  const payload = clonePayload(fixture.payload);
  const mutated: SDK.DaPayload = {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
  return {
    ...fixture,
    payload: mutated,
    header,
    headerHash,
    envelope: await reencode(mutated),
    record: watcherHeaderRecord(header, headerHash),
  };
};

const evaluateFixture = async (
  fixture: Fixture,
  overrides: {
    readonly envelope?: Uint8Array;
    readonly daProvenance?: SDK.EvidenceProvenance;
    readonly observation?: SDK.AuthenticatedStateQueueHeaderObservation;
    readonly minimumConfirmationDepth?: number;
  } = {},
) =>
  await evaluateWatcherHeaderRootReconstruction({
    observation: overrides.observation ?? (await observationFor(fixture)),
    payloadEnvelopeCbor: overrides.envelope ?? fixture.envelope,
    daProvenance: overrides.daProvenance ?? DA_PROVENANCE,
    ...(overrides.minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth: overrides.minimumConfirmationDepth }),
  });

// ---------------------------------------------------------------------------

describe("W22 authenticated header observation (non-circular binding)", () => {
  it("admits a W14 index record and re-derives its header hash", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const observation = await observationFor(fixture);
    expect(observation.headerHash).toBe(fixture.headerHash);
    expect(observation.header).toStrictEqual(fixture.header);
    expect(observation.provenance.trustClass).toBe("authenticated_cardano_l1");
  });

  it("rejects a header hash that does not re-derive from the header fields", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, headerHash: h28(0xab) },
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });

  it("rejects a record whose datum bytes are not the re-encoding of its fields", async () => {
    const fixture = await buildFixture();
    const other = await buildFixture({ transactions: [corpusTransaction(0)] });
    await expect(
      observationFor(fixture, {
        record: {
          ...fixture.record,
          headerCborHex: other.record.headerCborHex,
        },
      }),
    ).rejects.toBeInstanceOf(WatcherHeaderRootReconstructionError);
  });

  it("rejects malformed header record fields", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, utxosRoot: "not-hex" },
      }),
    ).rejects.toMatchObject({ code: "invalid_header_record" });
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, blockSlot: "-1" },
      }),
    ).rejects.toMatchObject({ code: "invalid_header_record" });
  });

  it("rejects a confirmation depth below the required minimum", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        confirmationDepth: 2,
        minimumConfirmationDepth: 10,
      }),
    ).rejects.toMatchObject({ code: "insufficient_confirmation_depth" });
  });

  it("rejects an L1 observation that is not authenticated Cardano L1", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        provenance: {
          trustClass: "operator_private_database",
          sourceId: "operator-db",
          grade: "security",
        },
      }),
    ).rejects.toMatchObject({ code: "prohibited_trust_class" });
  });
});

describe("W22 positive reconstruction", () => {
  it("reconstructs all eight header roots for a valid block", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedRoots).toStrictEqual(result.headerRoots);
    for (const field of WATCHER_HEADER_ROOT_FIELDS) {
      expect(result.reconstructedRoots?.[field]).toMatch(/^[0-9a-f]{64}$/u);
    }
  });

  it("reconstructs all seven header counts for a valid block", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.reconstructedCounts).toStrictEqual(result.headerCounts);
    expect(result.headerCounts).toStrictEqual({
      withdrawal_count: "1",
      forced_transaction_count: "0",
      l2_transaction_count: "2",
      deposit_count: "1",
      total_event_count: "4",
      transition_step_count: "0",
      validation_trace_count: "2",
    });
    expect(WATCHER_HEADER_COUNT_FIELDS).toHaveLength(7);
  });

  it("accepts with empty mismatch lists, no reason codes, and both payload digests", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    expect(result.schemaVersion).toBe(
      WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
    );
    expect(result.reasonCodes).toStrictEqual([]);
    expect(result.rootMismatches).toStrictEqual([]);
    expect(result.countMismatches).toStrictEqual([]);
    expect(result.headerHash).toBe(fixture.headerHash);
    expect(result.payloadEnvelopeSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.payloadSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.payloadSha256).not.toBe(result.payloadEnvelopeSha256);
  });

  it("produces a stable result digest across repeated runs", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const first = await evaluateFixture(fixture);
    const second = await evaluateFixture(fixture);
    expect(second).toStrictEqual(first);
    expect(second.resultDigest).toBe(first.resultDigest);
  });

  it("changes the result digest when the outcome changes", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const accepted = await evaluateFixture(fixture);
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositsRoot: h32(0x5a),
    }));
    const rejected = await evaluateFixture(mutated);
    expect(rejected.resultDigest).not.toBe(accepted.resultDigest);
  });
});

describe("W22 adjacent boundaries", () => {
  it("accepts an empty-collection block with zero counts", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedCounts).toStrictEqual({
      withdrawal_count: "0",
      forced_transaction_count: "0",
      l2_transaction_count: "0",
      deposit_count: "0",
      total_event_count: "0",
      transition_step_count: "0",
      validation_trace_count: "0",
    });
    expect(result.reconstructedRoots?.utxos_root).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
  });

  it("accepts the one-element neighbour of the empty block", async () => {
    const empty = await buildFixture();
    const single = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const emptyResult = await evaluateFixture(empty);
    const singleResult = await evaluateFixture(single);
    expect(singleResult.action).toBe("accept");
    expect(singleResult.reconstructedRoots?.transactions_root).not.toBe(
      emptyResult.reconstructedRoots?.transactions_root,
    );
    expect(singleResult.reconstructedRoots?.deposits_root).not.toBe(
      emptyResult.reconstructedRoots?.deposits_root,
    );
    expect(singleResult.reconstructedRoots?.withdrawals_root).not.toBe(
      emptyResult.reconstructedRoots?.withdrawals_root,
    );
  });

  it("rejects a count that is off by exactly one", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositCount: header.depositCount + 1n,
    }));
    const result = await evaluateFixture(mutated);
    expect(result.action).toBe("reject");
    expect(result.countMismatches).toStrictEqual(["deposit_count"]);
  });

  it("accepts total_event_count at exactly the component sum", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.headerCounts.total_event_count).toBe("3");
  });

  it.each([
    ["above", 1n],
    ["below", -1n],
  ])(
    "rejects total_event_count one %s the component sum",
    async (_label, delta) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        totalEventCount: header.totalEventCount + delta,
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.countMismatches).toStrictEqual(["total_event_count"]);
      expect(result.rootMismatches).toStrictEqual([]);
    },
  );
});

describe("W22 per-root mismatch determinism", () => {
  const rootMutations: readonly [
    (typeof WATCHER_HEADER_ROOT_FIELDS)[number],
    keyof SDK.Header,
  ][] = [
    ["utxos_root", "utxosRoot"],
    ["withdrawals_root", "withdrawalsRoot"],
    ["forced_transactions_root", "forcedTransactionsRoot"],
    ["transactions_root", "transactionsRoot"],
    ["deposits_root", "depositsRoot"],
    ["transition_trace_root", "transitionTraceRoot"],
    ["event_to_step_root", "eventToStepRoot"],
    ["validation_traces_root", "validationTracesRoot"],
  ];

  it.each(rootMutations)(
    "reports exactly %s when that root diverges",
    async (field, headerField) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        [headerField]: h32(0xbe),
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual(["root_mismatch"]);
      expect(result.rootMismatches).toStrictEqual([field]);
      expect(result.countMismatches).toStrictEqual([]);
      expect(result.reconstructedRoots).toBeNull();
    },
  );

  it("covers every declared root field exactly once", () => {
    expect(rootMutations.map(([field]) => field)).toStrictEqual([
      ...WATCHER_HEADER_ROOT_FIELDS,
    ]);
  });
});

describe("W22 per-count mismatch determinism", () => {
  const countMutations: readonly [
    (typeof WATCHER_HEADER_COUNT_FIELDS)[number],
    keyof SDK.Header,
  ][] = [
    ["withdrawal_count", "withdrawalCount"],
    ["forced_transaction_count", "forcedTransactionCount"],
    ["l2_transaction_count", "l2TransactionCount"],
    ["deposit_count", "depositCount"],
    ["total_event_count", "totalEventCount"],
    ["transition_step_count", "transitionStepCount"],
    ["validation_trace_count", "validationTraceCount"],
  ];

  it.each(countMutations)(
    "reports exactly %s when that count diverges",
    async (field, headerField) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        [headerField]: (header[headerField] as bigint) + 7n,
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual(["count_mismatch"]);
      expect(result.countMismatches).toStrictEqual([field]);
      expect(result.rootMismatches).toStrictEqual([]);
      expect(result.reconstructedCounts).toBeNull();
    },
  );

  it("covers every declared count field exactly once", () => {
    expect(countMutations.map(([field]) => field)).toStrictEqual([
      ...WATCHER_HEADER_COUNT_FIELDS,
    ]);
  });

  it("reports the declared-versus-member count divergence separately", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
    });
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        counts: {
          ...payload.block_body.counts,
          depositCount: payload.block_body.counts.depositCount + 1n,
        },
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual([
      "count_mismatch",
      "declared_counts_member_mismatch",
    ]);
    expect(result.countMismatches).toStrictEqual(["deposit_count"]);
  });
});

describe("W22 payload-entry mutations", () => {
  it("rejects reordered payload entries", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
    });
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        transactions: [...payload.block_body.transactions].reverse(),
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["invalid_payload_entries"]);
  });

  it("rejects a duplicated source event key", async () => {
    const fixture = await buildFixture({ depositBytes: [11, 12] });
    const payload = clonePayload(fixture.payload);
    const first = payload.block_body.deposits[0]!;
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        deposits: [first, first],
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["invalid_payload_entries"]);
  });

  it("rejects two swapped transactions", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
    });
    const payload = clonePayload(fixture.payload);
    const [a, b] = payload.block_body.transactions;
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        transactions: [
          [a![0], b![1]],
          [b![0], a![1]],
        ],
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["root_mismatch"]);
    expect(result.rootMismatches).toStrictEqual(["transactions_root"]);
  });
});

describe("W22 malformed payload bytes", () => {
  it("rejects an undecodable envelope", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture, {
      envelope: Buffer.from("deadbeef", "hex"),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
    expect(result.payloadSha256).toBeNull();
  });

  it("rejects an unknown content encoding", async () => {
    const fixture = await buildFixture();
    const inner = SDK.encodeDaPayload(fixture.payload);
    // The canonical encoder refuses to emit an unknown encoding, so the wire
    // bytes are assembled directly: [version, content_encoding, inner_bytes,
    // inner_sha256, body] with an encoding the decoder must not accept.
    const envelope = encodeCbor([
      1n,
      7n,
      BigInt(inner.length),
      createHash("sha256").update(inner).digest(),
      inner,
    ]);
    const result = await evaluateFixture(fixture, { envelope });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
    expect(result.payloadSha256).toBeNull();
  });

  it("rejects an oversize payload", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture, {
      envelope: Buffer.alloc(DA_TRANSPORT_LIMITS.maxPayloadBytes + 1),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects truncated CBOR", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture, {
      envelope: fixture.envelope.subarray(0, fixture.envelope.length - 8),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects a wrong DA payload version", async () => {
    const fixture = await buildFixture();
    // `encodeDaPayload` refuses to serialise a non-V1 version, so the version
    // integer is patched on the wire. Its offset is fixed by the encoding:
    // constructor tag `d8799f` then the version integer.
    const inner = Buffer.from(SDK.encodeDaPayload(fixture.payload));
    expect(inner.subarray(0, 4).toString("hex")).toBe("d8799f01");
    inner[3] = 0x02;
    const result = await evaluateFixture(fixture, {
      envelope: await wrapDaPayload(inner, { mode: "identity" }),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects an embedded header_hash that is not the embedded header's hash", async () => {
    const fixture = await buildFixture();
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: { ...payload.block_body, header_hash: h28(0x4d) },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["payload_header_mismatch"]);
  });
});

describe("W22 fail-closed and non-circularity", () => {
  /**
   * PROVENANCE: the observation is the W14 record for block A; the payload is a
   * complete, internally consistent public payload for block B (its embedded
   * header hashes to its own header_hash, and its own roots/counts agree with
   * it). The only correct outcome is rejection, and the reported expected root
   * set must remain block A's.
   */
  it("rejects a self-consistent payload describing a different block, and never adopts its header", async () => {
    const blockA = await buildFixture({ transactions: [corpusTransaction(0)] });
    const blockB = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
    });
    const standalone = await reconstructDaPayload({
      payloadEnvelopeCbor: blockB.envelope,
    });
    expect(standalone.headerHash).toBe(blockB.headerHash);

    const result = await evaluateFixture(blockA, { envelope: blockB.envelope });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["payload_header_mismatch"]);
    expect(result.headerHash).toBe(blockA.headerHash);
    expect(result.headerRoots.transactions_root).toBe(
      blockA.header.transactionsRoot,
    );
    expect(result.headerRoots.transactions_root).not.toBe(
      blockB.header.transactionsRoot,
    );
    expect(result.reconstructedRoots).toBeNull();
    expect(result.reconstructedCounts).toBeNull();
  });

  /**
   * PROVENANCE: the header struct is taken from the payload (operator-supplied),
   * while the header hash is the real L1-observed one. Admission re-derives the
   * hash, so the pairing is refused.
   */
  it("rejects a caller header that is not the one the W14 index committed", async () => {
    const blockA = await buildFixture({ transactions: [corpusTransaction(0)] });
    const blockB = await buildFixture({ depositBytes: [11] });
    const forged: SDK.AuthenticatedStateQueueHeaderObservation = {
      schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
      sourceMode: "local_node",
      provenance: L1_PROVENANCE,
      chainPoint: CHAIN_POINT,
      confirmationDepth: 12,
      headerHash: blockA.headerHash,
      header: blockB.header,
    };
    const result = await evaluateFixture(blockA, {
      observation: forged,
      envelope: blockB.envelope,
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["header_hash_mismatch"]);
    expect(result.reconstructedRoots).toBeNull();
  });

  it("rejects an insufficient confirmation depth at evaluation time", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const observation = await observationFor(fixture, {
      confirmationDepth: 3,
    });
    const result = await evaluateFixture(fixture, {
      observation,
      minimumConfirmationDepth: 20,
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual([
      "insufficient_confirmation_depth",
    ]);
  });

  it.each([
    ["authenticated_cardano_l1", "da_evidence_wrong_trust_class"],
    ["signed_deployment_identity", "da_evidence_wrong_trust_class"],
    ["deterministic_local_computation", "da_evidence_wrong_trust_class"],
  ])(
    "rejects DA bytes carrying trust class %s",
    async (trustClass, expected) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
      });
      const result = await evaluateFixture(fixture, {
        daProvenance: {
          trustClass: trustClass as SDK.EvidenceProvenance["trustClass"],
          sourceId: "some-source",
          grade: "security",
        },
      });
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual([expected]);
    },
  );

  it("rejects operator-private DA provenance", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture, {
      daProvenance: {
        trustClass: "operator_private_database",
        sourceId: "operator-db",
        grade: "security",
      },
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["prohibited_trust_class"]);
  });

  it("never sources the expected root set from the payload", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    // The header the payload embeds is byte-identical to the L1 one here, so
    // the only way to see whose values were used is to change the L1 record and
    // observe the expected set follow it.
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      transactionsRoot: h32(0x3c),
    }));
    const result = await evaluateFixture(mutated);
    expect(result.headerRoots.transactions_root).toBe(h32(0x3c));
    expect(result.headerRoots.transactions_root).not.toBe(
      fixture.header.transactionsRoot,
    );
    expect(result.rootMismatches).toStrictEqual(["transactions_root"]);
  });
});

describe("W22 reconstructed-state durable record", () => {
  it("builds the reserved record from an accepted result and the W21 input ids", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    const record = makeWatcherHeaderRootReconstructedState({
      result,
      chainPointId: "chain-point-1",
      inputIds: ["da-payload-1", "proof-bundle-1"],
    });
    expect(record.blockHash).toBe(fixture.headerHash);
    expect(record.chainPointId).toBe("chain-point-1");
    expect(record.priorStateRoot).toBe(fixture.header.prevUtxosRoot);
    expect(record.postStateRoot).toBe(result.reconstructedRoots?.utxos_root);
    expect(record.inputIds).toStrictEqual(["da-payload-1", "proof-bundle-1"]);
    expect(record.state.sha256).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("produces the same state bytes for the same reconstruction", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const build = async () =>
      makeWatcherHeaderRootReconstructedState({
        result: await evaluateFixture(fixture),
        chainPointId: "chain-point-1",
        inputIds: ["da-payload-1"],
      });
    expect(await build()).toStrictEqual(await build());
  });

  it("refuses to build a record from a rejected result", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositsRoot: h32(0x11),
    }));
    const result = await evaluateFixture(mutated);
    expect(() =>
      makeWatcherHeaderRootReconstructedState({
        result,
        chainPointId: "chain-point-1",
        inputIds: ["da-payload-1"],
      }),
    ).toThrowError(WatcherHeaderRootReconstructionError);
  });

  it.each([
    ["empty", []],
    ["duplicated", ["da-payload-1", "da-payload-1"]],
    ["blank", [""]],
  ])("refuses %s input ids", async (_label, inputIds) => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    expect(() =>
      makeWatcherHeaderRootReconstructedState({
        result,
        chainPointId: "chain-point-1",
        inputIds: inputIds as readonly string[],
      }),
    ).toThrowError(WatcherHeaderRootReconstructionError);
  });
});

describe("W22 canonical producer agreement", () => {
  /**
   * The watcher imports the canonical reconstruction rather than reimplementing
   * it, so agreement is an identity. This asserts it for a payload whose
   * transactions are the exact cross-language corpus entries at
   * demo/midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json.
   */
  it("equals reconstructDaPayload for a cross-language corpus fixture", async () => {
    expect(corpus.schema).toBe(
      "midgard-cardano-capability-p2-boundary-corpus-v1",
    );
    const transactions = [corpusTransaction(0), corpusTransaction(1)];
    expect(transactions[0]!.txId).toBe(corpus.entries[0]!.transactionIdHex);
    const fixture = await buildFixture({ transactions });
    const producer = await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.envelope,
      expectedHeaderHash: fixture.headerHash,
      committedHeader: fixture.header,
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedRoots).toStrictEqual({
      utxos_root: producer.roots.utxosRoot,
      withdrawals_root: producer.roots.withdrawalsRoot,
      forced_transactions_root: producer.roots.forcedTransactionsRoot,
      transactions_root: producer.roots.transactionsRoot,
      deposits_root: producer.roots.depositsRoot,
      transition_trace_root: producer.roots.transitionTraceRoot,
      event_to_step_root: producer.roots.eventToStepRoot,
      validation_traces_root: producer.roots.validationTracesRoot,
    });
    expect(result.reconstructedCounts).toStrictEqual({
      withdrawal_count: producer.counts.withdrawalCount.toString(),
      forced_transaction_count:
        producer.counts.forcedTransactionCount.toString(),
      l2_transaction_count: producer.counts.l2TransactionCount.toString(),
      deposit_count: producer.counts.depositCount.toString(),
      total_event_count: producer.counts.totalEventCount.toString(),
      transition_step_count: producer.counts.transitionStepCount.toString(),
      validation_trace_count: producer.counts.validationTraceCount.toString(),
    });
  });
});
