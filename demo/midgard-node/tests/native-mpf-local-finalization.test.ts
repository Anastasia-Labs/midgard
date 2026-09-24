import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/index.js";
import { recoverNativeMpfForLocalFinalization } from "../src/services/native-mpf-local-finalization.js";
import {
  deserializeStateQueueUTxO,
  serializeStateQueueUTxO,
} from "../src/workers/utils/commit-block-header.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { provideDatabaseLayers } from "./utils.js";

// Real SQL/serialization and authority checks; source rows below are explicit
// model inputs. The native IO spy is not evidence of durable native recovery.
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      program.pipe(
        Effect.mapError((error) => new Error(formatDatabaseError(error))),
      ),
    ),
  );
const bytes = (n: number, width = 32) => Buffer.alloc(width, n);
const sha = (value: Uint8Array) => createHash("sha256").update(value).digest();
const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const binding = bytes(71);
const incarnation = bytes(72);
const eventId = Buffer.from(
  Data.to(
    { transactionId: bytes(73).toString("hex"), outputIndex: 0n },
    SDK.OutputReference,
  ),
  "hex",
);
const time = new Date("2026-06-12T00:00:00.000Z");

const serializeHeader = async (header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const credential = getAddressDetails(address).paymentCredential;
  if (credential?.type !== "Script")
    throw new Error("State queue fixture requires a script credential");
  const queueUnit = toUnit(credential.hash, assetName);
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  };
  const serialized = await Effect.runPromise(
    serializeStateQueueUTxO({
      utxo: {
        txHash: bytes(74).toString("hex"),
        outputIndex: 0,
        address,
        assets: { lovelace: 3_000_000n, [queueUnit]: 1n },
        datum: SDK.encodeLinkedListNodeView(datum),
      },
      datum,
      assetName,
    }),
  );
  // Linked-list decoding derives the key from the NFT, not the datum. Exercise
  // the exact production decoder before testing the recovery authority gate.
  const decoded = await Effect.runPromise(
    deserializeStateQueueUTxO(serialized),
  );
  expect(decoded.assetName).toBe(assetName);
  expect(decoded.utxo.assets).toEqual({
    lovelace: 3_000_000n,
    [queueUnit]: 1n,
  });
  expect(decoded.datum).toEqual(datum);
  const decodedHeader = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(decoded.datum),
  );
  expect(decodedHeader).toEqual(header);
  expect(await Effect.runPromise(SDK.hashBlockHeader(decodedHeader))).toBe(
    headerHash,
  );
  return serialized;
};

const fixture = async () => {
  const roots = {
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const expectedRoots = {
    ...roots,
    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
  };
  const header: SDK.Header = {
    ...expectedRoots,
    ...counts,
    prevUtxosRoot: roots.utxosRoot,
    startTime: BigInt(time.getTime()),
    endTime: BigInt(time.getTime() + 60_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: bytes(75, 28).toString("hex"),
    operatorVkey: bytes(76, 28).toString("hex"),
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  const signedCbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
  const transaction = CML.Transaction.from_cbor_bytes(signedCbor);
  const body = transaction.body();
  const transactionHash = CML.hash_transaction(body);
  const txHash = Buffer.from(transactionHash.to_hex(), "hex");
  transactionHash.free();
  body.free();
  transaction.free();
  const replay = {
    schema: 1 as const,
    ownerBinarySha256: bytes(77),
    baseRoot: Buffer.from(roots.utxosRoot, "hex"),
    candidateRoot: Buffer.from(roots.utxosRoot, "hex"),
    eventLog: Buffer.alloc(92),
    eventLogDigest: sha(Buffer.alloc(92)),
    eventRoots: Buffer.from(roots.utxosRoot, "hex"),
    eventCount: 1,
  };
  const input: Pending.PrepareInput = {
    headerHash,
    headerCbor: Buffer.from(Data.to(header, SDK.Header), "hex"),
    preparedTxHash: txHash,
    metadata: {
      deploymentMarker: makeDeploymentMarker(bytes(78).toString("hex")),
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
      stateQueueLeaseToken: "native-local-finalization-fixture",
      baseSnapshotId: "native-local-finalization-base",
      baseTailOutRef: `${bytes(79).toString("hex")}#0`,
      baseTailHeaderHash: bytes(75, 28),
      baseTailDatumCbor: "d87980",
      baseRoots: roots,
      blockStartTime: time,
      expectedRoots,
      expectedCounts: counts,
    },
    blockEndTime: new Date(time.getTime() + 60_000),
    depositEventIds: [eventId],
    depositEntries: [
      {
        event_id: eventId,
        event_info: Buffer.from("00", "hex"),
        inclusion_time: time,
        deposit_l1_tx_hash: bytes(73),
        ledger_tx_id: bytes(80),
        ledger_output: Buffer.from("00", "hex"),
        ledger_address: address,
        projected_header_hash: null,
        status: "awaiting",
      },
    ],
    forcedTransactionEventIds: [],
    forcedTransactionEntries: [],
    withdrawalEventIds: [],
    withdrawalEntries: [],
    mempoolTxIds: [],
    mempoolTxs: [],
    mempoolTxSourceTable: "none",
    transitionTraceMembers: [],
    eventToStepMembers: [],
    validationTraceMembers: [],
    validationTraceWitnessMembers: [],
    ledgerDelta: { spent: [], produced: [] },
    nativeMpfReplay: replay,
  };
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      // This authority-boundary fixture does not fabricate an applied-L1 receipt.
      yield* sql`INSERT INTO event_history_cursor (
      binding_digest, manifest_id, origin_receipt, origin_receipt_digest,
      anchor_hash, anchor_slot, anchor_height, anchor_snapshot_digest,
      head_hash, head_slot, head_height, snapshot_digest, revision, addresses
    ) VALUES (${binding}, ${bytes(78)}, 'explicit SQL model', ${bytes(81)},
      ${bytes(82)}, 1, 1, ${bytes(83)}, ${bytes(82)}, 1, 1, ${bytes(83)}, 0, '[]'::jsonb)`;
      yield* sql`INSERT INTO event_history_incarnations (
      binding_digest, incarnation_id, kind, event_id, event_key,
      origin_canonical, incarnation_record, incarnation_digest
    ) VALUES (${binding}, ${incarnation}, 'deposit', ${eventId}, ${bytes(84)}, true,
      'explicit SQL model', ${bytes(85)})`;
      yield* sql`INSERT INTO deposits_utxos ${sql.insert({
        ...input.depositEntries[0]!,
        history_binding_digest: binding,
        history_incarnation_id: incarnation,
      })}`;
      yield* Pending.preparePendingSubmission(input);
      yield* sql`UPDATE pending_block_finalization_deposits
      SET history_binding_digest = ${binding}, history_incarnation_id = ${incarnation}
      WHERE header_hash = ${headerHash}`;
    }),
  );
  await run(Pending.recordSignedIntent(headerHash, txHash, signedCbor));
  return {
    input,
    header,
    headerHash,
    txHash,
    replay,
    block: await serializeHeader(header),
  };
};

const snapshot = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const journal = Option.getOrThrow(yield* Pending.retrieveActive());
      const deposits =
        yield* sql`SELECT * FROM deposits_utxos ORDER BY event_id`;
      const incarnations =
        yield* sql`SELECT * FROM event_history_incarnations ORDER BY incarnation_id`;
      const cursor =
        yield* sql`SELECT * FROM event_history_cursor ORDER BY binding_digest`;
      const authority = yield* sql`SELECT * FROM event_history_authority`;
      return { journal, deposits, incarnations, cursor, authority };
    }),
  );
const nativeBoundary = () => {
  const recover = vi
    .fn<(replay: unknown) => Promise<void>>()
    .mockResolvedValue(undefined);
  return { recover, owner: { recover } as unknown as NativeMpfOwnerService };
};

beforeEach(async () => {
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      // tests/utils.ts pins this file to its disposable worker database. Reset
      // the related fixture tables together, including their FK dependents.
      yield* sql`TRUNCATE pending_block_finalizations, deposits_utxos, withdrawal_utxos,
      pending_block_finalization_deposits, pending_block_finalization_withdrawals,
      event_history_cursor, event_history_block_applications,
      event_history_live_outputs, event_history_incarnations,
      event_history_authority, event_history_replay_receipts CASCADE`;
    }),
  );
});

describe.sequential("native local-finalization authority boundary", () => {
  it("refuses persisted signed intent until canonical observation, then permits the same replay without acknowledgement", async () => {
    const state = await fixture();
    const { recover, owner } = nativeBoundary();
    const before = await snapshot();
    await expect(
      run(recoverNativeMpfForLocalFinalization(owner, state.block)),
    ).rejects.toThrow(/Signed intent alone cannot authorize/);
    expect(recover).not.toHaveBeenCalled();
    expect(await snapshot()).toEqual(before);

    await run(Pending.markObservedWaitingStability(state.headerHash, 1n));
    const observed = await snapshot();
    expect(observed.journal.submitted_tx_hash).toBeNull();
    await run(recoverNativeMpfForLocalFinalization(owner, state.block));
    expect(recover).toHaveBeenCalledOnce();
    expect(recover).toHaveBeenCalledWith({
      ...state.replay,
      ownerBinarySha256: state.replay.ownerBinarySha256.toString("hex"),
      baseRoot: state.replay.baseRoot.toString("hex"),
      candidateRoot: state.replay.candidateRoot.toString("hex"),
      eventLogDigest: state.replay.eventLogDigest.toString("hex"),
    });
    expect(await snapshot()).toEqual(observed);
  });

  it("refuses a different header even when its UTxO root matches", async () => {
    const state = await fixture();
    await run(Pending.markSubmitted(state.headerHash, state.txHash));
    const otherBlock = await serializeHeader({
      ...state.header,
      endTime: state.header.endTime + 1n,
    });
    const { recover, owner } = nativeBoundary();
    const before = await snapshot();
    await expect(
      run(recoverNativeMpfForLocalFinalization(owner, otherBlock)),
    ).rejects.toThrow(/requires its journal/);
    expect(recover).not.toHaveBeenCalled();
    expect(await snapshot()).toEqual(before);
  });

  it("refuses internally matching replay and metadata roots that differ from the confirmed header", async () => {
    const state = await fixture();
    await run(Pending.markSubmitted(state.headerHash, state.txHash));
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations
        SET expected_utxos_root = ${bytes(86).toString("hex")}, mpf_replay_candidate_root = ${bytes(86)}
        WHERE header_hash = ${state.headerHash}`;
      }),
    );
    const { recover, owner } = nativeBoundary();
    const before = await snapshot();
    await expect(
      run(recoverNativeMpfForLocalFinalization(owner, state.block)),
    ).rejects.toThrow(/replay does not match the confirmed header/);
    expect(recover).not.toHaveBeenCalled();
    expect(await snapshot()).toEqual(before);
  });

  it("refuses an orphaned retained incarnation after the identical canonical member was accepted", async () => {
    const state = await fixture();
    await run(Pending.markSubmitted(state.headerHash, state.txHash));
    const { recover, owner } = nativeBoundary();
    await run(recoverNativeMpfForLocalFinalization(owner, state.block));
    expect(recover).toHaveBeenCalledOnce();
    recover.mockClear();
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE event_history_incarnations SET origin_canonical = false
        WHERE binding_digest = ${binding} AND incarnation_id = ${incarnation}`;
      }),
    );
    const before = await snapshot();
    await expect(
      run(recoverNativeMpfForLocalFinalization(owner, state.block)),
    ).rejects.toThrow(/no longer identifies its canonical history row/);
    expect(recover).not.toHaveBeenCalled();
    expect(await snapshot()).toEqual(before);
  });

  it("propagates a synthetic native durable-root refusal without changing SQL", async () => {
    const state = await fixture();
    await run(Pending.markSubmitted(state.headerHash, state.txHash));
    const { recover, owner } = nativeBoundary();
    recover.mockRejectedValueOnce(
      new Error("synthetic durable root is neither replay base nor candidate"),
    );
    const before = await snapshot();
    await expect(
      run(recoverNativeMpfForLocalFinalization(owner, state.block)),
    ).rejects.toThrow(/synthetic durable root/);
    expect(recover).toHaveBeenCalledOnce();
    expect(await snapshot()).toEqual(before);
  });
});
