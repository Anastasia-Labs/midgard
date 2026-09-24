import { createHash, randomUUID } from "node:crypto";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Deferred, Effect, Fiber, Option } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Deposits from "../src/database/deposits.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import {
  decodeJournalIncarnation,
  encodeJournalIncarnation,
} from "../src/database/eventHistoryJournalCodec.js";
import { materializeCanonicalHistory } from "../src/database/eventHistoryMaterialization.js";
import * as ReplayReceipts from "../src/database/eventHistoryReplayReceipts.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import * as Withdrawals from "../src/database/withdrawals.js";
import { historyIncarnationEntry } from "../src/l1-event-history-entries.js";
import {
  advanceEventHistoryListReplay,
  type EventHistoryListReplay,
  joinEventHistoryListReplay,
} from "../src/l1-event-history-list-replay.js";
import { historyIncarnationDigest } from "../src/l1-event-history-provenance.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { HistoryTransition } from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import {
  HistoryProducer,
  requireCandidateHistory,
  withHistoryWrite,
} from "../src/services/event-history-producer.js";
import {
  HistoryPreparation,
  HistoryRecoverySuperseded,
} from "../src/services/event-history-recovery.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { provideDatabaseLayers } from "./utils.js";

// Real PostgreSQL transactions and strict paired snapshot decoding. Block and
// source admission are explicit model inputs, not applied-ledger/live evidence.
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const modelOriginReceipt =
  "Explicit model source replay evidence; not ledger admission";
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      program.pipe(
        Effect.tapError((error) => Effect.logError(formatDatabaseError(error))),
      ),
    ),
  );
let binding: EventHistorySourceBinding;
let initial: Journal.Checkpoint["capture"];
let pair: SDK.EventHistoryContractPair;
const rootDatum = (next: string | null = null) =>
  Data.to(
    { position: "Root", next, protected_until: 0n, payload: "RootContent" },
    SDK.EventHistoryNode,
  );
const acquire = () =>
  Authority.acquire({
    deploymentIdentity: binding.manifestId,
    ownerToken: randomUUID(),
    leaseDurationMs: 60_000,
  });
const read = async () => {
  const result = await run(Journal.load(binding));
  if (result === null) throw new Error("Missing test checkpoint");
  return result;
};
const start = async () => {
  const token = await run(acquire());
  await run(
    Authority.withRecovery(
      token,
      Journal.seed({
        binding,
        capture: initial,
        height: 1,
        originReceipt: modelOriginReceipt,
        originReceiptDigest: sha(modelOriginReceipt),
        incarnations: [],
      }),
    ),
  );
  return { token, checkpoint: await read() };
};
const probe = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_journal_l2_probe (id, classification) VALUES (${id}, 'accepted')`;
  });
const counts = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql`SELECT (SELECT count(*) FROM event_history_block_applications) AS applications, (SELECT count(*) FROM history_journal_l2_probe) AS l2`;
    }),
  );

beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  pair = SDK.requireEventHistoryContracts(contracts);
  binding = {
    digest: hash(902),
    manifestId: hash(903),
    network: "Preprod",
    endpointIdentitySha256: hash(904),
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
    genesisSha256: hash(905),
    hubAddress: contracts.hubOracle.spendingScriptAddress,
    hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    hubDatumCbor: Data.to(
      await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
      SDK.HubOracleDatum,
    ),
    deployments: {
      deposit: SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      withdrawal: SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
    },
  };
  const outputs: LedgerSnapshotOutput[] = Object.values(
    binding.deployments,
  ).map((entry, outputIndex) => ({
    txHash: hash(910),
    outputIndex,
    address: entry.address,
    assets: { lovelace: 3_000_000n, [entry.policyId]: 1n },
    datum: rootDatum(),
    hasReferenceScript: false,
  }));
  outputs.push({
    txHash: hash(911),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  });
  initial = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        point: { id: hash(1), slot: 100 },
        addresses: [
          binding.hubAddress,
          ...Object.values(binding.deployments).flatMap((entry) => [
            entry.address,
            entry.retentionAddress,
          ]),
        ],
        outputs,
      },
      binding,
    ),
  );
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE history_journal_l2_probe (id integer PRIMARY KEY, classification text NOT NULL)`;
    }),
  );
});
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts, history_journal_l2_probe`;
    }),
  ),
);
afterAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_journal_l2_probe`;
    }),
  ),
);

const prepare = async (
  checkpoint: Journal.Checkpoint,
  n: number,
  transitions: readonly HistoryTransition[] = [],
  outputs = checkpoint.capture.history.ledger.outputs,
) => {
  const block: BoundHistoryChainBlock = {
    parent: checkpoint.head.id,
    point: {
      id: hash(n),
      slot: checkpoint.head.slot + 1,
      height: checkpoint.head.height + 1,
    },
    transactions: transitions.map((transition) => ({
      txHash: transition.transactionHash,
      spends: "inputs",
      inputs: transition.consumed,
      references: [],
      collaterals: [],
      outputs: transition.produced,
      mint: {},
      withdrawals: [],
      redeemers: [],
    })),
  };
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        ...checkpoint.capture.history.ledger,
        point: { id: block.point.id, slot: block.point.slot },
        outputs,
      },
      binding,
    ),
  );
  return Journal.prepareAppend(checkpoint, block, {
    capture,
    transitions: transitions.map((transition, transactionIndex) => ({
      transactionIndex,
      transition,
    })),
  });
};
const admit = async (
  checkpoint: Journal.Checkpoint,
  n: number,
  kind: "deposit" | "withdrawal" = "deposit",
) => {
  const owner = "aa".repeat(28);
  const address = {
    paymentCredential: { PublicKeyCredential: [owner] as [string] },
    stakeCredential: null,
  };
  const id = { transactionId: hash(999), outputIndex: 0n };
  const key = await Effect.runPromise(SDK.eventHistoryKey(id));
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: { l2_address: address, l2_network_id: 0n, l2_datum: "ab" },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: owner,
                  l2_value: new Map([["", new Map([["", 5_000_000n]])]]),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: "NoDatum",
          },
        };
  const plan = SDK.prepareEventHistoryPayload(
    payload,
    address.paymentCredential,
    pair[kind].recipe,
  );
  const facts: SDK.EventHistoryFacts = {
    event_id: id,
    inclusion_time: 1_000_000n,
    location: plan.location,
    structural_lovelace: 2_000_000n,
    structural_refund_key: owner,
  };
  const root = checkpoint.capture.history.ledger.outputs.find(
    (output) => output.address === binding.deployments[kind].address,
  )!;
  const rootAfter = {
    ...root,
    txHash: hash(n + 1000),
    outputIndex: 0,
    datum: rootDatum(key),
  };
  const order: LedgerSnapshotOutput = {
    ...root,
    txHash: hash(n + 1000),
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [key] },
        next: null,
        protected_until: 0n,
        payload: { Order: { facts } },
      },
      SDK.EventHistoryNode,
    ),
    assets: {
      lovelace: 7_000_000n,
      [binding.deployments[kind].policyId + key]: 1n,
    },
  };
  const outputs = [
    ...checkpoint.capture.history.ledger.outputs.filter(
      (output) => output !== root,
    ),
    rootAfter,
    order,
  ];
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      { ...checkpoint.capture.history.ledger, outputs },
      binding,
    ),
  );
  const event = (
    kind === "deposit" ? capture.history.deposits : capture.history.withdrawals
  )[0]!;
  const transition: HistoryTransition = {
    kind,
    operation: "InsertOrder",
    transactionHash: order.txHash,
    consumed: [root],
    produced: [rootAfter, order],
    continuations: [],
    admission: {
      key,
      idCbor: Data.to(id, SDK.OutputReference),
      inclusionTime: facts.inclusion_time,
      factsCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(order.datum!, [3, 0]),
      ),
      payloadCbor: event.history.payloadCbor,
      originalAssetsCbor: Data.to(
        SDK.assetsToValue(event.originalAssets),
        SDK.Value,
      ),
      outRef: { txHash: order.txHash, outputIndex: order.outputIndex },
    },
  };
  return prepare(checkpoint, n, [transition], outputs);
};

// Explicit model source admission, as in the other journal tests. The applied
// fixture separately exercises real initialization and both retained payloads.
const replayFixture = async (nativeQuantity = 0n) => {
  const block: BoundHistoryChainBlock = {
    point: { ...initial.history.ledger.point, height: 1 },
    parent: hash(0),
    transactions: [
      {
        txHash: hash(950),
        spends: "inputs",
        inputs: [],
        references: [],
        collaterals: [],
        outputs: initial.history.ledger.outputs,
        mint:
          nativeQuantity === 0n
            ? {}
            : { ["ab".repeat(28) + "01"]: nativeQuantity },
        withdrawals: [],
        redeemers: [],
      },
    ],
  };
  const receipt = eventHistoryCanonicalJson({
    domain: "midgard-node-authenticated-history-block-v1",
    bindingDigest: binding.digest,
    block,
    creatingBodies: [],
  });
  const replay: EventHistoryListReplay = {
    bindingDigest: binding.digest,
    manifestId: binding.manifestId,
    point: block.point,
    activation: {
      point: block.point,
      parent: block.parent,
      transactionIndex: 0,
      transactionHash: block.transactions[0]!.txHash,
      receipt,
    },
    outputs: initial.history.ledger.outputs,
    incarnations: [],
    replayDigest: sha(receipt),
    blocksReplayed: 1,
  };
  const secondBlock: BoundHistoryChainBlock = {
    point: { id: hash(2), slot: 101, height: 2 },
    parent: block.point.id,
    transactions: [],
  };
  const second = advanceEventHistoryListReplay({
    previous: replay,
    block: secondBlock,
    binding,
    histories: pair,
    slotToUnixTime: (slot) => slot * 1000,
    getCreatingBody: () => undefined,
    maximumBodyBytes: 16384,
  });
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        ...initial.history.ledger,
        point: { id: secondBlock.point.id, slot: secondBlock.point.slot },
      },
      binding,
    ),
  );
  const joined = joinEventHistoryListReplay({
    state: second.state,
    capture,
    binding,
  });
  return {
    first: {
      binding,
      block,
      receipt,
      replay,
      maximumReceiptBytes: 16 * 1024 * 1024,
    },
    second: {
      binding,
      block: secondBlock,
      receipt: second.receipt,
      replay: second.state,
      maximumReceiptBytes: 16 * 1024 * 1024,
    },
    seed: { binding, ...joined },
  };
};

describe("retained authenticated replay before journal seed", () => {
  it("retains exact integer tokens above Number precision through the range and journal reload", async () => {
    const quantity = 9_007_199_254_740_993n;
    const fixture = await replayFixture(quantity);
    expect(fixture.first.receipt).toContain(`:${quantity.toString()}`);
    expect(fixture.first.receipt).not.toContain(`:"${quantity.toString()}"`);
    const token = await run(acquire());
    for (const step of [fixture.first, fixture.second]) {
      await run(Authority.withRecovery(token, ReplayReceipts.put(step)));
      // Idempotent replay also decodes the persisted large-quantity receipt.
      await run(Authority.withRecovery(token, ReplayReceipts.put(step)));
    }
    await run(Authority.withRecovery(token, Journal.seed(fixture.seed)));
    const loaded = await read();
    expect(loaded.originReceipt).toBe(fixture.seed.originReceipt);
    expect(loaded.originReceipt).toContain(quantity.toString());
    expect(loaded.originReceipt).not.toContain((quantity - 1n).toString());
    const stored = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          receipt: string;
        }>`SELECT receipt FROM event_history_replay_receipts WHERE block_hash = ${Buffer.from(fixture.first.block.point.id, "hex")}`;
      }),
    );
    expect(stored[0]!.receipt).toBe(fixture.first.receipt);
  });

  it("requires the owner, exact range parent, and receipt bounds before writing a chunk", async () => {
    const fixture = await replayFixture();
    await expect(run(ReplayReceipts.put(fixture.first))).rejects.toThrow(
      /owned recovery/,
    );
    const token = await run(acquire());
    await expect(
      run(Authority.withRecovery(token, ReplayReceipts.put(fixture.second))),
    ).rejects.toThrow();
    await expect(
      run(
        Authority.withRecovery(
          token,
          ReplayReceipts.put({ ...fixture.first, maximumReceiptBytes: 1 }),
        ),
      ),
    ).rejects.toThrow();
    await expect(
      run(
        Authority.withRecovery(
          token,
          ReplayReceipts.put({
            ...fixture.first,
            replay: { ...fixture.first.replay, manifestId: hash(1234) },
          }),
        ),
      ),
    ).rejects.toThrow();
    const stored = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql`SELECT block_hash FROM event_history_replay_receipts`;
      }),
    );
    expect(stored).toEqual([]);
  });

  it("commits each chunk separately, refuses incomplete seed and retains the whole range on restart", async () => {
    const fixture = await replayFixture();
    const token = await run(acquire());
    const seed = () =>
      run(Authority.withRecovery(token, Journal.seed(fixture.seed)));
    await expect(seed()).rejects.toThrow();
    expect(await run(Journal.load(binding))).toBeNull();
    await run(Authority.withRecovery(token, ReplayReceipts.put(fixture.first)));
    await expect(seed()).rejects.toThrow();
    await run(
      Authority.withRecovery(token, ReplayReceipts.put(fixture.second)),
    );
    for (const step of [fixture.first, fixture.second])
      await run(Authority.withRecovery(token, ReplayReceipts.put(step)));
    await seed();
    const loaded = await read();
    expect(loaded.originReceipt).toBe(fixture.seed.originReceipt);
    expect(loaded.head).toEqual(fixture.second.block.point);
    expect(loaded.capture.snapshotDigest).toBe(
      fixture.seed.capture.snapshotDigest,
    );
    expect(
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`SELECT blocks_replayed::text FROM event_history_replay_receipts ORDER BY block_height`;
        }),
      ),
    ).toEqual([{ blocks_replayed: "1" }, { blocks_replayed: "2" }]);
    // Restrictive links prevent loss of the initialization chunk even before
    // any recovery material is consumed by the final journal.
    await expect(
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM event_history_replay_receipts WHERE block_hash = ${Buffer.from(fixture.first.block.point.id, "hex")}`;
        }),
      ),
    ).rejects.toThrow();
  });

  it("refuses conflicting receipt bytes and incorrect rolling digests without replacing persisted data", async () => {
    const fixture = await replayFixture();
    const token = await run(acquire());
    await run(Authority.withRecovery(token, ReplayReceipts.put(fixture.first)));
    await expect(
      run(
        Authority.withRecovery(
          token,
          ReplayReceipts.put({
            ...fixture.second,
            replay: { ...fixture.second.replay, replayDigest: hash(9998) },
          }),
        ),
      ),
    ).rejects.toThrow();
    const changed = fixture.first.receipt.replace(
      '"creatingBodies":[]',
      '"creatingBodies":[{"bodyCbor":"a0","txHash":"' + hash(123) + '"}]',
    );
    expect(changed).not.toBe(fixture.first.receipt);
    await expect(
      run(
        Authority.withRecovery(
          token,
          ReplayReceipts.put({ ...fixture.first, receipt: changed }),
        ),
      ),
    ).rejects.toThrow();
    await run(
      Authority.withRecovery(token, ReplayReceipts.put(fixture.second)),
    );
    await run(Authority.withRecovery(token, Journal.seed(fixture.seed)));
    expect((await read()).originReceipt).toBe(fixture.seed.originReceipt);
  });

  it("refuses a corrupt saved replay endpoint on restart", async () => {
    const fixture = await replayFixture();
    const token = await run(acquire());
    for (const step of [fixture.first, fixture.second])
      await run(Authority.withRecovery(token, ReplayReceipts.put(step)));
    await run(Authority.withRecovery(token, Journal.seed(fixture.seed)));
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE event_history_replay_receipts SET receipt = receipt || ' ' WHERE block_hash = ${Buffer.from(fixture.second.block.point.id, "hex")}`;
      }),
    );
    await expect(read()).rejects.toThrow(/retained history replay receipt/);
  });

  it("supports a one-block origin and refuses substitution of its activation receipt", async () => {
    const fixture = await replayFixture();
    const token = await run(acquire());
    await run(Authority.withRecovery(token, ReplayReceipts.put(fixture.first)));
    const joined = joinEventHistoryListReplay({
      state: fixture.first.replay,
      capture: initial,
      binding,
    });
    const origin = JSON.parse(joined.originReceipt) as {
      activation: { receipt: string };
    };
    origin.activation.receipt += " ";
    const different = eventHistoryCanonicalJson(origin);
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.seed({
            ...joined,
            binding,
            originReceipt: different,
            originReceiptDigest: sha(different),
          }),
        ),
      ),
    ).rejects.toThrow();
    await run(
      Authority.withRecovery(token, Journal.seed({ ...joined, binding })),
    );
    expect((await read()).originReceipt).toBe(joined.originReceipt);
  });
});

describe("durable paired history journal", () => {
  it("requires the owned recovery transaction, preserves restart state and refuses reseeding", async () => {
    const token = await run(acquire());
    const seed = Journal.seed({
      binding,
      capture: initial,
      height: 1,
      originReceipt: modelOriginReceipt,
      originReceiptDigest: sha(modelOriginReceipt),
      incarnations: [],
    });
    await expect(run(seed)).rejects.toThrow(/owned recovery transaction/);
    await expect(
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql.withTransaction(seed);
        }),
      ),
    ).rejects.toThrow(/owned recovery transaction/);
    await run(Authority.withRecovery(token, seed));
    const checkpoint = await read();
    expect(checkpoint.originReceipt).toBe(modelOriginReceipt);
    expect(checkpoint.originReceiptDigest).toBe(sha(modelOriginReceipt));
    expect(checkpoint.revision).toBe("0");
    expect(checkpoint.capture.snapshotDigest).toBe(initial.snapshotDigest);
    expect(
      Object.isFrozen(checkpoint.capture.history.ledger.outputs[0]!.assets),
    ).toBe(true);
    await expect(run(Authority.withRecovery(token, seed))).rejects.toThrow();
    await run(
      Authority.publishReady(token, {
        point: checkpoint.capture.history.ledger.point,
        snapshotDigest: checkpoint.capture.snapshotDigest,
      }),
    );
    const next = await run(
      Authority.acquire({
        deploymentIdentity: binding.manifestId,
        ownerToken: token.ownerToken,
        leaseDurationMs: 60_000,
      }),
    );
    expect(next.generation).not.toBe(token.generation);
    expect(await read()).toEqual(checkpoint);
  });

  it("persists origin evidence and seed projection atomically with dependent SQL", async () => {
    const token = await run(acquire());
    await expect(
      run(
        Authority.withRecovery(
          token,
          Effect.gen(function* () {
            yield* Journal.seed({
              binding,
              capture: initial,
              height: 1,
              originReceipt: modelOriginReceipt,
              originReceiptDigest: sha(modelOriginReceipt),
              incarnations: [],
            });
            yield* probe(99);
            return yield* Effect.fail(
              new Error("dependent materialization failed"),
            );
          }),
        ),
      ),
    ).rejects.toThrow(/dependent materialization failed/);
    expect(await run(Journal.load(binding))).toBeNull();
    expect(await counts()).toEqual([{ applications: "0", l2: "0" }]);
  });

  it("refuses a seed whose supplied receipt disagrees with its digest", async () => {
    const token = await run(acquire());
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.seed({
            binding,
            capture: initial,
            height: 1,
            originReceipt: modelOriginReceipt,
            originReceiptDigest: hash(901),
            incarnations: [],
          }),
        ),
      ),
    ).rejects.toThrow(/origin receipt digest disagrees/);
    expect(await run(Journal.load(binding))).toBeNull();
  });

  it("refuses corrupted retained origin evidence on restart", async () => {
    await start();
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE event_history_cursor SET origin_receipt = 'corrupted' WHERE binding_digest = ${Buffer.from(binding.digest, "hex")}`;
      }),
    );
    await expect(read()).rejects.toThrow(
      /Stored origin receipt digest disagrees/,
    );
  });

  it("appends exactly once, rejects stale siblings, undoes and reapplies with monotone revisions", async () => {
    const { token, checkpoint } = await start();
    const first = await prepare(checkpoint, 2);
    const sibling = await prepare(checkpoint, 3);
    expect(
      await run(
        Authority.withRecovery(token, Journal.append(binding, first, probe(1))),
      ),
    ).toMatchObject({ applied: true, revision: "1" });
    expect(
      await run(
        Authority.withRecovery(token, Journal.append(binding, first, probe(1))),
      ),
    ).toEqual({ applied: false, revision: "1" });
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.append(binding, sibling, probe(2)),
        ),
      ),
    ).rejects.toThrow(/revision or head changed/);
    const head = await read();
    await run(
      Authority.withRecovery(
        token,
        Journal.undoHead(binding, head, Effect.void),
      ),
    );
    const rolled = await read();
    expect(rolled.head).toEqual(checkpoint.head);
    expect(rolled.revision).toBe("2");
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, rolled, Effect.void),
        ),
      ),
    ).rejects.toThrow(/retained replay anchor/);
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.append(binding, first, Effect.void),
        ),
      ),
    ).rejects.toThrow(/revision or head changed/);
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, await prepare(rolled, 2), probe(2)),
      ),
    );
    expect((await read()).headApplicationRevision).toBe("3");
    expect(await counts()).toEqual([{ applications: "2", l2: "2" }]);
  });

  it("serializes competing siblings and rolls back a lease-expired journal write", async () => {
    const { token, checkpoint } = await start();
    const siblings = await Promise.all([
      prepare(checkpoint, 2),
      prepare(checkpoint, 3),
    ]);
    const outcomes = await run(
      Effect.all(
        siblings.map((prepared, index) =>
          Effect.either(
            Authority.withRecovery(
              token,
              Journal.append(binding, prepared, probe(index + 1)),
            ),
          ),
        ),
        { concurrency: 2 },
      ),
    );
    expect(outcomes.filter((outcome) => outcome._tag === "Right")).toHaveLength(
      1,
    );
    expect(outcomes.filter((outcome) => outcome._tag === "Left")).toHaveLength(
      1,
    );
    expect(await counts()).toEqual([{ applications: "1", l2: "1" }]);
    const head = await read();
    const next = await prepare(head, 4);
    const expire = Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() - interval '1 second'`;
    });
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.append(binding, next, probe(3).pipe(Effect.zipRight(expire))),
        ),
      ),
    ).rejects.toThrow(/lease expired/);
    expect(await read()).toEqual(head);
    expect(await counts()).toEqual([{ applications: "1", l2: "1" }]);
  });

  it("rolls back append and undo together with a failed final L2 repair", async () => {
    const { token, checkpoint } = await start();
    const first = await admit(checkpoint, 2);
    const failure = probe(1).pipe(
      Effect.zipRight(Effect.fail(new Error("last mutation failed"))),
    );
    await expect(
      run(
        Authority.withRecovery(token, Journal.append(binding, first, failure)),
      ),
    ).rejects.toThrow(/last mutation failed/);
    expect(await read()).toEqual(checkpoint);
    expect(await counts()).toEqual([{ applications: "0", l2: "0" }]);
    await run(
      Authority.withRecovery(token, Journal.append(binding, first, probe(1))),
    );
    const head = await read();
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(
            binding,
            head,
            probe(2).pipe(
              Effect.zipRight(Effect.fail(new Error("repair failed"))),
            ),
          ),
        ),
      ),
    ).rejects.toThrow(/repair failed/);
    expect(await read()).toEqual(head);
    expect(await counts()).toEqual([{ applications: "1", l2: "1" }]);
  });

  it.each(["deposit", "withdrawal"] as const)(
    "retains %s orphans on undo and uses application-specific before-images on reapplication",
    async (kind) => {
      const { token, checkpoint } = await start();
      const first = await admit(checkpoint, 2, kind);
      expect(Object.isFrozen(first.undo.outputs)).toBe(true);
      expect(Object.isFrozen(first.undo.outputs[0])).toBe(true);
      await run(
        Authority.withRecovery(token, Journal.append(binding, first, probe(1))),
      );
      const admitted = await read();
      const id = admitted.incarnations[0]!.id;
      await run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, admitted, Effect.void),
        ),
      );
      const orphan = await read();
      expect(orphan.incarnations[0]).toMatchObject({ id, placement: null });
      const replay = await admit(orphan, 2, kind);
      expect(replay.receipt).toBe(first.receipt);
      expect(replay.undo).not.toEqual(first.undo);
      await run(
        Authority.withRecovery(
          token,
          Journal.append(binding, replay, Effect.void),
        ),
      );
      expect((await read()).incarnations).toEqual(admitted.incarnations);
      expect(await counts()).toEqual([{ applications: "2", l2: "1" }]);
      await run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, await read(), Effect.void),
        ),
      );
      const branch = await admit(await read(), 20, kind);
      await run(
        Authority.withRecovery(
          token,
          Journal.append(binding, branch, Effect.void),
        ),
      );
      const origins = (await read()).incarnations;
      expect(origins).toHaveLength(2);
      expect(origins.filter((value) => value.placement !== null)).toHaveLength(
        1,
      );
      expect(origins.find((value) => value.placement !== null)!.id).not.toBe(
        id,
      );
    },
  );

  it.each(["deposit", "withdrawal"] as const)(
    "restores %s pointer and retirement images without reverting later L2 classification",
    async (kind) => {
      const { token, checkpoint } = await start();
      const admission = await admit(checkpoint, 2, kind);
      await run(
        Authority.withRecovery(
          token,
          Journal.append(binding, admission, probe(1)),
        ),
      );
      const admitted = await read();
      const event = admitted.incarnations[0]!.event;
      const order = admitted.capture.history.ledger.outputs.find(
        (output) =>
          output.txHash === event.outRef.txHash &&
          output.outputIndex === event.outRef.outputIndex,
      )!;
      const moved = { ...order, txHash: hash(1003) };
      const continuation: HistoryTransition = {
        kind,
        operation: "InsertFiller",
        transactionHash: moved.txHash,
        consumed: [order],
        produced: [moved],
        continuations: [{ key: event.key, before: order, after: moved }],
      };
      await run(
        Authority.withRecovery(
          token,
          Journal.append(
            binding,
            await prepare(
              admitted,
              3,
              [continuation],
              admitted.capture.history.ledger.outputs.map((output) =>
                output === order ? moved : output,
              ),
            ),
            Effect.void,
          ),
        ),
      );
      const continued = await read();
      expect(continued.incarnations[0]!.event).toEqual(event);
      expect(continued.incarnations[0]!.placement!.current!.outRef.txHash).toBe(
        moved.txHash,
      );
      const root = continued.capture.history.ledger.outputs.find(
        (output) =>
          output.address === binding.deployments[kind].address &&
          output.outputIndex === 0,
      )!;
      const rootAfter = { ...root, txHash: hash(1004), datum: rootDatum() };
      const retirement: HistoryTransition = {
        kind,
        operation: "RetireOrder",
        transactionHash: rootAfter.txHash,
        consumed: [root, moved],
        produced: [rootAfter],
        continuations: [],
        retirement: {
          event: {
            ...event,
            outRef: { txHash: moved.txHash, outputIndex: moved.outputIndex },
          },
          reason: kind === "deposit" ? "absorbed" : "payout_initialized",
          observerRedeemerIndex: 1,
          witnessCbor: "d87980",
        },
      };
      const retiredOutputs = [
        ...continued.capture.history.ledger.outputs.filter(
          (output) => output.address !== binding.deployments[kind].address,
        ),
        rootAfter,
      ];
      await run(
        Authority.withRecovery(
          token,
          Journal.append(
            binding,
            await prepare(continued, 4, [retirement], retiredOutputs),
            Effect.void,
          ),
        ),
      );
      const retired = await read();
      expect(retired.incarnations[0]!.placement!.current).toBeNull();
      expect(retired.incarnations[0]!.placement!.admission).toEqual(
        admitted.incarnations[0]!.placement!.admission,
      );
      await expect(admit(retired, 5, kind)).rejects.toThrow(
        /including a retired origin/,
      );
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE history_journal_l2_probe SET classification = 'settled' WHERE id = 1`;
        }),
      );
      await run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, retired, Effect.void),
        ),
      );
      expect((await read()).incarnations).toEqual(continued.incarnations);
      await run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, await read(), Effect.void),
        ),
      );
      expect((await read()).incarnations).toEqual(admitted.incarnations);
      expect(
        await run(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            return yield* sql`SELECT classification FROM history_journal_l2_probe`;
          }),
        ),
      ).toEqual([{ classification: "settled" }]);
    },
  );

  it("retains same-block admission/retirement history while undoing only net UTxO effects", async () => {
    const { token, checkpoint } = await start();
    const admitted = await admit(checkpoint, 2);
    const admission = admitted.projection.transitions[0]!.transition;
    const event = admission.admission!;
    const root = admitted.projection.capture.history.ledger.outputs.find(
      (output) =>
        output.address === binding.deployments.deposit.address &&
        output.outputIndex === 0,
    )!;
    const terminalRoot = { ...root, txHash: hash(1003), datum: rootDatum() };
    const retirement: HistoryTransition = {
      kind: "deposit",
      operation: "RetireOrder",
      transactionHash: terminalRoot.txHash,
      consumed: [root, event.outRef],
      produced: [terminalRoot],
      continuations: [],
      retirement: {
        event,
        reason: "absorbed",
        observerRedeemerIndex: 0,
        witnessCbor: "d87980",
      },
    };
    const outputs = [
      ...checkpoint.capture.history.ledger.outputs.filter(
        (output) => output.address !== binding.deployments.deposit.address,
      ),
      terminalRoot,
    ];
    const combined = await prepare(
      checkpoint,
      2,
      [admission, retirement],
      outputs,
    );
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, combined, Effect.void),
      ),
    );
    const retired = await read();
    expect(retired.capture.history.deposits).toHaveLength(0);
    expect(
      retired.incarnations[0]!.placement!.retirement!.at.transactionIndex,
    ).toBe(1);
    await run(
      Authority.withRecovery(
        token,
        Journal.undoHead(binding, retired, Effect.void),
      ),
    );
    const restored = await read();
    expect(restored.capture.snapshotDigest).toBe(
      checkpoint.capture.snapshotDigest,
    );
    expect(restored.incarnations[0]!.placement).toBeNull();
    await run(
      Authority.withRecovery(
        token,
        Journal.append(
          binding,
          await prepare(restored, 2, [admission, retirement], outputs),
          Effect.void,
        ),
      ),
    );
    expect((await read()).incarnations).toEqual(retired.incarnations);
  });

  it("checks indexed records and immutable facts even when local digests were recomputed", async () => {
    const { token, checkpoint } = await start();
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, await admit(checkpoint, 2), Effect.void),
      ),
    );
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{
          incarnation_record: string;
        }>`SELECT incarnation_record FROM event_history_incarnations`;
        const value = decodeJournalIncarnation(rows[0]!.incarnation_record);
        const changed = {
          ...value,
          event: { ...value.event, originalAssetsCbor: "01" },
        };
        yield* sql`UPDATE event_history_incarnations SET incarnation_record = ${encodeJournalIncarnation(changed)}, incarnation_digest = ${Buffer.from(historyIncarnationDigest(changed), "hex")}`;
      }),
    );
    await expect(read()).rejects.toThrow(/immutable facts disagree/);
  });

  it("binds retained receipt fields to indexed canonical ancestry", async () => {
    const { token, checkpoint } = await start();
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, await prepare(checkpoint, 2), Effect.void),
      ),
    );
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{
          ledger_receipt: string;
        }>`SELECT ledger_receipt FROM event_history_block_applications`;
        const receipt = JSON.parse(rows[0]!.ledger_receipt);
        receipt.block.parent = hash(888);
        const changed = JSON.stringify(receipt);
        yield* sql`UPDATE event_history_block_applications SET ledger_receipt = ${changed}, ledger_receipt_digest = ${Buffer.from(sha(changed), "hex")}`;
      }),
    );
    await expect(read()).rejects.toThrow(/receipt and application columns/);
  });

  it("keeps a coherent reader behind an uncommitted append and refuses a superseded owner", async () => {
    const { token, checkpoint } = await start();
    const prepared = await prepare(checkpoint, 2);
    await run(
      Effect.gen(function* () {
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        const writer = yield* Effect.fork(
          Authority.withRecovery(
            token,
            Journal.append(
              binding,
              prepared,
              Deferred.succeed(entered, undefined).pipe(
                Effect.zipRight(Deferred.await(finish)),
              ),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const reader = yield* Effect.fork(Journal.load(binding));
        yield* Effect.yieldNow();
        expect(Option.isNone(yield* Fiber.poll(reader))).toBe(true);
        yield* Deferred.succeed(finish, undefined);
        yield* Fiber.join(writer);
        const loaded = yield* Fiber.join(reader);
        expect(loaded?.head.id).toBe(hash(2));
      }),
    );
    await run(Authority.beginRecovery(token, "replacement"));
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, await read(), Effect.void),
        ),
      ),
    ).rejects.toThrow(/generation or owner changed/);
  });
});

it.each(["deposit", "withdrawal"] as const)(
  "materializes canonical %s admission atomically and refuses orphan credit",
  async (kind) => {
    const { token, checkpoint } = await start();
    const prepared = await admit(checkpoint, 2, kind);
    const reconcile = (before: Journal.Checkpoint) =>
      Journal.load(binding).pipe(
        Effect.flatMap((after) =>
          after === null
            ? Effect.die("missing checkpoint")
            : materializeCanonicalHistory(
                { kind: "forward", before, after },
                "Preprod",
              ),
        ),
      );
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, prepared, reconcile(checkpoint)),
      ),
    );
    const admitted = await read();
    const table = kind === "deposit" ? "deposits_utxos" : "withdrawal_utxos";
    const rows = () =>
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            history_binding_digest: Buffer;
            history_incarnation_id: Buffer;
            status: string;
          }>`SELECT event_id, history_binding_digest, history_incarnation_id, status FROM ${sql(table)}`;
        }),
      );
    let materialized = await rows();
    expect(materialized).toHaveLength(1);
    expect(materialized[0]!.history_binding_digest.toString("hex")).toBe(
      binding.digest,
    );
    expect(materialized[0]!.history_incarnation_id.toString("hex")).toBe(
      admitted.incarnations[0]!.id,
    );
    expect(materialized[0]!.status).toBe("awaiting");
    await run(
      Authority.withRecovery(
        token,
        materializeCanonicalHistory(
          { kind: "resume", before: admitted, after: admitted },
          "Preprod",
        ),
      ),
    );
    expect(await rows()).toEqual(materialized);
    // Explicit modeled retained-header dependency. An unassigned awaiting row
    // is now repairable; this fixture must not manufacture correction authority.
    await run(
      Authority.withRecovery(
        token,
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          if (kind === "deposit") {
            yield* sql`UPDATE deposits_utxos SET status = 'projected', projected_header_hash = ${Buffer.alloc(28, 0x71)}`;
          } else {
            yield* sql`UPDATE withdrawal_utxos SET status = 'projected', projected_header_hash = ${Buffer.alloc(28, 0x71)}, settlement_event_info = raw_event_info, validity = 'WithdrawalIsValid'`;
          }
        }),
      ),
    );
    materialized = await rows();
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, admitted, reconcile(admitted)),
        ),
      ),
    ).rejects.toThrow(
      "Orphan admission has retained header membership requiring authenticated published correction",
    );
    expect((await read()).head).toEqual(admitted.head);
    expect(await rows()).toEqual(materialized);
    expect((await run(Authority.retrieve)).pipe(Option.getOrThrow).state).toBe(
      "recovering",
    );
  },
);

it("requires exact Ready coverage for candidates even inside an owned transaction", async () => {
  const { token, checkpoint } = await start();
  const permit = {
    token,
    coverage: {
      bindingDigest: binding.digest,
      checkpointRevision: checkpoint.revision,
      point: checkpoint.head,
      snapshotDigest: checkpoint.capture.snapshotDigest,
      includedThroughMs: checkpoint.head.slot,
    },
  };
  await run(
    Authority.publishReady(token, {
      point: checkpoint.head,
      snapshotDigest: checkpoint.capture.snapshotDigest,
    }),
  );
  const candidate = (value: typeof permit) =>
    requireCandidateHistory.pipe(Effect.provideService(HistoryProducer, value));
  expect(
    Option.isSome(await run(Authority.withReady(token, candidate(permit)))),
  ).toBe(true);
  const rejects = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
    run(
      program.pipe(
        Effect.mapError((error) => new Error(formatDatabaseError(error))),
      ),
    );
  await expect(
    rejects(Authority.withReady(token, requireCandidateHistory)),
  ).rejects.toThrow(/no checked producer context/);
  await expect(
    rejects(
      Authority.withReady(
        token,
        candidate({
          ...permit,
          coverage: { ...permit.coverage, checkpointRevision: "999" },
        }),
      ),
    ),
  ).rejects.toThrow(/coverage changed/);
  await expect(rejects(withHistoryWrite(Effect.void))).rejects.toThrow(
    /cannot bypass an acquired history owner/,
  );
  const recovery = await run(
    Authority.beginRecovery(token, "candidate refusal"),
  );
  await expect(
    rejects(
      Authority.withRecovery(
        recovery,
        candidate({ ...permit, token: recovery }),
      ),
    ),
  ).rejects.toThrow(/Ready producer transaction/);
});

it("serializes an explicit unowned fixture against the first owner claim", async () => {
  await run(
    Effect.gen(function* () {
      const entered = yield* Deferred.make<void>();
      const finish = yield* Deferred.make<void>();
      const attempting = yield* Deferred.make<void>();
      const fixture = yield* Effect.fork(
        withHistoryWrite(
          Deferred.succeed(entered, undefined).pipe(
            Effect.zipRight(Deferred.await(finish)),
          ),
        ),
      );
      yield* Deferred.await(entered);
      const claimant = yield* Effect.fork(
        Deferred.succeed(attempting, undefined).pipe(
          Effect.zipRight(acquire()),
        ),
      );
      yield* Deferred.await(attempting);
      yield* Effect.yieldNow();
      expect(Option.isNone(yield* Fiber.poll(claimant))).toBe(true);
      yield* Deferred.succeed(finish, undefined);
      yield* Fiber.join(fixture);
      yield* Fiber.join(claimant);
      const refused = yield* Effect.either(withHistoryWrite(Effect.void));
      expect(refused._tag).toBe("Left");
    }),
  );
});

it.each(["deposit", "withdrawal"] as const)(
  "refuses Ready-producer %s polling inserts and admits canonical recovery",
  async (kind) => {
    const { token, checkpoint } = await start();
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, await admit(checkpoint, 2, kind), Effect.void),
      ),
    );
    const admitted = await read();
    const converted = await Effect.runPromise(
      historyIncarnationEntry(admitted.incarnations[0]!, "Preprod"),
    );
    const insert =
      converted.kind === "deposit"
        ? Deposits.insertEntries([converted.entry])
        : Withdrawals.insertEntries([converted.entry]);
    await run(
      Authority.publishReady(token, {
        point: admitted.head,
        snapshotDigest: admitted.capture.snapshotDigest,
      }),
    );
    const permit = {
      token,
      coverage: {
        bindingDigest: binding.digest,
        checkpointRevision: admitted.revision,
        point: admitted.head,
        snapshotDigest: admitted.capture.snapshotDigest,
        includedThroughMs: admitted.head.slot,
      },
    };
    await expect(
      run(
        insert.pipe(
          Effect.provideService(HistoryProducer, permit),
          Effect.mapError((error) => new Error(formatDatabaseError(error))),
        ),
      ),
    ).rejects.toThrow(/owned recovery transaction/);
    const count = () =>
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`SELECT event_id FROM ${sql(kind === "deposit" ? "deposits_utxos" : "withdrawal_utxos")}`;
        }),
      );
    expect(await count()).toEqual([]);
    const recovery = await run(
      Authority.beginRecovery(token, "canonical ingestion"),
    );
    await run(
      Authority.withRecovery(
        recovery,
        materializeCanonicalHistory(
          { kind: "resume", before: admitted, after: admitted },
          "Preprod",
        ),
      ),
    );
    expect(await count()).toHaveLength(1);
    await expect(
      run(insert.pipe(Effect.provideService(HistoryProducer, permit))),
    ).rejects.toThrow();
    expect(await count()).toHaveLength(1);
  },
);

it("rolls back bounded preparation writes when source preparation is superseded", async () => {
  const { token } = await start();
  let checks = 0;
  const preparation = {
    token,
    assertCurrent: Effect.suspend(() =>
      ++checks === 1
        ? Effect.void
        : Effect.fail(
            new HistoryRecoverySuperseded({
              message: "source advanced during bounded preparation write",
            }),
          ),
    ),
  };
  const insert = Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_journal_l2_probe VALUES (7, 'preparing')`;
  });
  await expect(
    run(
      withHistoryWrite(insert).pipe(
        Effect.provideService(HistoryPreparation, preparation),
      ),
    ),
  ).rejects.toThrow();
  expect(checks).toBe(2);
  expect(
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql`SELECT * FROM history_journal_l2_probe`;
      }),
    ),
  ).toEqual([]);
});

it.each(["deposit", "withdrawal"] as const)(
  "checks retained %s journal incarnation before atomic effects",
  async (kind) => {
    const { token, checkpoint } = await start();
    await run(
      Authority.withRecovery(
        token,
        Journal.append(binding, await admit(checkpoint, 2, kind), Effect.void),
      ),
    );
    const admitted = await read();
    await run(
      Authority.withRecovery(
        token,
        materializeCanonicalHistory(
          { kind: "resume", before: admitted, after: admitted },
          "Preprod",
        ),
      ),
    );
    const incarnation = admitted.incarnations[0]!;
    const converted = await Effect.runPromise(
      historyIncarnationEntry(incarnation, "Preprod"),
    );
    const member = {
      [Pending.MemberColumns.MEMBER_ID]: converted.entry.event_id,
      history_binding_digest: Buffer.from(binding.digest, "hex"),
      history_incarnation_id: Buffer.from(incarnation.id, "hex"),
    };
    const members = (value: typeof member) => ({
      depositMembers: kind === "deposit" ? [value] : [],
      withdrawalMembers: kind === "withdrawal" ? [value] : [],
    });
    await run(
      Authority.withRecovery(
        token,
        Pending.assertCanonicalEventMembers(members(member)),
      ),
    );
    for (const wrong of [
      { ...member, history_incarnation_id: Buffer.from(hash(999), "hex") },
      { ...member, history_binding_digest: Buffer.from(hash(998), "hex") },
    ]) {
      await expect(
        run(
          Authority.withRecovery(
            token,
            probe(17).pipe(
              Effect.zipRight(
                Pending.assertCanonicalEventMembers(members(wrong)),
              ),
            ),
          ),
        ),
      ).rejects.toThrow();
      expect((await counts())[0]!.l2).toBe("0");
    }
    await run(
      Authority.withRecovery(
        token,
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE event_history_incarnations SET origin_canonical = false WHERE incarnation_id = ${member.history_incarnation_id}`;
        }),
      ),
    );
    await expect(
      run(
        Authority.withRecovery(
          token,
          probe(17).pipe(
            Effect.zipRight(
              Pending.assertCanonicalEventMembers(members(member)),
            ),
          ),
        ),
      ),
    ).rejects.toThrow();
    expect((await counts())[0]!.l2).toBe("0");
  },
);
