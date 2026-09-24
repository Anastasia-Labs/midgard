import { createHash, randomUUID } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeHash32,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxWitnessSetCanonical,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
import { encode } from "cborg";
import { Effect } from "effect";
import { beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import * as Receipts from "../src/database/eventHistoryLedgerReceipts.js";
import { materializeCanonicalHistory } from "../src/database/eventHistoryMaterialization.js";
import * as Ledger from "../src/database/mempoolLedger.js";
import * as Admissions from "../src/database/txAdmissions.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import { historyIncarnationEntry } from "../src/l1-event-history-entries.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { HistoryTransition } from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import {
  HistoryProducer,
  UnownedHistoryFixture,
  withHistoryWrite,
} from "../src/services/event-history-producer.js";
import { breakDownTx, type ProcessedTx } from "../src/utils.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";
import { applyMidgardNodeTestEnv, testDatabaseName } from "./test-env.js";

// Real SQL and canonical signed Midgard bodies; L1 history admission below is
// explicitly modeled, not emulator acceptance or authority inferred from a receipt.
applyMidgardNodeTestEnv();
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const modelOriginReceipt =
  "Explicit model source replay evidence; not ledger admission";
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    program.pipe(
      Effect.mapError((error) => new Error(formatDatabaseError(error))),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const owner = key.to_public().hash().to_hex();
const address = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(key.to_public().hash()),
)
  .to_address()
  .to_bech32();
const sidecar = Buffer.from(encodeMidgardCekProgramMaterialSidecar([]));
const empty = Buffer.from([0x80]);
const byteList = (items: readonly Uint8Array[]) =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));
const output = (lovelace = 5_000_000n) =>
  Buffer.from(
    makeMidgardTxOutput(
      address,
      CML.Value.from_coin(lovelace),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("a3020a010b020c")),
    ).to_cbor_bytes(),
  );
const transaction = async (
  spent: readonly Buffer[],
  references: readonly Buffer[] = [],
  outputs: readonly Buffer[] = [output()],
): Promise<ProcessedTx> => {
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: byteList(spent),
    referenceInputsPreimageCbor: byteList(references),
    outputsPreimageCbor: byteList(outputs),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: empty,
    requiredSignersPreimageCbor: empty,
    mintPreimageCbor: empty,
    scriptIntegrityHash: computeHash32(Buffer.from([0xf6])),
    auxiliaryDataHash: computeHash32(Buffer.from([0xf6])),
    networkId: 0n,
  };
  const bodyHash = computeHash32(
    encodeMidgardNativeTxBodyCompact(deriveMidgardNativeTxBodyCompact(body)),
  );
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    key,
  );
  expect(key.to_public().verify(bodyHash, witness.ed25519_signature())).toBe(
    true,
  );
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: byteList([witness.to_cbor_bytes()]),
    scriptTxWitsPreimageCbor: empty,
    redeemerTxWitsPreimageCbor: empty,
  };
  return Effect.runPromise(
    breakDownTx(
      encodeMidgardNativeTxCanonical({
        version: MIDGARD_NATIVE_TX_VERSION,
        validity: "TxIsValid",
        body,
        witnessSet,
        compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
      }),
    ),
  );
};
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
});
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
const admit = async (checkpoint: Journal.Checkpoint, n: number) => {
  const address = {
    paymentCredential: { PublicKeyCredential: [owner] as [string] },
    stakeCredential: null,
  };
  const id = { transactionId: hash(999), outputIndex: 0n };
  const key = await Effect.runPromise(SDK.eventHistoryKey(id));
  const kind = "deposit";
  const payload: SDK.EventHistoryPayload = {
    DepositPayload: {
      event: {
        id,
        info: { l2_address: address, l2_network_id: 0n, l2_datum: "ab" },
      },
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
  const event = capture.history.deposits[0]!;
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

beforeEach(async () => {
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const [database] = yield* sql<{
        name: string;
      }>`SELECT current_database() AS name`;
      expect(database?.name).toBe(testDatabaseName());
      yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts, tx_admission_payloads, tx_admissions`;
    }),
  );
});

const ownedFixture = async () => {
  const { token, checkpoint } = await start();
  const prepared = await admit(checkpoint, 2);
  await run(
    Authority.withRecovery(
      token,
      Journal.append(
        binding,
        prepared,
        Effect.gen(function* () {
          const after = yield* Journal.load(binding);
          if (after === null) throw new Error("Missing admitted checkpoint");
          yield* materializeCanonicalHistory(
            { kind: "forward", before: checkpoint, after },
            "Preprod",
          );
        }),
      ),
    ),
  );
  const after = await read();
  const converted = await Effect.runPromise(
    historyIncarnationEntry(after.incarnations[0]!, "Preprod"),
  );
  if (converted.kind !== "deposit") throw new Error("Expected deposit source");
  const source: Ledger.DepositEntry = {
    tx_id: converted.entry.ledger_tx_id,
    outref: makeOutRefCbor(converted.entry.ledger_tx_id),
    output: converted.entry.ledger_output,
    address: converted.entry.ledger_address,
    source_event_id: converted.entry.event_id,
  };
  const reference = {
    tx_id: Buffer.from(hash(920), "hex"),
    outref: makeOutRefCbor(hash(920)),
    output: output(),
    address,
  };
  await run(
    Authority.withRecovery(
      token,
      Ledger.insertDepositEntriesStrict([source]).pipe(
        Effect.zipRight(Ledger.insert([reference])),
      ),
    ),
  );
  await run(
    Authority.publishReady(token, {
      point: after.head,
      snapshotDigest: after.capture.snapshotDigest,
    }),
  );
  const permit = {
    token,
    coverage: {
      bindingDigest: binding.digest,
      checkpointRevision: after.revision,
      point: after.head,
      snapshotDigest: after.capture.snapshotDigest,
      includedThroughMs: after.head.slot,
    },
  };
  const ready = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
    run(
      Authority.withReady(
        token,
        effect.pipe(Effect.provideService(HistoryProducer, permit)),
      ),
    );
  return { token, checkpoint: after, source, reference, permit, ready };
};

const admitTransactions = (txs: readonly ProcessedTx[]) =>
  run(
    Effect.forEach(txs, (tx) =>
      Admissions.tryInsert({
        txId: tx.txId,
        txCanonicalCbor: tx.txCbor,
        programMaterialSidecarCbor: sidecar,
        submitSource: "native",
      }),
    ),
  );
const mutate = (receipt: Receipts.AcceptedLedgerReceipt | undefined) =>
  Effect.gen(function* () {
    if (receipt === undefined) throw new Error("Owned receipt missing");
    const sql = yield* SqlClient.SqlClient;
    for (const spent of receipt.spent)
      yield* sql`DELETE FROM mempool_ledger WHERE outref = ${spent}`;
    yield* Ledger.insert(receipt.produced);
  });
const accept = (txs: readonly ProcessedTx[]) =>
  Effect.gen(function* () {
    const receipt = yield* Receipts.beginAcceptedLedgerReceipt(txs);
    yield* mutate(receipt);
    yield* Receipts.finishAcceptedLedgerReceipt(receipt);
    return receipt;
  });
const rows = (table: string, order: string) =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        row: Record<string, unknown>;
      }>`SELECT to_jsonb(t) AS row FROM ${sql(table)} t ORDER BY ${sql(order)}`;
    }),
  ).then((result) => result.map((entry) => entry.row));
const receiptRows = () => rows(Receipts.tableName, "sequence");
const snapshot = async () => ({
  ledger: await rows("mempool_ledger", "outref"),
  deposits: await rows("deposits_utxos", "event_id"),
  payloads: await rows("tx_admission_payloads", "tx_id"),
  receipts: await receiptRows(),
});
const bytea = (bytes: Buffer) => `\\x${bytes.toString("hex")}`;

// These exercise the receipt boundary inside real checked Ready transactions.
// They do not assert validation/CEK execution or receipt-authorized chain rollback.
describe("owned accepted-ledger inverse receipts", () => {
  it("retains exact external spend, reference, deposit incarnation and original admission bytes before scrubbing", async () => {
    const f = await ownedFixture();
    const tx = await transaction([f.source.outref], [f.reference.outref]);
    await admitTransactions([tx]);
    const before = await snapshot();
    const result = await f.ready(
      Effect.gen(function* () {
        const receipt = yield* accept([tx]);
        const sql = yield* SqlClient.SqlClient;
        // Model the existing post-acceptance payload scrub only after finish.
        yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${tx.txId}`;
        return receipt;
      }),
    );
    expect(result).toBeDefined();
    const after = await snapshot();
    expect(after.receipts).toHaveLength(1);
    const receipt = after.receipts[0]!;
    expect(receipt).toMatchObject({
      binding_digest: bytea(Buffer.from(binding.digest, "hex")),
      owner_generation: Number(f.token.generation),
      checkpoint_revision: Number(f.checkpoint.revision),
      head_hash: bytea(Buffer.from(f.checkpoint.head.id, "hex")),
      snapshot_digest: bytea(
        Buffer.from(f.checkpoint.capture.snapshotDigest, "hex"),
      ),
      tx_ids: [bytea(tx.txId)],
      reference_outrefs: [bytea(f.reference.outref)],
      reversed_at_revision: null,
      ledger_before: before.ledger.filter(
        (row) => row.outref === bytea(f.source.outref),
      ),
      reference_before: before.ledger.filter(
        (row) => row.outref === bytea(f.reference.outref),
      ),
      deposits_before: before.deposits,
      payloads_before: before.payloads,
      ledger_after: after.ledger.filter(
        (row) => row.outref === bytea(tx.produced[0]!.outref),
      ),
    });
    expect(before.deposits[0]).toMatchObject({
      history_binding_digest: bytea(Buffer.from(binding.digest, "hex")),
      history_incarnation_id: bytea(
        Buffer.from(f.checkpoint.incarnations[0]!.id, "hex"),
      ),
    });
    expect(before.payloads[0]).toMatchObject({
      tx_canonical_cbor: bytea(tx.txCbor),
      cek_program_material_sidecar_cbor: bytea(sidecar),
    });
    expect(after.payloads).toEqual([]);
    expect(after.deposits).toEqual(before.deposits);
    expect(
      after.ledger.find((row) => row.outref === bytea(f.reference.outref)),
    ).toEqual(
      before.ledger.find((row) => row.outref === bytea(f.reference.outref)),
    );
    const next = await transaction([tx.produced[0]!.outref]);
    await admitTransactions([next]);
    const second = await f.ready(accept([next]));
    expect(BigInt(second!.sequence)).toBeGreaterThan(BigInt(result!.sequence));
  });

  it("compacts internal batch spends while retaining internal reference identities and net after-images", async () => {
    const f = await ownedFixture();
    const first = await transaction(
      [f.source.outref],
      [f.reference.outref],
      [output(2_500_000n), output(2_500_000n)],
    );
    const second = await transaction(
      [first.produced[0]!.outref],
      [first.produced[1]!.outref, f.reference.outref],
      [output(2_500_000n)],
    );
    await admitTransactions([first, second]);
    const before = await snapshot();
    await f.ready(accept([first, second]));
    const after = await snapshot();
    const receipt = after.receipts[0]!;
    expect(receipt.tx_ids).toEqual([bytea(first.txId), bytea(second.txId)]);
    expect(receipt.reference_outrefs).toEqual([
      bytea(f.reference.outref),
      bytea(first.produced[1]!.outref),
    ]);
    expect(receipt.ledger_before).toEqual(
      before.ledger.filter((row) => row.outref === bytea(f.source.outref)),
    );
    expect(receipt.reference_before).toEqual(
      before.ledger.filter((row) => row.outref === bytea(f.reference.outref)),
    );
    expect(receipt.payloads_before).toEqual(before.payloads);
    expect(receipt.ledger_after).toEqual(
      after.ledger.filter((row) => row.outref !== bytea(f.reference.outref)),
    );
    expect(after.ledger.map((row) => row.outref)).not.toContain(
      bytea(first.produced[0]!.outref),
    );
    expect(after.ledger.map((row) => row.outref).sort()).toEqual(
      [
        f.reference.outref,
        first.produced[1]!.outref,
        second.produced[0]!.outref,
      ]
        .map(bytea)
        .sort(),
    );
  });

  it.each(["spend", "reference"] as const)(
    "refuses a missing external %s before writing a receipt",
    async (kind) => {
      const f = await ownedFixture();
      const tx = await transaction(
        [kind === "spend" ? makeOutRefCbor(hash(930)) : f.source.outref],
        kind === "reference" ? [makeOutRefCbor(hash(931))] : [],
      );
      await admitTransactions([tx]);
      const before = await snapshot();
      await expect(f.ready(accept([tx]))).rejects.toThrow(
        /complete consumed\/reference ledger before-images/,
      );
      expect(await snapshot()).toEqual(before);
    },
  );

  it("refuses a preexisting net output without changing its exact bytes", async () => {
    const f = await ownedFixture();
    const tx = await transaction([f.source.outref]);
    await admitTransactions([tx]);
    await f.ready(Ledger.insert(tx.produced));
    const before = await snapshot();
    await expect(f.ready(accept([tx]))).rejects.toThrow(
      /output already exists/,
    );
    expect(await snapshot()).toEqual(before);
  });

  it.each(["missing", "different"] as const)(
    "refuses %s original canonical admission payload",
    async (variant) => {
      const f = await ownedFixture();
      const tx = await transaction([f.source.outref]);
      if (variant === "different") {
        await admitTransactions([tx]);
        const other = await transaction(
          [f.source.outref],
          [f.reference.outref],
        );
        await run(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE tx_admission_payloads SET tx_canonical_cbor = ${other.txCbor} WHERE tx_id = ${tx.txId}`;
          }),
        );
      }
      const before = await snapshot();
      await expect(f.ready(accept([tx]))).rejects.toThrow(
        /every original canonical admission payload/,
      );
      expect(await snapshot()).toEqual(before);
    },
  );

  it.each([
    "unconsumed",
    "missing-output",
    "changed-output",
    "changed-source",
    "after-finish",
    "double-finish",
  ] as const)("rolls back every write on %s failure", async (mode) => {
    const f = await ownedFixture();
    const tx = await transaction([f.source.outref]);
    await admitTransactions([tx]);
    const before = await snapshot();
    const operation = Effect.gen(function* () {
      const receipt = yield* Receipts.beginAcceptedLedgerReceipt([tx]);
      if (mode !== "unconsumed") yield* mutate(receipt);
      const sql = yield* SqlClient.SqlClient;
      if (mode === "missing-output")
        yield* sql`DELETE FROM mempool_ledger WHERE outref=${tx.produced[0]!.outref}`;
      if (mode === "changed-output")
        yield* sql`UPDATE mempool_ledger SET output=${output(4_000_000n)} WHERE outref=${tx.produced[0]!.outref}`;
      if (mode === "changed-source")
        yield* sql`UPDATE mempool_ledger SET source_event_id=${f.source.source_event_id} WHERE outref=${tx.produced[0]!.outref}`;
      yield* Receipts.finishAcceptedLedgerReceipt(receipt);
      if (mode === "double-finish")
        yield* Receipts.finishAcceptedLedgerReceipt(receipt);
      yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id=${tx.txId}`;
      return yield* Effect.fail(
        new Error("deliberate failure after receipt completion"),
      );
    });
    const message =
      mode === "unconsumed"
        ? /still contains a consumed before-image/
        : mode === "double-finish"
          ? /already completed/
          : mode === "after-finish"
            ? /deliberate failure after receipt completion/
            : /after-image differs/;
    await expect(f.ready(operation)).rejects.toThrow(message);
    expect(await snapshot()).toEqual(before);
  });

  it("does not manufacture a production receipt for an explicit unowned fixture", async () => {
    const tx = await transaction([makeOutRefCbor(hash(950))]);
    const result = await run(
      withHistoryWrite(Receipts.beginAcceptedLedgerReceipt([tx])).pipe(
        Effect.provideService(UnownedHistoryFixture, true),
      ),
    );
    expect(result).toBeUndefined();
    await run(Receipts.finishAcceptedLedgerReceipt(result));
    expect(await receiptRows()).toEqual([]);
  });
});
