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
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
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
import { materializeCanonicalHistory } from "../src/database/eventHistoryMaterialization.js";
import * as Ledger from "../src/database/mempoolLedger.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import * as Admissions from "../src/database/txAdmissions.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import { collectAcceptedReferenceProgramEnvelopes } from "../src/fibers/tx-queue-processor.js";
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
import { HistoryProducer } from "../src/services/event-history-producer.js";
import { WriteBehind, WriteBehindLive } from "../src/services/write-behind.js";
import { breakDownTx, type ProcessedTx } from "../src/utils.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
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
const run = <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig | WriteBehind>,
) =>
  Effect.runPromise(
    program.pipe(
      Effect.mapError((error) => new Error(formatDatabaseError(error))),
      Effect.provide(WriteBehindLive),
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
const admit = async (
  checkpoint: Journal.Checkpoint,
  n: number,
  kind: "deposit" | "withdrawal" = "deposit",
  eventNumber = 999,
) => {
  const destination = {
    paymentCredential: { PublicKeyCredential: [owner] as [string] },
    stakeCredential: null,
  };
  const id = { transactionId: hash(eventNumber), outputIndex: 0n };
  const eventKey = await Effect.runPromise(SDK.eventHistoryKey(id));
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: destination,
                l2_network_id: 0n,
                l2_datum: "ab",
              },
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
                  l1_address: destination,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: destination,
            refund_datum: "NoDatum",
          },
        };
  const plan = SDK.prepareEventHistoryPayload(
    payload,
    destination.paymentCredential,
    pair[kind].recipe,
  );
  const facts: SDK.EventHistoryFacts = {
    event_id: id,
    inclusion_time: 1_000_000n,
    location: plan.location,
    structural_lovelace: 2_000_000n,
    structural_refund_key: owner,
  };
  const nodes = checkpoint.capture.history.ledger.outputs
    .filter((output) => output.address === binding.deployments[kind].address)
    .map((output) => ({
      output,
      node: Data.from(output.datum!, SDK.EventHistoryNode),
    }));
  const predecessor = nodes.find(
    ({ node }) =>
      (node.position === "Root" || node.position.Key[0] < eventKey) &&
      (node.next === null || node.next > eventKey),
  );
  if (predecessor === undefined)
    throw new Error("Missing modeled insertion predecessor");
  const continued = {
    ...predecessor.output,
    txHash: hash(n + 1000),
    outputIndex: 0,
    datum: Data.to(
      { ...predecessor.node, next: eventKey },
      SDK.EventHistoryNode,
    ),
  };
  const order: LedgerSnapshotOutput = {
    ...predecessor.output,
    txHash: hash(n + 1000),
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [eventKey] },
        next: predecessor.node.next,
        protected_until: 0n,
        payload: { Order: { facts } },
      },
      SDK.EventHistoryNode,
    ),
    assets: {
      lovelace: 7_000_000n,
      [binding.deployments[kind].policyId + eventKey]: 1n,
    },
  };
  const outputs = [
    ...checkpoint.capture.history.ledger.outputs.filter(
      (output) => output !== predecessor.output,
    ),
    continued,
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
  ).find((event) => event.event.id.transactionId === id.transactionId);
  if (event === undefined) throw new Error("Missing modeled admission");
  const transition: HistoryTransition = {
    kind,
    operation: "InsertOrder",
    transactionHash: order.txHash,
    consumed: [predecessor.output],
    produced: [continued, order],
    continuations:
      predecessor.node.position === "Root"
        ? []
        : [
            {
              key: predecessor.node.position.Key[0],
              before: predecessor.output,
              after: continued,
            },
          ],
    admission: {
      key: eventKey,
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
      yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalizations, pending_block_finalization_deposits, pending_block_finalization_withdrawals, pending_block_finalization_txs, pending_block_finalization_forced_transactions, pending_block_finalization_transition_trace, pending_block_finalization_event_to_step, pending_block_finalization_validation_traces, pending_block_finalization_validation_trace_witnesses, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts, tx_admission_payloads, tx_admissions, mempool, mempool_tx_deltas, address_history, processed_mempool, blocks, immutable`;
    }),
  );
});
const reconcile = (
  before: Journal.Checkpoint,
  kind: "forward" | "rollback" = "forward",
) =>
  Journal.load(binding).pipe(
    Effect.flatMap((after) =>
      after === null
        ? Effect.die("Missing checkpoint")
        : materializeCanonicalHistory({ kind, before, after }, "Preprod"),
    ),
  );
const append = async (
  token: Authority.Token,
  kind: "deposit" | "withdrawal",
  n: number,
  eventNumber = 999,
) => {
  const before = await read();
  await run(
    Authority.withRecovery(
      token,
      Journal.append(
        binding,
        await admit(before, n, kind, eventNumber),
        reconcile(before),
      ),
    ),
  );
  return read();
};
const readyFixture = async (withdrawalFirst = false) => {
  const { token } = await start();
  if (withdrawalFirst) await append(token, "withdrawal", 2, 998);
  const checkpoint = await append(token, "deposit", withdrawalFirst ? 3 : 2);
  const converted = await Effect.runPromise(
    historyIncarnationEntry(
      checkpoint.incarnations.find(
        (row) => row.kind === "deposit" && row.placement !== null,
      )!,
      "Preprod",
    ),
  );
  if (converted.kind !== "deposit") throw new Error("Expected deposit");
  const source: Ledger.DepositEntry = {
    tx_id: converted.entry.ledger_tx_id,
    outref: makeOutRefCbor(converted.entry.ledger_tx_id),
    output: converted.entry.ledger_output,
    address: converted.entry.ledger_address,
    source_event_id: converted.entry.event_id,
  };
  const bases = [920, 921].map((n) => ({
    tx_id: Buffer.from(hash(n), "hex"),
    outref: makeOutRefCbor(hash(n)),
    output: output(),
    address,
  }));
  await run(
    Authority.withRecovery(
      token,
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* Ledger.insertDepositEntriesStrict([source]);
        yield* Ledger.insert(bases);
        yield* sql`UPDATE deposits_utxos SET status='projected' WHERE event_id=${source.source_event_id}`;
      }),
    ),
  );
  await run(
    Authority.publishReady(token, {
      point: checkpoint.head,
      snapshotDigest: checkpoint.capture.snapshotDigest,
    }),
  );
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
  const ready = <A, E>(
    work: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig | WriteBehind>,
  ) =>
    run(
      Authority.withReady(
        token,
        work.pipe(Effect.provideService(HistoryProducer, permit)),
      ),
    );
  const accept = async (txs: readonly ProcessedTx[]) => {
    await run(
      Effect.forEach(txs, (tx) =>
        Admissions.tryInsert({
          txId: tx.txId,
          txCanonicalCbor: tx.txCbor,
          programMaterialSidecarCbor: sidecar,
          submitSource: "native",
        }),
      ),
    );
    const leaseOwner = randomUUID();
    const claimed = await run(
      Admissions.claimBatch({
        limit: txs.length,
        leaseOwner,
        leaseDurationMs: 60_000,
      }),
    );
    expect(claimed.map((row) => row.tx_id.toString("hex")).sort()).toEqual(
      txs.map((tx) => tx.txId.toString("hex")).sort(),
    );
    await ready(
      Effect.gen(function* () {
        // Supply persistence metadata from the actual modeled pre-state. This
        // does not claim Phase B validation of these SQL-only fixture bodies.
        const preState = yield* Ledger.retrieve;
        const referenceProgramEnvelopesByTxId =
          collectAcceptedReferenceProgramEnvelopes(
            txs.map((tx) => ({
              ledgerTx: { txId: tx.txId },
              submission: { txCbor: tx.txCbor },
              graph: { produced: tx.produced },
            })),
            new Map(
              preState.map((row) => [row.outref.toString("hex"), row.output]),
            ),
          );
        yield* Admissions.markAccepted({
          rows: txs.map((tx) => ({ tx_id: tx.txId })),
          leaseOwner,
          processedTxs: txs,
          referenceProgramEnvelopesByTxId,
        });
        const writes = yield* WriteBehind;
        yield* writes.flushNow;
      }),
    );
  };
  return { token, checkpoint, source, bases, permit, ready, accept };
};
const rows = (table: string, order: string) =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        row: Record<string, unknown>;
      }>`SELECT to_jsonb(t) AS row FROM ${sql(table)} t ORDER BY ${sql(order)}`;
    }),
  ).then((result) => result.map((entry) => entry.row));
const snapshot = async () => ({
  ledger: await rows("mempool_ledger", "outref"),
  deposits: await rows("deposits_utxos", "event_id"),
  withdrawals: await rows("withdrawal_utxos", "event_id"),
  admissions: await rows("tx_admissions", "tx_id"),
  payloads: await rows("tx_admission_payloads", "tx_id"),
  receipts: await rows("event_history_l2_ledger_receipts", "sequence"),
  mempool: await rows("mempool", "tx_id"),
  deltas: await rows("mempool_tx_deltas", "tx_id"),
  addresses: await rows("address_history", "tx_id"),
  cursor: await rows("event_history_cursor", "binding_digest"),
  liveOutputs: await rows("event_history_live_outputs", "tx_hash"),
  authority: await rows("event_history_authority", "singleton"),
  materialOwners: await rows("cek_program_material_admission_owners", "tx_id"),
  incarnations: await rows("event_history_incarnations", "incarnation_id"),
  applications: await rows(
    "event_history_block_applications",
    "application_revision",
  ),
  pending: await rows("pending_block_finalizations", "header_hash"),
  immutable: await rows("immutable", "tx_id"),
  blocks: await rows("blocks", "tx_id"),
});
const bytea = (bytes: Buffer) => `\\x${bytes.toString("hex")}`;
const reverse = async (f: Awaited<ReturnType<typeof readyFixture>>) => {
  const token = await run(
    Authority.beginRecovery(f.token, "modeled authenticated branch rollback"),
  );
  const before = await read();
  await run(
    Authority.withRecovery(
      token,
      Journal.undoHead(binding, before, reconcile(before, "rollback")),
    ),
  );
  return { token, checkpoint: await read() };
};
const assertQueued = async (txs: readonly ProcessedTx[]) => {
  const state = await snapshot();
  expect(state.admissions.map((row) => row.tx_id).sort()).toEqual(
    txs.map((tx) => bytea(tx.txId)).sort(),
  );
  for (const tx of txs) {
    expect(
      state.admissions.find((row) => row.tx_id === bytea(tx.txId)),
    ).toMatchObject({
      status: "queued",
      terminal_at: null,
      lease_owner: null,
      lease_expires_at: null,
      validation_started_at: null,
      reject_code: null,
      reject_detail: null,
    });
    expect(
      state.payloads.find((row) => row.tx_id === bytea(tx.txId)),
    ).toMatchObject({
      tx_canonical_cbor: bytea(tx.txCbor),
      cek_program_material_sidecar_cbor: bytea(sidecar),
    });
  }
  expect(state.mempool).toEqual([]);
  expect(state.deltas).toEqual([]);
  expect(state.addresses).toEqual([]);
  return state;
};

// SQL/model integration only: production acceptance persistence, inverse repair,
// journal CAS and materialization run normally. No Phase A/B or applied L1 claim.
describe("unpublished history-dependent ledger repair", () => {
  it("reverses the whole unpublished batch chain, including reference-only and independent transactions", async () => {
    const f = await readyFixture();
    const baseline = await snapshot();
    const referenceOnly = await transaction(
      [f.bases[0]!.outref],
      [f.source.outref],
    );
    const independent = await transaction([f.bases[1]!.outref]);
    await f.accept([referenceOnly, independent]);
    const dependent = await transaction([f.source.outref]);
    await f.accept([dependent]);
    const child = await transaction([dependent.produced[0]!.outref]);
    await f.accept([child]);
    const accepted = await snapshot();
    expect(accepted.receipts).toHaveLength(3);
    expect(accepted.mempool).toHaveLength(4);
    expect(accepted.deposits[0]?.status).toBe("consumed");
    const recovered = await reverse(f);
    const state = await assertQueued([
      referenceOnly,
      independent,
      dependent,
      child,
    ]);
    expect(state.ledger).toEqual(
      baseline.ledger.filter((row) => row.source_event_id === null),
    );
    expect(state.deposits).toEqual([]);
    expect(state.receipts.map((row) => row.reversed_at_revision)).toEqual([
      Number(recovered.checkpoint.revision),
      Number(recovered.checkpoint.revision),
      Number(recovered.checkpoint.revision),
    ]);
    expect(recovered.checkpoint.head).toEqual({
      ...initial.history.ledger.point,
      height: 1,
    });
    expect(recovered.checkpoint.incarnations).toHaveLength(1);
    expect(recovered.checkpoint.incarnations[0]?.placement).toBeNull();
    // Reconciliation is idempotent; receipts are not re-applied on resume.
    const exact = await snapshot();
    await run(
      Authority.withRecovery(
        recovered.token,
        materializeCanonicalHistory(
          {
            kind: "resume",
            before: recovered.checkpoint,
            after: recovered.checkpoint,
          },
          "Preprod",
        ),
      ),
    );
    expect(await snapshot()).toEqual(exact);
  });

  it("removes an orphan withdrawal and resets only unassigned canonical classifications", async () => {
    const f = await readyFixture(true);
    const token = await run(
      Authority.beginRecovery(f.token, "append modeled withdrawal"),
    );
    const checkpoint = await append(token, "withdrawal", 4, 1001);
    await run(
      Authority.withRecovery(
        token,
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE withdrawal_utxos SET status='projected', settlement_event_info=raw_event_info, validity='WithdrawalIsValid', validity_detail='{"fixture":"classified"}'::jsonb`;
        }),
      ),
    );
    const before = await snapshot();
    expect(before.withdrawals).toHaveLength(2);
    const retainedId = bytea(
      Buffer.from(
        Data.to(
          { transactionId: hash(998), outputIndex: 0n },
          SDK.OutputReference,
        ),
        "hex",
      ),
    );
    const original = before.withdrawals.find(
      (row) => row.event_id === retainedId,
    )!;
    expect(original).toMatchObject({
      withdrawal_l1_tx_hash: bytea(Buffer.from(hash(1004), "hex")),
      withdrawal_l1_output_index: 0,
    });
    const canonicalPointer = f.checkpoint.incarnations.find(
      (row) => row.kind === "withdrawal",
    )?.placement?.current?.outRef;
    expect(canonicalPointer).toEqual({ txHash: hash(1002), outputIndex: 1 });
    await run(
      Authority.withRecovery(
        token,
        Journal.undoHead(
          binding,
          checkpoint,
          reconcile(checkpoint, "rollback"),
        ),
      ),
    );
    const after = await snapshot();
    expect(after.withdrawals).toHaveLength(1);
    const retained = after.withdrawals[0]!;
    expect(retained).toMatchObject({
      ...original,
      withdrawal_l1_tx_hash: bytea(
        Buffer.from(canonicalPointer!.txHash, "hex"),
      ),
      withdrawal_l1_output_index: canonicalPointer!.outputIndex,
      status: "awaiting",
      settlement_event_info: null,
      validity: null,
      validity_detail: {},
      classification_revision: Number(original.classification_revision) + 1,
      updated_at: retained.updated_at,
    });
    expect(after.ledger).toEqual(before.ledger);
    expect(after.deposits).toEqual(before.deposits);
    expect(after.receipts).toEqual([]);
  });

  it("materializes a fresh same-ID incarnation without reviving orphan credit or old eligibility", async () => {
    const f = await readyFixture();
    const tx = await transaction([f.source.outref]);
    await f.accept([tx]);
    const oldId = f.checkpoint.incarnations[0]!.id;
    const recovered = await reverse(f);
    const fresh = await append(recovered.token, "deposit", 5);
    const incarnations = fresh.incarnations.filter(
      (row) => row.kind === "deposit",
    );
    expect(incarnations).toHaveLength(2);
    expect(incarnations.find((row) => row.id === oldId)?.placement).toBeNull();
    const active = incarnations.find((row) => row.placement !== null)!;
    expect(active.id).not.toBe(oldId);
    const state = await assertQueued([tx]);
    expect(state.deposits).toHaveLength(1);
    expect(state.deposits[0]).toMatchObject({
      event_id: bytea(f.source.source_event_id),
      history_incarnation_id: bytea(Buffer.from(active.id, "hex")),
      status: "awaiting",
      projected_header_hash: null,
    });
    expect(state.ledger.every((row) => row.source_event_id === null)).toBe(
      true,
    );
  });

  it.each([
    "signed-pending",
    "published",
    "immutable-baseline",
    "immutable-member",
    "missing-receipt",
    "after-image",
    "payload",
  ] as const)(
    "refuses %s and atomically preserves source, ledger, admission and intent state",
    async (mode) => {
      const f = await readyFixture();
      const tx = await transaction([f.source.outref]);
      await f.accept([tx]);
      if (mode === "signed-pending") await prepareSignedPending(f);
      await f.ready(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          if (mode === "published") {
            yield* sql`INSERT INTO blocks (height,header_hash,tx_id) VALUES (1,${Buffer.alloc(28, 0x41)},${tx.txId})`;
            yield* sql`DELETE FROM mempool WHERE tx_id=${tx.txId}`;
          }
          if (mode === "immutable-member")
            yield* sql`INSERT INTO immutable (tx_id,tx) VALUES (${tx.txId},${tx.txCbor})`;
          if (mode === "missing-receipt")
            yield* sql`DELETE FROM event_history_l2_ledger_receipts`;
          if (mode === "after-image")
            yield* sql`UPDATE mempool_ledger SET output=${output(4_000_000n)} WHERE outref=${tx.produced[0]!.outref}`;
          if (mode === "payload")
            yield* sql`UPDATE tx_admission_payloads SET tx_canonical_cbor=${Buffer.concat([tx.txCbor, Buffer.from([0])])} WHERE tx_id=${tx.txId}`;
        }),
      );
      if (mode === "immutable-baseline") {
        const imported = await transaction([makeOutRefCbor(hash(970))]);
        await f.ready(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`INSERT INTO immutable (tx_id,tx) VALUES (${imported.txId},${imported.txCbor})`;
          }),
        );
      }
      const token = await run(
        Authority.beginRecovery(f.token, "modeled rollback refusal"),
      );
      const before = await snapshot();
      const checkpoint = await read();
      const message =
        mode === "signed-pending"
          ? /pending-candidate or signed-submission disposition/
          : mode === "published"
            ? /left the unpublished ledger overlay/
            : mode === "missing-receipt" || mode === "immutable-baseline"
              ? /retained inverse evidence or authenticated accepted-baseline/
              : mode === "immutable-member"
                ? /incomplete, assigned or only partially unpublished/
                : /no longer matches current ledger or payload bytes/;
      await expect(
        run(
          Authority.withRecovery(
            token,
            Journal.undoHead(
              binding,
              checkpoint,
              reconcile(checkpoint, "rollback"),
            ),
          ),
        ),
      ).rejects.toThrow(message);
      expect(await snapshot()).toEqual(before);
      expect((await read()).head).toEqual(checkpoint.head);
    },
  );

  it("refuses a stale cursor without using retained receipts as replacement authority", async () => {
    const f = await readyFixture();
    const tx = await transaction([f.source.outref]);
    await f.accept([tx]);
    const token = await run(
      Authority.beginRecovery(f.token, "modeled stale repair request"),
    );
    const checkpoint = await read();
    const before = await snapshot();
    const stale = {
      ...checkpoint,
      revision: (BigInt(checkpoint.revision) + 1n).toString(),
    };
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(binding, stale, reconcile(checkpoint, "rollback")),
        ),
      ),
    ).rejects.toThrow(/History cursor revision or head changed/);
    expect(await snapshot()).toEqual(before);
  });

  it("rolls back an already reversed child when an older batch after-image cannot be proven", async () => {
    const f = await readyFixture();
    const parent = await transaction([f.source.outref]);
    await f.accept([parent]);
    const child = await transaction([parent.produced[0]!.outref]);
    await f.accept([child]);
    await f.ready(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE event_history_l2_ledger_receipts SET ledger_after=jsonb_set(ledger_after,'{0,output}',to_jsonb(${bytea(output(4_000_000n))}::text)) WHERE sequence=(SELECT min(sequence) FROM event_history_l2_ledger_receipts)`;
      }),
    );
    const token = await run(
      Authority.beginRecovery(f.token, "modeled late inverse failure"),
    );
    const before = await snapshot();
    const checkpoint = await read();
    await expect(
      run(
        Authority.withRecovery(
          token,
          Journal.undoHead(
            binding,
            checkpoint,
            reconcile(checkpoint, "rollback"),
          ),
        ),
      ),
    ).rejects.toThrow(/no longer matches current ledger or payload bytes/);
    expect(await snapshot()).toEqual(before);
    expect(
      before.receipts.every((row) => row.reversed_at_revision === null),
    ).toBe(true);
  });
});

const prepareSignedPending = async (
  f: Awaited<ReturnType<typeof readyFixture>>,
) => {
  const root = SDK.EMPTY_MERKLE_TREE_ROOT;
  const roots = {
    utxosRoot: root,
    forcedTransactionsRoot: root,
    transactionsRoot: root,
    depositsRoot: root,
    withdrawalsRoot: root,
  };
  const expectedRoots = {
    ...roots,
    transitionTraceRoot: root,
    eventToStepRoot: root,
    validationTracesRoot: root,
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const time = new Date(1_000_000);
  const header: SDK.Header = {
    ...expectedRoots,
    ...counts,
    prevUtxosRoot: root,
    startTime: BigInt(time.getTime()),
    endTime: BigInt(time.getTime() + 60_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "41".repeat(28),
    operatorVkey: owner,
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  const signedCbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
  const txHash = Buffer.from(
    CML.hash_transaction(
      CML.Transaction.from_cbor_bytes(signedCbor).body(),
    ).to_hex(),
    "hex",
  );
  // A genuine signed body retained in a modeled pending journal: no claim that
  // this L1 transaction was submitted or that this header has chain authority.
  await f.ready(
    Pending.preparePendingSubmission({
      headerHash,
      headerCbor: Buffer.from(Data.to(header, SDK.Header), "hex"),
      preparedTxHash: txHash,
      metadata: {
        deploymentMarker: makeDeploymentMarker(binding.manifestId),
        consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
        stateQueueLeaseToken: "modeled-pending-owner",
        baseSnapshotId: "modeled-pending-base",
        baseTailOutRef: `${hash(980)}#0`,
        baseTailHeaderHash: Buffer.alloc(28, 0x41),
        baseTailDatumCbor: "d87980",
        baseRoots: roots,
        blockStartTime: time,
        expectedRoots,
        expectedCounts: counts,
      },
      blockEndTime: new Date(time.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
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
    }),
  );
  // The preparation above has committed. The signed-intent writer must acquire
  // its own Ready transaction; inheriting f.ready's SQL transaction is refused.
  await run(
    Pending.recordSignedIntent(headerHash, txHash, signedCbor).pipe(
      Effect.provideService(HistoryProducer, f.permit),
    ),
  );
  const pending = await rows("pending_block_finalizations", "header_hash");
  expect(pending).toHaveLength(1);
  expect(pending[0]).toMatchObject({
    prepared_tx_hash: bytea(txHash),
    intended_tx_hash: bytea(txHash),
    signed_tx_cbor: bytea(signedCbor),
    submitted_tx_hash: null,
    status: "pending_submission",
  });
};
