import { createHash, randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { pendingHistoryLedgerDisposition } from "../src/database/eventHistoryLedgerRepair.js";
import {
  applyHistoryRecoveryPlan,
  type HistoryRecoveryIntent,
  type HistoryRecoveryPlan,
  prepareHistoryRecoveryPlan,
  prepareRetainedNativeHistoryRecoveryPlan,
} from "../src/database/eventHistoryRecoveryPlans.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { retainEverything } from "./helpers/history-journal-retention.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { applyMidgardNodeTestEnv, testDatabaseName } from "./test-env.js";

// Component verification: actual PostgreSQL authority, journal and atomic repair.
// The strict decoded L1 snapshot and empty block ancestry are explicitly modeled;
// neither these receipts nor the signed body establish source/native authority.
applyMidgardNodeTestEnv();
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");
const modelOriginReceipt =
  "Explicit model source replay evidence; not ledger admission";
const run = <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) =>
  Effect.runPromise(
    program.pipe(
      Effect.mapError((error) => new Error(formatDatabaseError(error))),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
const refusal = async <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
  message: string,
) => {
  const result = await run(program.pipe(Effect.either));
  expect(result._tag).toBe("Left");
  if (result._tag !== "Left") throw new Error("Expected refusal");
  expect(formatDatabaseError(result.left)).toContain(message);
};
let binding: EventHistorySourceBinding;
let initial: Journal.Checkpoint["capture"];
const rootDatum = () =>
  Data.to(
    {
      position: "Root",
      next: null,
      protected_until: 0n,
      payload: "RootContent",
    },
    SDK.EventHistoryNode,
  );
const read = async () => {
  const checkpoint = await run(Journal.load(binding));
  if (checkpoint === null) throw new Error("Missing checkpoint");
  return checkpoint;
};
const start = async (initialGeneration = false) => {
  const claimed = await run(
    Authority.acquire({
      deploymentIdentity: binding.manifestId,
      ownerToken: randomUUID(),
      leaseDurationMs: 60_000,
    }),
  );
  const token = initialGeneration
    ? claimed
    : await run(Authority.beginRecovery(claimed, "Modeled component replay"));
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
const intent = (): HistoryRecoveryIntent => {
  const signed = makeCardanoSignedMapOutputTxBytes();
  return {
    bindingDigest: binding.digest,
    manifestId: binding.manifestId,
    headerHash: "ab".repeat(28),
    signedTransactionHash: CML.hash_transaction(
      CML.Transaction.from_cbor_bytes(signed).body(),
    ).to_hex(),
    signedTransactionCborSha256: sha(signed),
    expectedRoot: hash(20),
    targetRoot: hash(21),
    journalDigest: hash(22),
  };
};
const document = (value: HistoryRecoveryIntent) =>
  eventHistoryCanonicalJson({
    domain: "midgard-history-recovery-intent-v1",
    ...value,
  });
const rows = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        row: Record<string, unknown>;
      }>`SELECT to_jsonb(p) AS row FROM event_history_recovery_plans p ORDER BY recovery_id`;
    }),
  );
const probes = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        label: string;
      }>`SELECT label FROM history_recovery_plan_probe ORDER BY label`;
    }),
  );
const repair = (label = "repaired") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_recovery_plan_probe(label) VALUES (${label})`;
  });
const append = async (
  token: Authority.Token,
  checkpoint: Journal.Checkpoint,
) => {
  const point = {
    id: hash(1000 + checkpoint.head.height),
    slot: checkpoint.head.slot + 1,
    height: checkpoint.head.height + 1,
  };
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        ...checkpoint.capture.history.ledger,
        point: { id: point.id, slot: point.slot },
      },
      binding,
    ),
  );
  const prepared = Journal.prepareAppend(
    checkpoint,
    { point, parent: checkpoint.head.id, transactions: [] },
    { capture, transitions: [] },
  );
  await run(
    Authority.withRecovery(
      token,
      Journal.append(binding, prepared, () => Effect.void, retainEverything),
    ),
  );
  return read();
};
beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  const pair = SDK.requireEventHistoryContracts(contracts);
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

const cleanup = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [database] = yield* sql<{
    name: string;
  }>`SELECT current_database() AS name`;
  expect(database?.name).toBe(testDatabaseName());
  yield* sql`CREATE TABLE IF NOT EXISTS history_recovery_plan_probe(label text PRIMARY KEY)`;
  yield* sql`TRUNCATE history_recovery_plan_probe, event_history_recovery_plans, event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts`;
});
beforeEach(async () => {
  await run(cleanup);
});
afterAll(async () => {
  await run(cleanup);
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_recovery_plan_probe`;
    }),
  );
});

describe("durable history recovery plans (modeled source, real SQL)", () => {
  it("accepts the actual initial authority generation zero", async () => {
    const { token, checkpoint } = await start(true);
    expect(token.generation).toBe("0");
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, intent(), hash(30)),
      ),
    );
    expect((await rows())[0]!.row.owner_generation).toBe(0);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it("persists exact immutable signed intent and native operation before repair", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    expect(plan).toEqual({
      recoveryId: sha(document(value)),
      intent: value,
      evidenceDigest: hash(30),
      checkpointRevision: checkpoint.revision,
      state: "prepared",
      native: {
        recoveryId: sha(document(value)),
        expectedRoot: value.expectedRoot,
        targetRoot: value.targetRoot,
      },
    });
    expect(Object.isFrozen(plan)).toBe(true);
    expect(Object.isFrozen(plan.intent)).toBe(true);
    expect(Object.isFrozen(plan.native)).toBe(true);
    const stored = await rows();
    expect(stored).toHaveLength(1);
    expect(stored[0]!.row).toMatchObject({
      intent: document(value),
      state: "prepared",
      owner_generation: Number(token.generation),
      checkpoint_revision: Number(checkpoint.revision),
      recovery_id: `\\x${plan.recoveryId}`,
      evidence_digest: `\\x${hash(30)}`,
    });
    expect(await probes()).toEqual([]);
  });
  it("retains one recovery ID across actual journal append and refreshed canonical evidence", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    const next = await append(token, checkpoint);
    const refreshed = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(next, value, hash(31)),
      ),
    );
    expect(refreshed.recoveryId).toBe(first.recoveryId);
    expect(refreshed.native).toEqual(first.native);
    expect(refreshed.checkpointRevision).not.toBe(first.checkpointRevision);
    expect((await rows())[0]!.row).toMatchObject({
      head_hash: `\\x${next.head.id}`,
      snapshot_digest: `\\x${next.capture.snapshotDigest}`,
      evidence_digest: `\\x${hash(31)}`,
    });
    expect(await rows()).toHaveLength(1);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(next, refreshed, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it.each([
    "headerHash",
    "expectedRoot",
    "targetRoot",
    "journalDigest",
    "signedTransactionHash",
    "signedTransactionCborSha256",
  ] as const)(
    "rejects conflicting outstanding %s without replacing intent",
    async (field) => {
      const { token, checkpoint } = await start();
      const value = intent();
      await run(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
        ),
      );
      const before = await rows();
      const changed = {
        ...value,
        [field]: field === "headerHash" ? "cd".repeat(28) : hash(999),
      };
      await refusal(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(checkpoint, changed, hash(30)),
        ),
        "A different durable native recovery must be resolved first",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );
  it("keeps a prepared recovery pending even when unchanged origins are canonical", async () => {
    const { token, checkpoint } = await start();
    const change = {
      kind: "resume" as const,
      before: checkpoint,
      after: checkpoint,
    };
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toBeUndefined();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, intent(), hash(30)),
      ),
    );
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toEqual({
      status: "pending",
      reason:
        "A durable native/SQL recovery operation requires current-branch disposition",
    });
    expect(await read()).toEqual(checkpoint);
    expect(await probes()).toEqual([]);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toBeUndefined();
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect(await read()).toEqual(checkpoint);
  });

  it("does not execute an applied repair twice, including after evidence refresh", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair("duplicate")),
      ),
    );
    const next = await append(token, checkpoint);
    const refreshed = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(next, value, hash(31)),
      ),
    );
    expect(refreshed.state).toBe("applied");
    expect(refreshed.recoveryId).toBe(plan.recoveryId);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(
          next,
          refreshed,
          repair("duplicate-after-refresh"),
        ),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect(await rows()).toHaveLength(1);
  });
  it("rejects replaced evidence at the same checkpoint", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const stale = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, stale, repair()),
      ),
      "Recovery application evidence changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("rejects stale checkpoints for prepare and apply after an actual append", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    await append(token, checkpoint);
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
      ),
      "Recovery plan checkpoint changed",
    );
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "Recovery plan checkpoint changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("requires newly validated evidence after owner generation revocation", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    const fresh = await run(
      Authority.beginRecovery(token, "Restart modeled owner"),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "History authority generation or owner changed",
    );
    await refusal(
      Authority.withRecovery(
        fresh,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "Recovery application evidence changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
    const renewed = await run(
      Authority.withRecovery(
        fresh,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
      ),
    );
    expect(renewed.recoveryId).toBe(plan.recoveryId);
    await run(
      Authority.withRecovery(
        fresh,
        applyHistoryRecoveryPlan(checkpoint, renewed, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it("refuses no authority, ordinary SQL transactions and Ready-only authority", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(30)),
      ),
    );
    const before = await rows();
    const message = "History journal requires an owned recovery transaction";
    await refusal(
      prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
      message,
    );
    await refusal(
      applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      message,
    );
    await refusal(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.withTransaction(
          prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
        );
      }),
      message,
    );
    await run(
      Authority.publishReady(token, {
        point: checkpoint.head,
        snapshotDigest: checkpoint.capture.snapshotDigest,
      }),
    );
    await refusal(
      Authority.withReady(
        token,
        prepareHistoryRecoveryPlan(checkpoint, value, hash(31)),
      ),
      message,
    );
    await refusal(
      Authority.withReady(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      message,
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("rolls back repair writes and retains prepared intent on failure, then retries exactly once", async () => {
    const { token, checkpoint } = await start();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, intent(), hash(30)),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(
          checkpoint,
          plan,
          repair().pipe(
            Effect.andThen(Effect.fail(new Error("modeled repair failed"))),
          ),
        ),
      ),
      "modeled repair failed",
    );
    expect(await probes()).toEqual([]);
    expect(await rows()).toEqual(before);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect((await rows())[0]!.row.state).toBe("applied");
  });
  it("snapshots caller intent before the first asynchronous authority read", async () => {
    const { token, checkpoint } = await start();
    const original = intent();
    const mutable = { ...original };
    Object.defineProperty(mutable, "bindingDigest", {
      enumerable: true,
      get() {
        queueMicrotask(() => {
          mutable.targetRoot = hash(999);
        });
        return original.bindingDigest;
      },
    });
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(checkpoint, mutable, hash(30)),
      ),
    );
    expect(mutable.targetRoot).toBe(hash(999));
    expect(plan.intent).toEqual(original);
    expect(plan.recoveryId).toBe(sha(document(original)));
    expect((await rows())[0]!.row.intent).toBe(document(original));
  });
  it.each(["intent", "native", "checkpointRevision"] as const)(
    "refuses altered returned plan %s",
    async (field) => {
      const { token, checkpoint } = await start();
      const plan = await run(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(checkpoint, intent(), hash(30)),
        ),
      );
      const before = await rows();
      const altered: HistoryRecoveryPlan =
        field === "intent"
          ? { ...plan, intent: { ...plan.intent, targetRoot: hash(999) } }
          : field === "native"
            ? { ...plan, native: { ...plan.native, targetRoot: hash(999) } }
            : { ...plan, checkpointRevision: "999" };
      await refusal(
        Authority.withRecovery(
          token,
          applyHistoryRecoveryPlan(checkpoint, altered, repair()),
        ),
        "Recovery application immutable identity changed",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );
});

// Native roots here are observed-input models, not an RPC/CAS simulation. The
// assertions exercise retained SQL operation selection under actual authority;
// fresh chain authorization and the drained native diagnostics belong to callers.
const retainedIntent = () => {
  const { expectedRoot: candidateRoot, ...value } = intent();
  return { value, candidateRoot };
};

describe("retained native recovery root selection (real SQL component)", () => {
  it.each(["unpromoted", "promoted"] as const)(
    "records %s baseline selection before repair",
    async (state) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      const durableRoot =
        state === "unpromoted" ? value.targetRoot : candidateRoot;
      const plan = await run(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(30),
            { durableRoot, candidateRoot },
          ),
        ),
      );
      const expectedIntent = { ...value, expectedRoot: durableRoot };
      expect(plan.intent).toEqual(expectedIntent);
      expect(plan.recoveryId).toBe(sha(document(expectedIntent)));
      expect(plan.native).toEqual({
        recoveryId: plan.recoveryId,
        expectedRoot: durableRoot,
        targetRoot: value.targetRoot,
      });
      const stored = await rows();
      expect(stored).toHaveLength(1);
      expect(stored[0]!.row.intent).toBe(document(expectedIntent));
      expect(stored[0]!.row.state).toBe("prepared");
      expect(await probes()).toEqual([]);
      await run(
        Authority.withRecovery(
          token,
          applyHistoryRecoveryPlan(checkpoint, plan, repair()),
        ),
      );
      expect(await probes()).toEqual([{ label: "repaired" }]);
    },
  );

  it("preserves the original operation after modeled native CAS, checkpoint append and actual owner restart", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(30), {
          durableRoot: candidateRoot,
          candidateRoot,
        }),
      ),
    );
    const afterNative = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(31), {
          durableRoot: value.targetRoot,
          candidateRoot,
        }),
      ),
    );
    expect(afterNative.recoveryId).toBe(first.recoveryId);
    expect(afterNative.native).toEqual(first.native);
    expect(afterNative.intent.expectedRoot).toBe(candidateRoot);
    const next = await append(token, checkpoint);
    const replacement = await run(
      Authority.acquire({
        deploymentIdentity: binding.manifestId,
        ownerToken: token.ownerToken,
        leaseDurationMs: 60_000,
      }),
    );
    expect(BigInt(replacement.generation)).toBeGreaterThan(
      BigInt(token.generation),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(next, value, hash(32), {
          durableRoot: value.targetRoot,
          candidateRoot,
        }),
      ),
      "History authority generation or owner changed",
    );
    expect(await rows()).toEqual(before);
    const resumed = await run(
      Authority.withRecovery(
        replacement,
        prepareRetainedNativeHistoryRecoveryPlan(next, value, hash(32), {
          durableRoot: value.targetRoot,
          candidateRoot,
        }),
      ),
    );
    expect(resumed.recoveryId).toBe(first.recoveryId);
    expect(resumed.intent).toEqual(first.intent);
    expect(resumed.native).toEqual(first.native);
    expect(resumed.checkpointRevision).toBe(next.revision);
    expect(resumed.evidenceDigest).toBe(hash(32));
    expect(resumed.state).toBe("prepared");
    const refreshed = await rows();
    expect(refreshed).toHaveLength(1);
    expect(refreshed[0]!.row).toMatchObject({
      intent: document(first.intent),
      owner_generation: Number(replacement.generation),
      checkpoint_revision: Number(next.revision),
      head_hash: `\\x${next.head.id}`,
      snapshot_digest: `\\x${next.capture.snapshotDigest}`,
      evidence_digest: `\\x${hash(32)}`,
    });
    expect(await probes()).toEqual([]);
    await run(
      Authority.withRecovery(
        replacement,
        applyHistoryRecoveryPlan(next, resumed, repair()),
      ),
    );
    await run(
      Authority.withRecovery(
        replacement,
        applyHistoryRecoveryPlan(next, resumed, repair("duplicate")),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });

  it.each([false, true])(
    "refuses a third durable root (prepared=%s) without SQL changes",
    async (prepared) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      if (prepared)
        await run(
          Authority.withRecovery(
            token,
            prepareRetainedNativeHistoryRecoveryPlan(
              checkpoint,
              value,
              hash(30),
              { durableRoot: candidateRoot, candidateRoot },
            ),
          ),
        );
      const before = await rows();
      await refusal(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(31),
            { durableRoot: hash(999), candidateRoot },
          ),
        ),
        "Native recovery root is outside the authenticated journal",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );

  it.each([
    "headerHash",
    "signedTransactionHash",
    "signedTransactionCborSha256",
    "journalDigest",
    "targetRoot",
  ] as const)(
    "refuses changed retained %s after native restoration",
    async (field) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      await run(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(30),
            { durableRoot: candidateRoot, candidateRoot },
          ),
        ),
      );
      const before = await rows();
      const changed = {
        ...value,
        [field]: field === "headerHash" ? "cd".repeat(28) : hash(999),
      };
      // The root is individually allowed by the newly supplied target/candidate;
      // only the retained immutable operation identity should reject this retry.
      await refusal(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            changed,
            hash(31),
            { durableRoot: changed.targetRoot, candidateRoot },
          ),
        ),
        "Retained native recovery requires a different disposition",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );

  it("refuses a different replay candidate after the original CAS reached its target", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(30), {
          durableRoot: candidateRoot,
          candidateRoot,
        }),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(31), {
          durableRoot: value.targetRoot,
          candidateRoot: hash(999),
        }),
      ),
      "Retained native recovery requires a different disposition",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });

  it("does not turn a prepared no-op restoration into a candidate-root undo", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(30), {
          durableRoot: value.targetRoot,
          candidateRoot,
        }),
      ),
    );
    expect(first.native.expectedRoot).toBe(value.targetRoot);
    expect(first.native.targetRoot).toBe(value.targetRoot);
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(checkpoint, value, hash(31), {
          durableRoot: candidateRoot,
          candidateRoot,
        }),
      ),
      "Retained native recovery requires a different disposition",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });

  it("requires owned recovery and an exact current checkpoint before selecting roots", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const observed = { durableRoot: candidateRoot, candidateRoot };
    await refusal(
      prepareRetainedNativeHistoryRecoveryPlan(
        checkpoint,
        value,
        hash(30),
        observed,
      ),
      "History journal requires an owned recovery transaction",
    );
    await append(token, checkpoint);
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          observed,
        ),
      ),
      "Recovery plan checkpoint changed",
    );
    expect(await rows()).toEqual([]);
    expect(await probes()).toEqual([]);
  });
});
