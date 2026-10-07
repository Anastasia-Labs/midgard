import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Adoptions from "../src/database/foreignNativeAdoptions.js";
import * as Engine from "../src/database/mpfEngineState.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { ledgerOutputToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import { foreignAdoptionProjection } from "../src/services/foreign-native-adoption-projection.js";
import { assertForeignVerificationSource } from "../src/services/foreign-verification-source.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import type { VerifiedForeignCommitBase } from "../src/workers/commit-block-header.verify-foreign-base.js";
import * as Verification from "../src/workers/commit-block-header.verify-foreign-base.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import {
  append,
  binding,
  hash,
  refusal,
  run,
  start,
} from "./event-history-recovery-plans.registration.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";
import {
  adoptionRecoveryFixture,
  assertRawOutputReplayRefused,
  withDepositMembership,
} from "./support/foreign-native-adoption-recovery.js";

// Actual PostgreSQL authority, durable plans and ledger/event projection.
// L1 source evidence is explicitly modeled by the existing journal fixture;
// these tests do not replace the complete production foreign source verifier.
const address = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("ab".repeat(28))),
)
  .to_address()
  .to_bech32();
const output = Buffer.from(
  makeMidgardTxOutput(address, CML.Value.from_coin(5_000_000n)).to_cbor_bytes(),
);
const beforeKey = makeOutRefCbor(11);
const afterKey = makeOutRefCbor(12);
const lease = "foreign-adoption-sql-component";
beforeEach(() => vi.restoreAllMocks());
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE foreign_native_adoptions,foreign_verified_segments,foreign_confirmed_frontier,mpf_engine_state,mempool,processed_mempool,
    mempool_tx_deltas,tx_admissions,tx_admission_payloads,pending_block_finalizations CASCADE`;
    }),
  ),
);
const setup = async (noop = false) => {
  const { token, checkpoint } = await start();
  const current = await append(token, checkpoint);
  const base: VerifiedForeignCommitBase = {
    authority: "recovery",
    headerHash: "cc".repeat(28),
    root: noop ? hash(20) : hash(21),
    entries: noop ? [] : [{ outref: afterKey, output }],
    history: {
      token,
      coverage: {
        bindingDigest: binding.digest,
        checkpointRevision: current.revision,
        point: { id: current.head.id, slot: current.head.slot },
        snapshotDigest: current.capture.snapshotDigest,
        includedThroughMs: 1000,
      },
    },
    observation: [],
    importedBlocks: [],
    verification: { status: "verified", foreignHeaderHash: "cc".repeat(28) },
  };
  const eventLog = encodeNativeMpfEventLog(
    hash(20),
    noop
      ? []
      : [
          [
            { type: "delete", key: beforeKey },
            ledgerOutputToInsertBatchOp({
              outRef: afterKey,
              outputCbor: output,
            }),
          ],
        ],
  );
  const replay = {
    schema: 1 as const,
    ownerBinarySha256: hash(30),
    baseRoot: hash(20),
    candidateRoot: base.root,
    eventLog,
    eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, eventLog).toString("hex"),
    eventRoots: noop ? Buffer.alloc(0) : Buffer.from(base.root, "hex"),
    eventCount: noop ? 0 : 1,
  };
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* Engine.acquireLedgerStoreLease({ owner: lease, ttlMs: 60000 });
      yield* Engine.stampLedgerMigration(hash(20));
      if (!noop)
        yield* sql`INSERT INTO mempool_ledger(tx_id,outref,output,address)
      VALUES (${Buffer.alloc(32, 11)},${beforeKey},${output},${address})`;
    }),
  );
  await run(Authority.withRecovery(token, Adoptions.request(base)));
  const plan = (await run(Adoptions.unresolved(binding.digest)))[0]!;
  const projection = foreignAdoptionProjection(replay, base.entries);
  const prepare = (
    events: Adoptions.AdoptionEvents = {
      deposits: [],
      forced: [],
      withdrawals: [],
    },
  ) =>
    Authority.withRecovery(
      token,
      Adoptions.prepare({
        request: plan,
        base,
        replay,
        ...projection,
        leaseOwner: lease,
        events,
        ...(noop ? { nativeNoop: true as const } : {}),
      }),
    );
  return { token, base, replay, plan, prepare };
};
const state = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const ledger = yield* sql<{
        outref: Buffer;
      }>`SELECT outref FROM mempool_ledger ORDER BY outref`;
      const [stamp] = yield* sql<{
        root_hex: string;
      }>`SELECT root_hex FROM mpf_engine_state WHERE store_name='ledger'`;
      const [plan] = yield* sql<{
        state: string;
      }>`SELECT state FROM foreign_native_adoptions ORDER BY sequence DESC LIMIT 1`;
      return {
        keys: ledger.map((row) => row.outref.toString("hex")),
        root: stamp?.root_hex,
        plan: plan?.state,
      };
    }),
  );
const prepared = () =>
  run(Adoptions.unresolved(binding.digest)).then((plans) => plans[0]!);

describe("source-owned foreign native adoption SQL", () => {
  it("retains requested/prepared crash material before changing projection", async () => {
    const f = await setup();
    const project = (replay: typeof f.replay) =>
      foreignAdoptionProjection(replay, f.base.entries);
    assertRawOutputReplayRefused(f.replay, f.base.entries[0]!);
    const [projected] = project(f.replay).rows;
    expect(projected?.output).toBe(output.toString("hex"));
    expect(projected?.address).toBe(address);
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "requested",
    });
    await run(f.prepare());
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "prepared",
    });
    const resumed = await prepared();
    expect(Adoptions.retainedReplay(resumed)).toEqual(f.replay);
    await run(Authority.withRecovery(f.token, Adoptions.apply(resumed, lease)));
    expect(await state()).toEqual({
      keys: [afterKey.toString("hex")],
      root: hash(21),
      plan: "applied",
    });
  });

  it("rolls back projection and its stamp atomically when final SQL fails", async () => {
    const f = await setup();
    await run(f.prepare());
    const plan = await prepared();
    const failed = await run(
      Authority.withRecovery(
        f.token,
        Adoptions.apply(plan, lease).pipe(
          Effect.zipRight(
            Effect.fail(new Error("modeled crash before SQL commit")),
          ),
        ),
      ).pipe(Effect.either),
    );
    expect(failed._tag).toBe("Left");
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "prepared",
    });
  });

  it("refuses stale recovery generation and ordinary writer authority", async () => {
    const f = await setup();
    await refusal(Adoptions.request(f.base), "current history ownership");
    await refusal(
      Authority.withRecovery(
        f.token,
        Adoptions.prepare({
          request: f.plan,
          base: {
            ...f.base,
            history: {
              ...f.base.history,
              token: {
                ...f.token,
                generation: (BigInt(f.token.generation) + 1n).toString(),
              },
            },
          },
          replay: f.replay,
          keys: [],
          rows: [],
          events: { deposits: [], forced: [], withdrawals: [] },
          leaseOwner: lease,
        }),
      ),
      "generation changed",
    );
    expect((await state()).plan).toBe("requested");
  });

  it("retains and applies unchanged-root rejected-event metadata and its inverse", async () => {
    const f = await setup(true);
    const id = makeOutRefCbor(15);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Explicit modeled follower admission of the source event.
        const key = Buffer.alloc(32, 15);
        const origin = Buffer.concat([Buffer.alloc(32, 15), Buffer.alloc(2)]);
        yield* sql`INSERT INTO l1_event_keys(kind,key,origin_outref,first_canonical_slot)
        VALUES ('withdrawal',${key},${origin},0)`;
        yield* sql`INSERT INTO withdrawal_utxos(event_id,raw_event_info,inclusion_time,withdrawal_l1_tx_hash,
        withdrawal_l1_output_index,asset_name,l2_outref,l2_owner,l2_value,l1_address,l1_datum,
        refund_address,refund_datum,status,l1_event_key,l1_origin_outref)
        VALUES (${id},${Buffer.from("80", "hex")},NOW(),${Buffer.alloc(32, 15)},0,${Buffer.from("01", "hex")},
          ${id},${Buffer.alloc(28, 1)},${Buffer.from("80", "hex")},${Buffer.from("01", "hex")},${Buffer.from("80", "hex")},
          ${Buffer.from("01", "hex")},${Buffer.from("80", "hex")},'awaiting',${key},${origin})`;
      }),
    );
    await run(
      f.prepare({
        deposits: [],
        forced: [],
        withdrawals: [
          {
            id: id.toString("hex"),
            header: f.base.headerHash,
            validity: "NonExistentWithdrawalUtxo",
            detail: {},
            settlement: "80",
          },
        ],
      }),
    );
    const plan = await prepared();
    expect(Adoptions.isNativeNoop(plan)).toBe(true);
    await run(Authority.withRecovery(f.token, Adoptions.apply(plan, lease)));
    expect((await state()).root).toBe(hash(20));
    const readEvent = () =>
      run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return (yield* sql<{
            status: string;
            projected_header_hash: Buffer | null;
            validity: string | null;
          }>`
        SELECT status,projected_header_hash,validity FROM withdrawal_utxos WHERE event_id=${id}`)[0];
        }),
      );
    expect(await readEvent()).toMatchObject({
      status: "projected",
      projected_header_hash: Buffer.from(f.base.headerHash, "hex"),
      validity: "NonExistentWithdrawalUtxo",
    });
    await run(
      Authority.withRecovery(
        f.token,
        Adoptions.markRewinding(await prepared(), lease),
      ),
    );
    await run(
      Authority.withRecovery(
        f.token,
        Adoptions.rewind(await prepared(), lease),
      ),
    );
    expect(await readEvent()).toMatchObject({
      status: "awaiting",
      projected_header_hash: null,
      validity: null,
    });
    expect((await state()).plan).toBe("rewound");
  });

  it("retains exact bounded ledger beforeimages for a source-authorized inverse", async () => {
    const f = await setup();
    await run(f.prepare());
    await run(
      Authority.withRecovery(f.token, Adoptions.apply(await prepared(), lease)),
    );
    await run(
      Authority.withRecovery(
        f.token,
        Adoptions.markRewinding(await prepared(), lease),
      ),
    );
    expect((await state()).plan).toBe("rewinding");
    await run(
      Authority.withRecovery(
        f.token,
        Adoptions.rewind(await prepared(), lease),
      ),
    );
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "rewound",
    });
  });
});

const withReplaySegment = async (base: VerifiedForeignCommitBase) => {
  const header = {
    ...headerFor(
      {
        utxosRoot: base.root,
        transactionsRoot: hash(0),
        depositsRoot: hash(0),
        withdrawalsRoot: hash(0),
        forcedTransactionsRoot: hash(0),
        transitionTraceRoot: hash(0),
        eventToStepRoot: hash(0),
        validationTracesRoot: hash(0),
      },
      countsFromLengths({}),
    ),
    prevUtxosRoot: hash(20),
    prevHeaderHash: "bb".repeat(28),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    ...base,
    headerHash,
    verification: {
      status: "verified" as const,
      foreignHeaderHash: headerHash,
    },
    importedBlocks: [
      {
        kind: "foreign" as const,
        headerHash,
        headerCbor: Data.to(header, SDK.Header),
        ledgerKeys: [beforeKey, afterKey],
        ledgerBefore: [{ outref: beforeKey, output }],
        parentHeaderHash: "bb".repeat(28),
        parentUtxosRoot: hash(20),
        root: base.root,
        eventRoots: [base.root],
        events: [
          [
            { key: beforeKey.toString("hex"), output: null },
            { key: afterKey.toString("hex"), output },
          ],
        ],
        memberships: { deposits: [], forcedTransactions: [], withdrawals: [] },
      },
    ],
  };
};
describe("foreign native adoption production recovery coordinator", () => {
  it("rebinds to a new generation and completes a crash after native promotion", async () => {
    const f = await setup();
    await run(Engine.releaseLedgerStoreLease(lease));
    const deposit = await withDepositMembership(
      await withReplaySegment(f.base),
    );
    const base = deposit.base;
    const fixture = await adoptionRecoveryFixture(base, f.replay);
    fixture.crashAfterPromotion();
    await expect(
      fixture.recover({ token: f.token, assertCurrent: Effect.void }),
    ).rejects.toThrow("crash after native promotion");
    expect(fixture.root()).toBe(hash(21));
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "prepared",
    });
    const next = await run(
      Authority.beginRecovery(f.token, "Modeled cold restart"),
    );
    fixture.rebind({ ...base, history: { ...base.history, token: next } });
    await fixture.recover({ token: next, assertCurrent: Effect.void });
    expect(await state()).toEqual({
      keys: [afterKey.toString("hex")],
      root: hash(21),
      plan: "applied",
    });
    expect(fixture.phases).toEqual([
      "fork",
      "promote:prepared",
      "recover:prepared",
    ]);
    expect(fixture.verify.mock.calls.length).toBeGreaterThanOrEqual(3);
    expect(fixture.owner.fork).toHaveBeenCalledTimes(1);
    await deposit.assertProjected();
  });

  it("retains preparation but refuses promotion when the source generation changes", async () => {
    const f = await setup();
    await run(Engine.releaseLedgerStoreLease(lease));
    const fixture = await adoptionRecoveryFixture(
      await withReplaySegment(f.base),
      f.replay,
    );
    let checks = 0;
    vi.mocked(Verification.revalidateForeignCommitBase).mockImplementation(
      (base) =>
        ++checks === 2
          ? Effect.fail(
              new DatabaseError({
                table: "event_history_authority",
                message: "Modeled source generation superseded",
                cause: undefined,
              }),
            )
          : assertForeignVerificationSource({
              kind: "recovery",
              binding: base.history,
            }),
    );
    await expect(
      fixture.recover({ token: f.token, assertCurrent: Effect.void }),
    ).rejects.toThrow("generation superseded");
    expect((await state()).plan).toBe("prepared");
    expect(fixture.owner.promote).not.toHaveBeenCalled();
    expect(fixture.root()).toBe(hash(20));
  });

  it("discards an unretained generation when source revalidation fails before SQL preparation", async () => {
    const f = await setup();
    await run(Engine.releaseLedgerStoreLease(lease));
    const fixture = await adoptionRecoveryFixture(
      await withReplaySegment(f.base),
      f.replay,
    );
    vi.mocked(Verification.revalidateForeignCommitBase).mockReturnValue(
      Effect.fail(
        new DatabaseError({
          table: "event_history_authority",
          message: "Modeled changed full queue",
          cause: undefined,
        }),
      ),
    );
    await expect(
      fixture.recover({ token: f.token, assertCurrent: Effect.void }),
    ).rejects.toThrow("changed full queue");
    expect(await state()).toEqual({
      keys: [beforeKey.toString("hex")],
      root: hash(20),
      plan: "requested",
    });
    expect(fixture.phases).toEqual(["fork", "discard"]);
    expect(fixture.owner.promote).not.toHaveBeenCalled();
  });
});
