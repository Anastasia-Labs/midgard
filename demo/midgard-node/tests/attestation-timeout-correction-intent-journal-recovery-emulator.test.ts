/**
 * The attestation-timeout correction's recovery read (plan §8.2–§8.4, TC)
 * on a Lucid emulator: a journaled correction intent's observation, derived
 * from the intent journal's status of that transaction and the follower's
 * facts alone, in both polarities, and S6 as its only resubmitter.
 *
 * - honest: the correction lands and is reported included at its depth's
 *   level (landed, safe, then final once k blocks cover it);
 * - adversarial: another transaction spends its input first; it is reported
 *   a conflict (final once the spend is), and S6 never sends it;
 * - a rollback below k un-lands it: it is reported pending again, and S6
 *   resubmits its exact journaled bytes.
 *
 * The recovery reads no L1: every provider method, `fetch` and `WebSocket`
 * record their use while it reads, and the module imports no L1 client.
 *
 * The status is family-agnostic, so the journaled transaction is a wallet
 * payment under a correction workflow key. S6 here wants every live intent:
 * the correction predicate (the queue still holds the timed-out target) is
 * the I1 predicates' and tested with them.
 */
import { readFile } from "node:fs/promises";

import { timeoutCorrectionAttemptStatus } from "@al-ft/midgard-fault-proofs";
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { type Effect, ManagedRuntime, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { intentJournalTimeoutCorrectionRecovery } from "../src/fibers/attestation-timeout-correction.intent-journal-recovery.js";
import {
  type IntentPlan,
  journaledIntent,
} from "../src/services/intent-journal.js";
import { createNodeIntentStage } from "../src/services/l1-follower.intents.js";
import { RECORD_ONLY } from "./helpers/intent-journal.js";
import {
  EMULATOR_K,
  type IntentEmulator,
  openIntentEmulator,
  signedPayment,
} from "./helpers/intent-journal-emulator.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const CONFIRMATION_DEPTH = 2;
const depths = {
  confirmationDepth: CONFIRMATION_DEPTH,
  securityParameter: EMULATOR_K,
};

const PROVIDER_METHODS = [
  "getProtocolParameters",
  "getUtxos",
  "getUtxosWithUnit",
  "getUtxoByUnit",
  "getUtxosByOutRef",
  "getDelegation",
  "getDatum",
  "awaitTx",
  "submitTx",
  "evaluateTx",
] as const;

const databases = testDatabases();
const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  await Promise.all(closers.splice(0).map((close) => close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const open = async () => {
  const env: IntentEmulator = await openIntentEmulator(databases);
  // The first pass seeds the own wallet at the origin.
  expect(await env.stage.run()).toEqual([]);
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(env.connectionString) }),
  );
  closers.push(async () => {
    await runtime.dispose();
    await env.close();
  });
  const recovery = intentJournalTimeoutCorrectionRecovery(
    <A>(effect: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
      runtime.runPromise(effect),
    depths,
  );
  // S6 over the same store and the emulator's mempool.
  const sent: Buffer[] = [];
  const s6 = createNodeIntentStage({
    store: env.store,
    transport: {
      hasTx: (txId: string) =>
        Promise.resolve(
          env.emulator.transactionHistory[txId]?.status === "pending",
        ),
      submit: async (bytes: Uint8Array) => {
        sent.push(Buffer.from(bytes));
        try {
          await env.emulator.submitTx(Buffer.from(bytes).toString("hex"));
          return { accepted: true } as const;
        } catch (error) {
          return {
            accepted: false,
            rejection: Buffer.from(String(error)),
          } as const;
        }
      },
      withLedgerState: () =>
        Promise.reject(new Error("S6 here seeds no wallet")),
    } as never,
    securityParameter: EMULATOR_K,
    seededAddresses: [],
    wanted: () => Promise.resolve(true),
    log: () => undefined,
  });
  closers.unshift(() => Promise.resolve(s6.close()));

  /** The L1 touches made while the recovery observes `hash`. */
  const observe = async (hash: string, cbor: string) => {
    const touches: string[] = [];
    for (const method of PROVIDER_METHODS)
      vi.spyOn(env.emulator, method).mockImplementation(
        (...args: unknown[]) => {
          touches.push(`emulator.${method}(${JSON.stringify(args)})`);
          return Promise.reject(new Error(`L1 read: ${method}`));
        },
      );
    vi.spyOn(globalThis, "fetch").mockImplementation((input) => {
      touches.push(`fetch(${String(input)})`);
      return Promise.reject(new Error("L1 read: fetch"));
    });
    vi.stubGlobal(
      "WebSocket",
      class {
        constructor(url: string) {
          touches.push(`WebSocket(${url})`);
          throw new Error("L1 read: WebSocket");
        }
      },
    );
    try {
      const observed = await recovery.observeAttempt({
        transactionHash: hash,
        signedTransactionCborHex: cbor,
      });
      expect(touches).toEqual([]);
      return observed;
    } finally {
      vi.restoreAllMocks();
      vi.unstubAllGlobals();
    }
  };

  /** Lands one wallet payment in its own block, deepening the chain by one. */
  const deepen = async (blocks: number) => {
    for (let i = 0; i < blocks; i++) {
      const filler = await signedPayment(
        await env.wallet(),
        env.payee.address,
        1_000_000n,
      );
      await env.emulator.submitTx(filler.cbor);
      env.emulator.awaitBlock(1);
      await env.follow();
    }
  };

  const tip = async () => {
    const cursor = (await env.store.cursor())!;
    return { slot: cursor.point.slot, hash: cursor.point.hash.toString("hex") };
  };
  const blockPoint = async (height: number) => {
    const block = (await env.store.blockAtHeight(height))!;
    return { slot: block.slot, hash: block.hash.toString("hex") };
  };
  return { env, observe, s6, sent, deepen, tip, blockPoint };
};

/** A correction's intent, as `withCorrectionIntentJournal` records it. */
const correctionIntent = (removed: string, plan: IntentPlan) =>
  journaledIntent(
    "correction",
    `correction:${"aa".repeat(32)}:terminal:${removed}`,
    plan,
    Buffer.from(removed, "hex"),
  );

const hex = (bytes: readonly Buffer[]) =>
  bytes.map((entry) => entry.toString("hex"));

describe("the attestation-timeout correction's intent-journal recovery", () => {
  it("reports a landed correction included at its depth's level, and final past k", async () => {
    const { env, observe, s6, sent, deepen, tip, blockPoint } = await open();
    const tx = await signedPayment(
      await env.wallet(),
      env.payee.address,
      2_000_000n,
    );
    await env.record(
      correctionIntent("bb".repeat(32), await env.plan()),
      tx.cbor,
      tx.hash,
      RECORD_ONLY,
    );

    // Journaled, its first send lost: live, and S6 sends it.
    expect(await observe(tx.hash, tx.cbor)).toMatchObject({
      status: "pending",
      final: false,
      inputsAvailable: true,
      canonicalPoint: await tip(),
      releaseFinalPoint: null,
    });
    await s6.run();
    expect(hex(sent)).toEqual([tx.cbor]);

    env.emulator.awaitBlock(1);
    await env.follow();
    const landed = await observe(tx.hash, tx.cbor);
    expect(landed).toMatchObject({
      status: "included",
      final: false,
      inclusion: { height: 1, depth: 1, level: "landed" },
      canonicalPoint: await tip(),
      releaseFinalPoint: null,
    });
    expect(timeoutCorrectionAttemptStatus(landed)).toBe("confirmed");

    await deepen(CONFIRMATION_DEPTH - 1);
    expect(await observe(tx.hash, tx.cbor)).toMatchObject({
      status: "included",
      final: false,
      inclusion: { depth: CONFIRMATION_DEPTH, level: "safe" },
    });

    await deepen(EMULATOR_K + 1 - CONFIRMATION_DEPTH);
    expect(await observe(tx.hash, tx.cbor)).toMatchObject({
      status: "included",
      final: true,
      inclusion: { height: 1, depth: EMULATOR_K + 1, level: "final" },
      canonicalPoint: await tip(),
      // The release-final block (depth k + 1) is the correction's own.
      releaseFinalPoint: await blockPoint(1),
    });
    await s6.run();
    expect(hex(sent)).toEqual([tx.cbor]);
  }, 120_000);

  it("reports a correction whose input another tx spent as a conflict, final once the spend is, and S6 never sends it", async () => {
    const { env, observe, s6, sent, deepen } = await open();
    const journaled = await signedPayment(
      await env.wallet(),
      env.payee.address,
      2_000_000n,
    );
    await env.record(
      correctionIntent("cc".repeat(32), await env.plan()),
      journaled.cbor,
      journaled.hash,
      RECORD_ONLY,
    );
    // The same wallet, used outside the node, spends the same input first.
    const foreign = await signedPayment(
      await env.wallet(),
      env.payee.address,
      3_000_000n,
    );
    const spent = (cbor: string) =>
      decodeTransaction(Buffer.from(cbor, "hex")).inputs.map(
        (input) => `${input.txHash.toString("hex")}#${input.index.toString()}`,
      );
    expect(spent(foreign.cbor)).toEqual(spent(journaled.cbor));
    await env.emulator.submitTx(foreign.cbor);
    env.emulator.awaitBlock(1);
    await env.follow();

    const conflicted = await observe(journaled.hash, journaled.cbor);
    expect(conflicted).toMatchObject({ status: "conflict", final: false });
    expect(conflicted.reason).toContain(`foreign tx ${foreign.hash}`);
    // Dead at the tip only: the workflow abandons it, and a replacement
    // shares an input with it.
    expect(timeoutCorrectionAttemptStatus(conflicted)).toBe("superseded");
    await s6.run();
    expect(sent).toEqual([]);

    await deepen(EMULATOR_K);
    const final = await observe(journaled.hash, journaled.cbor);
    expect(final).toMatchObject({ status: "conflict", final: true });
    expect(timeoutCorrectionAttemptStatus(final)).toBe("invalidated");
    await s6.run();
    expect(sent).toEqual([]);
    expect(env.emulator.transactionHistory[journaled.hash]).toBeUndefined();
  }, 120_000);

  it("reports a correction a rollback below k un-landed as pending, and S6 resubmits its exact bytes", async () => {
    const { env, observe, s6, sent, tip } = await open();
    const tx = await signedPayment(
      await env.wallet(),
      env.payee.address,
      2_000_000n,
    );
    await env.record(
      correctionIntent("dd".repeat(32), await env.plan()),
      tx.cbor,
      tx.hash,
      RECORD_ONLY,
    );
    await s6.run();
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await observe(tx.hash, tx.cbor)).toMatchObject({
      status: "included",
      inclusion: { depth: 1 },
    });

    // The chain rolls back past the correction's block (depth 1 < k).
    const origin = (await env.store.blockAtHeight(0))!;
    expect(
      await env.store.rewind({ slot: origin.slot, hash: origin.hash }),
    ).toMatchObject({ kind: "rewound" });
    const unlanded = await observe(tx.hash, tx.cbor);
    expect(unlanded).toMatchObject({
      status: "pending",
      final: false,
      inputsAvailable: true,
      canonicalPoint: await tip(),
    });
    expect(timeoutCorrectionAttemptStatus(unlanded)).toBe("pending");

    // Not in the mempool any more: S6 resends the identical journaled bytes.
    await s6.run();
    expect(hex(sent)).toEqual([tx.cbor, tx.cbor]);
  }, 120_000);

  it("imports no L1 client, and the action wires no Kupo or Ogmios into its recovery", async () => {
    const source = await readFile(
      new URL(
        "../src/fibers/attestation-timeout-correction.intent-journal-recovery.ts",
        import.meta.url,
      ),
      "utf8",
    );
    const imports = [...source.matchAll(/from "([^"]+)";/gu)].map(
      (match) => match[1],
    );
    expect(imports).toEqual([
      "@al-ft/midgard-fault-proofs",
      "@al-ft/midgard-l1-follower",
      "@al-ft/midgard-l1-follower/heads",
      "@effect/sql",
      "effect",
      "../database/follower-schema.js",
      "../l1-heads.js",
    ]);
    expect(source).not.toMatch(/lucid|kupo|ogmios|fetch\(|WebSocket/iu);
    const action = await readFile(
      new URL(
        "../src/fibers/attestation-timeout-correction.attestation-timeout-correction-action.ts",
        import.meta.url,
      ),
      "utf8",
    );
    expect(action).not.toMatch(/Kupmios\w*TimeoutCorrectionRecovery/u);
    expect(action).toContain(
      "recovery: intentJournalTimeoutCorrectionRecovery(",
    );
  });
});
