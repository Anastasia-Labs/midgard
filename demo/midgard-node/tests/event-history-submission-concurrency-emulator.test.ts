import { randomUUID } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import { historyPairPayloads } from "@al-ft/midgard-fault-proofs/test-support/history-pair";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Logger, Option } from "effect";
import { expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { submitDurableEventHistoryProgram } from "../src/transactions/event-history-submission.js";
import {
  depositRequest,
  emulatorClock,
  setupHistoryContracts,
  sharedPredecessorNonces,
} from "./event-history-submission-emulator.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

it("admits concurrent deposits contending for one list predecessor, each in a single run", async () => {
  const { h, contracts } = await setupHistoryContracts();
  const nonces = await sharedPredecessorNonces(h);
  const ids = nonces.map(() => `concurrent-${randomUUID()}`);
  const waits: string[] = [];
  let met = () => {};
  const contended = new Promise<void>((resolve) => {
    met = resolve;
  });
  const run = (nonce: UTxO, submissionId: string) =>
    Effect.runPromise(
      provideDatabaseLayers(
        submitDurableEventHistoryProgram({
          lucid: h.lucid,
          contracts,
          kind: "Deposit",
          submissionId,
          intentHash: "ef".repeat(32),
          nonceInput: nonce,
          scriptReference: h.scripts[0]!,
          prepare: () => Effect.succeed({ request: depositRequest(h, nonce) }),
          // Whichever records its admission first holds the predecessor, in
          // flight, until the other has met that reservation.
          beforeAdmission: () => Effect.promise(() => contended),
        }).pipe(
          Effect.withClock(emulatorClock(h)),
          Effect.provide(
            Logger.add(
              Logger.make(({ message }) => {
                const text = String(message);
                if (!text.includes("is waiting for local submission")) return;
                waits.push(text);
                met();
              }),
            ),
          ),
        ),
      ),
    );
  const results = await Promise.all(
    nonces.map((nonce, index) => run(nonce, ids[index]!)),
  );
  // Non-vacuous: one submission waited on the other's reservation, which it
  // names, and still landed in the same run instead of failing.
  expect(waits.length).toBeGreaterThan(0);
  expect(ids.some((id) => waits[0]!.includes(`local submission ${id} `))).toBe(
    true,
  );
  expect(results[0]!.admission.txHash).not.toBe(results[1]!.admission.txHash);
  for (const [index, result] of results.entries()) {
    expect(
      (await h.lucid.transactionStatus(result.admission.txHash)).status,
    ).toBe("confirmed");
    const saved = await Effect.runPromise(
      provideDatabaseLayers(Journal.retrieve(ids[index]!)),
    );
    if (Option.isNone(saved)) throw new Error("Missing submission journal");
    expect(saved.value.checkpoint.pending).toBeUndefined();
    expect(saved.value.checkpoint.admission?.txHash).toBe(
      result.admission.txHash,
    );
  }
}, 300_000);

const attemptInputs = (attempt: SDK.EventHistorySubmissionAttempt) => {
  const inputs = CML.Transaction.from_cbor_hex(attempt.transactionCbor)
    .body()
    .inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index()}`;
  });
};

const query = <A extends object>(
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<readonly A[], unknown>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(Effect.flatMap(SqlClient.SqlClient, statement)),
  );

const until = async (condition: () => Promise<boolean>, what: string) => {
  const deadline = Date.now() + 120_000;
  while (!(await condition())) {
    if (Date.now() > deadline) throw new Error(`Timed out until ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};

const journal = async (submissionId: string) =>
  Effect.runPromise(provideDatabaseLayers(Journal.retrieve(submissionId)));

/** `lucid` whose reads of `wallet` run `before` and `after` around the read,
 * numbered from 1: the first is a fresh submission's nonce choice, each
 * later one a funding. Every read still returns the emulator's view. */
const gateWalletReads = (
  lucid: LucidEvolution,
  wallet: string,
  gate: {
    readonly before?: (read: number) => Promise<void>;
    readonly after?: (read: number, view: readonly UTxO[]) => Promise<void>;
  },
): LucidEvolution => {
  let reads = 0;
  return {
    ...lucid,
    utxosAt: async (address: Parameters<LucidEvolution["utxosAt"]>[0]) => {
      if (address !== wallet) return lucid.utxosAt(address);
      const read = ++reads;
      await gate.before?.(read);
      const view = await lucid.utxosAt(address);
      await gate.after?.(read, view);
      return view;
    },
  };
};

it("keeps a nonce another submission has chosen out of its funding until the chooser reserves it", async () => {
  const { h, contracts } = await setupHistoryContracts();
  const [chosen, other] = await sharedPredecessorNonces(h);
  const [chooser, funder] = ["chooser", "funder"].map(
    (role) => `${role}-${randomUUID()}`,
  );
  const submit = (
    nonce: UTxO,
    submissionId: string,
    prepare: () => Effect.Effect<{
      request: SDK.EventHistorySubmissionRequest;
    }>,
    beforeAdmission?: () => Promise<void>,
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        submitDurableEventHistoryProgram({
          lucid: h.lucid,
          contracts,
          kind: "Deposit",
          submissionId,
          intentHash: "ef".repeat(32),
          nonceInput: nonce,
          scriptReference: h.scripts[0]!,
          prepare,
          beforeAdmission:
            beforeAdmission && (() => Effect.promise(beforeAdmission)),
        }).pipe(Effect.withClock(emulatorClock(h))),
      ),
    );
  const recorded = async (submissionId: string, pending: boolean) =>
    (
      await query(
        (sql) => sql`SELECT 1 FROM event_history_submissions
          WHERE submission_id = ${submissionId}
          AND (NOT ${pending} OR checkpoint -> 'pending' IS NOT NULL)`,
      )
    ).length > 0;
  const waitingOnLock = async () =>
    (
      await query<{ waiting: number }>(
        (sql) => sql`SELECT count(*)::int AS waiting FROM pg_stat_activity
          WHERE datname = current_database() AND wait_event_type = 'Lock'`,
      )
    )[0]!.waiting > 0;
  let chooserSettled = false;
  let funding: ReturnType<typeof submit> | undefined;
  // The funder funds with every wallet output it may take. It starts after
  // the chooser picked its nonce, before the chooser reserves it, and runs
  // until it records an attempt or waits on a lock in Postgres. It broadcasts
  // its admission only once the chooser reserved its nonce or stopped.
  const chooserRun = submit(chosen, chooser, () =>
    Effect.promise(async () => {
      funding = submit(
        other,
        funder,
        () => Effect.succeed({ request: depositRequest(h, other) }),
        () =>
          until(
            async () => chooserSettled || (await recorded(chooser, false)),
            "the chooser reserved its nonce",
          ),
      );
      await until(
        async () => (await recorded(funder, true)) || (await waitingOnLock()),
        "the funder recorded an attempt or waited on a lock",
      );
      return { request: depositRequest(h, chosen) };
    }),
  );
  const stopped = () => {
    chooserSettled = true;
    return funding;
  };
  const [chosenResult, fundedResult] = await Promise.allSettled([
    chooserRun,
    chooserRun.then(stopped, stopped),
  ]);
  if (fundedResult.status === "rejected") throw fundedResult.reason;
  if (chosenResult.status === "rejected") throw chosenResult.reason;
  if (fundedResult.value === undefined) throw new Error("Funder never ran");
  const results = {
    [chooser]: chosenResult.value,
    [funder]: fundedResult.value,
  };
  // Both land, the chooser on its chosen nonce, and no attempt of the funder
  // ever spends it.
  expect(attemptInputs(results[chooser]!.admission)).toContain(
    outRefLabel(chosen),
  );
  for (const [submissionId, result] of Object.entries(results)) {
    expect(
      (await h.lucid.transactionStatus(result.admission.txHash)).status,
    ).toBe("confirmed");
    const saved = await Effect.runPromise(
      provideDatabaseLayers(Journal.retrieve(submissionId)),
    );
    if (Option.isNone(saved)) throw new Error("Missing submission journal");
    expect(saved.value.checkpoint.admission?.txHash).toBe(
      result.admission.txHash,
    );
  }
  for (const attempt of [
    results[funder]!.checkpoint.publicationAttempt,
    results[funder]!.admission,
  ])
    if (attempt !== undefined && "transactionCbor" in attempt)
      expect(attemptInputs(attempt)).not.toContain(outRefLabel(chosen));
}, 300_000);

it("rebuilds an attempt funded from a wallet view that predates its holder's settlement, in the same run", async () => {
  const { h, contracts } = await setupHistoryContracts();
  h.lucid.clearUTxOOverride();
  let tx = h.lucid.newTx();
  for (let i = 0; i < 8; i++)
    tx = tx.pay.ToAddress(h.wallet.address, { lovelace: 5_000_000n });
  const fresh = await h.submit(
    "stale-view-nonces",
    await tx.complete({ localUPLCEval: true }),
  );
  const [waiterNonce] = await h.lucid.utxosByOutRef([
    { txHash: fresh, outputIndex: 0 },
  ]);
  const holderNonce = h.eventNonces[1]!;
  const withdrawalPayload = historyPairPayloads(h)[1]!;
  const [holder, waiter] = ["holder", "waiter"].map(
    (role) => `${role}-${randomUUID()}`,
  );
  const wallet = await h.lucid.wallet().address();
  // The holder records its admission, which spends wallet outputs, and holds
  // it in flight until the waiter has read the wallet a third time (its nonce,
  // then two fundings). That read returns the view from before the holder
  // broadcast, and only once the holder settled and released its outputs.
  let holderPending = false;
  let releaseHolder = () => {};
  const holderGate = new Promise<void>((resolve) => {
    releaseHolder = resolve;
  });
  let staleView: string[] = [];
  const waiterLucid = gateWalletReads(h.lucid, wallet, {
    before: async (read) => {
      if (read === 2)
        await until(async () => holderPending, "the holder recorded");
    },
    after: async (read, view) => {
      if (read !== 3) return;
      staleView = view.map(outRefLabel);
      releaseHolder();
      await until(async () => {
        const row = await journal(holder);
        return (
          Option.isSome(row) && row.value.checkpoint.admission !== undefined
        );
      }, "the holder settled");
    },
  });
  const staleWaits: string[] = [];
  const submit = (
    lucid: LucidEvolution,
    submissionId: string,
    kind: "Deposit" | "Withdrawal",
    nonce: UTxO,
    request: SDK.EventHistorySubmissionRequest,
    beforeAdmission?: () => Promise<void>,
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        submitDurableEventHistoryProgram({
          lucid,
          contracts,
          kind,
          submissionId,
          intentHash: "ef".repeat(32),
          nonceInput: nonce,
          scriptReference: h.scripts[kind === "Deposit" ? 0 : 1]!,
          prepare: () => Effect.succeed({ request }),
          beforeAdmission:
            beforeAdmission && (() => Effect.promise(beforeAdmission)),
        }).pipe(
          Effect.withClock(emulatorClock(h)),
          Effect.provide(
            Logger.add(
              Logger.make(({ message }) => {
                const text = String(message);
                if (text.includes("held when this submission read its funding"))
                  staleWaits.push(text);
              }),
            ),
          ),
        ),
      ),
    );
  const waiting = submit(
    waiterLucid,
    waiter,
    "Deposit",
    waiterNonce!,
    depositRequest(h, waiterNonce!),
  );
  await until(
    async () => Option.isSome(await journal(waiter)),
    "the waiter reserved its nonce",
  );
  const holding = submit(
    h.lucid,
    holder,
    "Withdrawal",
    holderNonce,
    {
      payload: withdrawalPayload,
      nonce: holderNonce,
      assets: { lovelace: 20_000_000n },
      structuralLovelace: 0n,
      structuralRefundKey: h.owner,
      reclaimAuth: { PublicKeyCredential: [h.owner] },
    },
    async () => {
      holderPending = true;
      await holderGate;
    },
  );
  const [held, waited] = await Promise.allSettled([holding, waiting]);
  if (held.status === "rejected") throw held.reason;
  if (waited.status === "rejected") throw waited.reason;
  // Non-vacuous: the waiter's stale view offered outputs the holder's
  // admission spent, and the waiter refused an attempt it funded with one.
  expect(
    attemptInputs(held.value.admission).some((input) =>
      staleView.includes(input),
    ),
  ).toBe(true);
  expect(staleWaits.length).toBeGreaterThan(0);
  for (const [submissionId, result] of [
    [holder, held.value],
    [waiter, waited.value],
  ] as const) {
    expect(
      (await h.lucid.transactionStatus(result.admission.txHash)).status,
    ).toBe("confirmed");
    const saved = await journal(submissionId);
    if (Option.isNone(saved)) throw new Error("Missing submission journal");
    expect(saved.value.checkpoint.pending).toBeUndefined();
    expect(saved.value.checkpoint.admission?.txHash).toBe(
      result.admission.txHash,
    );
  }
}, 300_000);
