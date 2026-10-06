import { randomUUID } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, type UTxO } from "@lucid-evolution/lucid";
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
  const until = async (condition: () => Promise<boolean>, what: string) => {
    const deadline = Date.now() + 120_000;
    while (!(await condition())) {
      if (Date.now() > deadline) throw new Error(`Timed out until ${what}`);
      await new Promise((resolve) => setTimeout(resolve, 25));
    }
  };
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
