import { randomUUID } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import {
  historyAdmissionMetadata,
  submitDurableEventHistoryProgram,
} from "../src/transactions/event-history-submission.js";
import {
  depositRequest,
  emulatorClock,
  setupHistoryContracts,
  sharedPredecessorNonces,
} from "./event-history-submission-emulator.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const body = (attempt: SDK.EventHistorySubmissionAttempt) =>
  CML.Transaction.from_cbor_hex(attempt.transactionCbor).body();

const inputsOf = (attempt: SDK.EventHistorySubmissionAttempt) => {
  const inputs = body(attempt).inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index()}`;
  });
};

const journal = async (submissionId: string) => {
  const saved = await Effect.runPromise(
    provideDatabaseLayers(Journal.retrieve(submissionId)),
  );
  if (Option.isNone(saved)) throw new Error("Missing submission journal");
  return saved.value;
};

const holdings = (submissionId: string) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{ out_ref: string }>`SELECT out_ref
          FROM event_history_submission_inputs
          WHERE submission_id = ${submissionId}`;
        return rows.map((row) => row.out_ref);
      }),
    ),
  );

it("lets a submission take over a dead holder's expired attempt, and the holder's rerun settles it and lands once", async () => {
  const { h, contracts } = await setupHistoryContracts();
  const [holderNonce, takerNonce] = await sharedPredecessorNonces(h);
  const [holder, taker] = [1, 2].map(() => `takeover-${randomUUID()}`);
  let prepared = 0;
  let dies = true;
  const run = (nonce: UTxO, submissionId: string) =>
    Effect.runPromise(
      provideDatabaseLayers(
        Effect.either(
          submitDurableEventHistoryProgram({
            lucid: h.lucid,
            contracts,
            kind: "Deposit",
            submissionId,
            intentHash: "ef".repeat(32),
            nonceInput: nonce,
            scriptReference: h.scripts[0]!,
            prepare: () => {
              if (submissionId === holder) prepared++;
              return Effect.succeed({ request: depositRequest(h, nonce) });
            },
            // The holder records its admission and dies before broadcasting.
            beforeAdmission: () =>
              submissionId === holder && dies
                ? Effect.die(new Error("holder died"))
                : Effect.void,
          }).pipe(Effect.withClock(emulatorClock(h))),
        ),
      ),
    );

  const died = await run(holderNonce!, holder!);
  if (died._tag !== "Left") throw new Error("The holder did not die");
  expect(died.left.message).toContain("requires reconciliation");
  const dead = (await journal(holder!)).checkpoint.pending;
  if (dead === undefined) throw new Error("The holder saved no attempt");
  const ttl = Number(body(dead).ttl());
  // The dead attempt funds itself with every free wallet output, the taker's
  // nonce included, and spends the list predecessor both deposits share.
  const takerOutRef = `${takerNonce!.txHash}#${takerNonce!.outputIndex}`;
  expect(inputsOf(dead)).toContain(takerOutRef);
  const [head] = (
    await h.lucid.utxosByOutRef(
      inputsOf(dead).map((input) => {
        const [txHash, index] = input.split("#");
        return { txHash: txHash!, outputIndex: Number(index) };
      }),
    )
  ).filter((utxo) => utxo.address !== h.wallet.address);
  if (head === undefined) throw new Error("Dead attempt spends no list node");

  // While the dead attempt could still land, nothing it holds is free.
  const early = await run(takerNonce!, taker!);
  if (early._tag !== "Left") throw new Error("Took a landable attempt's input");
  expect(early.left.message).toContain(
    "No unreserved plain wallet nonce is available",
  );
  expect(await holdings(holder!)).toContain(takerOutRef);

  // Past its TTL, the same new submission takes over its nonce, funding and
  // the list predecessor, without anyone rerunning the holder.
  const slot = h.lucid.unixTimeToSlot(h.emulator.now());
  if (slot <= ttl) h.emulator.awaitSlot(ttl - slot + 1);
  const took = await run(takerNonce!, taker!);
  if (took._tag !== "Right") throw took.left;
  expect(inputsOf(took.right.admission)).toContain(
    `${head.txHash}#${head.outputIndex}`,
  );
  expect((await journal(holder!)).checkpoint.pending).toEqual(dead);
  expect(
    (await h.lucid.transactionStatus(took.right.admission.txHash)).status,
  ).toBe("confirmed");

  dies = false;
  const rerun = await run(holderNonce!, holder!);
  if (rerun._tag !== "Right") throw rerun.left;
  expect(rerun.right.admission.txHash).not.toBe(dead.txHash);
  expect(
    (await h.lucid.transactionStatus(rerun.right.admission.txHash)).status,
  ).toBe("confirmed");
  expect((await h.lucid.transactionStatus(dead.txHash)).status).not.toBe(
    "confirmed",
  );
  const again = await run(holderNonce!, holder!);
  if (again._tag !== "Right") throw again.left;
  expect(again.right.admission.txHash).toBe(rerun.right.admission.txHash);
  expect(prepared).toBe(1);
  // The dead attempt stays revivable in case a rollback lands it.
  expect(
    (await journal(holder!)).checkpoint.abandoned?.map(({ txHash }) => txHash),
  ).toEqual([dead.txHash]);
  // The holder's event is on the list exactly once.
  const { output, key } = historyAdmissionMetadata(
    h.lucid,
    rerun.right.admission,
  );
  const unit = contracts.deposit.policyId + key;
  expect(
    (await h.lucid.utxosAt(output.address)).filter(
      (utxo) => (utxo.assets[unit] ?? 0n) > 0n,
    ),
  ).toHaveLength(1);
  for (const id of [holder!, taker!]) {
    const saved = await journal(id);
    expect(saved.checkpoint.pending).toBeUndefined();
    expect(await holdings(id)).toEqual([saved.nonce_out_ref]);
  }
}, 300_000);
