import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, it, vi } from "vitest";

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

it("adopts an admission that landed although the rerun first settled it as expired and unseen", async () => {
  const { h, contracts } = await setupHistoryContracts();
  const [nonce] = await sharedPredecessorNonces(h);
  const submissionId = `revival-${randomUUID()}`;
  let prepared = 0;
  let dies = true;
  const run = () =>
    Effect.runPromise(
      provideDatabaseLayers(
        Effect.either(
          submitDurableEventHistoryProgram({
            lucid: h.lucid,
            contracts,
            kind: "Deposit",
            submissionId,
            intentHash: "ab".repeat(32),
            nonceInput: nonce,
            scriptReference: h.scripts[0]!,
            prepare: () => {
              prepared++;
              return Effect.succeed({ request: depositRequest(h, nonce!) });
            },
            beforeAdmission: () =>
              dies ? Effect.die(new Error("process died")) : Effect.void,
          }).pipe(Effect.withClock(emulatorClock(h))),
        ),
      ),
    );

  const died = await run();
  if (died._tag !== "Left") throw new Error("The submission did not die");
  const landed = (await journal(submissionId)).checkpoint.pending;
  if (landed === undefined) throw new Error("The submission saved no attempt");
  // The process broadcast the admission before it died, and it landed.
  h.lucid.clearUTxOOverride();
  await h.submit("dead-admission", h.lucid.fromTx(landed.transactionCbor));
  const ttl = Number(
    CML.Transaction.from_cbor_hex(landed.transactionCbor).body().ttl(),
  );
  const slot = h.lucid.unixTimeToSlot(h.emulator.now());
  if (slot <= ttl) h.emulator.awaitSlot(ttl - slot + 1);

  // Past its TTL, the provider answering the rerun's two reconcile
  // observations has not indexed it (it follows a fork without it), so the
  // rerun settles the admission as unable to land; the nonce read after that
  // sees the chain that has it, and the revival's own observation finds it.
  let observed = 0;
  const status = h.lucid.transactionStatus.bind(h.lucid);
  vi.spyOn(h.lucid, "transactionStatus").mockImplementation(async (txHash) =>
    txHash === landed.txHash && ++observed <= 2
      ? { status: "not_found", txHash }
      : status(txHash),
  );
  dies = false;
  const rerun = await run();
  if (rerun._tag !== "Right") throw rerun.left;
  expect(observed).toBe(3);
  expect(rerun.right.admission).toEqual(landed);
  const saved = await journal(submissionId);
  expect(saved.checkpoint.admission).toEqual(landed);
  expect(saved.checkpoint.pending).toBeUndefined();
  expect(saved.checkpoint.abandoned).toEqual([]);
  expect(await holdings(submissionId)).toEqual([saved.nonce_out_ref]);
  expect(prepared).toBe(1);
  // No second admission was built: the event is on the list exactly once.
  const { output, key } = historyAdmissionMetadata(h.lucid, landed);
  const unit = contracts.deposit.policyId + key;
  expect(
    (await h.lucid.utxosAt(output.address)).filter(
      (utxo) => (utxo.assets[unit] ?? 0n) > 0n,
    ),
  ).toHaveLength(1);
}, 300_000);
