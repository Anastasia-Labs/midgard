import { randomUUID } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import {
  historyAdmissionMetadata,
  submitDurableEventHistoryProgram,
} from "../src/transactions/event-history-submission.js";
import {
  depositRequest,
  emulatorClock,
  type Fixture,
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

/** A deposit whose datum exceeds the inline limit, so it is published first. */
const externalRequest = (
  h: Fixture,
  nonce: UTxO,
): SDK.EventHistorySubmissionRequest => {
  const request = depositRequest(h, nonce);
  if (request.payload === undefined || !("DepositPayload" in request.payload))
    throw new Error("Fixture deposit payload is missing");
  const { event } = request.payload.DepositPayload;
  return {
    ...request,
    payload: {
      DepositPayload: {
        event: {
          ...event,
          info: { ...event.info, l2_datum: "ab".repeat(600) },
        },
      },
    },
  };
};

const setup = async () => {
  const { h, contracts } = await setupHistoryContracts();
  const [holderNonce, takerNonce] = await sharedPredecessorNonces(h);
  let prepared = 0;
  const run = (
    nonce: UTxO,
    submissionId: string,
    external: boolean,
    dieBeforeAdmission = false,
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        Effect.either(
          submitDurableEventHistoryProgram({
            lucid: h.lucid,
            contracts,
            kind: "Deposit",
            submissionId,
            intentHash: "cd".repeat(32),
            nonceInput: nonce,
            scriptReference: h.scripts[0]!,
            prepare: () => {
              if (external) prepared++;
              return Effect.succeed({
                request: external
                  ? externalRequest(h, nonce)
                  : depositRequest(h, nonce),
              });
            },
            // The process records its admission and dies before broadcasting.
            beforeAdmission: () =>
              dieBeforeAdmission
                ? Effect.die(new Error("process died before broadcasting"))
                : Effect.void,
          }).pipe(Effect.withClock(emulatorClock(h))),
        ),
      ),
    );
  /** The holder saves its pending publication and dies before signing it. */
  const dieBeforeBroadcast = async (submissionId: string) => {
    const signing = vi
      .spyOn(h.lucid.wallet(), "signTx")
      .mockRejectedValueOnce(new Error("process died before signing"));
    const died = await run(holderNonce!, submissionId, true);
    signing.mockRestore();
    if (died._tag !== "Left") throw new Error("The holder did not die");
    expect(died.left.message).toContain("requires reconciliation");
    const dead = (await journal(submissionId)).checkpoint.pending;
    if (dead?.phase !== "Publication")
      throw new Error("The holder saved no pending publication");
    const ttl = body(dead).ttl();
    if (ttl === undefined) throw new Error("The publication has no TTL");
    return { dead, ttl: Number(ttl) };
  };
  const slot = () => h.lucid.unixTimeToSlot(h.emulator.now());
  const retained = async () =>
    (await h.lucid.utxosAt(h.applied[0]!.retention.address)).map(
      ({ txHash }) => txHash,
    );
  /** Exactly one list node carries the admitted event's token. */
  const listedOnce = async (admission: SDK.EventHistorySubmissionAttempt) => {
    const { output, key } = historyAdmissionMetadata(h.lucid, admission);
    const unit = contracts.deposit.policyId + key;
    expect(
      (await h.lucid.utxosAt(output.address)).filter(
        (utxo) => (utxo.assets[unit] ?? 0n) > 0n,
      ),
    ).toHaveLength(1);
  };
  return {
    h,
    run,
    holderNonce: holderNonce!,
    takerNonce: takerNonce!,
    dieBeforeBroadcast,
    slot,
    retained,
    listedOnce,
    prepared: () => prepared,
  };
};

afterEach(() => {
  vi.restoreAllMocks();
});

it("frees a dead publication holder's inputs only past its TTL, and the holder's rerun publishes and admits once", async () => {
  const s = await setup();
  const [holder, taker] = [1, 2].map(() => `publication-${randomUUID()}`);
  const { dead, ttl } = await s.dieBeforeBroadcast(holder!);
  // The dead publication funds itself with every free wallet output,
  // including the output a later submission would take as its nonce.
  const takerOutRef = `${s.takerNonce.txHash}#${s.takerNonce.outputIndex}`;
  expect(inputsOf(dead)).toContain(takerOutRef);
  expect(ttl).toBeGreaterThan(s.slot());

  // While it could still land, nothing it holds is free.
  const early = await s.run(s.takerNonce, taker!, false);
  if (early._tag !== "Left") throw new Error("Took a landable attempt's input");
  expect(early.left.message).toContain(
    "No unreserved plain wallet nonce is available",
  );
  expect(await holdings(holder!)).toContain(takerOutRef);

  // Past its TTL, the same new submission takes that output over without
  // anyone rerunning the holder, whose journal it leaves untouched.
  if (s.slot() <= ttl) s.h.emulator.awaitSlot(ttl - s.slot() + 1);
  const took = await s.run(s.takerNonce, taker!, false);
  if (took._tag !== "Right") throw took.left;
  expect(took.right.request.nonce.txHash).toBe(s.takerNonce.txHash);
  expect((await journal(holder!)).checkpoint.pending).toEqual(dead);

  const before = await s.retained();
  const rerun = await s.run(s.holderNonce, holder!, true);
  if (rerun._tag !== "Right") throw rerun.left;
  const publication = rerun.right.checkpoint.publicationAttempt;
  if (publication === undefined) throw new Error("No publication receipt");
  expect(publication.txHash).not.toBe(dead.txHash);
  expect((await s.h.lucid.transactionStatus(dead.txHash)).status).not.toBe(
    "confirmed",
  );
  // One retention output, the new publication's, which the admission used.
  expect((await s.retained()).filter((tx) => !before.includes(tx))).toEqual([
    publication.txHash,
  ]);
  expect(
    (await s.h.lucid.transactionStatus(rerun.right.admission.txHash)).status,
  ).toBe("confirmed");
  await s.listedOnce(rerun.right.admission);
  // The dead body stays adoptable in case a rollback lands it.
  expect(rerun.right.checkpoint.abandoned?.map(({ txHash }) => txHash)).toEqual(
    [dead.txHash],
  );
  const again = await s.run(s.holderNonce, holder!, true);
  if (again._tag !== "Right") throw again.left;
  expect(again.right.admission.txHash).toBe(rerun.right.admission.txHash);
  expect(s.prepared()).toBe(1);
  for (const id of [holder!, taker!]) {
    const saved = await journal(id);
    expect(saved.checkpoint.pending).toBeUndefined();
    expect(await holdings(id)).toEqual([saved.nonce_out_ref]);
  }
}, 300_000);

it.each([
  { hiddenReads: 1, settled: "Confirmed" },
  { hiddenReads: 2, settled: "InputConflict, then adopted" },
])(
  "keeps a publication that landed just before its TTL although $hiddenReads reads missed it ($settled)",
  async ({ hiddenReads }) => {
    const s = await setup();
    const holder = `publication-${randomUUID()}`;
    const { dead: landed, ttl } = await s.dieBeforeBroadcast(holder);
    // The process broadcast it in the last slot of its validity, and it
    // landed; the rerun comes after the TTL.
    s.h.emulator.awaitSlot(ttl - 1 - s.slot());
    expect(s.slot()).toBe(ttl - 1);
    const before = await s.retained();
    s.h.lucid.clearUTxOOverride();
    await s.h.submit(
      "landed-publication",
      s.h.lucid.fromTx(landed.transactionCbor),
    );
    expect((await s.retained()).filter((tx) => !before.includes(tx))).toEqual([
      landed.txHash,
    ]);
    if (s.slot() <= ttl) s.h.emulator.awaitSlot(ttl - s.slot() + 1);

    // The provider answering the first reads follows a fork without it.
    let observed = 0;
    const status = s.h.lucid.transactionStatus.bind(s.h.lucid);
    vi.spyOn(s.h.lucid, "transactionStatus").mockImplementation(
      async (txHash) =>
        txHash === landed.txHash && ++observed <= hiddenReads
          ? { status: "not_found", txHash }
          : status(txHash),
    );
    const rerun = await s.run(s.holderNonce, holder, true);
    if (rerun._tag !== "Right") throw rerun.left;
    // The reconcile read, the settlement's read after the slot and, once it
    // settled as unable to land, the adoption's read.
    expect(observed).toBe(hiddenReads + 1);
    expect(rerun.right.checkpoint.publicationAttempt).toEqual(landed);
    if (hiddenReads === 1)
      expect(rerun.right.checkpoint.abandoned).toBeUndefined();
    else expect(rerun.right.checkpoint.abandoned).toEqual([]);
    // Nothing was published again.
    expect((await s.retained()).filter((tx) => !before.includes(tx))).toEqual([
      landed.txHash,
    ]);
    expect(
      (await s.h.lucid.transactionStatus(rerun.right.admission.txHash)).status,
    ).toBe("confirmed");
    await s.listedOnce(rerun.right.admission);
    expect(s.prepared()).toBe(1);
    const saved = await journal(holder);
    expect(saved.checkpoint.pending).toBeUndefined();
    expect(await holdings(holder)).toEqual([saved.nonce_out_ref]);
  },
  300_000,
);

/** The holder's publication confirmed on a fork, it recorded an admission
 * against that fork's output and died before broadcasting it, and the fork
 * was rolled back: the emulator chain never had the publication. Past the
 * admission's TTL a second submission takes over one of its funding inputs as
 * its nonce and completes. */
const rolledBackPublicationTakenOver = async (
  s: Awaited<ReturnType<typeof setup>>,
) => {
  const [holder, taker] = [1, 2].map(() => `publication-${randomUUID()}`);
  const { dead: publication } = await s.dieBeforeBroadcast(holder!);
  const isPublished = (ref: Pick<UTxO, "txHash" | "outputIndex">) =>
    ref.txHash === publication.txHash &&
    ref.outputIndex === publication.outputIndex;
  const published: UTxO = {
    txHash: publication.txHash,
    outputIndex: publication.outputIndex,
    ...coreToTxOutput(body(publication).outputs().get(publication.outputIndex)),
  };
  const status = s.h.lucid.transactionStatus.bind(s.h.lucid);
  const byOutRef = s.h.lucid.utxosByOutRef.bind(s.h.lucid);
  const confirmedOnFork = vi
    .spyOn(s.h.lucid, "transactionStatus")
    .mockImplementation(async (txHash) =>
      txHash === publication.txHash
        ? { status: "confirmed", txHash, confirmation: { txHash } }
        : status(txHash),
    );
  const visibleOnFork = vi
    .spyOn(s.h.lucid, "utxosByOutRef")
    .mockImplementation(async (refs) => {
      const rest = refs.filter((ref) => !isPublished(ref));
      return [
        ...(rest.length === 0 ? [] : await byOutRef(rest)),
        ...(rest.length < refs.length ? [published] : []),
      ];
    });
  const died = await s.run(s.holderNonce, holder!, true, true);
  confirmedOnFork.mockRestore();
  visibleOnFork.mockRestore();
  if (died._tag !== "Left") throw new Error("The holder did not die");
  expect(died.left.message).toContain("requires reconciliation");
  const recorded = (await journal(holder!)).checkpoint;
  const admission = recorded.pending;
  if (admission?.phase !== "Admission")
    throw new Error("The holder saved no pending admission");
  expect(recorded.publicationAttempt).toEqual(publication);
  expect(
    (await s.h.lucid.transactionStatus(publication.txHash)).status,
  ).not.toBe("confirmed");
  // The admission funds itself with every free wallet output, including the
  // output the second submission takes as its nonce.
  const takerOutRef = `${s.takerNonce.txHash}#${s.takerNonce.outputIndex}`;
  expect(inputsOf(admission)).toContain(takerOutRef);

  const early = await s.run(s.takerNonce, taker!, false);
  if (early._tag !== "Left") throw new Error("Took a landable attempt's input");
  expect(early.left.message).toContain(
    "No unreserved plain wallet nonce is available",
  );
  const ttl = Number(body(admission).ttl());
  if (s.slot() <= ttl) s.h.emulator.awaitSlot(ttl - s.slot() + 1);
  const took = await s.run(s.takerNonce, taker!, false);
  if (took._tag !== "Right") throw took.left;
  expect(await holdings(taker!)).toEqual([takerOutRef]);
  expect((await journal(holder!)).checkpoint).toEqual(recorded);
  return {
    holder: holder!,
    taker: taker!,
    publication,
    admission,
    took: took.right.admission,
  };
};

/** The holder's rerun publishes and admits once, and both rolled-back
 * attempts stay adoptable in case a rollback lands them. */
const rerunAdmitsOnce = async (
  s: Awaited<ReturnType<typeof setup>>,
  scenario: Awaited<ReturnType<typeof rolledBackPublicationTakenOver>>,
  before: readonly string[],
) => {
  const rerun = await s.run(s.holderNonce, scenario.holder, true);
  if (rerun._tag !== "Right") throw rerun.left;
  const republished = rerun.right.checkpoint.publicationAttempt;
  if (republished === undefined) throw new Error("No publication receipt");
  expect(republished.txHash).not.toBe(scenario.publication.txHash);
  // One retention output, the new publication's, which the admission used.
  expect((await s.retained()).filter((tx) => !before.includes(tx))).toEqual([
    republished.txHash,
  ]);
  expect(rerun.right.admission.txHash).not.toBe(scenario.admission.txHash);
  expect(
    (await s.h.lucid.transactionStatus(rerun.right.admission.txHash)).status,
  ).toBe("confirmed");
  await s.listedOnce(rerun.right.admission);
  await s.listedOnce(scenario.took);
  expect(rerun.right.checkpoint.abandoned?.map(({ txHash }) => txHash)).toEqual(
    [scenario.admission.txHash, scenario.publication.txHash],
  );
  const again = await s.run(s.holderNonce, scenario.holder, true);
  if (again._tag !== "Right") throw again.left;
  expect(again.right.admission.txHash).toBe(rerun.right.admission.txHash);
  const takerAgain = await s.run(s.takerNonce, scenario.taker, false);
  if (takerAgain._tag !== "Right") throw takerAgain.left;
  expect(takerAgain.right.admission.txHash).toBe(scenario.took.txHash);
  expect(s.prepared()).toBe(1);
  for (const id of [scenario.holder, scenario.taker]) {
    const saved = await journal(id);
    expect(saved.checkpoint.pending).toBeUndefined();
    expect(await holdings(id)).toEqual([saved.nonce_out_ref]);
  }
};

it("settles the pending admission before abandoning a rolled-back publication receipt, so a takeover of its funding does not wedge the holder's rerun", async () => {
  const s = await setup();
  const scenario = await rolledBackPublicationTakenOver(s);
  await rerunAdmitsOnce(s, scenario, await s.retained());
}, 300_000);

it("abandons nothing while the pending admission's status is unknown, and completes once it resolves", async () => {
  const s = await setup();
  const scenario = await rolledBackPublicationTakenOver(s);
  const recorded = await journal(scenario.holder);
  const held = await holdings(scenario.holder);
  const before = await s.retained();
  // The provider cannot report the pending admission's status.
  const status = s.h.lucid.transactionStatus.bind(s.h.lucid);
  const unknown = vi
    .spyOn(s.h.lucid, "transactionStatus")
    .mockImplementation(async (txHash) => {
      if (txHash === scenario.admission.txHash)
        throw new Error("provider timed out");
      return status(txHash);
    });
  const stopped = await s.run(s.holderNonce, scenario.holder, true);
  unknown.mockRestore();
  if (stopped._tag !== "Left") throw new Error("Settled an unknown admission");
  expect(stopped.left.message).toContain(
    "Reconcile the pending history transaction before resubmission",
  );
  const unchanged = await journal(scenario.holder);
  expect(unchanged.revision).toBe(recorded.revision);
  expect(JSON.stringify(unchanged.checkpoint)).toBe(
    JSON.stringify(recorded.checkpoint),
  );
  expect(await holdings(scenario.holder)).toEqual(held);
  expect(await s.retained()).toEqual(before);
  await rerunAdmitsOnce(s, scenario, before);
}, 300_000);
