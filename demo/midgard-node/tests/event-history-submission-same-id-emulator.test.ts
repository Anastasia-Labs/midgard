import { randomUUID } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, it } from "vitest";

import { submitDurableEventHistoryProgram } from "../src/transactions/event-history-submission.js";
import {
  gateWalletReads,
  journal,
  until,
  waitingOnLock,
} from "./event-history-submission-concurrency.fixture.js";
import {
  depositRequest,
  emulatorClock,
  setupHistoryContracts,
  sharedPredecessorNonces,
} from "./event-history-submission-emulator.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

/** What stops the run that did not land, by where the winner's admission
 * lands relative to it: before its journal save, between its nonce check and
 * its build's read of the event, or before its nonce check. */
const stops = {
  journal: /Concurrent history submission changed; reload and reconcile/u,
  event: /History event is already admitted/u,
  nonce: /History nonce is unavailable; reconcile its spending transaction/u,
};

/** Runs one submission ID twice at once. The first reserves its pinned nonce
 * only once the second, which found no journal for the ID, waits for the
 * wallet lock. A forced `stop` holds the second until the first settled, at
 * its funding read (`event`), which follows its nonce check, or at that nonce
 * check (`nonce`); the first admits only once the second is held. Returns the
 * rejections, the journal row once both settled and the landed admission. */
const sameIdRuns = async (stop?: "event" | "nonce") => {
  const { h, contracts } = await setupHistoryContracts();
  const [nonce] = await sharedPredecessorNonces(h);
  const submissionId = `same-id-${randomUUID()}`;
  let [held, firstSettled] = [false, false];
  const hold = async () => {
    held = true;
    await until(async () => firstSettled, "the first run settled");
  };
  const run = (
    prepare: () => Effect.Effect<{
      request: SDK.EventHistorySubmissionRequest;
    }>,
    lucid = h.lucid,
    beforeAdmission?: () => Promise<void>,
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        submitDurableEventHistoryProgram({
          lucid,
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
  let picked = false;
  const first = run(
    () =>
      Effect.promise(async () => {
        picked = true;
        await until(waitingOnLock, "the second run waited on the wallet lock");
        return { request: depositRequest(h, nonce!) };
      }),
    h.lucid,
    stop && (() => until(async () => held, "the second run was held")),
  ).finally(() => {
    firstSettled = true;
  });
  await until(async () => picked, "the first run picked its nonce");
  const nonceLabel = outRefLabel(nonce!);
  // The second's first wallet read is its funding.
  const gated: LucidEvolution =
    stop === "nonce"
      ? {
          ...h.lucid,
          utxosByOutRef: async (refs) => {
            if (!held && refs.some((ref) => outRefLabel(ref) === nonceLabel))
              await hold();
            return h.lucid.utxosByOutRef(refs);
          },
        }
      : stop === undefined
        ? h.lucid
        : gateWalletReads(h.lucid, await h.lucid.wallet().address(), {
            before: async (read) => {
              if (read === 1) await hold();
            },
          });
  const second = run(
    () =>
      Effect.die(new Error("A second run of one submission ID chose a nonce")),
    gated,
  );
  const outcomes = await Promise.allSettled([first, second]);
  // The second run took the first's reservation instead of choosing again.
  // Both then build on one journal row, so whichever saves first lands; the
  // other lands the same admission or stops on the winner's progress.
  const landed = outcomes.flatMap((outcome) =>
    outcome.status === "fulfilled" ? [outcome.value.admission.txHash] : [],
  );
  if (landed.length === 0) throw (outcomes[0] as PromiseRejectedResult).reason;
  expect(new Set(landed).size).toBe(1);
  const settled = await journal(submissionId);
  // A stopped run wrote nothing: a plain rerun of the ID, which may not
  // choose a nonce, finishes on the landed admission without repair.
  const rerun = await run(() => Effect.die(new Error("A rerun chose a nonce")));
  expect(rerun.admission.txHash).toBe(landed[0]);
  const saved = await journal(submissionId);
  if (Option.isNone(saved) || Option.isNone(settled))
    throw new Error("Missing submission journal");
  expect(saved.value.nonce_out_ref).toBe(outRefLabel(nonce!));
  expect(saved.value.checkpoint.admission?.txHash).toBe(landed[0]);
  const rejections = outcomes.flatMap((outcome) =>
    outcome.status === "rejected" ? [String(outcome.reason)] : [],
  );
  return { rejections, settled: settled.value, landed: landed[0] };
};

it("continues a concurrent run of the same submission ID with the nonce that run reserved", async () => {
  // Unforced, the order depends on load: locally the second stops at its
  // journal save; the rows below force the two later stops.
  const { rejections } = await sameIdRuns();
  for (const reason of rejections)
    expect(reason).toMatch(
      new RegExp(
        Object.values(stops)
          .map((pattern) => pattern.source)
          .join("|"),
        "u",
      ),
    );
}, 300_000);

it.each([
  ["between its nonce check and its read of the event", "event"],
  ["before its nonce check", "nonce"],
] as const)(
  "stops the other run of a same-ID submission, leaving it to a rerun, when one lands %s",
  async (_where, stop) => {
    const { rejections, settled, landed } = await sameIdRuns(stop);
    expect(rejections).toHaveLength(1);
    expect(rejections[0]).toMatch(stops[stop]);
    // Only the first run wrote the journal: its reservation, its pending
    // attempt and its admission.
    expect(settled.revision).toBe(2);
    expect(settled.checkpoint.pending).toBeUndefined();
    expect(settled.checkpoint.admission?.txHash).toBe(landed);
  },
  300_000,
);
