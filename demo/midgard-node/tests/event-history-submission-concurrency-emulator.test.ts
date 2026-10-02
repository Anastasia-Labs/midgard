import { randomUUID } from "node:crypto";

import { type UTxO } from "@lucid-evolution/lucid";
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
