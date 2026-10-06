import { randomUUID } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import type { LucidEvolution, OutRef } from "@lucid-evolution/lucid";
import { Effect, Logger, Option } from "effect";
import { expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { submitDurableEventHistoryProgram } from "../src/transactions/event-history-submission.js";
import {
  depositRequest,
  emulatorClock,
  type Fixture,
  setupHistoryContracts,
  sharedPredecessorNonces,
} from "./event-history-submission-emulator.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

/** Runs one deposit whose provider calls `onAttemptRead` on each read of a
 * new attempt's inputs (every input, the nonce among them) before serving
 * it, checks that the run landed its admission, and returns the run's
 * result, its journal and its warnings. */
const depositWithAttemptReads = async (
  onAttemptRead: (
    h: Fixture,
    refs: readonly OutRef[],
    nonce: OutRef,
  ) => Promise<void>,
) => {
  const { h, contracts } = await setupHistoryContracts();
  const [nonce] = await sharedPredecessorNonces(h);
  const submissionId = `backstop-${randomUUID()}`;
  const lucid: LucidEvolution = {
    ...h.lucid,
    utxosByOutRef: async (
      refs: Parameters<LucidEvolution["utxosByOutRef"]>[0],
    ) => {
      if (
        refs.length > 1 &&
        refs.some((ref) => outRefLabel(ref) === outRefLabel(nonce!))
      )
        await onAttemptRead(h, refs, nonce!);
      return h.lucid.utxosByOutRef(refs);
    },
  };
  const warnings: string[] = [];
  const result = await Effect.runPromise(
    provideDatabaseLayers(
      submitDurableEventHistoryProgram({
        lucid,
        contracts,
        kind: "Deposit",
        submissionId,
        intentHash: "ef".repeat(32),
        nonceInput: nonce!,
        scriptReference: h.scripts[0]!,
        prepare: () => Effect.succeed({ request: depositRequest(h, nonce!) }),
      }).pipe(
        Effect.withClock(emulatorClock(h)),
        Effect.provide(
          Logger.add(
            Logger.make(({ logLevel, message }) => {
              if (logLevel._tag === "Warning") warnings.push(String(message));
            }),
          ),
        ),
      ),
    ),
  );
  const saved = await Effect.runPromise(
    provideDatabaseLayers(Journal.retrieve(submissionId)),
  );
  if (Option.isNone(saved)) throw new Error("Missing submission journal");
  expect(
    (await h.lucid.transactionStatus(result.admission.txHash)).status,
  ).toBe("confirmed");
  expect(saved.value.checkpoint.pending).toBeUndefined();
  expect(saved.value.checkpoint.admission?.txHash).toBe(
    result.admission.txHash,
  );
  return { result, saved: saved.value, warnings };
};

it("abandons an unsent attempt whose inputs the provider fails to read, and lands in the same run", async () => {
  let failed = 0;
  const { saved, result, warnings } = await depositWithAttemptReads(
    async () => {
      if (failed++ === 0) throw new Error("transient provider 503");
    },
  );
  expect(failed).toBeGreaterThan(1);
  const abandoned = (saved.checkpoint.abandoned ?? []).map(
    ({ txHash }) => txHash,
  );
  expect(abandoned).toHaveLength(1);
  expect(abandoned).not.toContain(result.admission.txHash);
  expect(
    warnings.filter((warning) =>
      warning.includes("could not read the inputs of its unsent attempt"),
    ),
  ).toHaveLength(1);
}, 300_000);

it("abandons an unsent attempt whose input was spent outside the journal, and lands in the same run", async () => {
  let spent: string | undefined;
  const { saved, result } = await depositWithAttemptReads(
    async (h, refs, nonce) => {
      if (spent !== undefined) return;
      // The wallet spends one of the attempt's funding inputs itself, as an
      // unrelated transaction would, after the attempt was built.
      const nonceLabel = outRefLabel(nonce);
      const labels = new Set(refs.map(outRefLabel));
      h.lucid.clearUTxOOverride();
      const funding = (await h.lucid.utxosAt(h.wallet.address)).find(
        (utxo) =>
          labels.has(outRefLabel(utxo)) && outRefLabel(utxo) !== nonceLabel,
      );
      if (funding === undefined)
        throw new Error("The attempt spends no wallet funding output");
      spent = outRefLabel(funding);
      await h.submit(
        "spend-attempt-input",
        await h.lucid
          .newTx()
          .collectFrom([funding])
          .complete({ coinSelection: false, localUPLCEval: true }),
      );
    },
  );
  if (spent === undefined) throw new Error("No attempt input was spent");
  expect(saved.checkpoint.abandoned ?? []).toHaveLength(1);
  expect(Journal.attemptSpend(result.admission).inputs).not.toContain(spent);
}, 300_000);
