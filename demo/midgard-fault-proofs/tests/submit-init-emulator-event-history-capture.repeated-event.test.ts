/** A committed event repeated by a later block, captured on the actual list
 * policies and classified by fabricated proof stages 02/03. The hub and
 * initial CT issuer are native fixtures, as in the capture suite. */
import "@al-ft/midgard-sdk";
import "effect";
import "vitest";
import "./submit-init-emulator-event-history-capture.setup-proof.js";
import "./submit-init-emulator-event-history-capture.build-capture.js";
import "./submit-init-emulator-event-history-capture.submit-production-classification.js";
import "./submit-init-emulator-event-history-list.journey.js";
import "./support/emulator/history-pair.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  buildCapture,
  continueCaptured,
} from "./submit-init-emulator-event-history-capture.build-capture.js";
import {
  blueprint,
  deployment,
  familyIndex,
  fetchWitness,
  type Harness,
  protectionDurationMs,
  records,
  setupProof,
  timing,
} from "./submit-init-emulator-event-history-capture.setup-proof.js";
import { submitProductionClassification } from "./submit-init-emulator-event-history-capture.submit-production-classification.js";
import { runJourney } from "./submit-init-emulator-event-history-list.journey.js";
import {
  promoteHistoryPair,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";

for (const kind of ["Deposit", "Withdrawal"] as const) {
  it(`${kind}: re-including an honestly committed event in the next block convicts as ineligible`, async () => {
    const h = await setupHistoryPair({
      blueprint,
      records,
      protectionDurationMs,
    });
    const payloads = await promoteHistoryPair(h);
    const payload = payloads[familyIndex(kind)]!;
    const nonce = h.eventNonces[familyIndex(kind)]!;
    const id = {
      transactionId: nonce.txHash,
      outputIndex: BigInt(nonce.outputIndex),
    };
    const w = await fetchWitness(h, kind, id);
    if (w.kind !== "Present") throw new Error("Missing admitted Order");
    const inclusionTime = SDK.captureEventHistoryWitness(
      w,
      deployment(h, kind).policyId,
      kind,
    ).commitment.inclusion_time;
    // The honest block ending at the inclusion time is the accused block of
    // the capture suite's "honest eligible event" test. Its successor starts
    // exactly there and repeats the identity with the same authentic content.
    const authenticHash =
      "DepositPayload" in payload
        ? Effect.runSync(
            SDK.depositInfoCommitment(payload.DepositPayload.event.info),
          )
        : Effect.runSync(
            SDK.withdrawalContentCommitment({
              ...payload.WithdrawalPayload.event.info,
              validity: "NonExistentWithdrawalUtxo",
            }),
          );
    const p = await setupProof(
      h,
      kind,
      id,
      inclusionTime + 100_000n,
      authenticHash,
    );
    expect(p.common.header_start_time).toBe(inclusionTime);
    const captureFrom = p.headerEnd + timing.slotLengthMs;
    if (BigInt(h.emulator.now()) < captureFrom)
      h.emulator.awaitSlot(
        Number((captureFrom - BigInt(h.emulator.now()) + 999n) / 1000n),
      );
    const window = SDK.eventHistoryCaptureWindow({
      now: BigInt(h.emulator.now()),
      headerEnd: p.headerEnd,
      mergeDeadline: p.headerEnd + SDK.MATURITY_DURATION_MS,
      protectedUntil: w.anchor.node.protected_until,
      timing,
    });
    const built = await buildCapture(h, p, w, window);
    const hash = await h.submit(`${kind}-capture-repeated-event`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    // Step 03 accepts the ineligible classification on chain with matching
    // content, and the production classifier reaches the same fault.
    await continueCaptured(h, p, thread, built.captured);
    const classified = await submitProductionClassification(
      h,
      p,
      thread,
      built.captured,
    );
    expect(classified.fault).toEqual(
      kind === "Deposit"
        ? { IneligibleDepositEvent: { event_inclusion_time: inclusionTime } }
        : {
            IneligibleWithdrawalEvent: { event_inclusion_time: inclusionTime },
          },
    );
    expect(BigInt(h.emulator.now())).toBeLessThan(
      p.headerEnd + SDK.MATURITY_DURATION_MS,
    );
  });

  it(`${kind}: re-including an event after its Order retired convicts as nonexistent`, async () => {
    // A real settlement-backed retirement on the applied single-kind list.
    const list = await runJourney(kind, false, "settle");
    if (list === undefined) throw new Error("Retirement journey stopped early");
    // The capture helpers read only the shared harness fields and this
    // kind's list deployment.
    const h = {
      ...list,
      applied: [list.applied, list.applied],
    } as unknown as Harness;
    const id = list.originalId;
    const absence = await fetchWitness(h, kind, id);
    expect(absence.kind).toBe("Absent");
    // Move past the retired event's inclusion time so the accused block lies
    // wholly after the block that legitimately carried it.
    h.emulator.awaitSlot(Number(SDK.MATURITY_DURATION_MS / 1000n));
    const headerEnd = BigInt(h.emulator.now()) - 200_000n;
    const p = await setupProof(h, kind, id, headerEnd);
    const window = SDK.eventHistoryCaptureWindow({
      now: BigInt(h.emulator.now()),
      headerEnd,
      mergeDeadline: headerEnd + SDK.MATURITY_DURATION_MS,
      protectedUntil: absence.anchor.node.protected_until,
      timing,
    });
    const built = await buildCapture(h, p, absence, window);
    expect(built.captured).toBeUndefined();
    const hash = await h.submit(`${kind}-capture-retired-event`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    await continueCaptured(h, p, thread, undefined);
    const classified = await submitProductionClassification(
      h,
      p,
      thread,
      undefined,
    );
    expect(classified.fault).toBe(
      kind === "Deposit"
        ? "NonexistentDepositIdentity"
        : "NonexistentWithdrawalIdentity",
    );
  });
}
