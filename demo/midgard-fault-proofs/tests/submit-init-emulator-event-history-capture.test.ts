/** Actual list mutations and fabricated proof stages 02/03. The hub and initial
 * CT issuer are native fixtures: this does not claim full installed-family acceptance. */
import "node:crypto";
import "node:fs";
import "node:path";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/fabricated-history-witness.js";
import "../src/submit-fabricated-deposit-step-02.js";
import "../src/submit-fabricated-deposit-step-03.js";
import "../src/submit-fabricated-withdrawal-step-02.js";
import "../src/submit-fabricated-withdrawal-step-03.js";
import "../src/workflow/transaction-boundary.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/history-pair.js";
import "./support/emulator/measurement.js";
import "./support/emulator/protocol-parameters.js";
import "./submit-init-emulator-event-history-capture.setup-proof.js";
import "./submit-init-emulator-event-history-capture.build-capture.js";
import "./submit-init-emulator-event-history-capture.submit-production-classification.js";

import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it, vi } from "vitest";

import { fabricatedHistoryOpeningCbor } from "../src/fabricated-history-witness.js";
import { submitFabricatedDepositStep02 } from "../src/submit-fabricated-deposit-step-02.js";
import { submitFabricatedWithdrawalStep02 } from "../src/submit-fabricated-withdrawal-step-02.js";
import { workflowTransactionReferenceInputOutRefs } from "../src/workflow/transaction-boundary.js";
import {
  buildCapture,
  continueCaptured,
  productionContracts,
} from "./submit-init-emulator-event-history-capture.build-capture.js";
import {
  blueprint,
  blueprintBytes,
  deployment,
  familyIndex,
  fetchWitness,
  nextChurnKey,
  protectionDurationMs,
  records,
  setupProof,
  timing,
  waitForMutation,
} from "./submit-init-emulator-event-history-capture.setup-proof.js";
import { submitProductionClassification } from "./submit-init-emulator-event-history-capture.submit-production-classification.js";
import {
  historyPairPayloads,
  insertHistoryFillerAfter as buildChurn,
  promoteHistoryPair,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

afterAll(() => {
  const dir = join(process.cwd(), "../../artifacts/event-history");
  mkdirSync(dir, { recursive: true });
  writeFileSync(
    join(dir, "history-capture-applied.json"),
    JSON.stringify(
      {
        scope:
          "Actual list policies and fabricated steps 02/03; native fixture hub and initial CT issuer. Simulated bounded inclusion, not live or full-family acceptance.",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protectionDurationMs,
        timing,
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

for (const kind of ["Deposit", "Withdrawal"] as const)
  describe(`${kind} history proof capture`, () => {
    for (const mode of ["absent", "inline", "external"] as const)
      it(`production capture discovers current ${mode} history and ignores stale pointer hints`, async () => {
        const presence = mode !== "absent";
        const h = await setupHistoryPair({
          blueprint,
          records,
          protectionDurationMs,
        });
        if (presence) {
          const payloads = historyPairPayloads(h);
          if (mode === "external")
            for (const payload of payloads) {
              if ("DepositPayload" in payload)
                payload.DepositPayload.event.info.l2_datum = "ab".repeat(2000);
              else
                payload.WithdrawalPayload.event.info.body.l1_datum = {
                  InlineDatum: { data: "cd".repeat(2000) },
                };
            }
          await promoteHistoryPair(h, false, payloads);
        }
        const nonce = h.eventNonces[familyIndex(kind)]!;
        const id = presence
          ? {
              transactionId: nonce.txHash,
              outputIndex: BigInt(nonce.outputIndex),
            }
          : { transactionId: "fe".repeat(32), outputIndex: 987654321n };
        const headerEnd =
          BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 700_000n;
        const p = await setupProof(h, kind, id, headerEnd);
        const old = await fetchWitness(h, kind, id);
        waitForMutation(h, old);
        await h.submit(
          `${kind}-production-pointer-churn`,
          await buildChurn(h, kind, old, await nextChurnKey(old, id), [p.fee]),
        );
        const current = await fetchWitness(h, kind, id);
        const captured =
          current.kind === "Present"
            ? SDK.captureEventHistoryWitness(
                current,
                deployment(h, kind).policyId,
                kind,
              )
            : undefined;
        const openingCbor = captured
          ? fabricatedHistoryOpeningCbor(captured)
          : null;
        const submit =
          kind === "Deposit"
            ? submitFabricatedDepositStep02
            : submitFabricatedWithdrawalStep02;
        const args = {
          lucid: h.lucid,
          contracts: productionContracts(h, p),
          network: "Custom" as const,
          signer: {
            source: "emulator fixture",
            address: h.wallet.address,
            paymentKeyHash: h.owner,
            selectWallet: (lucid: typeof h.lucid) =>
              lucid.selectWallet.fromSeed(h.wallet.seedPhrase),
          },
          threadOutRef: `${p.thread.txHash}#${p.thread.outputIndex}`,
          evidence: presence
            ? {
                kind: "present_event" as const,
                eventOutRef: `${old.anchor.utxo.txHash}#${old.anchor.utxo.outputIndex}`,
              }
            : { kind: "absent_identity" as const },
          referenceScriptUtxo: p.refs[0]!,
          expectedOpeningCbor: openingCbor,
          now: () => h.emulator.now(),
        };
        await expect(
          submit({
            ...args,
            evidence: presence
              ? { kind: "absent_identity" }
              : { kind: "present_event" },
          }),
        ).rejects.toThrow("History facts changed");
        if (presence)
          await expect(
            submit({ ...args, expectedOpeningCbor: null }),
          ).rejects.toThrow("History facts changed");
        await expect(
          submit({
            ...args,
            contracts: {
              ...args.contracts,
              stateQueuePolicyId: "ab".repeat(28),
            },
          }),
        ).rejects.toThrow("state queue policy");
        await expect(
          submit({
            ...args,
            now: () => Number(headerEnd + SDK.MATURITY_DURATION_MS),
          }),
        ).rejects.toThrow("no usable validity window");
        if (mode === "external") {
          const lookup = h.lucid.utxosAt.bind(h.lucid);
          const unavailable = vi
            .spyOn(h.lucid, "utxosAt")
            .mockImplementation((address) =>
              address === deployment(h, kind).retentionAddress
                ? Promise.resolve([])
                : lookup(address),
            );
          try {
            await expect(submit(args)).rejects.toThrow(
              "retained event data is unavailable on L1",
            );
          } finally {
            unavailable.mockRestore();
          }
        }
        const result = await submit({
          ...args,
          preSubmitBoundary: ({ signed, txHash }) => {
            if (mode === "external") {
              expect(current.kind).toBe("Present");
              if (current.kind !== "Present" || !current.retainedDataUtxo)
                throw new Error("Missing external fixture");
              expect(
                workflowTransactionReferenceInputOutRefs(signed),
              ).toContain(
                `${current.retainedDataUtxo.txHash}#${current.retainedDataUtxo.outputIndex}`,
              );
            }
            const transactionCbor = signed.toCBOR();
            const measurement =
              measureCompleteSignedTransaction(transactionCbor);
            expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
            );
            expect(measurement.executionMemory).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
            );
            expect(measurement.executionSteps).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
            );
            records.push({
              label: `${kind}-production-capture-${mode}`,
              txHash,
              transactionCbor,
              measurement,
              fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
            });
          },
        });
        expect(result.historyOutRef).toBe(
          `${current.anchor.utxo.txHash}#${current.anchor.utxo.outputIndex}`,
        );
        expect(result.openingCbor).toBe(openingCbor);
        expect(result.historyOutRef).not.toBe(
          `${old.anchor.utxo.txHash}#${old.anchor.utxo.outputIndex}`,
        );
        const [txHash, index] = result.nextThreadOutRef.split("#");
        const thread = (
          await h.lucid.utxosByOutRef([
            { txHash: txHash!, outputIndex: Number(index) },
          ])
        )[0]!;
        await submitProductionClassification(h, p, thread, captured);
      });
    for (const presence of [false, true])
      it(`captures ${presence ? "Order facts" : "arbitrary-ID absence"} after targeted churn and continues without a live history pointer`, async () => {
        SDK.requireEventHistoryCaptureProtection(protectionDurationMs, timing);
        const h = await setupHistoryPair({
          blueprint,
          records,
          protectionDurationMs,
        });
        if (presence) await promoteHistoryPair(h);
        const nonce = h.eventNonces[familyIndex(kind)]!;
        const id = presence
          ? {
              transactionId: nonce.txHash,
              outputIndex: BigInt(nonce.outputIndex),
            }
          : { transactionId: "ea".repeat(32), outputIndex: 987_654n };
        // The header predates these events. Its last ten minutes remain; later
        // admission cannot manufacture an eligible event for this accused interval.
        const headerEnd =
          BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 700_000n;
        const p = await setupProof(h, kind, id, headerEnd);
        const mergeDeadline = headerEnd + SDK.MATURITY_DURATION_MS;
        let conflicts = 0;
        const successful = await SDK.captureEventHistoryWithRetry<{
          thread: UTxO;
          captured:
            | ReturnType<typeof SDK.captureEventHistoryWitness>
            | undefined;
          witness: SDK.EventHistoryWitness;
        }>({
          now: () => BigInt(h.emulator.now()),
          headerEnd,
          mergeDeadline,
          timing,
          maxAttempts: 4,
          fetch: () => fetchWitness(h, kind, id),
          submit: async (w, window, attempt) => {
            const built = await buildCapture(h, p, w, window);
            if (attempt <= 3) {
              // Deliberately violate the declared inclusion bound on attempts 2/3:
              // allow protection to expire, then submit the attack first.
              waitForMutation(h, w);
              const key = await nextChurnKey(w, id);
              await h.submit(
                `${kind}-targeted-pointer-churn-${attempt}`,
                await buildChurn(h, kind, w, key, [p.fee]),
              );
              expect(await h.lucid.utxosByOutRef([w.anchor.utxo])).toHaveLength(
                0,
              );
              const signed = await built.tx.sign.withWallet().complete();
              await expect(signed.submit()).rejects.toThrow(
                attempt === 1
                  ? /does not exist or was already spent/
                  : /Upper bound .* not in slot range/,
              );
              expect(await h.lucid.utxosByOutRef([p.fee])).toHaveLength(1);
              expect(await h.lucid.utxosByOutRef([p.thread])).toHaveLength(1);
              records.push({
                label: `${kind}-stale-capture-${attempt}`,
                witness: w.anchor.utxo,
                window,
                at: h.emulator.now(),
                transactionCbor: signed.toCBOR(),
              });
              conflicts++;
              return { kind: "ReferenceConflict" };
            }
            expect(window.protected).toBe(true);
            // A second immediate mutation cannot take the refreshed reference away.
            await expect(
              buildChurn(h, kind, w, await nextChurnKey(w, id), [p.fee]),
            ).rejects.toThrow();
            const hash = await h.submit(
              `${kind}-capture-protected-${presence ? "present" : "absent"}`,
              built.tx,
            );
            const thread = (
              await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
            )[0]!;
            expect(thread.datum).toBe(built.datum);
            return {
              kind: "Captured",
              value: { thread, captured: built.captured, witness: w },
            };
          },
        });
        expect(conflicts).toBe(3);
        // Churn after capture is no longer a dependency for content classification.
        waitForMutation(h, successful.witness);
        await h.submit(
          `${kind}-churn-after-capture`,
          await buildChurn(
            h,
            kind,
            successful.witness,
            await nextChurnKey(successful.witness, id),
            [],
          ),
        );
        expect(
          await h.lucid.utxosByOutRef([successful.witness.anchor.utxo]),
        ).toHaveLength(0);
        if (presence)
          await expect(
            continueCaptured(
              h,
              p,
              successful.thread,
              successful.captured,
              true,
            ),
          ).rejects.toThrow();
        const classified = await submitProductionClassification(
          h,
          p,
          successful.thread,
          successful.captured,
        );
        expect(classified.awaitedConfirmation).toBe(true);
        expect(BigInt(h.emulator.now())).toBeLessThan(mergeDeadline);
      });
  });

for (const kind of ["Deposit", "Withdrawal"] as const) {
  it(`${kind}: an honest eligible event remains unchallengeable after pointer mutation`, async () => {
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
    expect(w.kind).toBe("Present");
    if (w.kind !== "Present") throw new Error("Missing admitted Order");
    const facts = SDK.captureEventHistoryWitness(
      w,
      deployment(h, kind).policyId,
      kind,
    );
    const committedHash =
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
      facts.commitment.inclusion_time,
      committedHash,
    );
    // The accused header ends at the event's inclusion time, an event wait
    // after admission; capture may start only once that interval has closed.
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
    const hash = await h.submit(`${kind}-capture-honest-eligible`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    waitForMutation(h, w);
    await h.submit(
      `${kind}-honest-pointer-continuation`,
      await buildChurn(h, kind, w, await nextChurnKey(w, id), []),
    );
    await expect(
      continueCaptured(h, p, thread, built.captured),
    ).rejects.toThrow();
    await expect(
      submitProductionClassification(h, p, thread, built.captured),
    ).rejects.toThrow(/matches/);
    expect(await h.lucid.utxosByOutRef([thread])).toHaveLength(1);
    const refreshed = await fetchWitness(h, kind, id);
    if (refreshed.kind !== "Present")
      throw new Error("Pointer continuation lost the Order");
    expect(
      SDK.captureEventHistoryWitness(
        refreshed,
        deployment(h, kind).policyId,
        kind,
      ),
    ).toEqual(facts);
  });

  it(`${kind}: capture and classification both refuse merge-deadline overlap`, async () => {
    const h = await setupHistoryPair({
      blueprint,
      records,
      protectionDurationMs,
    });
    const id = { transactionId: "fe".repeat(32), outputIndex: 123n };
    const headerEnd =
      BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 250_000n;
    const p = await setupProof(h, kind, id, headerEnd);
    const mergeDeadline = headerEnd + SDK.MATURITY_DURATION_MS;
    const w = await fetchWitness(h, kind, id);
    await expect(
      buildCapture(h, p, w, {
        validFrom: BigInt(h.emulator.now()) - 60_000n,
        validTo: mergeDeadline + 1_000n,
        protected: false,
      }),
    ).rejects.toThrow();
    const window = SDK.eventHistoryCaptureWindow({
      now: BigInt(h.emulator.now()),
      headerEnd,
      mergeDeadline,
      protectedUntil: w.anchor.node.protected_until,
      timing,
    });
    const built = await buildCapture(h, p, w, window);
    const hash = await h.submit(`${kind}-capture-before-deadline`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    h.emulator.awaitSlot(
      Number((mergeDeadline - BigInt(h.emulator.now()) + 999n) / 1000n),
    );
    await expect(continueCaptured(h, p, thread, undefined)).rejects.toThrow();
    await expect(
      submitProductionClassification(h, p, thread, undefined),
    ).rejects.toThrow(/window/);
    expect(await h.lucid.utxosByOutRef([thread])).toHaveLength(1);
  });
}
