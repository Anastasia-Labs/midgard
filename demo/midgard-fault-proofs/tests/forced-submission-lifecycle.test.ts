import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { runInvalidSignatureWrongfulRejectionScenario } from "./support/invalid-signature-wrongful-emulator.js";
import { makeMinFeeWrongfulRejectionScenario } from "./support/min-fee-wrongful-emulator.js";
import {
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildHonestAcceptedValidationDisputeFixture,
  buildNonEmptyClaimedLedgerDeltaRoot,
  expectOnchainRefusal,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// The registered direct fee proof adjudicates the typed fee predicate. The
// interactive route independently adjudicates actual execution. Both carry
// the same immutable forced source format and real terminal correction.
describe("single-verdict forced submission correction and refusal", () => {
  it("refuses a substituted signature coordinate with the same rejection tag, then corrects the original exact claim", async () => {
    await runInvalidSignatureWrongfulRejectionScenario({
      decoyWitnessCount: 0,
      accused: "honest",
      rejectedIndex: null,
      deepMembership: true,
    });
  }, 300_000);

  it("corrects a wrongful fee rejection without rewriting the submitted bytes", async () => {
    const s = await makeMinFeeWrongfulRejectionScenario(1_000n);
    const bytes = encodeMidgardForcedTxCanonical(s.tx);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    await s.finish(bound.nextThreadOutRef);
    const removal = await s.remove();
    expect(removal.length).toBeGreaterThan(0);
    expect(encodeMidgardForcedTxCanonical(s.tx)).toEqual(bytes);
    expect(s.forcedSource.membership.value.submitted_source).toEqual(
      s.leaf.submitted_source,
    );
  }, 180_000);

  it("refuses a false challenge to an honestly rejected below-minimum fee", async () => {
    const s = await makeMinFeeWrongfulRejectionScenario(1_001n);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    await expectOnchainRefusal(() => s.finish(bound.nextThreadOutRef, true));
    await s.cancel(bound.nextThreadOutRef, 1);
    expect(
      await s.h.proverLucid.utxosAtWithUnit(
        s.h.contracts.stateQueue.spendingScriptAddress,
        s.seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(1);
  }, 180_000);

  it.each(["header", "order", "compact", "lengths", "context"] as const)(
    "refuses %s substitution at the authenticated forced-source door",
    async (kind) => {
      const s = await makeMinFeeWrongfulRejectionScenario(1_000n);
      const initial = await s.init();
      const source = structuredClone(s.forcedSource);
      const state = structuredClone(s.state);
      if (kind === "header") source.header.blockSlot += 1n;
      if (kind === "order") source.membership.key.outputIndex += 1n;
      if (kind === "compact")
        source.membership.value.submitted_source.compact_cbor =
          source.membership.value.submitted_source.compact_cbor.slice(0, -2) +
          "ff";
      if (kind === "lengths")
        source.membership.value.submitted_source.field_preimage_lengths_cbor =
          "89010101010101010102";
      if (kind === "context") state.min_fee_a += 1n;
      await expectOnchainRefusal(() =>
        s.bind(initial.nextThreadOutRef, { forcedSource: source, state }),
      );
      await s.cancel(initial.nextThreadOutRef, 0);
    },
    180_000,
  );

  it("corrects wrongful acceptance while retaining the malicious nonempty claimed delta", async () => {
    const delta = await buildNonEmptyClaimedLedgerDeltaRoot();
    const result = await runForcedValidationDisputeScenario((input) =>
      buildAcceptedClaimOverRejectingTransactionFixture({
        ...input,
        claimedLedgerDeltaRoot: delta,
      }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
    const terminal = result.fixture.challengerTrace.states.at(-1)!;
    expect(terminal.verdict).toBe("rejected");
    expect(terminal.ledgerDeltaRoot).toEqual(delta);
  }, 300_000);

  it("refuses a false rejection challenge against honest accepted execution", async () => {
    await expect(
      runForcedValidationDisputeScenario(
        buildHonestAcceptedValidationDisputeFixture,
      ),
    ).rejects.toThrow(
      /emulator lifecycle stage (prepare-selected|semantic-resolution) failed/u,
    );
  }, 300_000);

  it("refuses a rejection proof that rewrites the immutable claimed delta", async () => {
    const delta = await buildNonEmptyClaimedLedgerDeltaRoot();
    await expect(
      runForcedValidationDisputeScenario(
        (input) =>
          buildAcceptedClaimOverRejectingTransactionFixture({
            ...input,
            claimedLedgerDeltaRoot: delta,
            clearChallengerTerminalDelta: true,
          }),
        { stopAfter: "semantic-resolution" },
      ),
    ).rejects.toThrow(/emulator lifecycle stage prepare-selected failed/u);
  }, 300_000);
});
