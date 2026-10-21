/** Our case closed by another party, under the prover funding permit, on
 * the emulator. Our funded watcher opens our claim and its process stops
 * before broadcasting our VerifySource; another party verifies our pending
 * claim first, nobody defends, and once the defense deadline has passed that
 * party closes the case before our runner does. The record names us, the
 * claim's prover, so our runner takes the convicted block from that record
 * without building a close of its own.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/adapter.js";
import {
  buildValidationTraceClose,
  validationTraceFraudProofUnit,
} from "../src/validation-trace-dispute/close.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import {
  anotherParty,
  outRefOf,
} from "./support/validation-trace-dispute-foreign-verify.js";
import {
  type ForeignClose,
  openForeignVerifiedCase,
} from "./support/validation-trace-dispute-funded-close.js";
import { confirmedMoves } from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let journey: ForeignClose;

describe("validation-trace dispute case of ours closed by another party", () => {
  beforeAll(async () => {
    journey = await openForeignVerifiedCase();
  }, TIMEOUT);

  afterAll(async () => {
    await journey?.dispute.close();
    journey?.recorder.restore();
  });

  it(
    "takes the convicted block from the record another party's close left, naming us, without a close of its own",
    async () => {
      const { scene, dispute, verified, removal, artifact, closes, capture } =
        journey;
      const { h } = scene;
      const prover = h.proverLucid;
      const deadline = validationTraceDefenseDeadlineMs(
        Data.from(verified.caseOutput.datum!, SDK.Case),
      );
      h.emulator.awaitSlot(
        prover.unixTimeToSlot(deadline) - prover.currentSlot() + 1,
      );
      // The L1 view stops here, before the record.
      removal.holdView();
      await removal
        .l1For(scene.failing.evidence.headerHash)
        .observe({ headerHash: scene.failing.evidence.headerHash });

      const lucid = await anotherParty(scene);
      const { tx } = await buildValidationTraceClose({
        lucid,
        // The other party's wallet is already selected on its Lucid.
        signer: { selectWallet: () => undefined } as never,
        deployment: dispute.deployment,
        artifact,
        caseOutRef: outRefOf(verified.caseOutput),
        bondOutRef: outRefOf(verified.bond),
        validFrom: lucid.slotToUnixTime(lucid.currentSlot()),
      });
      const closeHash = await (
        await (await tx.complete({ localUPLCEval: true })).sign
          .withWallet()
          .complete()
      ).submit();
      await lucid.awaitTx(closeHash);
      const proofUnit = validationTraceFraudProofUnit(
        dispute.deployment,
        artifact.headerHash,
      );
      const [record] = await prover.utxosAtWithUnit(
        h.contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      );
      expect(
        Data.from(record!.datum!, SDK.FraudProofTokenDatum).fraud_prover,
      ).toBe(h.proverSigner.paymentKeyHash);

      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
        { limit: 12, wrap: capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      expect(confirmedMoves(result).map(({ stage }) => stage)).toStrictEqual([
        "init",
        "open",
      ]);
      expect(closes).toHaveLength(0);
    },
    TIMEOUT,
  );
});
