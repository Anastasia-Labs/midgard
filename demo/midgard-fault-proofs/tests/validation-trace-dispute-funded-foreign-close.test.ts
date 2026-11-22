/** The close of our case after another party verified it and nobody
 * defended it, under the prover funding permit, on the emulator. Our funded
 * watcher opens our claim and signs our VerifySource, and its process stops
 * before broadcasting it; another party runs VerifySource over our pending
 * claim first, and nobody defends. Until the defense deadline has passed
 * the next process holds the case open, and a close valid from the last
 * slot not past the deadline is refused by the case validator. Once it has
 * passed, our runner closes the case Convicted, spending the case and bond
 * that other party's VerifySource created, read from the case token's L1
 * history: the record names us and the bond and the case's lovelace come
 * back to us.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk/da";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../src/validation-trace-dispute/adapter.js";
import { validationTraceCaseOrigin } from "../src/validation-trace-dispute/case-origin.js";
import {
  buildValidationTraceClose,
  validationTraceFraudProofUnit,
} from "../src/validation-trace-dispute/close.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { network } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  outRefOf,
  spentBy,
} from "./support/validation-trace-dispute-foreign-verify.js";
import {
  type ForeignClose,
  openForeignVerifiedCase,
} from "./support/validation-trace-dispute-funded-close.js";
import { confirmedMoves } from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import { BOND_LOVELACE } from "./support/validation-trace-dispute-open-scene.js";

let journey: ForeignClose;

const liveAt = async (outRef: string): Promise<UTxO | undefined> =>
  (
    await journey.scene.h.emulator.getUtxosByOutRef([parseOutRefLabel(outRef)])
  )[0];

const deadlineOf = (caseOutput: UTxO) =>
  validationTraceDefenseDeadlineMs(Data.from(caseOutput.datum!, SDK.Case));

describe("validation-trace dispute close of our case after another party verified it", () => {
  beforeAll(async () => {
    journey = await openForeignVerifiedCase();
  }, TIMEOUT);

  afterAll(() => {
    journey?.recorder.restore();
  });

  it(
    "holds the case open until its defense deadline has passed, and a close valid from the last slot not past it is refused at case_can_close",
    async () => {
      const { scene, dispute, verified, artifact, closes, capture } = journey;
      const { h } = scene;
      const lucid = h.proverLucid;
      const deadline = deadlineOf(verified.caseOutput);
      // The last slot whose time is not past the deadline.
      h.emulator.awaitSlot(
        lucid.unixTimeToSlot(deadline) - lucid.currentSlot(),
      );
      const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
      expect(validFrom).toBeLessThanOrEqual(deadline);
      expect(deadline - validFrom).toBeLessThan(1_000);
      const { tx } = await buildValidationTraceClose({
        lucid,
        signer: h.proverSigner,
        deployment: dispute.deployment,
        artifact,
        caseOutRef: outRefOf(verified.caseOutput),
        bondOutRef: outRefOf(verified.bond),
        validFrom,
      });
      await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }), {
        refusedBy: "fraud_proofs/validation_trace/case_v1",
        check: /case_can_close/u,
      });
      const held = await dispute.turn(capture);
      expect(held).toMatchObject({ kind: "pending" });
      expect("reason" in held && held.reason).toMatch(
        new RegExp(`^${VALIDATION_TRACE_CASE_OPEN}`, "u"),
      );
      expect(closes).toHaveLength(0);
      expect(await liveAt(outRefOf(verified.caseOutput))).toBeDefined();
    },
    TIMEOUT,
  );

  it(
    "closes our case Convicted once its defense deadline has passed, spending the case and bond the other party's VerifySource created: the record names us, and the bond and the case's lovelace come back to us",
    async () => {
      const { scene, dispute, verified, removal, artifact, closes } = journey;
      const { h } = scene;
      const lucid = h.proverLucid;
      const caseOutRef = outRefOf(verified.caseOutput);
      const bondOutRef = outRefOf(verified.bond);
      const ahead =
        lucid.unixTimeToSlot(deadlineOf(verified.caseOutput)) -
        lucid.currentSlot() +
        1;
      if (ahead > 0) h.emulator.awaitSlot(ahead);
      // The L1 view stops here, before our record.
      removal.holdView();
      await removal
        .l1For(scene.failing.evidence.headerHash)
        .observe({ headerHash: scene.failing.evidence.headerHash });
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
        { limit: 12, wrap: journey.capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const stages = await confirmedMoves(dispute);
      expect(stages.map(({ stage }) => stage)).toStrictEqual([
        "init",
        "open",
        "close",
      ]);
      expect(closes).toHaveLength(1);
      const [close] = closes;
      expect(close!.action.input.consumes).toBe(caseOutRef);
      expect(spentBy(close!.signed.toTransaction().to_cbor_hex())).toEqual(
        expect.arrayContaining([caseOutRef, bondOutRef]),
      );
      expect(await liveAt(caseOutRef)).toBeUndefined();
      expect(await liveAt(bondOutRef)).toBeUndefined();

      const proofUnit = validationTraceFraudProofUnit(
        dispute.deployment,
        artifact.headerHash,
      );
      const record = (await liveAt(stages[2]!.outputs[0]!))!;
      expect(record.address).toBe(h.contracts.fraudProof.spendingScriptAddress);
      expect(record.assets).toEqual({ lovelace: 2_000_000n, [proofUnit]: 1n });
      expect(
        Data.from(record.datum!, SDKFraudProof.FraudProofTokenDatum)
          .fraud_prover,
      ).toBe(h.proverSigner.paymentKeyHash);
      const ours = credentialToAddress(network, {
        type: "Key",
        hash: h.proverSigner.paymentKeyHash,
      });
      const closeHash = close!.signed.toHash();
      const [refund, remainder] = await h.emulator.getUtxosByOutRef([
        { txHash: closeHash, outputIndex: 0 },
        { txHash: closeHash, outputIndex: 1 },
      ]);
      expect(refund!.address).toBe(ours);
      expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
      expect(remainder!.address).toBe(ours);
    },
    TIMEOUT,
  );

  it(
    "reads our case's case and bond from the other party's VerifySource in the case token's L1 history, and refuses a history holding no VerifySource",
    async () => {
      const { scene, dispute, verified, artifact } = journey;
      const caseUnit = Object.keys(verified.caseOutput.assets).find((unit) =>
        unit.startsWith(scene.chain.case.policyId),
      )!;
      const transactions = await dispute.caseHistory.transactions({
        headerHash: artifact.headerHash,
        caseUnit,
      });
      expect(
        validationTraceCaseOrigin({ transactions, caseUnit }),
      ).toStrictEqual({
        caseOutRef: outRefOf(verified.caseOutput),
        bondOutRef: outRefOf(verified.bond),
      });
      const verifySourceHash = verified.caseOutput.txHash;
      expect(() =>
        validationTraceCaseOrigin({
          transactions: transactions.filter(
            ({ txHash }) => txHash !== verifySourceHash,
          ),
          caseUnit,
        }),
      ).toThrow(
        new RegExp(
          `^validation-trace case ${caseUnit} has no single VerifySource in its L1 history$`,
          "u",
        ),
      );
    },
    TIMEOUT,
  );
});
