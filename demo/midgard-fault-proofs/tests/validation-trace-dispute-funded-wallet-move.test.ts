/** The validation-trace dispute's wallet-paid thread moves under the
 * watcher's prover funding, its terms following the emulator's live fees.
 * A defender opens a thread on our case and reveals, the watcher reveals
 * from a short wallet, its thread paying the fee, and the defender goes
 * silent. Fees then rise until the timeout and the thread's close are priced
 * over the per-move cap, so our plain-Ada wallet pays each of them whole. A
 * wallet holding less than the timeout needs holds it under the operator's
 * alarm, naming the case, the thread and the deadline, before any byte
 * reaches the ledger, releasing what it reserved, and keeps retrying at each
 * observation without wedging. Topped up to less than the wallet-move
 * reserve but enough for the move, it lands the timeout with no alarm, its
 * successor keeping all of the thread's lovelace; refilled to that reserve,
 * it lands the thread's close, which pays the thread's rest less the receipt
 * to our funding key, and the case closes Convicted.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/removal.js";
import { VALIDATION_TRACE_THREAD_AWAITING } from "../src/validation-trace-dispute/threads.js";
import { validationTraceCaseExpiryMs } from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import { watchLedger } from "./support/own-outputs-installed.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  confirmedMoves,
  type FundedMoves,
  liveFundedThread,
  moveIntents,
  openDefendedFundedCase,
  passDefenderDeadline,
  recordedOutput,
} from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  expectWithinWalletMoveShape,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  reportFit,
  revealAsDefender,
} from "./support/validation-trace-dispute-scripted-defender.js";

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

const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);
/** Less than the timeout's fee alone at the raised fees. */
const SHORT = 1n * ADA;

let journey: FundedMoves;
let restoreFees: () => void;
/** The silent defender's thread, before our timeout. */
let silent: UTxO;

/** The `kind` moves that landed. */
const hashesOf = (kind: string) =>
  journey.moves
    .filter(({ authority }) => authority.kind === kind)
    .map(({ signed }) => signed.toHash())
    .filter((hash) => journey.scene.l1.signedCbors.has(hash));

/** Each input of `txHash`'s signed bytes, as out-refs. */
const inputsOf = (txHash: string) => {
  const move = journey.moves.find(({ signed }) => signed.toHash() === txHash);
  const inputs = move!.signed.toTransaction().body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

describe("validation-trace dispute wallet-paid thread moves under the prover funding permit", () => {
  beforeAll(async () => {
    journey = await openDefendedFundedCase({ liveParameters: true });
    const { scene, defender, trace } = journey;
    await revealAsDefender(
      scene,
      defender,
      await liveFundedThread(journey),
      trace,
    );
  }, TIMEOUT);

  afterAll(async () => {
    restoreFees?.();
    await journey?.dispute.close();
  });

  it(
    "lands a reveal its thread pays from a wallet too short to pay any move",
    async () => {
      const { scene, dispute, capture } = journey;
      const thread = await liveFundedThread(journey);
      await drainProver(scene.h as never, SHORT);
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_THREAD_AWAITING,
        { wrap: capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const [reveal, ...again] = hashesOf("reveal");
      expect(reveal).toBeDefined();
      expect(again).toStrictEqual([]);
      const fee = await landedMoveFee(scene, reveal!);
      expect(fee.paid).toBeLessThanOrEqual(
        VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
      );
      // The thread pays the fee: it is the move's only spent input.
      expect(inputsOf(reveal!)).toStrictEqual([outRefOf(thread)]);
      silent = await liveFundedThread(journey);
      await passDefenderDeadline(journey);
    },
    TIMEOUT,
  );

  it(
    "holds a timeout priced over the cap under the operator's alarm, naming the case, the thread and the deadline, when the wallet holds less than it needs, before any byte reaches the ledger, and keeps retrying without wedging",
    async () => {
      const { scene, dispute, capture, caseOrigin } = journey;
      // Fees rise until the timeout's and the close's minimums pass the
      // per-move cap.
      const { emulator } = scene.h;
      const parameters = emulator.protocolParameters;
      emulator.protocolParameters = { ...parameters, minFeeA: 1_500 };
      restoreFees = () => {
        emulator.protocolParameters = parameters;
      };
      const caseDatum = Data.from(caseOrigin.datum!, SDK.Case);
      const caseUnit = Object.keys(caseOrigin.assets).find((unit) =>
        unit.startsWith(scene.chain.case.policyId),
      )!;
      // Our wallet still holds the short balance the reveal left it.
      const watch = watchLedger(scene.h.emulator);
      try {
        for (let pause = 0; pause < 2; pause += 1) {
          const { result, history } = await turnUntil(
            dispute,
            VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
            { wrap: capture },
          );
          expect(result.kind, history.join("\n")).toBe("pending");
          if (result.kind !== "pending") return;
          expect(result.reason).toContain(
            "cannot fund validation-trace-timeout:",
          );
          expect(result.reason).toContain(
            `on thread ${outRefOf(silent)} of case ${caseUnit},`,
          );
          // A timeout is not on our clock: it must land before the case
          // expires.
          expect(result.reason).toContain(
            `before its deadline ${validationTraceCaseExpiryMs(caseDatum).toString()};`,
          );
          // An alarm the next observation retries, not a stall.
          expect(validationTraceRunState(result)).toMatchObject({
            kind: "pending",
            resumeOnObservation: true,
            namedState: {
              name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
              alarm: true,
            },
          });
          // The refused attempt released every input it held.
          for (const reservation of await dispute.readReservations())
            expect(reservation).toMatchObject({ activeInputs: [] });
        }
        expect(watch.ours()).toHaveLength(0);
      } finally {
        watch.restore();
      }
      expect(hashesOf("timeout")).toStrictEqual([]);
      expect(outRefOf(await liveFundedThread(journey))).toBe(outRefOf(silent));
    },
    TIMEOUT,
  );

  it(
    "lands the timeout, priced over the cap, once the wallet is topped up to less than the wallet-move reserve but enough for the move, with no alarm, our wallet paying its whole fee and its successor keeping all of the thread's lovelace",
    async () => {
      const { scene, dispute, capture } = journey;
      const reserve = BigInt(await dispute.walletMoveReserve());
      // Enough for the builder to select the move's coins; less than the
      // reserve.
      const held = reserve / 2n;
      await topUpProver(scene.h as never, 100n * ADA);
      await drainProver(scene.h as never, held);
      const reasons: string[] = [];
      for (let turns = 0; turns < 6; turns += 1) {
        if (hashesOf("timeout").length > 0) break;
        const result = await dispute.turn(capture);
        if ("reason" in result) reasons.push(result.reason);
      }
      for (const reason of reasons)
        expect(reason).not.toContain(VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED);
      const [timeout, ...again] = hashesOf("timeout");
      expect(timeout, reasons.join("\n")).toBeDefined();
      expect(again).toStrictEqual([]);
      const fee = await landedMoveFee(scene, timeout!);
      expect(fee.paid).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
      expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
      expect(fee.paid).toBeGreaterThan(SHORT);
      expect(fee.paid).toBeLessThanOrEqual(held);
      expect(recordedOutput(scene, `${timeout!}#0`).assets.lovelace).toBe(
        silent.assets.lovelace,
      );
      // The thread and our wallet's coins pay it; nothing else is spent.
      expect(inputsOf(timeout!)).toContain(outRefOf(silent));
      expect(inputsOf(timeout!).length).toBeGreaterThan(1);
    },
    TIMEOUT,
  );

  it(
    "lands the thread's close priced over the cap from a wallet refilled to the reserve, paying the thread's rest less the receipt to our funding key, then closes the case Convicted",
    async () => {
      const { scene, dispute, capture } = journey;
      await topUpProver(scene.h as never, 100n * ADA);
      await drainProver(scene.h as never, await dispute.walletMoveReserve());
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
        { wrap: capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const [timeout] = hashesOf("timeout");
      const [close, ...again] = hashesOf("close_thread");
      expect(again).toStrictEqual([]);
      expect(
        confirmedMoves(result)
          .map(({ stage }) => stage)
          .slice(-3),
      ).toStrictEqual(["timeout", "close_thread", "close"]);
      expect(
        moveIntents(result, "close-thread").map(({ attempt, txHash }) => [
          attempt,
          txHash,
        ]),
      ).toStrictEqual([[1, close]]);
      const fee = await landedMoveFee(scene, close!);
      expect(fee.paid).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
      expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
      expect(inputsOf(close!)).toContain(`${timeout!}#0`);
      const caseOutput = recordedOutput(scene, `${close!}#0`);
      const caseDatum = Data.from(caseOutput.datum!, SDK.Case);
      expect(caseDatum.open_threads).toBe(0n);
      const funding = credentialToAddress(network, {
        type: "Key",
        hash: caseDatum.bounty_claimant,
      });
      const outputs = journey.moves
        .find(({ signed }) => signed.toHash() === close)!
        .signed.toTransaction()
        .body()
        .outputs();
      const leftover = Array.from({ length: outputs.len() }, (_, index) =>
        recordedOutput(scene, `${close!}#${index.toString()}`),
      ).find(({ address, datum }) => address === funding && datum != null);
      // Our wallet paid the fee: the thread's rest is all of it but the
      // receipt.
      expect(leftover?.assets).toStrictEqual({
        lovelace: (silent.assets.lovelace ?? 0n) - RECEIPT_LOVELACE,
      });
      // Each wallet-paid move is within the shape the reserve is priced at.
      await expectWithinWalletMoveShape(scene, [timeout!, close!]);
      reportFit(
        "funded wallet-paid timeout",
        journey.moves
          .filter(({ authority }) => authority.kind === "timeout")
          .map(({ signed }) => signed.toCBOR()),
      );
      reportFit(
        "funded wallet-paid close_thread",
        journey.moves
          .filter(({ authority }) => authority.kind === "close_thread")
          .map(({ signed }) => signed.toCBOR()),
      );
    },
    TIMEOUT,
  );
});
