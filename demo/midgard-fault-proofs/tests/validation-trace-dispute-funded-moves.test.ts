/** The validation-trace dispute's thread moves under the watcher's prover
 * funding, on the emulator. A defender opens a thread on our case with a
 * scripted trace and reveals. Our reveal lands under the funding permit,
 * and when the defender answers it before the watcher looks again, the
 * watcher still confirms that reveal from its own signed bytes and makes
 * its next one. The defender then goes silent: once its clock ends the
 * watcher times the thread out and closes it into our case, minting the
 * defender's lost-defense receipt and paying the rest of the thread to our
 * funding key, then closes the case Convicted; the defender, the convicted
 * block's operator, is not slashed by that receipt before the case closes.
 * A move whose fee fits under
 * the per-move cap is paid from the thread it spends; the timeout, priced
 * over the cap, is paid entirely from our plain-Ada wallet through the
 * funding permit, and its successor keeps all of the thread's lovelace. The
 * wallet otherwise lends plain-Ada collateral only.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/removal.js";
import { VALIDATION_TRACE_THREAD_AWAITING } from "../src/validation-trace-dispute/threads.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  confirmedMoves,
  type FundedMoves,
  liveFundedThread,
  moveIntents,
  openDefendedFundedCase,
  passDefenderDeadline,
  recordedOutput,
  walletOutRefs,
} from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  expectRealMoveFee,
  type LandedMoveFee,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  isTurn,
  liveThread,
  reportFit,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

let journey: FundedMoves;
/** The prover wallet's outputs before the watcher's first move. */
let wallet: Map<string, UTxO>;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const hashesOf = (kind: string) =>
  journey.moves
    .filter(({ authority }) => authority.kind === kind)
    .map(({ signed }) => signed.toHash());

/** A move under the cap spends no wallet output from before the first
 * move, one over it spends only such outputs holding plain Ada beside its
 * thread, and each move's collateral is one of them holding plain Ada. */
const expectWalletOnlyLendsCollateral = () => {
  for (const { signed } of journey.moves) {
    const body = signed.toTransaction().body();
    const walletPaid = body.fee() > VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE;
    const inputs = body.inputs();
    let spent = 0;
    for (let index = 0; index < inputs.len(); index += 1) {
      const input = inputs.get(index);
      const held = wallet.get(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
      if (held === undefined) continue;
      spent += 1;
      expect(walletPaid).toBe(true);
      expect(Object.keys(held.assets)).toStrictEqual(["lovelace"]);
    }
    expect(spent > 0).toBe(walletPaid);
    const collateral = body.collateral_inputs();
    expect(collateral?.len()).toBe(1);
    const input = collateral!.get(0);
    const lent = wallet.get(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
    expect(Object.keys(lent?.assets ?? {})).toStrictEqual(["lovelace"]);
  }
};

describe("validation-trace dispute thread moves under the prover funding permit", () => {
  beforeAll(async () => {
    journey = await openDefendedFundedCase();
    wallet = await walletOutRefs(journey);
  }, TIMEOUT);

  it(
    "reveals our midpoint, and confirms it from its own signed bytes after the defender answers it before the watcher looks again",
    async () => {
      const { scene, dispute, defender, trace, capture } = journey;
      await revealAsDefender(
        scene,
        defender,
        await liveFundedThread(journey),
        trace,
      );
      // Turns until our reveal has landed and put the thread on the
      // defender's turn.
      const history: string[] = [];
      for (let turns = 0; turns < 12; turns += 1) {
        const result = await dispute.turn(capture);
        history.push(
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        );
        if (
          isTurn(threadOf(await liveFundedThread(journey)), "AwaitingOperator")
        )
          break;
      }
      const [first] = hashesOf("reveal");
      expect(first, history.join("\n")).toBeDefined();
      expect((await liveFundedThread(journey)).txHash).toBe(first);
      // The defender answers before the watcher sees its reveal land.
      await revealAsDefender(
        scene,
        defender,
        await liveFundedThread(journey),
        trace,
      );
      const { result, history: later } = await turnUntil(
        dispute,
        VALIDATION_TRACE_THREAD_AWAITING,
        { wrap: capture },
      );
      expect(result.kind, [...history, ...later].join("\n")).toBe("pending");
      const reveals = hashesOf("reveal");
      expect(reveals).toHaveLength(2);
      expect(
        confirmedMoves(result).filter(({ stage }) => stage === "reveal"),
      ).toStrictEqual(
        reveals.map((hash) => ({ stage: "reveal", outputs: [`${hash}#0`] })),
      );
      // One signed attempt per reveal, nothing sent again.
      expect(
        moveIntents(result, "reveal").map(({ attempt, txHash }) => [
          attempt,
          txHash,
        ]),
      ).toStrictEqual(reveals.map((hash) => [1, hash]));
      expect(dispute.rebroadcasts).toStrictEqual([]);
      const live = await liveFundedThread(journey);
      expect(live.txHash).toBe(reveals[1]);
      expect(isTurn(threadOf(live), "AwaitingOperator")).toBe(true);
      // Each reveal paid its real fee, below the cap.
      for (const hash of reveals)
        expectRealMoveFee(await landedMoveFee(scene, hash));
      expectWalletOnlyLendsCollateral();
    },
    TIMEOUT,
  );

  it(
    "times out the silent defender with our wallet paying the timeout priced over the cap, and closes its thread into our case, minting its lost-defense receipt and paying the rest of the thread to our funding key, then closes the case Convicted with no slash by that receipt before it",
    async () => {
      const { scene, dispute, capture, unit, defender } = journey;
      const silent = await liveFundedThread(journey);
      await passDefenderDeadline(journey);
      // Fees rise until the timeout's minimum passes the per-move cap, so
      // our wallet pays it; they fall again before the thread's close.
      const parameters = scene.h.emulator.protocolParameters;
      scene.h.emulator.protocolParameters = { ...parameters, minFeeA: 1_500 };
      let timeoutFee: LandedMoveFee;
      try {
        for (let turns = 0; turns < 4; turns += 1) {
          if (hashesOf("timeout").length > 0) break;
          await dispute.turn(capture);
        }
        expect(hashesOf("timeout")).toHaveLength(1);
        timeoutFee = await landedMoveFee(scene, hashesOf("timeout")[0]!);
      } finally {
        scene.h.emulator.protocolParameters = parameters;
      }
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
        { wrap: capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const [timeout] = hashesOf("timeout");
      const [close] = hashesOf("close_thread");
      // With its only thread closed and its defense deadline passed, the
      // case closes Convicted; no slash by the thread's receipt precedes it.
      expect(
        confirmedMoves(result)
          .map(({ stage }) => stage)
          .slice(-3),
      ).toStrictEqual(["timeout", "close_thread", "close"]);
      expect(
        confirmedMoves(result).filter(
          ({ stage }) => stage === "timeout" || stage === "close_thread",
        ),
      ).toStrictEqual([
        { stage: "timeout", outputs: [`${timeout!}#0`] },
        { stage: "close_thread", outputs: [`${close!}#0`, `${close!}#1`] },
      ]);
      expect(
        [
          ...moveIntents(result, "timeout"),
          ...moveIntents(result, "close-thread"),
        ].map(({ attempt, txHash }) => [attempt, txHash]),
      ).toStrictEqual([
        [1, timeout],
        [1, close],
      ]);
      const { chain } = scene;
      expect(await liveThread(scene, unit)).toBeUndefined();
      const closeOutputs = Array.from(
        {
          length: journey.moves
            .find(({ authority }) => authority.kind === "close_thread")!
            .signed.toTransaction()
            .body()
            .outputs()
            .len(),
        },
        (_, index) => recordedOutput(scene, `${close!}#${index.toString()}`),
      );
      const [caseOutput] = closeOutputs;
      const caseDatum = Data.from(caseOutput!.datum!, SDK.Case);
      expect(caseOutput!.address).toBe(chain.case.spendingScriptAddress);
      expect(caseDatum.open_threads).toBe(0n);
      expect(caseDatum.bounty_claimant).toBe(
        scene.h.proverSigner.paymentKeyHash,
      );
      const name = unit.slice(56);
      // The receipt the close minted, which the slash has since burned.
      const receipt = recordedOutput(scene, `${close!}#1`);
      expect(receipt.address).toBe(chain.award.spendingScriptAddress);
      expect(receipt.assets).toStrictEqual({
        lovelace: RECEIPT_LOVELACE,
        [chain.award.policyId + name]: 1n,
      });
      expect(Data.from(receipt.datum!, SDK.LostDefenseReceipt)).toStrictEqual({
        case_id: threadOf(silent).case_id,
        defender_vkh: defender.vkey,
        outcome: "ChallengerWins",
        bounty_claimant: caseDatum.bounty_claimant,
      });
      // The timeout paid at least its ledger minimum, over the cap, and its
      // successor kept all of the thread's lovelace; the close paid its real
      // fee from the thread, below the cap.
      expect(timeoutFee.paid).toBeGreaterThanOrEqual(timeoutFee.ledger);
      expect(timeoutFee.paid).toBeGreaterThan(
        VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
      );
      expect(recordedOutput(scene, `${timeout!}#0`).assets.lovelace).toBe(
        silent.assets.lovelace,
      );
      const closeFee = await landedMoveFee(scene, close!);
      expectRealMoveFee(closeFee);
      const leftover = closeOutputs.find(
        ({ address }) =>
          address ===
          credentialToAddress(network, {
            type: "Key",
            hash: caseDatum.bounty_claimant,
          }),
      );
      expect(leftover?.assets).toStrictEqual({
        lovelace:
          (silent.assets.lovelace ?? 0n) - closeFee.paid - RECEIPT_LOVELACE,
      });
      expect(leftover!.datum).toBe(Data.to(new Constr(0, [timeout!, 0n])));
      expect(
        journey.moves.map(({ authority }) => authority.kind),
      ).toStrictEqual(["reveal", "reveal", "timeout", "close_thread"]);
      expect(
        journey.moves.map(({ thread }) => outRefOf(thread)).slice(2),
      ).toStrictEqual([outRefOf(silent), `${timeout!}#0`]);
      expectWalletOnlyLendsCollateral();
    },
    TIMEOUT,
  );

  it(
    "reads the closed thread no more once its close has burned its token and the case has closed",
    async () => {
      const { scene, dispute, capture, unit } = journey;
      const reads = vi.spyOn(scene.h.emulator, "getUtxosWithUnit");
      try {
        await dispute.turn(capture);
        const units = reads.mock.calls.map(([, read]) => read);
        // Neither the closed thread nor the closed case is read again.
        expect(units).not.toContain(unit);
        expect(units).not.toContain(
          Object.keys(journey.caseOrigin.assets).find((held) =>
            held.startsWith(scene.chain.case.policyId),
          ),
        );
      } finally {
        reads.mockRestore();
      }
    },
    TIMEOUT,
  );

  it("fits each funded move within the transaction limits", () => {
    for (const kind of ["reveal", "timeout", "close_thread"])
      reportFit(
        `funded ${kind}`,
        journey.moves
          .filter(({ authority }) => authority.kind === kind)
          .map(({ signed }) => signed.toCBOR()),
      );
  });
});
