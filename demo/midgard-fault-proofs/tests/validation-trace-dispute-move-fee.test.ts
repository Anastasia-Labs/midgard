/** Who pays a validation-trace thread move, decided by the move's builds on
 * the emulator, with a plain key output standing in for the thread. The
 * thread pays a move that builds with it paying the cap. A move whose cap
 * build is short of its fee is our wallet's, however near its ledger minimum
 * is to the cap; a wallet that cannot pay it throws the build's funding
 * shortfall, which pauses the run until a top-up; a wallet-paid fee past the
 * funding permit's per-transaction ceiling is refused with the named alarm.
 * A cap build that fails for any other reason keeps its error, which the
 * runner holds the stage on and retries.
 *
 * No real move's ledger minimum can pass the ceiling: the ceiling is the
 * fee of a transaction at the protocol's size, execution and reference
 * script limits. The ceiling's refusal is pinned here with a builder that
 * sets a fee above it. */
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
  VALIDATION_TRACE_MOVE_FEE_OVER_CAP,
  validationTraceMoveFeeCeiling,
  ValidationTraceMoveFeeOverCapError,
  validationTraceMoveMinimumFee,
} from "../src/validation-trace-dispute/move-fee.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { isWorkflowFundingShortfall } from "../src/workflow/funding-reservation-permit.workflow-funding-shortfall.js";

const ADA = 1_000_000n;
const CAP = VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE;

const ours = generateEmulatorAccount({ lovelace: 1_000n * ADA });
const owner = generateEmulatorAccount({ lovelace: 20n * ADA });
const poor = generateEmulatorAccount({ lovelace: 3n * ADA });

let emulator: Emulator;
let lucid: LucidEvolution;
let thread: UTxO;

/** A move of `thread` back to its owner, in which the thread pays `fee`
 * and its output takes `extra` more than the thread less that fee. */
const move =
  (on: LucidEvolution, extra = 0n) =>
  (fee: bigint): TxBuilder =>
    on
      .newTx()
      .collectFrom([thread])
      .pay.ToAddress(owner.address, {
        lovelace: thread.assets.lovelace! - fee + extra,
      });

const complete = async (
  build: (fee: bigint) => TxBuilder,
  on: LucidEvolution = lucid,
) =>
  await completeValidationTraceMove({
    lucid: on,
    stage: "resolve",
    build,
    referenced: [],
  });

const bodyOf = (unsigned: { toCBOR: () => string }) =>
  CML.Transaction.from_cbor_hex(unsigned.toCBOR()).body();

const rejectionOf = async (pending: Promise<unknown>) =>
  await pending.then(
    () => {
      throw new Error("the move completed");
    },
    (cause: unknown) => cause,
  );

describe("validation-trace dispute move fee: who pays", () => {
  beforeAll(async () => {
    emulator = new Emulator([ours, owner, poor]);
    lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(ours.seedPhrase);
    [thread] = await lucid.utxosAt(owner.address);
  });

  it("has the thread pay a move that builds with it paying the cap, at the move's minimum", async () => {
    const body = bodyOf(await complete(move(lucid)));
    expect(body.inputs().len()).toBe(1);
    expect(body.fee()).toBeLessThan(CAP);
    expect(body.outputs().get(0).amount().coin()).toBe(
      thread.assets.lovelace! - body.fee(),
    );
  });

  it("has our wallet pay a move whose cap build is short of its fee though its ledger minimum is under the cap", async () => {
    // At the cap the thread's output takes one lovelace more than the
    // thread less the cap: the cap build is short of its fee by one.
    const paid = await complete(move(lucid, 1n));
    const body = bodyOf(paid);
    expect(
      validationTraceMoveMinimumFee({
        lucid,
        transactionCborHex: paid.toCBOR(),
        referenced: [],
      }),
    ).toBeLessThan(CAP);
    // Our wallet's inputs pay the whole fee; the thread keeps its lovelace.
    expect(body.inputs().len()).toBeGreaterThan(1);
    expect(body.outputs().get(0).amount().coin()).toBe(
      thread.assets.lovelace! + 1n,
    );
  });

  it("throws the wallet build's funding shortfall when our wallet cannot pay the move", async () => {
    const wallet = await Lucid(emulator, "Custom");
    wallet.selectWallet.fromSeed(poor.seedPhrase);
    const cause = await rejectionOf(complete(move(wallet, 10n * ADA), wallet));
    expect(isWorkflowFundingShortfall(cause)).toBe(true);
  });

  it("keeps a cap build's own error when it fails for another reason, and never builds the move for our wallet", async () => {
    const fees: bigint[] = [];
    const cause = await rejectionOf(
      complete((fee) => {
        fees.push(fee);
        if (fee === CAP) throw new Error("provider unreachable");
        return move(lucid)(fee);
      }),
    );
    expect(String(cause)).toContain("provider unreachable");
    expect(isWorkflowFundingShortfall(cause)).toBe(false);
    expect(fees).toStrictEqual([CAP]);
  });

  it("refuses, under the named alarm, a wallet-paid move whose fee passes the funding permit's per-transaction ceiling", async () => {
    const ceiling = validationTraceMoveFeeCeiling(lucid);
    const overpaid = (fee: bigint) =>
      // eslint-disable-next-line midgard/exact-fee-no-change-output -- a floor on purpose: the wallet-paid build overpays past the ceiling, change and all.
      move(lucid, 1n)(fee).setMinFee(ceiling + 1n);
    const cause = await rejectionOf(
      complete((fee) => (fee === 0n ? overpaid(fee) : move(lucid, 1n)(fee))),
    );
    expect(cause).toBeInstanceOf(ValidationTraceMoveFeeOverCapError);
    // A changed action: the run resumes on its next observation.
    expect(cause).toBeInstanceOf(WorkflowActionChangedError);
    const refused = cause as ValidationTraceMoveFeeOverCapError;
    expect(refused.message.startsWith(VALIDATION_TRACE_MOVE_FEE_OVER_CAP)).toBe(
      true,
    );
    expect(refused.stage).toBe("resolve");
    expect(refused.feeLovelace).toBe(ceiling + 1n);
    expect(refused.ceilingLovelace).toBe(ceiling);
  });
});
