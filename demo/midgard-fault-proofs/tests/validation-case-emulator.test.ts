/**
 * The validation case on the emulator: the open through the VerifySource
 * seam, defenses, the close of a timed-out thread into its case, and the
 * case close. Every Plutus script runs from its published reference script.
 */
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import {
  CaseRedeemer,
  EMPTY_MERKLE_TREE_ROOT,
  ReceiptRedeemer,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  buildCloseCase,
  buildCloseTimedOutThread,
  buildExpireThread,
  closeTimedOutThread,
} from "./support/validation-case-close.js";
import {
  bondUnlockTimeOf,
  buildDefend,
  buildOpenCase,
  caseUnit,
  defend,
  type Defender,
  fraudProofUnit,
  markerUnit,
  type OpenCase,
  openCase,
  registerDefender,
} from "./support/validation-case-moves.js";
import { threadTokenName } from "./support/validation-case-names.js";
import {
  type CaseWorld,
  defenseBondUnlockTime,
  defenseDeadline,
  referenceOf,
  setUpCaseWorld,
  sideClockMs,
  submitSigned,
  threadFloorLovelace,
} from "./support/validation-case-scene.js";
import {
  deepDivergentTraces,
  sparseTrace,
} from "./support/validation-game-deep-traces.js";
import {
  passDeadline,
  passOperatorDeadline,
  playToOneStep,
  reveal,
  timeoutMove,
} from "./support/validation-game-play.js";
import {
  buildMove,
  cap,
  createdAt,
  fraudProofOutputLovelace,
  keyAddress,
  tagOf,
  type Thread,
  threadDatum,
  threadUnit,
} from "./support/validation-game-scene.js";
import { machineState } from "./support/validation-game-traces.js";

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

const openedWorld = async () => {
  const world = await setUpCaseWorld();
  const traces = deepDivergentTraces(2);
  const opened = await openCase(world, traces);
  return { world, traces, opened };
};

/** Hands `thread` to the timeout once its side to move has run out. */
const timeOut = async (world: CaseWorld, thread: Thread): Promise<Thread> => {
  const deadline =
    thread.dispute.turnStartedAt +
    (thread.dispute.turn.type === "awaitingOperator"
      ? thread.dispute.operatorClockMs
      : thread.dispute.challengerClockMs);
  passDeadline(world, deadline);
  const { hash } = await submitSigned(
    world,
    await buildMove(world, timeoutMove(world, thread)),
  );
  return { ...thread, utxo: await createdAt(world, hash, 0) };
};

const defended = async (world: CaseWorld, opened: OpenCase) =>
  defend(world, opened, await registerDefender(world));

/** A state at the last step that ends the trace rejected. */
const rejectedTerminal = (index: number): MidgardValidationMachineState => ({
  ...machineState(index, "terminal"),
  verdict: "rejected",
  rejectionCodeHash: Buffer.alloc(32, 0x99),
});

describe("validation case open", () => {
  it("opens a case and its bond from the source, and refuses an opener elsewhere", async () => {
    const { world, opened } = await openedWorld();
    expect(opened.utxo.address).toBe(world.caseValidator.address);
    expect(opened.utxo.assets[caseUnit(world, opened)]).toBe(1n);
    expect(opened.datum.defenders_root).toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(opened.bond.address).toBe(world.bond.address);
    expect(opened.bond.assets[markerUnit(world, opened)]).toBe(1n);

    const { built } = await buildOpenCase(world, deepDivergentTraces(2), {
      opener: "wallet",
    });
    await expectOnchainRefusal(() => built);
  });
});

describe("validation case defense", () => {
  it("accepts a signed active operator and holds its bond through the case", async () => {
    const { world, opened } = await openedWorld();
    const result = await defended(world, opened);
    const startedAt = Number(opened.datum.started_at);
    expect(result.opened.datum.open_threads).toBe(1n);
    expect(result.opened.datum.defenders_root).not.toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(result.thread.utxo.address).toBe(world.game.address);
    expect(result.thread.utxo.datum).toBe(
      threadDatum(world, result.thread.identity, result.thread.dispute),
    );
    expect(
      result.thread.utxo.assets[threadUnit(world, result.thread.identity)],
    ).toBe(1n);
    expect(lovelaceOf(result.thread.utxo)).toBe(threadFloorLovelace);
    expect(bondUnlockTimeOf(result.defender.node)).toBe(
      defenseBondUnlockTime(startedAt),
    );
  });

  it("keeps a longer bond hold, and refuses one shortened below the case", async () => {
    const { world, opened } = await openedWorld();
    const needed = defenseBondUnlockTime(Number(opened.datum.started_at));
    const short = await registerDefender(world);
    const { built } = await buildDefend(world, opened, short, {
      bondUnlockTime: needed - 1,
    });
    await expectOnchainRefusal(() => built);
    const long = await registerDefender(world, needed + 3_600_000);
    const result = await defend(world, opened, long);
    expect(bondUnlockTimeOf(result.defender.node)).toBe(needed + 3_600_000);
  });

  it("refuses a second defense by the same operator, and accepts another operator", async () => {
    const { world, opened } = await openedWorld();
    const first = await defended(world, opened);
    const again = await buildDefend(world, first.opened, first.defender);
    await expectOnchainRefusal(() => again.built);
    const second = await defended(world, first.opened);
    expect(second.opened.datum.open_threads).toBe(2n);
  });

  it("refuses an unsigned defense, an inactive operator and a thread short of its reserve", async () => {
    const { world, opened } = await openedWorld();
    const defender = await registerDefender(world);
    const refused = async (
      who: Defender,
      changes: Parameters<typeof buildDefend>[3],
    ) => {
      const { built } = await buildDefend(world, opened, who, changes);
      await expectOnchainRefusal(() => built);
    };
    await refused(defender, { signed: false });
    await refused(defender, { threadLovelace: threadFloorLovelace - 1n });
    await refused(
      await registerDefender(world, null, world.computationThreadPolicy),
      {},
    );
  });

  it("refuses a defense claiming the challenger's high state or a rejected terminal state", async () => {
    const { world, traces, opened } = await openedWorld();
    const defender = await registerDefender(world);
    const rejected = sparseTrace(
      [
        { from: 0, state: machineState(0, "canonicalDecode") },
        { from: traces.stepCount, state: rejectedTerminal(traces.stepCount) },
      ],
      traces.stepCount,
    );
    for (const claim of [traces.challengerTrace, rejected]) {
      const { built } = await buildDefend(world, opened, defender, { claim });
      await expectOnchainRefusal(() => built);
    }
  });

  it("refuses a defense once the defender's clock has run out", async () => {
    const { world, opened } = await openedWorld();
    const defender = await registerDefender(world);
    passDeadline(world, Number(opened.datum.started_at) + sideClockMs);
    const { built } = await buildDefend(world, opened, defender);
    await expectOnchainRefusal(() => built);
  });

  it("couples the thread token to the defense: a Defend minting none or two, and a mint without a Defend, are refused", async () => {
    const { world, opened } = await openedWorld();
    const defender = await registerDefender(world);
    for (const threadTokens of [0, 2] as const) {
      const { built } = await buildDefend(world, opened, defender, {
        threadTokens,
      });
      await expectOnchainRefusal(() => built);
    }
    const unit = threadUnit(world, {
      caseId: opened.caseId,
      startedAt: Number(opened.datum.started_at),
      defenderVkh: defender.vkh,
    });
    await expectOnchainRefusal(() =>
      world.lucid
        .newTx()
        .mintAssets(
          { [unit]: 1n },
          Data.to(
            {
              ThreadClose: {
                case_output_index: 0n,
                thread_input_index: 0n,
                outcome: "DefenderWins",
                defender_vkh: defender.vkh,
              },
            },
            CaseRedeemer,
          ),
        )
        .readFrom([world.config, referenceOf(world, world.caseValidator)])
        .pay.ToAddress(world.game.address, {
          lovelace: threadFloorLovelace,
          [unit]: 1n,
        })
        .complete({ localUPLCEval: true }),
    );
  });
});

describe("validation case thread close", () => {
  it("closes a defender's timeout as the challenger's win, minting the receipt from the thread alone", async () => {
    const { world, opened } = await openedWorld();
    const result = await defended(world, opened);
    // The defender's clock has not run out yet.
    await expectOnchainRefusal(() =>
      buildMove(world, timeoutMove(world, result.thread)),
    );
    const thread = await timeOut(world, result.thread);
    const { built, receiptUnit, next } = buildCloseTimedOutThread(
      world,
      result.opened,
      thread,
      { lone: true, fee: cap },
    );
    const { hash } = await submitSigned(world, await built);
    const closed = await next(hash);
    expect(closed.datum.open_threads).toBe(0n);
    expect(closed.datum.defender_won).toBe(false);
    const [receipt] = await world.lucid.utxosAtWithUnit(
      world.award.address,
      receiptUnit,
    );
    expect(lovelaceOf(receipt!)).toBe(fraudProofOutputLovelace);
    const leftover = (await world.lucid.utxosAt(world.proverAddress)).filter(
      (utxo) => utxo.datum === tagOf(thread.utxo),
    );
    expect(leftover.map(lovelaceOf)).toEqual([
      lovelaceOf(thread.utxo) - fraudProofOutputLovelace - cap,
    ]);
    // The thread token is burned, and the operator cannot defend again.
    expect(
      await world.lucid.utxosAtWithUnit(
        world.timeout.address,
        threadUnit(world, thread.identity),
      ),
    ).toEqual([]);
    const again = await buildDefend(world, closed, result.defender);
    await expectOnchainRefusal(() => again.built);
  });

  it("refuses a close naming the other side as the winner", async () => {
    const { world, opened } = await openedWorld();
    const result = await defended(world, opened);
    const thread = await timeOut(world, result.thread);
    const { built } = buildCloseTimedOutThread(world, result.opened, thread, {
      outcome: "DefenderWins",
    });
    await expectOnchainRefusal(() => built);
  });

  it("closes a challenger's timeout, at ReadyForOneStep too, as the defender's win with no receipt", async () => {
    const { world, traces, opened } = await openedWorld();
    const first = await defended(world, opened);
    const second = await defended(world, first.opened);
    const revealed = await reveal(world, traces, first.thread);
    const ready = await playToOneStep(world, traces, second.thread);
    expect(ready.dispute.turn.type).toBe("readyForOneStep");

    let current: OpenCase = second.opened;
    for (const thread of [revealed, ready]) {
      const timedOut = await timeOut(world, thread);
      current = (await closeTimedOutThread(world, current, timedOut)).opened;
      const paid = await world.lucid.utxosAt(
        keyAddress(thread.identity.defenderVkh),
      );
      expect(
        paid.filter((utxo) => utxo.datum === tagOf(timedOut.utxo)),
      ).toHaveLength(1);
    }
    expect(current.datum.open_threads).toBe(0n);
    expect(current.datum.defender_won).toBe(true);
    // No receipt was minted.
    expect(await world.lucid.utxosAt(world.award.address)).toEqual([]);
  });
});

describe("validation case configuration and receipts", () => {
  it("refuses a second configuration once its initialization UTxO is spent", async () => {
    const { world } = await openedWorld();
    const [unit] = Object.keys(world.config.assets).filter(
      (asset) => asset !== "lovelace",
    );
    const policyId = unit!.slice(0, 56);
    await expectOnchainRefusal(() =>
      world.lucid
        .newTx()
        .mintAssets({ [unit!]: 1n }, Data.void())
        .readFrom([world.scriptReferences.get(policyId)!])
        .pay.ToContract(
          world.config.address,
          { kind: "inline", value: world.config.datum! },
          { [unit!]: 1n },
        )
        .complete({ localUPLCEval: true }),
    );
  });

  it("refuses a receipt minted without a case input", async () => {
    const { world, opened } = await openedWorld();
    const defender = await registerDefender(world);
    const unit =
      world.award.hash + threadTokenName(opened.caseId, defender.vkh);
    await expectOnchainRefusal(() =>
      world.lucid
        .newTx()
        .mintAssets(
          { [unit]: 1n },
          Data.to({ MintReceipt: { case_input_index: 0n } }, ReceiptRedeemer),
        )
        .readFrom([referenceOf(world, world.award)])
        .pay.ToAddress(world.award.address, {
          lovelace: fraudProofOutputLovelace,
          [unit]: 1n,
        })
        .complete({ localUPLCEval: true }),
    );
  });
});

describe("validation case thread expiry", () => {
  it("releases a live thread to its defender once the longest case is over, and not before", async () => {
    const { world, opened } = await openedWorld();
    const { thread } = await defended(world, opened);
    await expectOnchainRefusal(() => buildExpireThread(world, thread));
    passDeadline(
      world,
      Number(opened.datum.started_at) +
        MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
    );
    await submitSigned(world, await buildExpireThread(world, thread));
    const paid = (
      await world.lucid.utxosAt(keyAddress(thread.identity.defenderVkh))
    ).filter((utxo) => utxo.datum === tagOf(thread.utxo));
    expect(paid.map(lovelaceOf)).toEqual([lovelaceOf(thread.utxo) - cap]);
  });
});

describe("validation case close", () => {
  it("refuses a close with a live thread until the longest case is over, then forfeits the bond", async () => {
    const { world, opened } = await openedWorld();
    const result = await defended(world, opened);
    passOperatorDeadline(world, result.thread);
    await expectOnchainRefusal(() => buildCloseCase(world, result.opened));
    passDeadline(
      world,
      Number(opened.datum.started_at) +
        MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
    );
    await submitSigned(world, await buildCloseCase(world, result.opened));
    expect(
      await world.lucid.utxosAtWithUnit(
        world.caseValidator.address,
        caseUnit(world, opened),
      ),
    ).toEqual([]);
  });

  it("convicts an undefended case only after the defense deadline, refunding the bond", async () => {
    const { world, opened } = await openedWorld();
    await expectOnchainRefusal(() => buildCloseCase(world, opened));
    passDeadline(world, defenseDeadline(Number(opened.datum.started_at)));
    await submitSigned(world, await buildCloseCase(world, opened));
    const [record] = await world.lucid.utxosAtWithUnit(
      world.fraudProofAddress,
      fraudProofUnit(world, opened),
    );
    expect(lovelaceOf(record!)).toBe(fraudProofOutputLovelace);
    const [lock] = await world.lucid.utxosAtWithUnit(
      world.fraudProofAddress,
      caseUnit(world, opened),
    );
    expect(lock!.assets[markerUnit(world, opened)]).toBe(1n);
  });

  it("convicts two cases on the same block, and an indexer finds both records", async () => {
    const world = await setUpCaseWorld();
    const first = await openCase(world, deepDivergentTraces(2));
    const second = await openCase(world, deepDivergentTraces(2));
    expect(first.caseId).not.toBe(second.caseId);
    passDeadline(world, defenseDeadline(Number(second.datum.started_at)));
    await submitSigned(world, await buildCloseCase(world, first));
    await submitSigned(world, await buildCloseCase(world, second));
    expect(
      await world.lucid.utxosAtWithUnit(
        world.fraudProofAddress,
        fraudProofUnit(world, first),
      ),
    ).toHaveLength(2);
  });
});
