/**
 * The validation game at the consensus maximum: a 2^32 - 1 step trace, all 32
 * bisection rounds, then resolution, the award and the bond. Each depth runs
 * as its own case so a measurement can start small and double.
 *
 * Segments not yet available run through seams: the game is seeded at its
 * VerifySource successor (no TypeScript claim builder before L-N I5), the
 * one-step resolver chain is replaced by terminal padding (the award path),
 * and the fraud record is read by reference straight from the finalize
 * output (record-by-reference lands with the state-queue change).
 */
import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  type MidgardValidationDispute,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  DisputeBond,
  DisputeBondSpendRedeemer,
  protocolTreasuryAddress,
} from "@al-ft/midgard-sdk";
import {
  Data,
  mintingPolicyToId,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
  network,
} from "./support/submit-init-emulator-shared.js";
import {
  sparseTrace,
  type DeepGameTraces,
  deepDivergentTraces,
  deepTerminalPaddingTraces,
} from "./support/validation-game-deep-traces.js";
import {
  awardFinalizeMove,
  awardTerminalPaddingMove,
  gameMove,
} from "./support/validation-game-moves.js";
import {
  enterResolution,
  nextReveal,
  options,
  type Thread,
} from "./support/validation-game-play.js";
import {
  buildMove,
  cap,
  createdAt,
  deploy,
  fraudProofOutputLovelace,
  gameDatum,
  type GameScene,
  type Move,
  seedThreads,
  setUpGameScene,
  tagOf,
} from "./support/validation-game-scene.js";
import {
  machineState,
  openGame,
  type TraceSide,
} from "./support/validation-game-traces.js";

const BOND_TITLE = "fraud_proofs/validation_trace/dispute_bond_v1.main.spend";
const bondLovelace = 10_000_000_000n;
const timing = SELECTED_DEPLOYMENT_PROFILE.timing;
const maxBisectionRounds =
  SELECTED_DEPLOYMENT_PROFILE.limits.max_bisection_rounds;

/** Every move the game reserve pays for: all reveals, entry, the boundary. */
const reservedMoves = BigInt(2 * maxBisectionRounds + 2);

/** What VerifySource leaves on the thread: the record, the reserve, change. */
const threadLovelace =
  fraudProofOutputLovelace + reservedMoves * cap + 3_000_000n;

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

type Budget = ReturnType<typeof measureCompleteSignedTransaction>;

type MaxShapeScene = GameScene & {
  readonly thread: Thread;
  readonly bond: UTxO;
  readonly bondScript: GameScene["game"];
  readonly forfeitAfter: number;
};

/**
 * The game thread VerifySource opens, and the bond it locks, for `traces`.
 * The challenged block ends when the scene starts, so the bond's
 * `forfeit_after` is that instant plus maturity, both clocks and the
 * resolution allowance.
 */
const setUpMaxShape = async (
  traces: DeepGameTraces,
): Promise<MaxShapeScene> => {
  const scene = await setUpGameScene();
  const blockEnd = scene.emulator.now();
  const forfeitAfter =
    blockEnd +
    timing.block_maturity_ms +
    2 * timing.dispute_side_clock_ms +
    timing.dispute_resolution_allowance_ms;
  const bondScript = deploy(BOND_TITLE, [
    mintingPolicyToId(scene.fraudProofPolicy),
  ]);
  const dispute = openGame(traces, scene.emulator.now());
  const [threadUtxo] = await seedThreads(
    scene,
    scene.game.address,
    [gameDatum(scene, dispute)],
    threadLovelace,
  );
  const bondTx = await scene.lucid
    .newTx()
    .pay.ToContract(
      bondScript.address,
      {
        kind: "inline",
        value: Data.to(
          {
            challenged_header_hash: "aa".repeat(28),
            fraud_prover: scene.proverKey,
            forfeit_after: BigInt(forfeitAfter),
          },
          DisputeBond,
        ),
      },
      { lovelace: bondLovelace },
    )
    .complete();
  const bondHash = await (await bondTx.sign.withWallet().complete()).submit();
  await scene.lucid.awaitTx(bondHash);
  const bond = (await scene.lucid.utxosAt(bondScript.address))[0]!;
  return {
    ...scene,
    thread: { utxo: threadUtxo!, dispute },
    bond,
    bondScript,
    forfeitAfter,
  };
};

/** Signs with the wallet (and the prover when named), submits, measures. */
const submitBuilt = async (
  scene: GameScene,
  built: TxSignBuilder,
  proverSigns: boolean,
): Promise<{ readonly hash: string; readonly budget: Budget }> => {
  let signing = built.sign.withWallet();
  if (proverSigns)
    signing = signing.sign.withPrivateKey(scene.proverSigningKey);
  const signed = await signing.complete();
  const budget = measureCompleteSignedTransaction(signed.toCBOR());
  const hash = await signed.submit();
  await scene.lucid.awaitTx(hash);
  return { hash, budget };
};

/** The move with the larger CPU budget. */
const heavier = (left: Budget | undefined, right: Budget) =>
  left === undefined || right.executionSteps > left.executionSteps
    ? right
    : left;

/**
 * Plays honest reveals by a third party, each paying the capped fee from
 * the thread, until `stop` holds for the next honest successor. `wait` runs
 * before each move and may advance the clock.
 */
const playReveals = async (
  scene: GameScene,
  traces: DeepGameTraces,
  opened: Thread,
  stop: (next: MidgardValidationDispute) => boolean,
  wait: (dispute: MidgardValidationDispute) => void = () => undefined,
): Promise<{ readonly thread: Thread; readonly heaviest?: Budget }> => {
  let thread = opened;
  let heaviest: Budget | undefined;
  for (;;) {
    wait(thread.dispute);
    const from = scene.emulator.now();
    const { action, next } = nextReveal(traces, thread.dispute, from);
    if (stop(next)) return { thread, heaviest };
    const built = await buildMove(
      scene,
      gameMove(scene, thread.utxo, action, next, options(from)),
    );
    const { hash, budget } = await submitBuilt(scene, built, false);
    heaviest = heavier(heaviest, budget);
    thread = { utxo: await createdAt(scene, hash, 0), dispute: next };
    if (next.turn.type === "readyForOneStep") return { thread, heaviest };
  }
};

const bondSpend = (
  scene: MaxShapeScene,
  redeemer: string,
  destination: string,
  references: readonly UTxO[],
  validFrom?: number,
) => {
  let tx = scene.lucid.newTx();
  if (references.length > 0) tx = tx.readFrom([...references]);
  tx = tx
    .collectFrom([scene.bond], redeemer)
    .pay.ToAddressWithData(
      destination,
      { kind: "inline", value: tagOf(scene.bond) },
      { lovelace: lovelaceOf(scene.bond) - cap },
    );
  if (validFrom !== undefined) tx = tx.validFrom(validFrom);
  // The lone bond pays exactly the cap: no change output; the wallet only
  // supplies collateral.
  return tx.attach
    .SpendingValidator(scene.bondScript.script)
    .setMinFee(cap)
    .complete({ coinSelection: false, localUPLCEval: true });
};

const treasuryAddress = () =>
  protocolTreasuryAddress(
    network,
    SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
  );

/**
 * Makes each side's first move as late as its clock allows while keeping one
 * slot for the rest: a prompt move lands one slot-step after the previous
 * move's upper bound and is charged that millisecond.
 */
const spendWholeClock =
  (scene: GameScene) => (dispute: MidgardValidationDispute) => {
    if (dispute.round !== 0) return;
    const clock =
      dispute.turn.type === "awaitingOperator"
        ? dispute.operatorClockMs
        : dispute.challengerClockMs;
    const slots =
      Math.floor(
        (dispute.turnStartedAt + clock - scene.emulator.now()) / 1000,
      ) - 1;
    if (slots > 0) scene.emulator.awaitSlot(slots);
  };

const report = (label: string, depth: number, budget: Budget | undefined) =>
  console.log(
    `max-shape ${label} depth=${depth} bytes=${budget?.completeSignedBytes} ` +
      `mem=${budget?.executionMemory} steps=${budget?.executionSteps}`,
  );

describe("sparse trace", () => {
  it("commits exactly what the core trace tree commits, for every index", () => {
    const states = [
      machineState(0, "canonicalDecode"),
      machineState(1, "cek"),
      machineState(2, "terminal"),
      machineState(3, "cek"),
    ];
    // 37 steps: the last run is padded up to 64 leaves.
    const runs = [
      { from: 0, state: states[0]! },
      { from: 1, state: states[1]! },
      { from: 22, state: states[2]! },
      { from: 23, state: states[3]! },
    ];
    const stepCount = 37;
    const listed = Array.from({ length: stepCount + 1 }, (_, index) =>
      hashMidgardValidationMachineState(
        index >= 23
          ? states[3]!
          : index >= 22
            ? states[2]!
            : index >= 1
              ? states[1]!
              : states[0]!,
      ),
    );
    const tree = buildMidgardValidationTraceTree(listed, "accepted");
    const sparse: TraceSide = sparseTrace(runs, stepCount);
    expect(sparse.descriptor).toEqual(tree.descriptor);
    tree.proofs.forEach((proof, index) => {
      expect(sparse.proofAt(index)).toEqual(proof);
    });
  });
});

describe.each([4, 8, 16, 32])("validation game at trace depth %i", (depth) => {
  it(`depth ${depth} convicts terminal padding after every bisection round, and refunds the bond`, async () => {
    const traces = deepTerminalPaddingTraces(depth);
    const scene = await setUpMaxShape(traces);
    expect(traces.stepCount).toBe(2 ** depth - 1);

    const { thread: ready, heaviest } = await playReveals(
      scene,
      traces,
      scene.thread,
      () => false,
    );
    expect(ready.dispute.turn.type).toBe("readyForOneStep");
    expect(ready.dispute.round).toBe(depth);
    expect(ready.dispute.lowIndex).toBe(traces.boundary);
    expect(ready.dispute.highIndex).toBe(traces.boundary + 1);
    report("reveal", depth, heaviest);

    // EnterResolution is the prover's: the unsigned move is refused.
    await expectOnchainRefusal(() =>
      buildMove(scene, enterResolution(scene, ready, { signed: false }).move),
    );
    const entered = enterResolution(scene, ready, { signed: true });
    const entry = await submitBuilt(
      scene,
      await buildMove(scene, entered.move),
      true,
    );
    report("enter-resolution", depth, entry.budget);
    const boundary = await createdAt(scene, entry.hash, 0);

    const award = await submitBuilt(
      scene,
      await buildMove(
        scene,
        awardTerminalPaddingMove(
          scene,
          boundary,
          traces.agreedLowState,
          options(scene.emulator.now(), { signed: true }),
        ),
      ),
      true,
    );
    report("award-terminal-padding", depth, award.budget);
    const awarded = await createdAt(scene, award.hash, 0);
    // Every reveal, the entry and the award came out of the reserve.
    const movesPaid = BigInt(2 * depth + 2);
    expect(lovelaceOf(awarded)).toBe(threadLovelace - movesPaid * cap);

    const finalize = await submitBuilt(
      scene,
      await buildMove(
        scene,
        awardFinalizeMove(
          scene,
          awarded,
          { kind: "thread" },
          scene.emulator.now(),
        ),
      ),
      false,
    );
    report("award-finalize", depth, finalize.budget);
    const record = await createdAt(scene, finalize.hash, 1);
    expect(record.assets[scene.fraudProofUnit]).toBe(1n);
    expect(lovelaceOf(record)).toBe(fraudProofOutputLovelace);

    const refund = Data.to(
      { Refund: { fraud_proof_ref_input_index: 0n, output_index: 0n } },
      DisputeBondSpendRedeemer,
    );
    await submitBuilt(
      scene,
      await bondSpend(scene, refund, scene.proverAddress, [record]),
      false,
    );
    const paid = (await scene.lucid.utxosAt(scene.proverAddress)).filter(
      (utxo) => utxo.datum === tagOf(scene.bond),
    );
    expect(paid.map(lovelaceOf)).toEqual([bondLovelace - cap]);
  });

  it(`depth ${depth} never convicts an honest operator whose challenger stops, and forfeits the bond`, async () => {
    const traces = deepDivergentTraces(depth);
    const scene = await setUpMaxShape(traces);

    // Play to the challenger's last reveal and stop there.
    const { thread } = await playReveals(
      scene,
      traces,
      scene.thread,
      (next) => next.turn.type === "readyForOneStep",
    );
    expect(thread.dispute.turn.type).toBe("awaitingChallenger");
    expect(thread.dispute.round).toBe(depth - 1);

    const challengerMove = (from: number, corrupt: boolean): Move => {
      const { action, next } = nextReveal(traces, thread.dispute, from);
      if (action.kind !== "RevealChallenger") throw new Error("not a reveal");
      const siblings = [...action.proof.siblings];
      // The sibling next to the root: the one only a full-depth tree has.
      const top = Buffer.from(siblings[siblings.length - 1]!);
      top[0] = top[0]! ^ 1;
      if (corrupt) siblings[siblings.length - 1] = top;
      return gameMove(
        scene,
        thread.utxo,
        { kind: "RevealChallenger", proof: { ...action.proof, siblings } },
        next,
        options(from),
      );
    };
    // The honest reveal is accepted; the same reveal with one sibling of
    // the proof changed is refused at the proof check.
    await buildMove(scene, challengerMove(scene.emulator.now(), false));
    await expectOnchainRefusal(() =>
      buildMove(scene, challengerMove(scene.emulator.now(), true)),
    );

    // The challenger lets its clock run out. (A late reveal has no valid
    // successor; the overdraw refusal is pinned in validation-dispute-v1.test.ak.)
    const deadline =
      thread.dispute.turnStartedAt + thread.dispute.challengerClockMs;
    // The turn starts at an upper bound ending in 999 ms: wait whole slots.
    scene.emulator.awaitSlot(
      Math.ceil((deadline - scene.emulator.now()) / 1000) + 1,
    );
    // A challenger timeout convicts only an operator whose clock ran out.
    await expectOnchainRefusal(() =>
      buildMove(
        scene,
        gameMove(
          scene,
          thread.utxo,
          { kind: "EnterChallengerTimeout" },
          thread.dispute,
          options(scene.emulator.now()),
        ),
      ),
    );

    const forfeit = Data.to(
      { Forfeit: { output_index: 0n } },
      DisputeBondSpendRedeemer,
    );
    await expectOnchainRefusal(() =>
      bondSpend(scene, forfeit, treasuryAddress(), [], scene.forfeitAfter),
    );
    scene.emulator.awaitSlot(
      Math.ceil((scene.forfeitAfter - scene.emulator.now()) / 1000) + 20,
    );
    await submitBuilt(
      scene,
      await bondSpend(
        scene,
        forfeit,
        treasuryAddress(),
        [],
        scene.emulator.now(),
      ),
      false,
    );
    const paid = await scene.lucid.utxosAt(treasuryAddress());
    expect(paid.map(lovelaceOf)).toEqual([bondLovelace - cap]);
    // The operator's thread is still where the game left it.
    const [still] = await scene.lucid.utxosByOutRef([thread.utxo]);
    expect(still?.datum).toBe(gameDatum(scene, thread.dispute));
  });
});

/**
 * The latest a validation-trace conviction can land: a game at the maximum
 * depth where each side spends its whole clock. VerifySource opens a game
 * only while the maximum dispute duration still fits before the challenged
 * block matures, so a conviction inside that duration lands no later than
 * the block's maturity, the earliest instant its committing operator's bond
 * can unlock. The selected testing profile is non-interactive (its maturity
 * is shorter than one dispute), so the game is seeded and the bound is
 * measured from its opening.
 *
 * The rest of this timeline, the exempt prunes at the maximum descendant
 * depth and the operator's bond recovery, needs the record-by-reference and
 * prune builders of the state-queue change and a committed block whose
 * operator node carries a bond hold; neither is on this base.
 */
describe("validation game at the consensus maximum, with both clocks spent", () => {
  it("max-depth game with both clocks spent convicts within the maximum dispute duration", async () => {
    const depth = maxBisectionRounds;
    const traces = deepTerminalPaddingTraces(depth);
    const scene = await setUpMaxShape(traces);
    const opened = scene.thread.dispute.turnStartedAt;

    const { thread: ready } = await playReveals(
      scene,
      traces,
      scene.thread,
      () => false,
      spendWholeClock(scene),
    );
    expect(ready.dispute.round).toBe(depth);
    // Each side has less than two slots of its clock left.
    expect(ready.dispute.operatorClockMs).toBeLessThan(2000);
    expect(ready.dispute.challengerClockMs).toBeLessThan(2000);
    const gameEnded = ready.dispute.turnStartedAt;

    const entered = enterResolution(scene, ready, { signed: true });
    const entry = await submitBuilt(
      scene,
      await buildMove(scene, entered.move),
      true,
    );
    const boundary = await createdAt(scene, entry.hash, 0);
    const award = await submitBuilt(
      scene,
      await buildMove(
        scene,
        awardTerminalPaddingMove(
          scene,
          boundary,
          traces.agreedLowState,
          options(scene.emulator.now(), { signed: true }),
        ),
      ),
      true,
    );
    const awarded = await createdAt(scene, award.hash, 0);
    const finalizedFrom = scene.emulator.now();
    const finalize = await submitBuilt(
      scene,
      await buildMove(
        scene,
        awardFinalizeMove(scene, awarded, { kind: "thread" }, finalizedFrom),
      ),
      false,
    );
    const record = await createdAt(scene, finalize.hash, 1);
    expect(record.assets[scene.fraudProofUnit]).toBe(1n);

    // The finalize's validity upper bound: the latest the record can exist.
    const convictedBy =
      finalizedFrom + MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS - 1;
    expect(convictedBy - opened).toBeLessThanOrEqual(
      MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
    );
    console.log(
      `max-shape timing depth=${depth} game_ms=${gameEnded - opened} ` +
        `resolution_ms=${convictedBy - gameEnded} ` +
        `max_duration_ms=${MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS} ` +
        `slack_ms=${MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS - (convictedBy - opened)}`,
    );
  });
});
