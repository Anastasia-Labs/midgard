/**
 * The scene of the validation game at the consensus maximum: the opened game
 * thread and its bond, and the helpers that play and measure its moves.
 */
import {
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE,
  type MidgardValidationDispute,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { DisputeBond, protocolTreasuryAddress } from "@al-ft/midgard-sdk";
import {
  Data,
  mintingPolicyToId,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  measureCompleteSignedTransaction,
  network,
} from "./submit-init-emulator-shared.js";
import { type DeepGameTraces } from "./validation-game-deep-traces.js";
import { gameMove } from "./validation-game-moves.js";
import { nextReveal, options, type Thread } from "./validation-game-play.js";
import {
  buildMove,
  cap,
  createdAt,
  deploy,
  fraudProofOutputLovelace,
  gameDatum,
  type GameScene,
  seedThreads,
  setUpGameScene,
  tagOf,
} from "./validation-game-scene.js";
import { openGame } from "./validation-game-traces.js";

const BOND_TITLE = "fraud_proofs/validation_trace/dispute_bond_v1.main.spend";
export const bondLovelace = 10_000_000_000n;
const timing = SELECTED_DEPLOYMENT_PROFILE.timing;

/** What the thread starts with: the record, the defender's reserve, change. */
export const threadLovelace =
  fraudProofOutputLovelace +
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE +
  3_000_000n;

export const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

export type Budget = ReturnType<typeof measureCompleteSignedTransaction>;

export type MaxShapeScene = GameScene & {
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
export const setUpMaxShape = async (
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
export const submitBuilt = async (
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
export const playReveals = async (
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

export const bondSpend = (
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

export const treasuryAddress = () =>
  protocolTreasuryAddress(
    network,
    SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
  );

/**
 * Makes each side's first move as late as its clock allows while keeping one
 * slot for the rest: a prompt move lands one slot-step after the previous
 * move's upper bound and is charged that millisecond.
 */
export const spendWholeClock =
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

export const report = (
  label: string,
  depth: number,
  budget: Budget | undefined,
) =>
  console.log(
    `max-shape ${label} depth=${depth} bytes=${budget?.completeSignedBytes} ` +
      `mem=${budget?.executionMemory} steps=${budget?.executionSteps}`,
  );
