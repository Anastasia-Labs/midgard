/**
 * The challenger bond of a validation case on the emulator: VerifySource
 * locks it beside its case, and only that case's close spends it, refunding
 * it to the challenger on a conviction and forfeiting it to the treasury
 * otherwise.
 */
import {
  DisputeBond,
  DisputeBondSpendRedeemer,
  requireInputIndex,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  buildCloseCase,
  closeTimedOutThread,
} from "./support/validation-case-close.js";
import {
  defend,
  type OpenCase,
  openCase,
  registerDefender,
} from "./support/validation-case-moves.js";
import {
  bondLovelace,
  type CaseWorld,
  defenseDeadline,
  referenceOf,
  setUpCaseWorld,
  submitSigned,
  treasuryAddress,
} from "./support/validation-case-scene.js";
import { deepDivergentTraces } from "./support/validation-game-deep-traces.js";
import {
  passDeadline,
  passOperatorDeadline,
  reveal,
  timeoutMove,
} from "./support/validation-game-play.js";
import {
  buildMove,
  createdAt,
  tagOf,
} from "./support/validation-game-scene.js";

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

/** Two undefended cases on one block, both past their defense deadline. */
const twoClosableCases = async () => {
  const world = await setUpCaseWorld();
  const first = await openCase(world, deepDivergentTraces(2));
  const second = await openCase(world, deepDivergentTraces(2));
  passDeadline(world, defenseDeadline(Number(second.datum.started_at)));
  return { world, first, second };
};

const paidWithTag = async (world: CaseWorld, address: string, tagged: UTxO) =>
  (await world.lucid.utxosAt(address)).filter(
    (utxo) => utxo.datum === tagOf(tagged),
  );

/** A case whose one defender won its thread on the challenger's timeout. */
const defenderWonCase = async (): Promise<{
  readonly world: CaseWorld;
  readonly opened: OpenCase;
}> => {
  const world = await setUpCaseWorld();
  const traces = deepDivergentTraces(2);
  const opened = await openCase(world, traces);
  const defended = await defend(world, opened, await registerDefender(world));
  // The defender reveals; the challenger then lets its clock run out.
  const revealed = await reveal(world, traces, defended.thread);
  passDeadline(
    world,
    revealed.dispute.turnStartedAt + revealed.dispute.challengerClockMs,
  );
  const timedOut = await submitSigned(
    world,
    await buildMove(world, timeoutMove(world, revealed)),
  );
  const closed = await closeTimedOutThread(world, defended.opened, {
    ...revealed,
    utxo: await createdAt(world, timedOut.hash, 0),
  });
  return { world, opened: closed.opened };
};

describe("validation case bond on the emulator", () => {
  it("refunds the bond to the challenger when its case convicts", async () => {
    const { world, first } = await twoClosableCases();
    await submitSigned(world, await buildCloseCase(world, first));
    const refund = await paidWithTag(world, world.proverAddress, first.bond);
    expect(refund.map(lovelaceOf)).toEqual([bondLovelace]);
  });

  it("refuses a convicted close that pays the bond anywhere but the challenger", async () => {
    const { world, first } = await twoClosableCases();
    await expectOnchainRefusal(() =>
      buildCloseCase(world, first, { bondTo: treasuryAddress() }),
    );
  });

  it("forfeits the bond to the treasury when a defender won, and refuses paying it to the challenger", async () => {
    const { world, opened } = await defenderWonCase();
    await expectOnchainRefusal(() =>
      buildCloseCase(world, opened, { bondTo: world.proverAddress }),
    );
    await submitSigned(world, await buildCloseCase(world, opened));
    const forfeit = await paidWithTag(world, treasuryAddress(), opened.bond);
    expect(forfeit.map(lovelaceOf)).toEqual([bondLovelace]);
  });

  it("refuses a bond spent without a case close", async () => {
    const { world, first } = await twoClosableCases();
    await expectOnchainRefusal(() =>
      world.lucid
        .newTx()
        .collectFrom([first.bond], (ctx) =>
          Data.to(
            { case_input_index: requireInputIndex(ctx, first.bond, "bond") },
            DisputeBondSpendRedeemer,
          ),
        )
        .readFrom([referenceOf(world, world.bond)])
        .pay.ToAddress(world.proverAddress, { lovelace: bondLovelace })
        .complete({ localUPLCEval: true }),
    );
  });

  it("refuses a case close that takes another case's bond", async () => {
    const { world, first, second } = await twoClosableCases();
    await expectOnchainRefusal(() =>
      buildCloseCase(world, first, { bond: second.bond }),
    );
  });

  it("refuses a case close over a bond without the case's marker", async () => {
    const { world, first } = await twoClosableCases();
    const { hash } = await submitSigned(
      world,
      await world.lucid
        .newTx()
        .pay.ToContract(
          world.bond.address,
          {
            kind: "inline",
            value: Data.to(
              {
                challenged_header_hash: first.datum.block_header_hash,
                fraud_prover: world.proverKey,
                case_id: first.caseId,
              },
              DisputeBond,
            ),
          },
          { lovelace: bondLovelace },
        )
        .complete({ localUPLCEval: true }),
    );
    const unmarked = await createdAt(world, hash, 0);
    await expectOnchainRefusal(() =>
      buildCloseCase(world, first, { bond: unmarked }),
    );
  });

  it("keeps a bond locked while its case has a thread open", async () => {
    const world = await setUpCaseWorld();
    const opened = await openCase(world, deepDivergentTraces(2));
    const defended = await defend(world, opened, await registerDefender(world));
    passOperatorDeadline(world, defended.thread);
    passDeadline(world, defenseDeadline(Number(opened.datum.started_at)));
    await expectOnchainRefusal(() => buildCloseCase(world, defended.opened));
  });
});
