import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  deepRows,
  shallowSpend,
} from "@al-ft/midgard-validation/tests/phase-b.deep-data.fixture";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { beforeAll, describe, expect, it } from "vitest";

import { evaluateWatcherBlockReplay } from "../../src/index.js";
import {
  buildPublicReplayFixture,
  committedStepsForEffects,
  entries,
  nativeEffect,
  publicInput,
} from "../support/block-replay-public-fixture.js";

/**
 * The watcher replays a committed block whose one transaction spends a
 * script output with Data nested thousands of levels deep, up to the deepest
 * a transaction can carry: Phase B on the watcher's own thread reaches the
 * script's verdict and the block is accepted, and the next block, with a
 * shallow spend, is accepted too.
 */

beforeAll(() => ledgerEd25519SignatureReady());

type Spend = ReturnType<typeof shallowSpend>;

/** Replays one committed block holding `spend`, accepted by its script. */
const replayAccepted = async (spend: Spend) => {
  const { candidate } = spend;
  const native = {
    txId: candidate.ledgerTx.txId,
    txCbor: candidate.submission.txCbor,
  };
  const [spentEntry] = [...spend.preState.entries()];
  if (spentEntry === undefined) throw new Error("spend without a spent output");
  const [spentHex, spentOutput] = spentEntry;
  const spent = Buffer.from(spentHex, "hex");
  // Every spend in the fixture pays one funded output.
  const outputs = [makeOutput(FUNDED_OUTPUT_LOVELACE)];
  const priorState = entries([[spent, spentOutput]]);
  const effect = nativeEffect({ spent: [spent], native, outputs });
  const steps = await committedStepsForEffects(priorState, [
    {
      eventKey: {
        L2TransactionEventKey: { tx_id: native.txId.toString("hex") },
      },
      phase: "L2Transaction",
      effect,
    },
  ]);
  const fixture = await buildPublicReplayFixture({
    txCbors: [native.txCbor],
    steps,
    priorState,
    postState: entries(
      outputs.map((output, index) => [
        outRefFromTxId(native.txId, BigInt(index)),
        output,
      ]),
    ),
  });
  const result = await evaluateWatcherBlockReplay(publicInput(fixture));
  expect(result.reasonCodes).toStrictEqual([]);
  expect(result.action).toBe("accept");
};

describe("watcher replay of a committed block with deeply nested script Data", () => {
  it.each(deepRows())(
    "$label is accepted, and so is a shallow spend in the next block",
    async ({ accepting }) => {
      await replayAccepted(accepting(0x41));
      await replayAccepted(shallowSpend(0x42));
    },
    120_000,
  );
});
