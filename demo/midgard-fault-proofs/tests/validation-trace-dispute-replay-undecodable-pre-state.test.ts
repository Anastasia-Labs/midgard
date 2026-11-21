/** A Plutus re-execution over a pre-state that also holds an output the
 * event never touches, whose native script_ref does not decode: the ledger
 * admits that output with a descriptor derived from its bytes, and the replay
 * proves the failed script run exactly as it does without the output, rather
 * than stopping as unavailable and holding the event. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import { checkLedgerAgainstRoot, ledgerEntriesRoot } from "../src/ledger-at.js";
import { replayPlutusExecution } from "../src/validation-trace-dispute/replay.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./support/validation-trace-claim-material.js";

const unrelated = {
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x55),
    outputIndex: 0,
  }),
  // An output whose native script_ref body, `81 09`, does not decode.
  output: Buffer.from(
    "a300581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0038200428109",
    "hex",
  ),
};

describe("validation-trace dispute replay: an undecodable native script_ref elsewhere in the pre-state", () => {
  it("proves the failed script run as it does without that output", async () => {
    const block = await plutusExecutionBlockFixture("failing");
    const { event, ledger } = await eventAndLedger(block);
    const entries = [...ledger.entries, unrelated];
    const withUndecodable = await checkLedgerAgainstRoot({
      entries,
      utxosRoot: await ledgerEntriesRoot(entries),
      label: "pre-state with an undecodable native script_ref",
    });
    expect(
      withUndecodable.entries.find(({ outRef }) =>
        outRef.equals(unrelated.outRef),
      )?.descriptor,
    ).toBeInstanceOf(Buffer);
    const replay = (checked: typeof ledger) =>
      replayPlutusExecution({
        evidence: block.evidence,
        event,
        ledger: checked,
        l1SlotConfig,
      });
    const expected = await replay(ledger);
    expect(expected.kind).toBe("fails");
    expect(await replay(withUndecodable)).toStrictEqual(expected);
  }, 600_000);
});
