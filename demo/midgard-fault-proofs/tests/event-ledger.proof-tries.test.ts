/**
 * The block's walk keeps the membership tries of the first
 * `RETAINED_PROOF_TRIES` steps any reader proves against, for the walk's
 * life, and no others: a later step's ledger builds its trie for that ledger
 * alone, once however many keys it proves, and a later read of that step
 * builds it again. Work is counted as in `helpers/trie-leaves.ts`, in
 * ledger-sized tries.
 */
import { describe, expect, it } from "vitest";

import { eventLedgerBefore } from "../src/event-ledger.js";
import { RETAINED_PROOF_TRIES } from "../src/event-ledger.view.js";
import { checkedLedgerPreState } from "../src/ledger-at.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";
import { filler, trieLeaves } from "./helpers/trie-leaves.js";

const STEPS = RETAINED_PROOF_TRIES + 2;
const FILLER = filler(64);
/** An entry every ledger of the block holds. */
const HELD = FILLER[0]!.outRef;

describe("the block's walk keeps the first proved steps' tries", () => {
  it("keeps exactly the first four, and builds a later step's trie per read", async () => {
    const block = await plutusExecutionEventsBlockFixture(
      Array.from({ length: STEPS }, () => "passing" as const),
      { predecessorFiller: FILLER },
    );
    const predecessor = completeCanonicalReplayPredecessorLedger(block)!;
    /** Ledger-sized tries built reading step `index` and proving two keys. */
    const triesAt = async (index: number) => {
      const ledger = await eventLedgerBefore({
        block: block.evidence,
        eventKey: { L2TransactionEventKey: { tx_id: block.txs[index]!.txId } },
        stepIndex: BigInt(index),
        predecessor,
      });
      const proof = checkedLedgerPreState(ledger);
      const { leaves } = await trieLeaves(async () => {
        await proof.proveResolution([HELD]);
        await proof.proveResolution([HELD]);
      });
      return leaves / Number(ledger.trie.count);
    };
    const pass = async () => {
      const tries: number[] = [];
      for (let index = 0; index < STEPS; index += 1)
        tries.push(await triesAt(index));
      return tries;
    };
    expect(await pass()).toStrictEqual([1, 1, 1, 1, 1, 1]);
    expect(await pass()).toStrictEqual([0, 0, 0, 0, 1, 1]);
  });
});
