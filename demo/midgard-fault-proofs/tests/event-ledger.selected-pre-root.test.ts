/**
 * A trace-relative family reads the ledger before its selected event from
 * the shared walk, which refuses that event when its committed pre-state is
 * not the ledger the earlier steps leave. The family never receives a ledger
 * whose root differs from its event's committed `pre_utxos_root`, so it needs
 * no comparison of its own: for each family a block whose selected event
 * commits another pre-state meets the walk's refusal. The walk compares the
 * reconstruction's own opening, so even a reconstruction no evidence check
 * admitted cannot reach past it.
 */
import { encodeMidgardMintPolicyItem } from "@al-ft/midgard-core/codec";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { prepareDirectScriptMaterial } from "../src/direct-script-derivation/material.js";
import { ledgerEntriesRoot } from "../src/ledger-at.js";
import { prepareMintAuthorizationReplay } from "../src/mint-authorization/replay.js";
import type { TransitionTraceReconstruction } from "../src/transition-trace/reconstruct.js";
import { prepareWithdrawalMistagReplay } from "../src/withdrawal-mistag/replay.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import { checkedLedgerAfter } from "./support/predecessor-ledger.js";
import { withdrawalMistagRetainedFixture } from "./support/withdrawal-mistag-retained.js";

const REFUSED = "event ledger: event prior ledger changed";

/** Spends `OTHER` and mints one unit of a policy at step 1, whose committed
 * pre-state is the predecessor's, not the ledger step 0 leaves. */
const mintingAfterMovedPreState = async () =>
  await block([
    {
      input: { spendInputs: [SPENT.outRef], fee: 7n },
      applied: true,
    },
    {
      input: {
        spendInputs: [OTHER.outRef],
        outputs: [makeOutput(2_999_993n)],
        mintPolicyItems: [
          encodeMidgardMintPolicyItem({
            policyId: Buffer.alloc(28, 0x11),
            assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
          }),
        ],
        fee: 7n,
      },
      applied: true,
      pre: await ledgerEntriesRoot([NATIVE, SPENT, OTHER]),
    },
  ]);

describe("the shared walk refuses a selected event's moved pre-state before its family reads it", () => {
  it("for direct script derivation", async () => {
    const fixture = await mintingAfterMovedPreState();
    await expect(
      prepareDirectScriptMaterial({
        current: fixture.evidence.reconstruction,
        predecessor: completeCanonicalReplayPredecessorLedger(fixture)!,
        eventKey: fixture.key(1),
      }),
    ).rejects.toThrow(REFUSED);
  });

  it("for mint authorization", async () => {
    const fixture = await mintingAfterMovedPreState();
    await expect(
      prepareMintAuthorizationReplay({
        current: fixture.evidence.reconstruction,
        predecessor: completeCanonicalReplayPredecessorLedger(fixture)!,
        sourceIndex: Number(fixture.position(1)),
      }),
    ).rejects.toThrow(REFUSED);
  });

  it("for withdrawal mistag", async () => {
    const fixture = await withdrawalMistagRetainedFixture({
      direction: "valid-marked-invalid",
    });
    const current = fixture.evidence.reconstruction;
    const opening = current.traceByStepIndex.get(0n)!;
    const moved: TransitionTraceReconstruction = {
      ...current,
      traceByStepIndex: new Map([
        [
          0n,
          {
            ...opening,
            value: { ...opening.value, pre_utxos_root: "ee".repeat(32) },
          },
        ],
      ]),
    };
    await expect(
      prepareWithdrawalMistagReplay({
        current: moved,
        predecessor: await checkedLedgerAfter(fixture.predecessor),
        index: 0,
      }),
    ).rejects.toThrow(REFUSED);
  });
});
