import "./utils.js";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
import {
  Columns as LedgerColumns,
  type MinimalEntry,
} from "../src/database/utils/ledger.js";
import { commitDaSourceContributions } from "../src/workers/commit-block-header.plan-additive-da.js";
import { verifyAdditiveCommitDaFrame } from "../src/workers/commit-block-header.verify-additive-da.js";
import { selectAdditiveCommitFrame } from "../src/workers/utils/commit-block-planner.additive-frame.js";
import {
  delayProgram,
  openNativeFrameFixture,
  programScript,
  runNativeFrameDatabase,
} from "./helpers/commit-additive-native-fixture.js";

/** This drives real signatures, production Phase A/B and native MPF. The
 * ConfirmedState parent is a model authority; no L1 deployment receipt is
 * claimed by these boundary controls. */
it("keeps a canonical supplier reachable through native building and the actual-header rescue guard", async () => {
  const h = await openNativeFrameFixture();
  try {
    const program = delayProgram(32);
    const outputs = [
      h.output(2_000_000n, 0, programScript(program)),
      h.output(49_998_000_000n),
    ];
    const supplier = h.signed(h.inputRef, [], outputs);
    await h.validate(supplier, h.initial, program.material);
    const built = await h.build(supplier, h.initial, h.root, program.material);
    expect(built.processed.processedMempoolTxs).toHaveLength(1);
    expect(built.builds).toBe(1);
    expect(await h.exact(built.processed, h.root)).toBeLessThanOrEqual(
      maxDaPayloadInnerBytes(readDaHardeningConfig().envelopeMode),
    );
    await h.native.discard(built.handle);
  } finally {
    await h.close();
  }
}, 180_000);

it("rescues one real ordinary consumer whose repeated verified program references overestimate the frame", async () => {
  const h = await openNativeFrameFixture();
  try {
    const program = delayProgram(1_200);
    const references: Buffer[] = [];
    let base: readonly MinimalEntry[] = h.initial;
    let spend = h.inputRef;
    let root = h.root;
    let balance = 50_000_000_000n;
    // Each supplier is independently signed, admitted by Phase A/B, and built
    // by the real native owner. Their model parent remains explicit. Each source
    // is below the actual canonical field cap; the shared graph is small.
    for (let round = 0; round < 4; round += 1) {
      balance -= 195n * 2_000_000n;
      const outputs = [
        ...Array.from({ length: 195 }, () =>
          h.output(2_000_000n, 0, programScript(program)),
        ),
        h.output(balance),
      ];
      const supplier = h.signed(spend, [], outputs);
      await h.validate(supplier, base, program.material);
      const supplied = await h.build(supplier, base, root, program.material);
      expect(supplied.processed.processedMempoolTxs).toHaveLength(1);
      expect(supplied.builds).toBe(1);
      await h.exact(supplied.processed, root);
      await h.native.promote(supplied.handle);
      const created = outputs.map((output, outputIndex) => ({
        [LedgerColumns.OUTREF]: encodeMidgardSpendInputItem({
          txId: supplier.txId,
          outputIndex,
        }),
        [LedgerColumns.OUTPUT]: output,
      }));
      references.push(
        ...created.slice(0, 195).map((row) => row[LedgerColumns.OUTREF]),
      );
      base = [
        ...base.filter((row) => !row[LedgerColumns.OUTREF].equals(spend)),
        ...created,
      ];
      spend = created[195]![LedgerColumns.OUTREF];
      root = supplied.processed.utxoRoot;
    }
    const consumer = h.signed(spend, references, [h.output(balance)]);
    await h.validate(consumer, base, program.material);
    const maxInnerBytes = maxDaPayloadInnerBytes(
      readDaHardeningConfig().envelopeMode,
    );
    const contributions = commitDaSourceContributions({
      base,
      deposits: [],
      withdrawals: [],
      forced: [],
      candidates: [h.entry(consumer)],
      profile: MIDGARD_CONSENSUS_PROFILE,
    });
    const plan = selectAdditiveCommitFrame({
      ...contributions,
      maxInnerBytes,
      initialWindow: {
        mode: "normal",
        endTimeMs: h.time,
        includedInstantCount: 0,
        counts: { deposits: 0, withdrawals: 0, forcedTransactions: 0 },
      },
    });
    expect(plan).toMatchObject({
      kind: "selected",
      candidateCount: 1,
      rescue: "ordinary",
    });
    if (plan.kind !== "selected")
      throw new Error("real consumer was not selected");
    expect(plan.upperBoundBytes).toBeGreaterThan(BigInt(maxInnerBytes));
    const built = await h.build(consumer, base, root, program.material);
    expect(built.processed.processedMempoolTxs).toHaveLength(1);
    expect(built.builds).toBe(1);
    const notices: unknown[] = [];
    await runNativeFrameDatabase(
      verifyAdditiveCommitDaFrame({
        processed: built.processed,
        baseRoot: root,
        profile: MIDGARD_CONSENSUS_PROFILE,
        nodeConfig: { NETWORK: "Preprod", MIN_FEE_A: 0n, MIN_FEE_B: 0n },
        additivePlan: plan,
        latestBlockForSchedulerPlanning: h.parent(root),
        lucid: h.lucid,
        fixedHistoryEndTime: new Date(h.time),
        blockTimeMs: h.time,
        proofValidationSlotConfig: h.slotConfig,
        daFrameInnerLimit: maxInnerBytes,
        notifyParent: (notice) =>
          Effect.sync(() => {
            notices.push(notice);
          }),
      }),
    );
    expect(notices).toEqual([
      expect.objectContaining({
        type: "CommitDaFrameNotice",
        status: "fits",
      }),
    ]);
    expect(await h.exact(built.processed, root)).toBeLessThanOrEqual(
      maxInnerBytes,
    );
    await h.native.discard(built.handle);
  } finally {
    await h.close();
  }
}, 180_000);
