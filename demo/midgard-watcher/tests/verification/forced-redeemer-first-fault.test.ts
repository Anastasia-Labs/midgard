import { RejectCodes } from "@al-ft/midgard-validation";
import { Exit } from "effect";
import { Columns } from "midgard-node/database/forcedTransactions.exact-forced-transaction-journal-member";
import { expect, it } from "vitest";

import { journey } from "./forced-redeemer-first-fault.journey.js";

it.each([
  { data: "d8798101", missing: false },
  { data: "d8798101", missing: true },
  { data: "d8799f8101ff", missing: false },
  { data: "d8799f8101ff", missing: true },
  { data: "d8799f01810102ff", missing: false },
  { data: "d8799f01810102ff", missing: true },
  { data: "d8799fa20101810102ff", missing: false },
  { data: "d8799fa20101810102ff", missing: true },
])(
  "node, watcher and trace cite exact Data refusal $data before missing=$missing script",
  async ({ data, missing }) => {
    const j = await journey(data, false, missing);
    expect(j.leaf.verdict).toEqual({
      ForcedTxInvalid: {
        reason: { RedeemerMalformed: { redeemer_index: 0n } },
      },
    });
    expect(j.classified!.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    expect(j.classified!.entry[Columns.NATIVE_TX_CBOR]).toEqual(j.txCbor);
    expect(j.classified!.ledgerOps).toEqual([]);
    expect(j.classified!.transitionEffect.operations).toEqual([]);
    expect(j.watcher!.fact.canonicalOperatorValidity).toBe("RedeemerMalformed");
    expect(j.watcher!.effect.operations).toEqual([]);
    expect(Exit.isSuccess(j.replay)).toBe(true);
    if (Exit.isSuccess(j.replay)) {
      expect(j.replay.value.trace.verdict).toBe("rejected");
      expect(j.replay.value.statePatch).toEqual({
        deletedOutRefs: [],
        upsertedOutRefs: [],
      });
      expect(j.replay.value.trace.witnesses.at(-2)!.auxiliary).toMatchObject({
        kind: "redeemerItemStep",
        control: {
          itemIndex: 0,
          traversal: {
            offset:
              data === "d8798101"
                ? 0
                : data === "d8799f8101ff"
                  ? 3
                  : data === "d8799f01810102ff"
                    ? 4
                    : 6,
          },
        },
        witness: { action: { kind: "traverseData", action: null } },
      });
    }
  },
);
it.each([
  "d8799f01ff",
  "d8799f9f01ffff",
  "d8799f019f01ff02ff",
  "d8799fa201019f01ff02ff",
])(
  "canonical node/watcher/trace accepts %s without normalization",
  async (data) => {
    const j = await journey(data, false);
    expect(j.leaf.verdict).toBe("ForcedTxValid");
    expect(j.classified!.entry[Columns.NATIVE_TX_CBOR]).toEqual(j.txCbor);
    expect(j.watcher!.fact.canonicalOperatorValidity).toBe("ForcedTxValid");
    expect(j.classified!.ledgerOps).toHaveLength(2);
    expect(Exit.isSuccess(j.replay)).toBe(true);
    if (Exit.isSuccess(j.replay))
      expect(j.replay.value.trace.verdict).toBe("accepted");
  },
);
it("canonical nested Data with missing source cites ScriptSourceMissing consistently", async () => {
  const j = await journey("d8799f9f01ffff", false, true);
  expect(j.leaf.verdict).toEqual({
    ForcedTxInvalid: {
      reason: { ScriptSourceMissing: { purpose_kind: 0n, purpose_index: 0n } },
    },
  });
  expect(j.watcher!.fact.canonicalOperatorValidity).toBe("ScriptSourceMissing");
  expect(Exit.isSuccess(j.replay)).toBe(true);
});
it("nested refusal after canonical earlier redeemer names ordinal1 before the duplicate-pointer audit", async () => {
  const j = await journey("d8799f8101ff", true, true);
  expect(j.leaf.verdict).toEqual({
    ForcedTxInvalid: { reason: { RedeemerMalformed: { redeemer_index: 1n } } },
  });
  expect(j.watcher!.fact.canonicalOperatorValidity).toBe("RedeemerMalformed");
  expect(Exit.isSuccess(j.replay)).toBe(true);
  if (Exit.isSuccess(j.replay))
    expect(j.replay.value.trace.witnesses.at(-2)!.auxiliary).toMatchObject({
      kind: "redeemerItemStep",
      control: { itemIndex: 1, traversal: { offset: 3 } },
    });
});

it.each(
  [
    { data: "1801", honest: "01", offset: 0 },
    { data: "d8799f1801ff", honest: "d8799f01ff", offset: 3 },
    { data: "d8799f011801ff", honest: "d8799f0101ff", offset: 4 },
    { data: "bf0101ff", honest: "a10101", offset: 0 },
    { data: "d8668218808101", honest: "d8668218809f01ff", offset: 5 },
  ].flatMap((vector) =>
    [false, true].map((missing) => ({ ...vector, missing })),
  ),
)(
  "normal intake rejects and forced node, watcher, replay and retention close malformed $data missing=$missing",
  async ({ data, offset, missing }) => {
    const j = await journey(data, false, missing);
    expect(j.leaf.verdict).toEqual({
      ForcedTxInvalid: {
        reason: { RedeemerMalformed: { redeemer_index: 0n } },
      },
    });
    expect(j.classified!.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    expect(j.classified!.ledgerOps).toEqual([]);
    expect(j.watcher!.fact.canonicalOperatorValidity).toBe("RedeemerMalformed");
    expect(j.watcher!.effect.operations).toEqual([]);
    expect(j.normal).toMatchObject({
      rejected: [
        {
          code: RejectCodes.InvalidFieldType,
          consensusPhase: "canonicalDecode",
        },
      ],
      statePatch: { deletedOutRefs: [], upsertedOutRefs: [] },
    });
    expect(j.classified!.entry[Columns.NATIVE_TX_CBOR]).toEqual(j.txCbor);
    for (const replay of [j.replay]) {
      expect(Exit.isSuccess(replay)).toBe(true);
      if (Exit.isFailure(replay))
        throw new Error("mandatory malformed Data trace refused");
      expect(replay.value.trace.verdict).toBe("rejected");
      expect(replay.value.statePatch).toEqual({
        deletedOutRefs: [],
        upsertedOutRefs: [],
      });
      expect(replay.value.trace.witnesses.at(-2)!.auxiliary).toMatchObject({
        kind: "redeemerItemStep",
        control: { itemIndex: 0, traversal: { offset } },
        witness: { action: { kind: "traverseData", action: null } },
      });
    }
    expect(j.normalReplay).toBeNull();
    expect(j.retained).toHaveLength(1);
    expect(
      j.retained.every((member) => member.value.verdict === "Rejected"),
    ).toBe(true);
  },
);
it.each(["01", "d8799f01ff", "d8799f0101ff", "a10101", "d8668218809f01ff"])(
  "normal and forced canonical counterpart %s survives replay and retention",
  async (data) => {
    const j = await journey(data, false);
    expect(j.leaf.verdict).toBe("ForcedTxValid");
    expect(j.watcher!.fact.canonicalOperatorValidity).toBe("ForcedTxValid");
    expect(j.normal.rejected).toEqual([]);
    expect(j.normalReplay).not.toBeNull();
    expect(j.normalReplay !== null && Exit.isSuccess(j.normalReplay)).toBe(
      true,
    );
    expect(j.retained).toHaveLength(2);
    expect(
      j.retained.every((member) => member.value.verdict === "Accepted"),
    ).toBe(true);
  },
);
