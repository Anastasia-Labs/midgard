import { publicReadinessPresentation } from "midgard-node/commands/listen-readiness-public-presentation";
import { afterEach, describe, expect, it } from "vitest";

import { JOURNEY_HEADER_END_AHEAD_MS } from "./fixture.js";
import {
  assertNoHonestHeaderOnFault,
  classifyNodeFaultHold,
  planLateShiftFaultCommit,
} from "./honest-successor.observables.js";
import {
  closeServers,
  faultHold,
  headerOn,
  honestOperator,
  type IndexedOutput,
  kupo,
  operator2,
  policyId,
} from "./honest-successor.test-support.js";

afterEach(closeServers);

describe("our node's hold on the fault", () => {
  it("binds the hold to the fault by its header hash where the node names it", async () => {
    const fault = await headerOn("00".repeat(28), operator2, 1n);
    expect(
      classifyNodeFaultHold(faultHold(fault.headerHash), fault.headerHash),
    ).toMatchObject({ state: "held", binding: "header" });
    const other = await headerOn("00".repeat(28), operator2, 2n);
    expect(() =>
      classifyNodeFaultHold(faultHold(other.headerHash), fault.headerHash),
    ).toThrow(/holds landed_block_invalid on block .* not on the fault/u);
  });

  it("falls back to the reason alone under a production profile's redaction", async () => {
    const fault = await headerOn("00".repeat(28), operator2, 1n);
    const other = await headerOn("00".repeat(28), operator2, 2n);
    const redacted = publicReadinessPresentation(
      faultHold(other.headerHash),
      "preprod-rehearsal",
      {},
    );
    expect(classifyNodeFaultHold(redacted, fault.headerHash)).toMatchObject({
      state: "held",
      binding: "reason_only",
    });
  });

  it("tells a missing payload and a cleared hold apart from the hold", async () => {
    const fault = await headerOn("00".repeat(28), operator2, 1n);
    expect(
      classifyNodeFaultHold(
        { ready: false, reasons: ["landed_block_awaiting_da"] },
        fault.headerHash,
      ).state,
    ).toBe("awaiting_da");
    expect(
      classifyNodeFaultHold({ ready: true, reasons: [] }, fault.headerHash)
        .state,
    ).toBe("not_held");
    expect(() =>
      classifyNodeFaultHold({ ready: false }, fault.headerHash),
    ).toThrow(/no readiness reasons/u);
  });
});

describe("no header of ours on the fault", () => {
  it("reads every header created since the fault, spent or not", async () => {
    const predecessor = await headerOn("00".repeat(28), honestOperator, 1n);
    const fault = await headerOn(predecessor.headerHash, operator2, 2n);
    const ours = await headerOn(predecessor.headerHash, honestOperator, 3n);
    const before = await headerOn("00".repeat(28), honestOperator, 4n);
    const outputs: IndexedOutput[] = [
      { block: before, slot: 5 },
      { block: fault, slot: 10 },
      { block: ours, slot: 20 },
    ];
    const kupoUrl = await kupo(() => outputs);
    const input = {
      kupoUrl,
      policyId,
      faultHeaderHash: fault.headerHash,
      honestOperatorKeyHash: honestOperator,
    };
    await expect(assertNoHonestHeaderOnFault(input)).resolves.toEqual({
      faultCommittedAtSlot: 10,
      headersRead: 1,
    });
    // Operator-2's own header on the fault is not our node's.
    outputs.push({
      block: await headerOn(fault.headerHash, operator2, 5n),
      slot: 30,
    });
    await expect(assertNoHonestHeaderOnFault(input)).resolves.toMatchObject({
      headersRead: 2,
    });
    outputs.push({
      block: await headerOn(fault.headerHash, honestOperator, 6n),
      slot: 40,
    });
    await expect(assertNoHonestHeaderOnFault(input)).rejects.toThrow(
      /Our node built header .* on the fault/u,
    );
  });

  it("refuses a fault Kupo never indexed", async () => {
    const fault = await headerOn("00".repeat(28), operator2, 2n);
    const kupoUrl = await kupo(() => []);
    await expect(
      assertNoHonestHeaderOnFault({
        kupoUrl,
        policyId,
        faultHeaderHash: fault.headerHash,
        honestOperatorKeyHash: honestOperator,
      }),
    ).rejects.toThrow(/indexes no state-queue output of the fault/u);
  });
});

describe("late-shift fault commit", () => {
  // A commit's allowance plus the time the fault's header runs past its
  // build: the shortest lead.
  const lead = 1_000 + JOURNEY_HEADER_END_AHEAD_MS + 1;
  it("commits in the last lead of operator-2's shift, before it ends", () => {
    expect(
      planLateShiftFaultCommit({
        shift: { startMs: 1_000, endMs: 100_000 },
        leadMs: lead,
        commitAllowanceMs: 1_000,
      }),
    ).toEqual({ notBeforeMs: 100_000 - lead, notAfterMs: 99_000 });
    // A shift shorter than the lead starts the window at the shift.
    expect(
      planLateShiftFaultCommit({
        shift: { startMs: 80_000, endMs: 100_000 },
        leadMs: lead,
        commitAllowanceMs: 1_000,
      }),
    ).toEqual({ notBeforeMs: 80_000, notAfterMs: 99_000 });
  });

  it("refuses a window the commit cannot land in", () => {
    for (const input of [
      {
        shift: { startMs: 0, endMs: 100_000 },
        leadMs: 1_000,
        commitAllowanceMs: 1_000,
      },
      // The fault's header would end after operator-2's shift.
      {
        shift: { startMs: 0, endMs: 100_000 },
        leadMs: lead - 1,
        commitAllowanceMs: 1_000,
      },
      {
        shift: { startMs: 99_500, endMs: 100_000 },
        leadMs: lead,
        commitAllowanceMs: 1_000,
      },
      {
        shift: { startMs: 0, endMs: 100_000 },
        leadMs: Number.NaN,
        commitAllowanceMs: 1_000,
      },
    ])
      expect(() => planLateShiftFaultCommit(input)).toThrow(
        /Late-shift|too short/u,
      );
  });
});
