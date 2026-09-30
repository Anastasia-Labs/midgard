import "./terminal-retention-observation.terminal-retention-outcomes-v1.js";

import { describe, expect, it } from "vitest";

import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import { catchUpRetentionOutcomes } from "../src/l1/terminal-retention-observation.js";
import {
  config,
  h28,
  merge,
  outRef,
  record,
  thrownBy,
} from "./terminal-retention-observation.derive.js";

describe("catchUpRetentionOutcomes", () => {
  const twoHeaders = () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    return { first, second, initial };
  };

  it("takes steps, outcomes, and the anchor only from the final prefix of the walk", () => {
    const { first, second, initial } = twoHeaders();
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 29);
    const result = catchUpRetentionOutcomes(
      [first, second],
      [one, two],
      config(initial),
    );
    expect([...result!.finalSteps.keys()]).toEqual([first.headerHash]);
    expect(result!.terminalStatuses).toEqual(
      new Map([[first.headerHash, "merged"]]),
    );
    expect(
      result!.terminalRecords.map(({ headerHash, status }) => [
        headerHash,
        status,
      ]),
    ).toEqual([[first.headerHash, "merged"]]);
    expect(result!.finalAnchor).toEqual({
      queue: one.nextQueue,
      blockNo: one.blockNo,
      transactionIndex: one.transactionIndex,
    });
  });

  it("makes no progress on a walk none of whose checkpoints is final", () => {
    const { first, second, initial } = twoHeaders();
    expect(
      catchUpRetentionOutcomes(
        [first, second],
        [merge(1, initial, 29)],
        config(initial),
      ),
    ).toBeUndefined();
  });

  it("refuses an anchor of another release as an integrity failure", () => {
    const { first, second, initial } = twoHeaders();
    const base = config(initial);
    const failure = thrownBy(() =>
      catchUpRetentionOutcomes([first, second], [merge(1, initial)], {
        ...base,
        replayAnchor: {
          ...base.replayAnchor,
          deploymentIdentityDigest: "cc".repeat(32),
        },
      }),
    );
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
    expect((failure as Error).message).toBe(
      "state-queue durable replay anchor release mismatch",
    );
  });

  it("refuses a stored header of another deployment as an integrity failure", () => {
    const { first, second, initial } = twoHeaders();
    const failure = thrownBy(() =>
      catchUpRetentionOutcomes(
        [first, { ...second, deploymentFingerprint: "cc".repeat(32) }],
        [merge(1, initial)],
        config(initial),
      ),
    );
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
    expect((failure as Error).message).toBe(
      `stored state-queue header ${second.headerHash} belongs to a foreign deployment`,
    );
  });
});
