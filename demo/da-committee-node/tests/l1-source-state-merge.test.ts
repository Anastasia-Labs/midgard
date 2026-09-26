import { describe, expect, it } from "vitest";

import {
  type L1ObservedDecision,
  type L1SourceState,
  mergeL1SourceState,
  parseL1SourceState,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "../src/store.js";

const anchor = {
  deploymentIdentityDigest: "aa".repeat(32),
  stateQueuePolicyId: "bb".repeat(28),
  queue: [{ headerHash: null, outRef: `${"00".repeat(32)}#0` }],
  blockNo: "90",
  transactionIndex: "0",
};
const decided: L1ObservedDecision = {
  headerHash: "11".repeat(28),
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  stateQueueStatus: "unattested",
  slot: 100,
  blockHash: "22".repeat(32),
  finalized: true,
  hasPersistedDecision: true,
};
const state = (
  observations: readonly L1ObservedDecision[],
  replayAnchor?: typeof anchor,
): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "cc".repeat(32),
  status: "healthy",
  observations,
  observedAt: "2026-09-25T00:00:00.000Z",
  ...(replayAnchor === undefined
    ? {}
    : { stateQueueReplayAnchor: replayAnchor }),
});

describe("L1 source state merge", () => {
  it("keeps the recorded replay anchor when a healthy write carries none", () => {
    const merged = mergeL1SourceState(state([], anchor), state([decided]));
    expect(merged.stateQueueReplayAnchor).toEqual(anchor);

    const advanced = { ...anchor, blockNo: "95" };
    expect(
      mergeL1SourceState(state([], anchor), state([], advanced))
        .stateQueueReplayAnchor,
    ).toEqual(advanced);
  });

  const outRefB = `${"12".repeat(32)}#0`;
  const outRefC = `${"13".repeat(32)}#1`;
  const at = (slot: number, byte: string) => ({
    slot,
    blockHash: byte.repeat(32),
  });
  /** `decided`, later observed at `outRef` after `steps`. */
  const moved = (
    outRef: string,
    steps: NonNullable<L1ObservedDecision["authenticatedSteps"]>,
    change: Partial<L1ObservedDecision> = {},
  ): L1ObservedDecision => ({
    ...decided,
    stateQueueOutRef: outRef,
    ...at(steps.at(-1)!.slot, steps.at(-1)!.blockHash.slice(0, 2)),
    hasPersistedDecision: false,
    authenticatedSteps: steps,
    ...change,
  });
  const append = { fromOutRef: decided.stateQueueOutRef, toOutRef: outRefB };
  const explained: readonly (readonly [string, L1ObservedDecision])[] = [
    [
      "an append continuing its output",
      moved(outRefB, [{ ...append, ...at(120, "33") }]),
    ],
    [
      "a datum update attesting it",
      moved(outRefB, [{ ...append, ...at(120, "33") }], {
        stateQueueStatus: "attested",
      }),
    ],
    [
      "several steps, some before the recorded output",
      moved(outRefC, [
        { fromOutRef: `${"10".repeat(32)}#0`, ...at(80, "30") },
        { ...append, ...at(120, "33") },
        { fromOutRef: outRefB, toOutRef: outRefC, ...at(130, "34") },
      ]),
    ],
    [
      "a merge of its output",
      moved(
        decided.stateQueueOutRef,
        [{ fromOutRef: decided.stateQueueOutRef, ...at(140, "35") }],
        { stateQueueStatus: "merged" },
      ),
    ],
    [
      "a move then a removal",
      moved(
        outRefB,
        [
          { ...append, ...at(120, "33") },
          { fromOutRef: outRefB, ...at(140, "35") },
        ],
        { stateQueueStatus: "removed" },
      ),
    ],
  ];

  it.each(explained)(
    "accepts a final change explained by authenticated replay: %s",
    (_, next) => {
      const merged = mergeL1SourceState(state([decided]), state([next]));
      expect(merged.observations).toEqual([
        { ...next, hasPersistedDecision: true },
      ]);
    },
  );

  const unexplained: readonly (readonly [string, L1ObservedDecision])[] = [
    [
      "explained but not final",
      moved(outRefB, [{ ...append, ...at(120, "33") }], { finalized: false }),
    ],
    [
      "without any authenticated step",
      { ...decided, stateQueueOutRef: outRefB, ...at(120, "33") },
    ],
    [
      "with steps that do not start at the recorded output",
      moved(outRefC, [
        { fromOutRef: outRefB, toOutRef: outRefC, ...at(130, "34") },
      ]),
    ],
    [
      "with steps that do not reach the observed output",
      moved(outRefC, [{ ...append, ...at(120, "33") }], at(120, "33")),
    ],
    [
      "with a broken chain of steps",
      moved(outRefC, [
        { ...append, ...at(120, "33") },
        {
          fromOutRef: `${"14".repeat(32)}#0`,
          toOutRef: outRefC,
          ...at(130, "34"),
        },
      ]),
    ],
    [
      "observed somewhere other than the last step",
      moved(outRefB, [{ ...append, ...at(120, "33") }], at(125, "36")),
    ],
    [
      "taking the status backwards",
      moved(outRefB, [{ ...append, ...at(120, "33") }], {
        stateQueueStatus: "conflicted",
      }),
    ],
    [
      "terminal without a step taking it out of the queue",
      moved(outRefB, [{ ...append, ...at(120, "33") }], {
        stateQueueStatus: "merged",
      }),
    ],
    [
      "still queued after a step took it out of the queue",
      moved(decided.stateQueueOutRef, [
        { fromOutRef: decided.stateQueueOutRef, ...at(140, "35") },
      ]),
    ],
    [
      "whose last step precedes the recorded observation",
      moved(outRefB, [{ ...append, ...at(90, "33") }]),
    ],
    [
      "a terminal outcome on another output",
      moved(
        outRefB,
        [{ fromOutRef: decided.stateQueueOutRef, ...at(140, "35") }],
        { stateQueueStatus: "merged" },
      ),
    ],
  ];

  it.each(unexplained)(
    "rejects a change of a persisted decision %s",
    (_, next) => {
      expect(() => mergeL1SourceState(state([decided]), state([next]))).toThrow(
        /persisted L1 decision changed canonical output/u,
      );
    },
  );

  it("rejects any move of a persisted conflicted decision, however explained", () => {
    const conflicted = { ...decided, stateQueueStatus: "conflicted" as const };
    expect(() =>
      mergeL1SourceState(
        state([conflicted]),
        state([
          moved(outRefB, [{ ...append, ...at(120, "33") }], {
            stateQueueStatus: "conflicted",
          }),
        ]),
      ),
    ).toThrow(/persisted L1 decision changed canonical output/u);
  });

  it("rejects any change of a persisted terminal outcome", () => {
    const merged = { ...decided, stateQueueStatus: "merged" as const };
    expect(() =>
      mergeL1SourceState(
        state([merged]),
        state([
          moved(outRefB, [{ ...append, ...at(120, "33") }], {
            stateQueueStatus: "merged",
          }),
        ]),
      ),
    ).toThrow(/persisted L1 decision changed canonical output/u);
  });

  describe("an unknown status", () => {
    const unknown: L1ObservedDecision = {
      ...decided,
      stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
      lastKnownStatus: "unattested",
    };
    const unknownIn = (
      lastKnownStatus: NonNullable<L1ObservedDecision["lastKnownStatus"]>,
    ): L1SourceState => state([{ ...unknown, lastKnownStatus }]);

    it("is recorded by a final move whose output's datum was not seen", () => {
      const next = moved(outRefB, [{ ...append, ...at(120, "33") }], {
        stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
        lastKnownStatus: "unattested",
      });
      expect(
        mergeL1SourceState(state([decided]), state([next])).observations,
      ).toEqual([{ ...next, hasPersistedDecision: true }]);
    });

    it.each([
      [
        "the same output",
        { ...decided, stateQueueStatus: "attested" as const },
      ],
      [
        "an explained move",
        moved(outRefB, [{ ...append, ...at(120, "33") }], {
          stateQueueStatus: "attested",
        }),
      ],
      [
        "an explained merge",
        moved(
          decided.stateQueueOutRef,
          [{ fromOutRef: decided.stateQueueOutRef, ...at(140, "35") }],
          { stateQueueStatus: "merged" },
        ),
      ],
    ] as const)(
      "is filled in by an observation of %s",
      (_, next: L1ObservedDecision) => {
        expect(
          mergeL1SourceState(state([unknown]), state([next])).observations,
        ).toEqual([{ ...next, hasPersistedDecision: true }]);
      },
    );

    it.each([
      [
        "a known status contradicted at the same output",
        state([decided]),
        { ...decided, stateQueueStatus: "attested" as const },
      ],
      [
        "a known status turned unknown at the same output",
        state([decided]),
        {
          ...decided,
          stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          lastKnownStatus: "unattested" as const,
        },
      ],
      [
        "an unknown status observed at the same output but another chain point",
        state([unknown]),
        { ...decided, stateQueueStatus: "attested" as const, ...at(120, "33") },
      ],
      [
        "an unknown status on a step that took the header out of the queue",
        state([decided]),
        moved(
          decided.stateQueueOutRef,
          [{ fromOutRef: decided.stateQueueOutRef, ...at(140, "35") }],
          {
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
            lastKnownStatus: "unattested",
          },
        ),
      ],
      [
        "an unknown status filled in as terminal by a step that kept the header queued",
        state([unknown]),
        moved(outRefB, [{ ...append, ...at(120, "33") }], {
          stateQueueStatus: "merged",
        }),
      ],
      [
        "an attested status regressed across an unknown one by a move",
        unknownIn("attested"),
        moved(outRefB, [{ ...append, ...at(120, "33") }]),
      ],
      [
        "an unattested status turned conflicted across an unknown one by a move",
        unknownIn("unattested"),
        moved(outRefB, [{ ...append, ...at(120, "33") }], {
          stateQueueStatus: "conflicted",
        }),
      ],
      [
        "an attested status regressed across an unknown one at its output",
        unknownIn("attested"),
        decided,
      ],
      [
        "an unknown status that lost the status known before it",
        state([
          {
            ...unknown,
            lastKnownStatus: undefined,
          } as unknown as L1ObservedDecision,
        ]),
        { ...decided, stateQueueStatus: "attested" as const },
      ],
    ] as const)("rejects %s", (_, prior, next: L1ObservedDecision) => {
      expect(() => mergeL1SourceState(prior, state([next]))).toThrow(
        /persisted L1 decision changed canonical output/u,
      );
    });

    it("refuses a status regression taken across it, as it refuses a direct one", () => {
      // attested -> unattested by a move is a fork; so is the same regression
      // taken as a move to an unknown status, then a move filling it in.
      const attested = { ...decided, stateQueueStatus: "attested" as const };
      const hop = moved(outRefB, [{ ...append, ...at(120, "33") }], {
        stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
        lastKnownStatus: "attested",
      });
      const step = { fromOutRef: outRefB, toOutRef: outRefC, ...at(130, "34") };
      const regressed = moved(outRefC, [step], {
        stateQueueStatus: "unattested",
      });
      expect(() =>
        mergeL1SourceState(
          state([attested]),
          state([
            moved(outRefC, [{ ...append, ...at(120, "33") }, step], {
              stateQueueStatus: "unattested",
            }),
          ]),
        ),
      ).toThrow(/persisted L1 decision changed canonical output/u);
      const hopped = mergeL1SourceState(state([attested]), state([hop]));
      expect(hopped.observations).toEqual([
        { ...hop, hasPersistedDecision: true },
      ]);
      expect(() => mergeL1SourceState(hopped, state([regressed]))).toThrow(
        /persisted L1 decision changed canonical output/u,
      );
      expect(
        mergeL1SourceState(
          hopped,
          state([{ ...regressed, stateQueueStatus: "attested" }]),
        ).observations,
      ).toEqual([
        {
          ...regressed,
          stateQueueStatus: "attested",
          hasPersistedDecision: true,
        },
      ]);
    });

    it("round-trips through the persisted form", () => {
      expect(parseL1SourceState(state([unknown])).observations).toEqual([
        unknown,
      ]);
    });

    it.each([
      [
        "an unknown status without the status known before it",
        { ...unknown, lastKnownStatus: undefined },
      ],
      [
        "an unknown status whose last known status is unknown",
        { ...unknown, lastKnownStatus: UNKNOWN_STATE_QUEUE_STATUS },
      ],
      [
        "a known status with a last known status",
        { ...decided, lastKnownStatus: "attested" },
      ],
    ])("refuses to persist %s", (_, observation) => {
      expect(() =>
        parseL1SourceState(
          JSON.parse(JSON.stringify(state([observation as never]))),
        ),
      ).toThrow(/observation is malformed/u);
    });
  });

  it("round-trips authenticated steps through the persisted form", () => {
    const next = moved(outRefB, [
      { ...append, ...at(120, "33") },
      { fromOutRef: outRefB, ...at(140, "35") },
    ]);
    expect(parseL1SourceState(state([next])).observations).toEqual([next]);
    expect(() =>
      parseL1SourceState(
        state([
          { ...next, authenticatedSteps: [{ ...append, slot: -1 }] },
        ] as never),
      ),
    ).toThrow(/observation is malformed/u);
  });
});
