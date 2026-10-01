import "./block-replay.w25-roots-and-deterministic-replay.js";

import { LedgerColumns } from "@al-ft/midgard-validation";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES,
  watcherBlockReplayCommittedSteps,
  type WatcherBlockReplayPriorUtxo,
  watcherBlockReplayStageForRejection,
} from "../../src/verification/block-replay.js";
import { entries } from "../support/block-replay-public-fixture.js";
import { replay } from "./block-replay.registration.js";

describe("W25 canonical rejection attribution and adversarial ordering", () => {
  it.each([
    [RejectCodes.InputNotFound, "resolveInputs", null, "spends"],
    [
      RejectCodes.InputNotFound,
      "resolveInputs",
      "reference input not found: x",
      "references",
    ],
    [RejectCodes.NativeScriptInvalid, "nativeScripts", null, "scripts"],
    [RejectCodes.ValueNotPreserved, "valueAndMint", null, "value"],
    [RejectCodes.DependencyCycle, "resolveInputs", null, "dependencies"],
    [RejectCodes.DependsOnRejectedTx, "resolveInputs", null, "dependencies"],
  ] as const)("attributes %s to %s", (code, consensusPhase, detail, stage) => {
    expect(
      watcherBlockReplayStageForRejection({ code, consensusPhase, detail }),
    ).toBe(stage);
  });

  it("attributes canonical spend, reference, script, and value rejections without watcher predicates", async () => {
    const spend = outRefFromByte(0x21);
    const reference = outRefFromByte(0x22);
    const cases = [
      [
        makePhaseBCandidate({ spent: [spend] }),
        [],
        "spends",
        RejectCodes.InputNotFound,
      ],
      [
        makePhaseBCandidate({ spent: [spend], referenceInputs: [reference] }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
        "references",
        RejectCodes.InputNotFound,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 1n })],
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
        "scripts",
        RejectCodes.InvalidFieldType,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          outputLovelace: FUNDED_OUTPUT_LOVELACE + 1n,
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
        "value",
        RejectCodes.ValueNotPreserved,
      ],
    ] as const;
    for (const [candidate, state, stage, code] of cases) {
      const result = await replay([candidate], entries(state));
      expect(result.action).toBe("reject");
      expect(result.rejections).toMatchObject([{ code, stage }]);
    }
  });

  it("reports cycles and rejected descendants in canonical priority order", async () => {
    const input = outRefFromByte(0x31);
    const parent = makePhaseBCandidate({
      spent: [input],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
      arrivalSeq: 0n,
    });
    const child = makePhaseBCandidate({
      spent: [parent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
      arrivalSeq: 1n,
    });
    const cascade = await replay(
      [parent, child],
      entries([[input, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );
    expect(cascade.rejections.map((rejection) => rejection.code)).toStrictEqual(
      [RejectCodes.ValueNotPreserved, RejectCodes.DependsOnRejectedTx],
    );
    expect(cascade.selectedRejection?.code).toBe(
      RejectCodes.DependsOnRejectedTx,
    );
    expect(cascade.rejections[1]?.stage).toBe("dependencies");

    const first = makePhaseBCandidate({ spent: [outRefFromByte(0x32)] });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [first.graph.produced[0]![LedgerColumns.OUTREF]],
    });
    const cyclic = {
      ...first,
      graph: {
        ...first.graph,
        spentOutRefHexes: [
          second.graph.produced[0]![LedgerColumns.OUTREF].toString("hex"),
        ],
      },
    };
    const cycle = await replay([cyclic, second], []);
    expect(cycle.rejections.map((rejection) => rejection.code)).toStrictEqual([
      RejectCodes.DependencyCycle,
      RejectCodes.DependencyCycle,
    ]);
    expect(
      cycle.rejections.every(({ stage }) => stage === "dependencies"),
    ).toBe(true);
  });

  it("executes a deterministic corpus for every evidenced Phase-B rejection code", async () => {
    const observed = new Set<string>();
    const collect = async (
      candidates: Parameters<typeof replay>[0],
      state: readonly WatcherBlockReplayPriorUtxo[],
    ) => {
      const result = await replay(candidates, state);
      for (const rejection of result.rejections) {
        observed.add(rejection.code);
      }
      return result;
    };

    const missing = outRefFromByte(0x81);
    await collect([makePhaseBCandidate({ spent: [missing] })], []);

    const invalidField = outRefFromByte(0x82);
    await collect(
      [
        makePhaseBCandidate({
          spent: [invalidField],
          scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 1n })],
        }),
      ],
      entries([[invalidField, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const minAda = outRefFromByte(0x8c);
    await collect(
      [
        makePhaseBCandidate({
          spent: [minAda],
          outputs: [makeOutput(1n)],
        }),
      ],
      entries([[minAda, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const value = outRefFromByte(0x83);
    await collect(
      [
        makePhaseBCandidate({
          spent: [value],
          outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
        }),
      ],
      entries([[value, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const witness = outRefFromByte(0x84);
    await collect(
      [makePhaseBCandidate({ spent: [witness], omitVkeyWitness: true })],
      entries([[witness, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const validity = outRefFromByte(0x85);
    await collect(
      [
        makePhaseBCandidate({
          spent: [validity],
          validityIntervalStart: 1n,
          validityIntervalEnd: 10n,
        }),
      ],
      entries([[validity, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const doubleSpend = outRefFromByte(0x86);
    const reference = outRefFromByte(0x87);
    await collect(
      [
        makePhaseBCandidate({ arrivalSeq: 0n, spent: [doubleSpend] }),
        makePhaseBCandidate({
          arrivalSeq: 1n,
          spent: [doubleSpend],
          referenceInputs: [reference],
        }),
      ],
      entries([
        [doubleSpend, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [reference, makeOutput(1n)],
      ]),
    );

    const cascadeInput = outRefFromByte(0x88);
    const cascadeParent = makePhaseBCandidate({
      spent: [cascadeInput],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });
    const cascadeChild = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [cascadeParent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });
    await collect(
      [cascadeParent, cascadeChild],
      entries([[cascadeInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const cycleFirst = makePhaseBCandidate({
      spent: [outRefFromByte(0x89)],
    });
    const cycleSecond = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [cycleFirst.graph.produced[0]![LedgerColumns.OUTREF]],
    });
    await collect(
      [
        {
          ...cycleFirst,
          graph: {
            ...cycleFirst.graph,
            spentOutRefHexes: [
              cycleSecond.graph.produced[0]![LedgerColumns.OUTREF].toString(
                "hex",
              ),
            ],
          },
        },
        cycleSecond,
      ],
      [],
    );

    const plutusInput = outRefFromByte(0x8a);
    const plutus = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    await collect(
      [
        makePhaseBCandidate({
          spent: [plutusInput],
          outputs: [
            makeProtectedScriptOutput(
              hashScriptWitness(plutus),
              FUNDED_OUTPUT_LOVELACE,
            ),
          ],
          scriptWitnesses: [plutus],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
          scriptLanguages: ["PlutusV3"],
        }),
      ],
      entries([[plutusInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const nativeInput = outRefFromByte(0x8b);
    const native = nativeScriptWitness({ type: "after", slot: 1n });
    await collect(
      [
        makePhaseBCandidate({
          spent: [nativeInput],
          outputs: [
            makeProtectedScriptOutput(
              hashScriptWitness(native),
              FUNDED_OUTPUT_LOVELACE,
            ),
          ],
          scriptWitnesses: [native],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
        }),
      ],
      entries([[nativeInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    // Sixteen inputs fold 16,384 distinct assets, the bound; the output's
    // one asset is a new unit, so the ValueAndMint walk crosses there.
    const distinctAssets = (start: number, count: number) => {
      const assets = new Map<string, Map<string, bigint>>();
      for (let unit = start; unit < start + count; unit += 1) {
        const policy = Buffer.concat([
          Buffer.alloc(27, 0xc0),
          Buffer.from([unit >> 10]),
        ]).toString("hex");
        const names = assets.get(policy) ?? new Map<string, bigint>();
        names.set(
          Buffer.from([(unit >> 8) & 0x03, unit & 0xff]).toString("hex"),
          1n,
        );
        assets.set(policy, names);
      }
      return assets;
    };
    const assetInputs = Array.from({ length: 16 }, (_, index) =>
      outRefFromByte(0x90 + index),
    );
    await collect(
      [
        makePhaseBCandidate({
          spent: assetInputs,
          outputs: [
            makeOutput(
              FUNDED_OUTPUT_LOVELACE,
              undefined,
              distinctAssets(16_384, 1),
            ),
          ],
        }),
      ],
      entries(
        assetInputs.map((outRef, index) => [
          outRef,
          makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            undefined,
            distinctAssets(index * 1_024, 1_024),
          ),
        ]),
      ),
    );

    expect([...observed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES].sort(),
    );
  });

  it("binds every transition event to its canonical event-to-step identity", () => {
    const txId = "aa".repeat(32);
    const steps = watcherBlockReplayCommittedSteps({
      transitionTrace: [
        {
          key: 0n,
          value: {
            schema_version: 1n,
            step_index: 0n,
            event_key: { L2TransactionEventKey: { tx_id: txId } },
            phase: "L2Transaction",
            pre_utxos_root: "bb".repeat(32),
            post_utxos_root: "cc".repeat(32),
          },
        },
      ],
      eventToStep: [
        {
          key: { L2TransactionEventKey: { tx_id: txId } },
          value: { step_index: 0n, phase: "L2Transaction" },
        },
      ],
    });
    expect(steps).toStrictEqual([
      expect.objectContaining({
        stepIndex: 0,
        txId,
        eventToStepIndex: 0,
        eventToStepPhase: "L2Transaction",
      }),
    ]);
  });
});
