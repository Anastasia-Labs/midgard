import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import {
  makeNativeTx,
  nativeScriptWitness,
  TEST_ADDRESS_BYTES,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import type { PhaseAConfig, QueuedTx } from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherPhaseAQueuedTxs,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODES,
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
} from "../../src/verification/phase-a-verifier.js";
import {
  bigInlineDatum,
  CONFIG,
  configFor,
  KEY,
  manyOutputs,
  nestedNativeScript,
  queuedTx,
  SLOW_TEST_TIMEOUT_MS,
} from "./phase-a-verifier.base-header.js";
import {
  type CanonicalVerdict,
  verdictCache,
} from "./phase-a-verifier.build-block.js";
import {
  EVIDENCE_CASES,
  VALID_CASES,
} from "./phase-a-verifier.evidence-cases.js";

export const canonicalVerdict = (
  queued: QueuedTx,
  config: PhaseAConfig,
): CanonicalVerdict => {
  let byConfig = verdictCache.get(queued);
  if (byConfig === undefined) {
    byConfig = new Map();
    verdictCache.set(queued, byConfig);
  }
  const cached = byConfig.get(config);
  if (cached !== undefined) {
    return cached;
  }
  const outcome = validatePhaseASingle(queued, config);
  const verdict: CanonicalVerdict =
    "ledgerTx" in outcome
      ? { accepted: true, rejected: null }
      : { accepted: false, rejected: outcome };
  byConfig.set(config, verdict);
  return verdict;
};

describe("differential against the canonical Phase A entry point", () => {
  const corpus: readonly {
    readonly label: string;
    readonly queued: QueuedTx;
    readonly config: PhaseAConfig;
  }[] = [
    ...VALID_CASES.map((queued, index) => ({
      label: `valid #${index.toString()}`,
      queued,
      config: CONFIG,
    })),
    ...EVIDENCE_CASES.map((entry) => ({
      label: entry.label,
      queued: entry.queued,
      config: entry.config,
    })),
  ];

  it.each(corpus.map((entry) => [entry.label, entry] as const))(
    "reproduces the canonical verdict exactly for %s",
    (_label, entry) => {
      const canonical = canonicalVerdict(entry.queued, entry.config);
      const result = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      expect(result.transactionCount).toBe(1);
      if (canonical.accepted) {
        expect(result.action).toBe("accept");
        expect(result.rejections).toStrictEqual([]);
        expect(result.acceptedTxIds).toStrictEqual([
          entry.queued.txId.toString("hex"),
        ]);
        return;
      }
      const rejected = canonical.rejected!;
      expect(result.action).toBe("reject");
      expect(result.acceptedTxIds).toStrictEqual([]);
      expect(result.rejections).toHaveLength(1);
      expect(result.rejections[0]).toMatchObject({
        index: 0,
        txId: rejected.txId.toString("hex"),
        code: rejected.code,
        stage: rejected.consensusPhase,
        detail: rejected.detail,
      });
      expect(result.selectedRejection).toStrictEqual(result.rejections[0]);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "never accepts a transaction the canonical path rejects",
    () => {
      for (const entry of corpus) {
        const canonical = canonicalVerdict(entry.queued, entry.config);
        const result = evaluateWatcherPhaseAQueuedTxs({
          queuedTxs: [entry.queued],
          config: entry.config,
        });
        if (!canonical.accepted) {
          expect(result.action).not.toBe("accept");
          expect(result.acceptedTxIds).toStrictEqual([]);
        }
      }
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "reproduces the canonical verdict for the whole corpus in one batch",
    () => {
      // A single batch shares one config, so only the cases that use CONFIG can
      // take part; the rest are covered individually above.
      const batch = corpus
        .filter((entry) => entry.config === CONFIG)
        .map((entry) => entry.queued);
      const result = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: batch,
        config: CONFIG,
      });
      const expectedAccepted: string[] = [];
      const expectedRejections: unknown[] = [];
      batch.forEach((queued, index) => {
        const canonical = canonicalVerdict(queued, CONFIG);
        if (canonical.accepted) {
          expectedAccepted.push(queued.txId.toString("hex"));
          return;
        }
        expectedRejections.push({
          index,
          txId: canonical.rejected!.txId.toString("hex"),
          code: canonical.rejected!.code,
          stage: canonical.rejected!.consensusPhase,
          detail: canonical.rejected!.detail,
        });
      });
      expect(result.transactionCount).toBe(batch.length);
      expect(result.acceptedTxIds).toStrictEqual(expectedAccepted);
      expect(
        result.rejections.map((rejection) => ({
          index: rejection.index,
          txId: rejection.txId,
          code: rejection.code,
          stage: rejection.stage,
          detail: rejection.detail,
        })),
      ).toStrictEqual(expectedRejections);
    },
    SLOW_TEST_TIMEOUT_MS,
  );
});

// ---------------------------------------------------------------------------
// One deterministic rejection-evidence case per reachable code
// ---------------------------------------------------------------------------

describe("rejection evidence per reachable code", () => {
  it.each(EVIDENCE_CASES.map((entry) => [entry.code, entry] as const))(
    "produces %s deterministically",
    (code, entry) => {
      const first = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      const second = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      expect(first.resultDigest).toBe(second.resultDigest);
      expect(first.action).toBe("reject");
      expect(first.reasonCodes).toStrictEqual(["phase_a_rejection"]);
      expect(first.rejections).toHaveLength(1);
      expect(first.rejections[0]!.code).toBe(code);
      expect(first.rejections[0]!.stage).toBe(entry.stage);
      expect(first.rejections[0]!.detail).toBe(
        canonicalVerdict(entry.queued, entry.config).rejected!.detail,
      );
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "covers exactly the published evidenced set",
    () => {
      const produced = new Set(
        EVIDENCE_CASES.map(
          (entry) =>
            canonicalVerdict(entry.queued, entry.config).rejected!
              .code as string,
        ),
      );
      expect([...produced].sort()).toStrictEqual(
        [...WATCHER_PHASE_A_EVIDENCED_REJECT_CODES].sort(),
      );
      expect(EVIDENCE_CASES).toHaveLength(
        WATCHER_PHASE_A_EVIDENCED_REJECT_CODES.length,
      );
    },
    SLOW_TEST_TIMEOUT_MS,
  );
});

// ---------------------------------------------------------------------------
// Adjacent boundary: the dominated codes and the bounds around them
// ---------------------------------------------------------------------------

describe("adjacent boundary", () => {
  it.each(["normal", "forced"] as const)(
    "shows output width dominates E_LEDGER_OUTPUT_SIZE for %s sources",
    (sourceKind) => {
      expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES).toContain(
        RejectCodes.LedgerOutputSize,
      );
      const fixture = makeNativeTx({
        privateKey: KEY,
        outputs: [
          encodeMidgardTxOutput({
            address: TEST_ADDRESS_BYTES,
            value: { lovelace: 1n, assets: new Map() },
            datum: { kind: "inline", cbor: bigInlineDatum(400) },
          }),
        ],
      });
      const txCbor =
        sourceKind === "normal"
          ? fixture.txCbor
          : encodeMidgardForcedTxCanonical(
              materializeMidgardForcedTxFromCanonical(fixture.tx),
            );
      const result = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [queuedTx(fixture.txId, txCbor, { sourceKind })],
        config: CONFIG,
      });
      expect(result.rejections[0]).toMatchObject({
        code: RejectCodes.InvalidFieldType,
        stage: "canonicalDecode",
      });
      expect(
        validatePhaseASingle(
          queuedTx(fixture.txId, txCbor, { sourceKind }),
          CONFIG,
        ),
      ).toMatchObject({
        subject: {
          arm: "FieldItemWidthIllegal",
          fieldIndex: 2n,
          itemIndex: 0n,
        },
      });
    },
  );

  it("accepts a fee exactly at the header minimum and rejects one below", () => {
    const fixture = makeNativeTx({ privateKey: KEY, fee: 7n });
    const config = configFor({ minFeeB: 7n });
    expect(
      evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [queuedTx(fixture.txId, fixture.txCbor)],
        config,
      }).action,
    ).toBe("accept");
    const below = makeNativeTx({ privateKey: KEY, fee: 6n });
    const result = evaluateWatcherPhaseAQueuedTxs({
      queuedTxs: [queuedTx(below.txId, below.txCbor)],
      config,
    });
    expect(result.rejections[0]!.code).toBe(RejectCodes.MinFee);
  });

  it(
    "accepts an outputs preimage under the field bound and rejects one over",
    () => {
      const under = makeNativeTx({
        privateKey: KEY,
        outputs: manyOutputs(700),
      });
      expect(
        evaluateWatcherPhaseAQueuedTxs({
          queuedTxs: [queuedTx(under.txId, under.txCbor)],
          config: CONFIG,
        }).action,
      ).toBe("accept");
      const over = makeNativeTx({
        privateKey: KEY,
        outputs: manyOutputs(2000),
      });
      expect(
        evaluateWatcherPhaseAQueuedTxs({
          queuedTxs: [queuedTx(over.txId, over.txCbor)],
          config: CONFIG,
        }).rejections[0]!.code,
      ).toBe(RejectCodes.FieldPreimageSize);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it.each([
    [
      RejectCodes.OutputCount,
      RejectCodes.InvalidOutput,
      () =>
        makeNativeTx({
          privateKey: KEY,
          outputs: Array.from({ length: 16385 }, () => Buffer.alloc(0)),
        }),
    ],
    [
      RejectCodes.RequiredSignerCount,
      RejectCodes.InvalidFieldType,
      () =>
        makeNativeTx({
          privateKey: KEY,
          requiredSignerItems: Array.from({ length: 16385 }, () =>
            Buffer.alloc(0),
          ),
        }),
    ],
  ])(
    "shows %s is dominated by %s rather than silently unreachable",
    (dominated, dominating, build) => {
      expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES).toContain(dominated);
      const fixture = build();
      const result = evaluateWatcherPhaseAQueuedTxs({
        queuedTxs: [queuedTx(fixture.txId, fixture.txCbor)],
        config: CONFIG,
      });
      expect(result.rejections[0]!.code).toBe(dominating);
      expect(result.rejections[0]!.code).not.toBe(dominated);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it("shows the canonical encoder refuses an over-deep native script", () => {
    expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES).toContain(
      RejectCodes.NativeScriptDepth,
    );
    expect(() =>
      makeNativeTx({
        privateKey: KEY,
        scriptWitnesses: [nativeScriptWitness(nestedNativeScript(16385))],
      }),
    ).toThrow(/nesting exceeds/u);
  });
});
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec/output";
