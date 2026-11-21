/** The validation-trace reclaim in front of the dispute, over a stub L1
 * view of one removed block. One observation pass reads the block once:
 * the check before Init or Open, the reclaim's read of our threads and the
 * orchestrator's guard after it share that view, and the next pass reads
 * again. A thread of ours at the opener is the opener's Cancel to reclaim.
 * One past the opener is not: the run holds under the reclaim alarm and
 * says that the dispute's own route settles that thread. */
import { describe, expect, it, vi } from "vitest";

import {
  VALIDATION_TRACE_DISPUTE_ACTION,
  validationTraceActionId,
} from "../src/validation-trace-dispute/actions.js";
import {
  PAST_THE_OPENER,
  withValidationTraceOwnThreadReclaim,
} from "../src/validation-trace-dispute/own-thread-reclaim.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import {
  FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE,
  threadReclaimAction,
  threadReclaimNamedState,
  threadReclaimUnavailableReason,
} from "../src/workflow/own-thread-reclaim.js";
import type { FraudProofOwnThreads } from "../src/workflow/raw-l1-family-derivation.own-thread.js";
import {
  FraudProofResolvedByAnotherRecordError,
  isFraudProofResolvedByAnotherRecordError,
} from "../src/workflow/resolved-by-another-record.js";
import {
  makeAdapter,
  RELEASE_FINALITY_POLICY,
} from "./workflow.make-adapter.js";

const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
const HEADER_HASH = "e1".repeat(28);
const THREAD = `${"e2".repeat(32)}#0`;

const removal = (confirmationDepth: number) => ({
  removalTxHash: "e3".repeat(32),
  referencedProofTokenOutRef: `${"e4".repeat(32)}#0`,
  confirmationDepth,
});

type Parameters0 = Parameters<typeof withValidationTraceOwnThreadReclaim>[0];

/** The dispute's next stage is Init, and the block's L1 view says another
 * record removed it at `depth`, with `threads` ours on it. */
const removedBlock = (
  threads: FraudProofOwnThreads,
  depth: number = FINAL_DEPTH,
) => {
  const port = {
    observeView: vi.fn(async () => ({
      observe: async () => {
        throw new FraudProofResolvedByAnotherRecordError(removal(depth));
      },
      ownThreads: () => threads,
    })),
    observe: vi.fn(async () => {
      throw new Error("read outside the pass's view");
    }),
    observeOwnThreads: vi.fn(async () => {
      throw new Error("read outside the pass's view");
    }),
  };
  const init = {
    actionId: validationTraceActionId({ stage: "init", consumes: null }),
    input: {
      schemaVersion: VALIDATION_TRACE_DISPUTE_ACTION,
      stage: "init",
      consumes: null,
    },
  };
  const base: FraudProofFamilyWorkflowAdapter = {
    ...makeAdapter(),
    category: "validationTraceDispute",
    observe: async () => ({ kind: "action_required", action: init }),
  };
  const adapter = withValidationTraceOwnThreadReclaim({
    base,
    lucid: {} as Parameters0["lucid"],
    signer: {} as Parameters0["signer"],
    deployment: {
      chain: { opener: {} },
      // A cancel the deployment can build: its minting script and that
      // script's reference are bound.
      init: {
        contracts: { computationThread: { mintingScript: {} } },
        category: { categoryId: "0006" },
        witnessReferenceScripts: { computationThreadMint: {} },
      },
      references: {},
    } as unknown as Parameters0["deployment"],
    l1For: () => port,
    signedRecovery: {} as Parameters0["signedRecovery"],
    releaseFinality: { policy: RELEASE_FINALITY_POLICY },
  });
  const context = {
    identity: {
      target: { kind: "state_queue_header", headerHash: HEADER_HASH },
    },
    workflowId: "validation-trace-own-thread-reclaim",
    artifact: { headerHash: HEADER_HASH },
    entries: [],
  } as unknown as Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0];
  return { port, adapter, context };
};

describe("validation-trace reclaim: a removed block's L1 view", () => {
  it("is read once in an observation pass, for the check before Init, the reclaim and the orchestrator's guard", async () => {
    const { port, adapter, context } = removedBlock({ spends: [] });
    for (const pass of [1, 2]) {
      const resolved = await adapter.observe(context).then(
        () => undefined,
        (cause: unknown) => cause,
      );
      expect(isFraudProofResolvedByAnotherRecordError(resolved)).toBe(true);
      expect(
        await adapter.observeOwnThreads?.({ headerHash: HEADER_HASH }),
      ).toEqual({ spends: [] });
      expect(port.observeView).toHaveBeenCalledTimes(pass);
    }
    expect(port.observe).not.toHaveBeenCalled();
    expect(port.observeOwnThreads).not.toHaveBeenCalled();
  });

  it("cancels a thread of ours at the opener once the removal is past rollback recovery", async () => {
    const standing = { threadOutRef: THREAD, step: 1 as const, lovelace: "1" };
    const { adapter, context } = removedBlock({ standing, spends: [] });
    expect(await adapter.observe(context)).toEqual({
      kind: "action_required",
      action: threadReclaimAction(standing),
    });
  });

  it("holds under the reclaim alarm, saying the dispute's own route settles it, while a thread of ours stands past the opener", async () => {
    for (const depth of [1, FINAL_DEPTH]) {
      const { adapter, context } = removedBlock(
        {
          standing: { threadOutRef: THREAD, step: 2, lovelace: "1" },
          spends: [],
        },
        depth,
      );
      const held = await adapter.observe(context);
      expect(held).toEqual({
        kind: "pending",
        reason: threadReclaimUnavailableReason(THREAD, PAST_THE_OPENER),
      });
      if (held.kind !== "pending") return;
      expect(threadReclaimNamedState(held.reason)).toEqual({
        name: FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE,
        alarm: true,
      });
      expect(held.reason).toContain("stands past the dispute opener");
    }
  });
});
