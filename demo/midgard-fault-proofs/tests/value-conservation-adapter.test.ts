import { describe, expect, it, vi } from "vitest";

import { createValueConservationAdapter } from "../src/value-not-preserved/adapter.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "../src/workflow/identity.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
const hash = (byte: string) => byte.repeat(32);
const outRef = (byte: string, index = 0) => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");
const signed = ({
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScriptKind,
}: {
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScriptKind?: "native" | "plutusV1" | "plutusV2" | "plutusV3";
} = {}): LocallyEvaluatedTransaction["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => submittedHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () =>
          inlineScriptKind === "native" ? { len: () => 1 } : undefined,
        plutus_v1_scripts: () =>
          inlineScriptKind === "plutusV1" ? { len: () => 1 } : undefined,
        plutus_v2_scripts: () =>
          inlineScriptKind === "plutusV2" ? { len: () => 1 } : undefined,
        plutus_v3_scripts: () =>
          inlineScriptKind === "plutusV3" ? { len: () => 1 } : undefined,
      }),
      body: () => ({
        reference_inputs: () => ({
          len: () => 1,
          get: () => ({
            transaction_id: () => ({ to_hex: () => referenceTxHash! }),
            index: () => BigInt(referenceIndex!),
          }),
        }),
      }),
    }),
  } as unknown as LocallyEvaluatedTransaction["signed"];
};

const transaction = (
  overrides: Partial<LocallyEvaluatedTransaction> = {},
): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 fraud-proof value-not-preserved step-01",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  hubOraclePolicyId: "aa".repeat(28),
  category: "valueNotPreserved",
  target: { kind: "state_queue_header", headerHash: "ab".repeat(28) },
} as const;
const action = {
  actionId: "fold:9:thread",
  input: { stage: "fold", index: 9 },
} as const;
const context = {
  identity,
  workflowId: computeFraudProofWorkflowId(identity),
  artifact: { retained: "authenticated-value-source" },
  action,
} as const;
const fixture = () => {
  const lease = {
    token: "leased-exact-outref",
    source: "state-queue",
    renew: vi.fn(async () => undefined),
    release: vi.fn(async () => undefined),
    fail: vi.fn(async (_reason: string) => undefined),
  };
  const capture = vi.fn(async () => ({
    transaction: transaction(),
    mutationLease: lease,
  }));
  const make = () =>
    createValueConservationAdapter({
      prepare: async () => context.artifact,
      current: async () => ({ kind: "action_required", action }),
      capture,
    });
  return { make, lease, capture };
};
describe("value conservation adapter", () => {
  it("yields a stale unsigned action before capturing a replacement transaction", async () => {
    const f = fixture();
    await expect(
      f.make().preflight({
        ...context,
        action: { ...action, input: { ...action.input, index: 8 } },
      }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
    expect(f.capture).not.toHaveBeenCalled();
  });
  it("holds the exact mutation lease until its submission, then releases it", async () => {
    const f = fixture();
    const adapter = f.make();
    const preflight = await adapter.preflight(context);
    expect(f.lease.release).not.toHaveBeenCalled();
    expect(await adapter.submit({ ...context, preflight })).toEqual({
      kind: "submitted",
      txHash,
    });
    expect(f.lease.release).toHaveBeenCalledOnce();
    expect(f.lease.fail).not.toHaveBeenCalled();
    // The submission consumed the captured body: a second send has none.
    await expect(adapter.submit({ ...context, preflight })).rejects.toThrow(
      "matching locally evaluated body",
    );
  });
  it("refuses substituted cursors before capture and duplicate preflight", async () => {
    const f = fixture();
    const adapter = f.make();
    await expect(
      adapter.preflight({
        ...context,
        action: { ...action, input: { ...action.input, index: 10 } },
      }),
    ).rejects.toThrow("authenticated current checkpoint");
    expect(f.capture).not.toHaveBeenCalled();
    await adapter.preflight({
      ...context,
      action: { input: { index: 9, stage: "fold" }, actionId: action.actionId },
    });
    await expect(adapter.preflight(context)).rejects.toThrow(
      "outstanding preflight",
    );
  });
  it("refuses a substituted body hash", async () => {
    const f = fixture();
    const adapter = f.make();
    const preflight = await adapter.preflight(context);
    await expect(
      adapter.submit({
        ...context,
        preflight: { ...preflight, txHash: hash("66") },
      }),
    ).rejects.toThrow("matching locally evaluated body");
  });
});
