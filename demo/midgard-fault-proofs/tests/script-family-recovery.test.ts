import { expect, it } from "vitest";

import { runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow } from "../src/execution-native-script-invalid/v1.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../src/execution-native-script-invalid/workflow-spec.js";
import {
  executeManifestBoundExecutionSourceScriptDecodingWorkflow,
  EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
} from "../src/execution-source-script-decoding/v1.js";
import {
  executeManifestBoundMissingRedeemerWorkflow,
  MISSING_REDEEMER_CURSOR_SPEC,
} from "../src/missing-redeemer/v1.js";
import {
  executeManifestBoundMissingScriptSourceWorkflow,
  MISSING_SCRIPT_SOURCE_CURSOR_SPEC,
} from "../src/missing-script-source/v1.js";
import {
  executeManifestBoundReceivePurposeLanguageWorkflow,
  RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
} from "../src/receive-purpose-language/manifest-workflow.js";
import {
  executeManifestBoundScriptIntegrityHashMismatchWorkflow,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
} from "../src/script-integrity-hash-mismatch/manifest-workflow.js";
import {
  executeManifestBoundUnusedRedeemerWorkflow,
  UNUSED_REDEEMER_CURSOR_SPEC,
} from "../src/unused-redeemer/v1.js";
import {
  executeManifestBoundUnusedScriptWitnessWorkflow,
  UNUSED_SCRIPT_WITNESS_CURSOR_SPEC,
} from "../src/unused-script-witness/v1.js";
import { type FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { bindFamilyRecoveryFixture } from "./support/bound-family-recovery-fixture.js";
import {
  customWorkflowRecoveryFixture as fixture,
  verifyCompletionHandoffRestart,
} from "./support/custom-workflow-recovery.js";

const cases = [
  [
    EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    executeManifestBoundExecutionSourceScriptDecodingWorkflow,
  ],
  [
    EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
  ],
  [
    RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
    executeManifestBoundReceivePurposeLanguageWorkflow,
  ],
  [
    UNUSED_SCRIPT_WITNESS_CURSOR_SPEC,
    executeManifestBoundUnusedScriptWitnessWorkflow,
  ],
  [
    MISSING_SCRIPT_SOURCE_CURSOR_SPEC,
    executeManifestBoundMissingScriptSourceWorkflow,
  ],
  [MISSING_REDEEMER_CURSOR_SPEC, executeManifestBoundMissingRedeemerWorkflow],
  [UNUSED_REDEEMER_CURSOR_SPEC, executeManifestBoundUnusedRedeemerWorkflow],
  [
    SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
    executeManifestBoundScriptIntegrityHashMismatchWorkflow,
  ],
] as const;
for (const [spec, execute] of cases) {
  const run = async (input: Awaited<ReturnType<typeof fixture>>) =>
    (
      execute as unknown as (
        value: unknown,
      ) => Promise<FraudProofWorkflowRunResult>
    )({
      ...input,
      workflow:
        spec.category === "executionNativeScriptInvalid"
          ? {
              ...input.workflow,
              deployment: await bindFamilyRecoveryFixture(input.workflow),
            }
          : input.workflow,
      decisionDigest: input.workflow.decisionDigest,
      sources: [],
    });
  it(`${spec.category}: resumes an acknowledged-lost intent without constructing the next action`, async () => {
    const f = await fixture(spec);
    f.advance();
    const result = await run(f);
    expect(result).toMatchObject({
      kind: "pending",
      reason: "Canonical workflow requires fresh submission authority",
    });
    expect(f.observeHeader).not.toHaveBeenCalled();
    expect(f.capture).not.toHaveBeenCalled();
    expect(f.built.submit).not.toHaveBeenCalled();
    const entries = await f.journal.load(f.workflowId);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === f.built.txHash,
      ),
    ).toBe(true);
  });
  it(`${spec.category}: records completion after exact removal made the target absent`, async () => {
    const f = await fixture(spec, true);
    f.advance();
    const result = await run(f);
    expect(result.kind, "reason" in result ? result.reason : "unknown").toBe(
      "completed",
    );
    expect(f.observeHeader).not.toHaveBeenCalled();
    expect(f.capture).not.toHaveBeenCalled();
    expect(f.built.submit).not.toHaveBeenCalled();
    expect((await f.journal.load(f.workflowId)).at(-1)?.event.kind).toBe(
      "completed",
    );
  });
  it(`${spec.category}: resumes the exact terminal handoff after funding release precedes journal completion`, async () => {
    await verifyCompletionHandoffRestart(await fixture(spec, true), run);
  });
  it(`${spec.category}: rollback revokes readonly recovery before observation`, async () => {
    const f = await fixture(spec);
    f.controller.revoke("native_chain_rollback");
    await expect(run(f)).rejects.toThrow("revoked");
    expect(f.capture).not.toHaveBeenCalled();
    expect(f.observeHeader).not.toHaveBeenCalled();
  });
  it(`${spec.category}: retains accepted live capture, submission, and exact successor reconciliation`, async () => {
    const f = await fixture(spec);
    const context = {
      identity: f.identity,
      workflowId: f.workflowId,
      artifact: f.artifact,
      entries: [],
    };
    const preflight = await f.adapter.preflight({
      ...context,
      action: f.action,
    });
    expect(
      workflowPreflightTransaction(preflight)!.toTransaction().to_cbor_hex(),
    ).toBe(f.built.transaction.to_cbor_hex());
    expect(
      (await f.adapter.submit({ ...context, action: f.action, preflight }))
        .kind,
    ).toBe("submitted");
    f.advance();
    expect(
      await f.adapter.reconcile({
        ...context,
        action: f.action,
        txHash: f.built.txHash,
      }),
    ).toEqual({ kind: "confirmed", txHash: f.built.txHash });
    expect(f.capture).toHaveBeenCalledOnce();
    expect(f.built.submit).toHaveBeenCalledOnce();
  });
}
