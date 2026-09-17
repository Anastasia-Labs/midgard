import { expect, it } from "vitest";

import { executeManifestBoundFieldItemWidthIllegalWorkflow } from "../src/field-item-width-illegal/workflow.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "../src/field-item-width-illegal/workflow-spec.js";
import { executeManifestBoundOutputReferenceScriptDecodingWorkflow } from "../src/output-reference-script-decoding/authenticated-workflow.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC } from "../src/output-reference-script-decoding/workflow-spec.js";
import { executeManifestBoundProtectedOutputSignerMissingWorkflow } from "../src/protected-output-signer-missing/authenticated-workflow.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/protected-output-signer-missing/workflow-spec.js";
import { executeManifestBoundResolvedOutputNonCanonicalWorkflow } from "../src/resolved-output-non-canonical/authenticated-workflow.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/resolved-output-non-canonical/workflow-spec.js";
import { executeManifestBoundSpendInputSignerMissingWorkflow } from "../src/spend-input-signer-missing/authenticated-workflow.js";
import { SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/spend-input-signer-missing/workflow-spec.js";
import { executeManifestBoundTransactionOutputNonCanonicalWorkflow } from "../src/transaction-output-non-canonical/workflow.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/transaction-output-non-canonical/workflow-spec.js";
import { executeManifestBoundWitnessScriptDecodingWorkflow } from "../src/witness-script-decoding/workflow.js";
import { WITNESS_SCRIPT_DECODING_CURSOR_SPEC } from "../src/witness-script-decoding/workflow-spec.js";
import { type FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { bindFamilyRecoveryFixture } from "./support/bound-family-recovery-fixture.js";
import {
  customWorkflowRecoveryFixture as fixture,
  verifyCompletionHandoffRestart,
} from "./support/custom-workflow-recovery.js";
const cases = [
  [
    FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
    executeManifestBoundFieldItemWidthIllegalWorkflow,
  ],
  [
    TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    executeManifestBoundTransactionOutputNonCanonicalWorkflow,
  ],
  [
    RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    executeManifestBoundResolvedOutputNonCanonicalWorkflow,
  ],
  [
    SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
    executeManifestBoundSpendInputSignerMissingWorkflow,
  ],
  [
    PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
    executeManifestBoundProtectedOutputSignerMissingWorkflow,
  ],
  [
    OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
    executeManifestBoundOutputReferenceScriptDecodingWorkflow,
  ],
  [
    WITNESS_SCRIPT_DECODING_CURSOR_SPEC,
    executeManifestBoundWitnessScriptDecodingWorkflow,
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
      workflow: {
        ...input.workflow,
        deployment: await bindFamilyRecoveryFixture(input.workflow),
        config: {
          binding: input.workflow.binding,
          // Read-only recovery must never use a provider or these builder-only inputs.
          lucid: {},
          signer: {
            paymentKeyHash: "aa".repeat(28),
            address: "unused-readonly-address",
          },
          contracts: {},
          referenceScripts: {},
        },
      },
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
  it(`${spec.category}: retains the shared adapter signed capture, submission, and exact successor reconciliation`, async () => {
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
