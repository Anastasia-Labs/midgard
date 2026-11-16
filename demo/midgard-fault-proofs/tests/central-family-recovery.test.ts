import { expect, it } from "vitest";

import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "../src/field-item-width-illegal/workflow-spec.js";
import { MINT_ITEM_NON_CANONICAL_CURSOR_SPEC } from "../src/mint-item-non-canonical/workflow.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC } from "../src/output-reference-script-decoding/workflow-spec.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/protected-output-signer-missing/workflow-spec.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/resolved-output-non-canonical/workflow-spec.js";
import { SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/spend-input-signer-missing/workflow-spec.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/transaction-output-non-canonical/workflow-spec.js";
import { WITNESS_SCRIPT_DECODING_CURSOR_SPEC } from "../src/witness-script-decoding/workflow-spec.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { runRegisteredFamilyReconciliation } from "./support/bound-family-recovery-fixture.js";
import {
  customWorkflowRecoveryFixture as fixture,
  verifyTerminalRecoveryFinality,
} from "./support/custom-workflow-recovery.js";
const specs = [
  FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
  TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
  RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
  SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
  PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
  OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
  WITNESS_SCRIPT_DECODING_CURSOR_SPEC,
  MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
] as const;
for (const spec of specs) {
  // Reconciliation reaches a family only through its registered runner.
  const run = async (input: Awaited<ReturnType<typeof fixture>>) =>
    await runRegisteredFamilyReconciliation(input);
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
  it(`${spec.category}: retains shallow inclusion and completes beyond the recovery horizon`, async () => {
    await verifyTerminalRecoveryFinality(await fixture(spec, true), run);
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
