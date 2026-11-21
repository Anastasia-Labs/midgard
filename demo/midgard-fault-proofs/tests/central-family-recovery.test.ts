import { expect, it } from "vitest";

import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "../src/field-item-width-illegal/workflow-spec.js";
import { MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION } from "../src/mint-declared-asset-limit/v1.js";
import { MINT_ITEM_NON_CANONICAL_CURSOR_SPEC } from "../src/mint-item-non-canonical/workflow-spec.js";
import { OBSERVER_ORDER_INVALID_FAMILY_DEFINITION } from "../src/observer-order-invalid/v1.js";
import { OBSERVERS_FORBIDDEN_FAMILY_DEFINITION } from "../src/observers-forbidden-on-untagged-network/v1.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC } from "../src/output-reference-script-decoding/workflow-spec.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/protected-output-signer-missing/workflow-spec.js";
import { REDEEMER_CANONICITY_FAMILY_DEFINITION } from "../src/redeemer-canonicity/runtime.capture-redeemer-action.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/resolved-output-non-canonical/workflow-spec.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC } from "../src/script-integrity-hash-missing/v1.js";
import { SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC } from "../src/spend-input-signer-missing/workflow-spec.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "../src/transaction-output-non-canonical/workflow-spec.js";
import { WITNESS_SCRIPT_DECODING_CURSOR_SPEC } from "../src/witness-script-decoding/workflow-spec.js";
import type { CursorFamilySpec } from "../src/workflow/cursor-family-state.js";
import type { InstalledFraudProofCategory } from "../src/workflow/installed-categories.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  prepareRegisteredFamilyReconciliation,
  runRegisteredFamilyReconciliation,
} from "./support/bound-family-recovery-fixture.js";
import {
  customWorkflowRecoveryFixture as fixture,
  verifyTerminalRecoveryFinality,
} from "./support/custom-workflow-recovery.js";
/** The cursor spec a family's definition lays its adapter out from. */
const cursorSpecOf = <Category extends InstalledFraudProofCategory>({
  adapter,
}: Readonly<{
  adapter: Readonly<{ kind: string; spec?: CursorFamilySpec<Category> }>;
}>): CursorFamilySpec<Category> => {
  if (adapter.kind !== "cursor" || adapter.spec === undefined)
    throw new Error("family definition has no cursor adapter");
  return adapter.spec;
};
const specs = [
  FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
  TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
  RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
  SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
  PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
  OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
  WITNESS_SCRIPT_DECODING_CURSOR_SPEC,
  MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
  // The decision-digest families: each binds the admitted decision digest.
  cursorSpecOf(MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION),
  cursorSpecOf(OBSERVER_ORDER_INVALID_FAMILY_DEFINITION),
  cursorSpecOf(OBSERVERS_FORBIDDEN_FAMILY_DEFINITION),
  cursorSpecOf(REDEEMER_CANONICITY_FAMILY_DEFINITION),
  SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC,
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
    f.advance();
    // The invocation is ready before the rollback lands, so the runner is
    // the one that refuses it.
    const resume = prepareRegisteredFamilyReconciliation(f);
    const recorded = await f.store.load(f.workflowId);
    f.controller.revoke("native_chain_rollback");
    await expect(resume()).rejects.toThrow(
      "production workflow actuation revoked before runner_start: native_chain_rollback",
    );
    expect(await f.store.load(f.workflowId)).toEqual(recorded);
    expect(f.observeHeader).not.toHaveBeenCalled();
  });
  // Drives the shared cursor adapter the fixture builds over this family's
  // spec, not the adapter the family's constructor builds: the spec's
  // successor table is the only family input on this path.
  it(`${spec.category}: the shared cursor adapter over this spec retains signed capture, submission, and exact successor reconciliation`, async () => {
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
