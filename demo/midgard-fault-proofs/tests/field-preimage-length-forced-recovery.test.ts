import { afterEach, expect, it, vi } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import * as submitters from "../src/field-preimage-length-mismatch/submit-lucid.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { forcedLengthMismatchFixture } from "./field-preimage-length-evidence.fixture.js";
import {
  bound,
  captured,
  createFieldPreimageLengthRecoveryAdapter,
  outRef,
  required,
} from "./field-preimage-length-recovery.bound.js";
import {
  authenticatedObservation,
  retainedSource,
} from "./transition-trace-challenger.build-payload-fixture.js";

afterEach(() => vi.restoreAllMocks());

it.each(["dispatch", "authenticate"] as const)(
  "recovers forced wrong acceptance through the forced %s branch",
  async (selected) => {
    const fixture = await forcedLengthMismatchFixture();
    const routed = await fetchFraudProofEvidence({
      observation: authenticatedObservation(fixture),
      sources: [retainedSource(fixture)],
    });
    if (routed.kind !== "field_preimage_length_mismatch")
      throw new Error("expected forced raw route");
    const stage: FraudProofRawL1FamilyStage = {
      kind: "step",
      step: selected === "dispatch" ? 1 : 3,
      threadOutRef: outRef("55"),
      stateQueueBlockOutRef: outRef("66"),
    };
    const workflow = (await bound(fixture.headerHash, { value: stage }))
      .workflow;
    const initial = createFieldPreimageLengthRecoveryAdapter(workflow);
    const artifact = await initial.transactions.prepareRaw!(routed);
    const recovery = createFieldPreimageLengthRecoveryAdapter(workflow);
    await recovery.transactions.validatePreparedRawArtifact!({
      routed,
      artifact: JSON.parse(JSON.stringify(artifact)),
    });
    const transaction = captured();
    const submit = vi
      .spyOn(
        submitters,
        selected === "dispatch"
          ? "submitFieldPreimageLengthForcedDispatch"
          : "submitFieldPreimageLengthForcedAuthentication",
      )
      .mockImplementation(async (input) => {
        await input.preSubmitBoundary?.(transaction);
        throw new Error("capture did not interrupt before submission");
      });
    const result = await recovery.transactions.capture({
      action: required(fixture.headerHash, stage),
      artifact,
    });
    expect(result.transaction).toBe(transaction);
    expect(submit).toHaveBeenCalledOnce();
    expect(submit.mock.calls[0]![0]).toMatchObject(
      selected === "dispatch"
        ? { direction: 0n }
        : {
            prepared: { sourceKind: "forced", direction: "wrongfulAcceptance" },
            membership: { value: { verdict: "ForcedTxValid" } },
          },
    );
  },
);
