import "./workflow.canonical-reobservation-with-pending-descendants.js";

import { expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import { makeAdapter, run } from "./workflow.make-adapter.js";

it("continues the same in-flight proof after the same authenticated commitment is re-included at a new L1 point", async () => {
  const fixture = await buildCanonicalBlockFixture({ transactions: [] });
  const evidenceAt = (slot: bigint, blockHash: string) =>
    canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture, {
        chainPoint: { slot, blockHash },
      }),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
    });
  const original = await evidenceAt(4242n, "31".repeat(32));
  const relocated = await evidenceAt(4300n, "32".repeat(32));
  expect(relocated.headerHash).toBe(original.headerHash);
  const process = createTestWorkflowProcess();
  const adapter = makeAdapter({ lands: false });
  const first = await run({ evidence: original, adapter, process });
  expect(first).toMatchObject({ kind: "pending", awaitingActionId: "prove" });
  const resumed = await run({ evidence: relocated, adapter, process });
  expect(resumed).toMatchObject({ kind: "pending", awaitingActionId: "prove" });
  // The relocated evidence is prepared afresh; the proof is not sent again.
  expect(adapter.prepare).toHaveBeenLastCalledWith(
    expect.objectContaining({ evidence: relocated }),
  );
  expect(adapter.submit).toHaveBeenCalledOnce();
  expect(process.sent()).toHaveLength(1);
});
