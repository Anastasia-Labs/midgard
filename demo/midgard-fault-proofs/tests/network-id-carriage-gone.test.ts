/**
 * The network-id adapter when field carriage it scheduled an action on is gone
 * from L1 by the time the action runs: each carriage-consuming action treats
 * that as a changed action.
 */
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { h32 } from "./helpers/canonical-block-evidence-fixture.js";
import {
  certifiedOpening,
  outputNetworkOpening,
} from "./support/network-id-certified-opening.js";
import {
  CERTIFICATE_ADDRESS,
  CERTIFICATE_UNIT,
  forcedHarness,
  STATE_QUEUE_OUT_REF,
  utxo,
  workflowIdentity,
} from "./support/network-id-forced-harness.js";

describe("network-id field carriage gone from L1 after scheduling", () => {
  it("treats a field chunk gone from L1 since scheduling as a changed certification action", async () => {
    const { prepared, harness, wallet } = await certifiedOpening({
      certificateUtxos: [],
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the certification action");
    expect(selected.action.input.kind).toBe("certify_field");

    // A rollback or a spend removes a chunk before the action runs.
    wallet.splice(0, 1);
    await expect(
      harness.adapter.preflight({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
      }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
  });

  it("treats a field chunk gone from L1 since scheduling as a changed forced-scan action", async () => {
    const certificates: UTxO[] = [];
    const { prepared, harness, certificate, wallet } = await certifiedOpening({
      certificateUtxos: certificates,
    });
    certificates.push(certificate(h32(0x61)));
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the forced scan action");
    expect(selected.action.input.certificateOutRef).toBe(`${h32(0x61)}#0`);

    // A rollback or a spend removes a chunk before the action runs.
    wallet.splice(0, 1);
    await expect(
      harness.adapter.preflight({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
      }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
  });

  it("treats a field chunk gone from L1 since scheduling as a changed final-step action", async () => {
    const { headerHash, artifact, opening } = outputNetworkOpening();
    const wallet = opening.plan.publications.map(
      (publication, index): UTxO =>
        utxo({
          txHash: h32(0x20 + index),
          datum: fieldPreimagePublicationDatumCbor(publication.bytes),
        }),
    );
    const harness = forcedHarness({
      prepared: { headerHash, artifact },
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      walletUtxos: wallet,
      certificateUtxos: [
        {
          txHash: h32(0x61),
          outputIndex: 0,
          address: CERTIFICATE_ADDRESS,
          assets: { lovelace: 5_000_000n, [CERTIFICATE_UNIT]: 1n },
          datum: deriveFieldPreimageCertification(opening.plan).datumCbor,
        },
      ],
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the final step");
    expect(selected.action.input.kind).toBe("step02");

    // A rollback or a spend removes a chunk before the action runs.
    wallet.splice(0, 1);
    await expect(
      harness.adapter.preflight({
        identity: workflowIdentity(headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: selected.action,
      }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
  });
});
