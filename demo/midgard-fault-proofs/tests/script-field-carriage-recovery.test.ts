import { createHash } from "node:crypto";

import { computeHash32 } from "@al-ft/midgard-core";
import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  FIELD_CARRIAGE_PREREQUISITE,
  FIELD_CARRIAGE_RECOVERY,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
} from "../src/workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";

const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const fixture = (certificate: boolean) => {
  const txHash = "11".repeat(32),
    requirementSha256 = "22".repeat(32),
    category = "missingRedeemer",
    headerHash = "33".repeat(28);
  const datumCbor = certificate
    ? Data.to(0n)
    : fieldPreimagePublicationDatumCbor(Buffer.from("80", "hex"));
  const unit = certificate
    ? "44".repeat(28) + FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX
    : null;
  const base = {
    actionId: "step_05:original-thread",
    input: { category, stage: "step_05" },
  };
  const action: FraudProofWorkflowAction = certificate
    ? {
        actionId: `certify-field-carriage:${base.actionId}:${requirementSha256}`,
        input: {
          schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
          category,
          stage: "certify_field_carriage",
          forAction: base,
          requirementSha256,
          certificateDatumCborSha256: sha(datumCbor),
          certificateUnit: unit,
        },
      }
    : {
        actionId: `publish-field-carriage:${base.actionId}:${requirementSha256}:0`,
        input: {
          schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
          category,
          stage: "publish_field_carriage",
          forAction: base,
          requirementSha256,
          publicationIndex: 0,
          publicationEncoding: "nothing_but_bytes",
          publicationDigest: computeHash32(Buffer.from("80", "hex")).toString(
            "hex",
          ),
          datumCborSha256: sha(datumCbor),
        },
      };
  const requirementForAction = vi.fn(async (): Promise<never> => {
    throw new Error("target replay unavailable");
  });
  const observeExact = vi.fn(async () => ({ kind: "confirmed" as const }));
  const port = createAuthenticatedFieldCarriagePrerequisitePort({
    category,
    lucid: {} as never,
    network: "Custom",
    signer: {
      address:
        "addr_test1vz0h7dj6klye53w94xxhfqdgpncjeayjpd374eklgt90m3g2ldecg",
    } as never,
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: observeExact as never,
    },
    requirementForAction,
    transactionConfirmed: async () => false,
  });
  const durableRecovery = {
    fieldCarriage: {
      schemaVersion: FIELD_CARRIAGE_RECOVERY,
      kind: certificate ? "certificate" : "publication",
      requirementSha256,
      outRef: `${txHash}#0`,
      datumCbor,
      unit,
    },
  };
  return {
    port,
    action,
    txHash,
    headerHash,
    durableRecovery,
    requirementForAction,
    observeExact,
  };
};
it.each([false, true])(
  "reconciles exact recorded field output without typed target replay (certificate=%s)",
  async (certificate) => {
    const f = fixture(certificate);
    expect(await f.port.reconcile({ ...f, artifact: {} })).toEqual({
      kind: "confirmed",
      txHash: f.txHash,
    });
    expect(f.requirementForAction).not.toHaveBeenCalled();
    expect(f.observeExact).toHaveBeenCalledOnce();
  },
);
it.each(["action", "datum", "hash", "category", "outRef", "unit"])(
  "rejects altered recorded field %s before asking an archive",
  async (changed) => {
    const f = fixture(changed === "unit");
    if (changed === "action")
      f.action = { ...f.action, actionId: "another-action" };
    if (changed === "category")
      f.action = {
        ...f.action,
        input: { ...f.action.input, category: "unusedRedeemer" },
      };
    if (changed === "datum") f.durableRecovery.fieldCarriage.datumCbor = "00";
    if (changed === "hash")
      f.durableRecovery.fieldCarriage.requirementSha256 = "aa".repeat(32);
    if (changed === "outRef")
      f.durableRecovery.fieldCarriage.outRef = `${"bb".repeat(32)}#0`;
    if (changed === "unit")
      f.durableRecovery.fieldCarriage.unit =
        "cc".repeat(28) + FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX;
    expect((await f.port.reconcile({ ...f, artifact: {} })).kind).toBe(
      "conflict",
    );
    expect(f.observeExact).not.toHaveBeenCalled();
    expect(f.requirementForAction).not.toHaveBeenCalled();
  },
);

it("reconciles the retained raw-datum publication recovery shape without target replay", async () => {
  const f = fixture(false);
  f.action = {
    ...f.action,
    actionId: f.action.actionId.replace(
      "publish-field-carriage:",
      "publish-raw-datum-preimage:",
    ),
    input: {
      ...f.action.input,
      schemaVersion: RAW_DATUM_PREIMAGE_PREREQUISITE,
    },
  };
  expect(await f.port.reconcile({ ...f, artifact: {} })).toEqual({
    kind: "confirmed",
    txHash: f.txHash,
  });
  expect(f.requirementForAction).not.toHaveBeenCalled();
  expect(f.observeExact).toHaveBeenCalledOnce();
});
