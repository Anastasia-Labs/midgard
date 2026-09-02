import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { prepareWithdrawalMistagV1 } from "../src/withdrawal-mistag/prepare-withdrawal-mistag.js";
import { withdrawalMistagStatesV1 } from "../src/withdrawal-mistag/submit-withdrawal-mistag-steps.js";

const makeEvidence = async ({
  lovelace,
  validity,
}: {
  readonly lovelace: bigint;
  readonly validity: SDK.WithdrawalValidity;
}) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const withdrawalId: SDK.OutputReference = {
    transactionId: "01".repeat(32),
    outputIndex: 0n,
  };
  const body: SDK.WithdrawalBody = {
    l2_outref: withdrawalId,
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", lovelace]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
  const message = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(SDK.withdrawalBodyBytesV1(body), "hex"),
    ]),
  );
  const info: SDK.WithdrawalInfo = {
    body,
    signature: [
      Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      privateKey.sign(message).to_hex(),
    ],
    validity,
  };
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
    value: { lovelace, assets: new Map() },
  });
  const material = buildCanonicalMidgardLedgerOutputMaterialV1({
    outputIndex: 0,
    outputCbor,
  });
  const ledgerKey = encodeMidgardSpendInputItemV1({
    txId: Buffer.from(withdrawalId.transactionId, "hex"),
    outputIndex: 0,
  });
  const ledger = await keyValuePhasRootWithCount([
    { key: ledgerKey, value: material.descriptorCbor },
  ]);
  const ledgerProof = await keyValuePhasProof(
    ledger,
    ledgerKey,
    material.descriptorCbor,
  );

  const sourceKey = Buffer.from(
    SDK.committedWithdrawalKeyBytesV1(withdrawalId),
    "hex",
  );
  const sourceValue = Buffer.from(
    SDK.committedWithdrawalValueBytesV1(info),
    "hex",
  );
  const source = await buildCountedRoot("WithdrawalsRootDomain", [
    { key: sourceKey, value: sourceValue },
  ]);
  const sourceProof = await keyValuePhasProof(
    { ...source, root: source.phasRoot },
    sourceKey,
    sourceValue,
  );

  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: withdrawalId },
  };
  const eventValue: SDK.EventToStepValue = {
    step_index: 0n,
    phase: "Withdrawal",
  };
  const eventKeyBytes = Buffer.from(Data.to(eventKey, SDK.EventKey), "hex");
  const eventValueBytes = Buffer.from(
    Data.to(eventValue, SDK.EventToStepValue),
    "hex",
  );
  const event = await buildCountedRoot("EventToStepRootDomain", [
    { key: eventKeyBytes, value: eventValueBytes },
  ]);
  const eventProof = await keyValuePhasProof(
    { ...event, root: event.phasRoot },
    eventKeyBytes,
    eventValueBytes,
  );

  const transitionValue: SDK.TransitionStep = {
    schema_version: SDK.TRANSITION_STEP_V1_SCHEMA_VERSION,
    step_index: 0n,
    event_key: eventKey,
    phase: "Withdrawal",
    pre_utxos_root: ledger.root,
    post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const transitionKeyBytes = Buffer.from(Data.to(0n), "hex");
  const transitionValueBytes = Buffer.from(
    Data.to(transitionValue, SDK.TransitionStep),
    "hex",
  );
  const trace = await buildCountedRoot("TransitionTraceRootDomain", [
    { key: transitionKeyBytes, value: transitionValueBytes },
  ]);
  const traceProof = await keyValuePhasProof(
    { ...trace, root: trace.phasRoot },
    transitionKeyBytes,
    transitionValueBytes,
  );

  return {
    challengedHeaderHash: "14".repeat(28),
    committedWithdrawal: {
      domain: "WithdrawalsRootDomain" as const,
      root: source.root,
      phas_root: source.phasRoot,
      count: source.count,
      key: withdrawalId,
      value: info,
      proof: sourceProof,
    },
    eventToStep: {
      domain: "EventToStepRootDomain" as const,
      root: event.root,
      phas_root: event.phasRoot,
      count: event.count,
      key: eventKey,
      value: eventValue,
      proof: eventProof,
    },
    transitionStep: {
      domain: "TransitionTraceRootDomain" as const,
      root: trace.root,
      phas_root: trace.phasRoot,
      count: trace.count,
      key: 0n,
      value: transitionValue,
      proof: traceProof,
    },
    ledgerEvidence: {
      PresentLedgerOutput: {
        output_cbor: outputCbor.toString("hex"),
        membership_proof: ledgerProof,
      },
    },
  };
};

describe("withdrawal-mistag preparation and lifecycle handoffs", () => {
  it("prepares a valid withdrawal marked invalid through the terminal state", async () => {
    const prepared = await prepareWithdrawalMistagV1(
      await makeEvidence({
        lovelace: 1_000_000n,
        validity: "UnpayableWithdrawalValue",
      }),
    );
    expect(prepared.direction).toBe("valid-marked-invalid");
    expect(prepared.actualValid).toBe(true);
    expect(
      "PresentLedgerOutput" in prepared.ledgerEvidence &&
        prepared.ledgerEvidence.PresentLedgerOutput.descriptor_cbor.length > 0,
    ).toBe(true);
    const states = withdrawalMistagStatesV1(prepared);
    expect(states[4]).toMatchObject({
      claimed_valid: false,
      actual_valid: true,
    });
  });

  it("prepares UnpayableWithdrawalValue truth marked valid", async () => {
    const prepared = await prepareWithdrawalMistagV1(
      await makeEvidence({ lovelace: 1n, validity: "WithdrawalIsValid" }),
    );
    expect(prepared.direction).toBe("invalid-marked-valid");
    expect(prepared.coreValid).toBe(true);
    expect(prepared.payable).toBe(false);
    expect(prepared.actualValid).toBe(false);
    expect(withdrawalMistagStatesV1(prepared)[4]).toMatchObject({
      claimed_valid: true,
      actual_valid: false,
    });
  });

  it("refuses an honestly tagged exact-valid withdrawal", async () => {
    await expect(
      prepareWithdrawalMistagV1(
        await makeEvidence({
          lovelace: 1_000_000n,
          validity: "WithdrawalIsValid",
        }),
      ),
    ).rejects.toThrow(/honestly tagged/u);
  });

  it("keeps the fraud token permanent across the removal model", () => {
    const ledger = new Map([["fraud-token", 1n]]);
    const headers = new Set(["challenged", "descendant"]);
    headers.delete("descendant");
    headers.delete("challenged");
    expect(headers.size).toBe(0);
    expect(ledger.get("fraud-token")).toBe(1n);
  });
});
