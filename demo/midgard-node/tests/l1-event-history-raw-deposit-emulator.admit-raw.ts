import { createHash } from "node:crypto";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as ordered,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  datumToHash,
  fromText,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { userEventEntry } from "../src/l1-events/index.js";
import { projectOrderAsFollower } from "./helpers/emulator-l1-follower.js";

// Deployment manifests admit only the compiled profile's network.
export const network = SELECTED_DEPLOYMENT_PROFILE.network;

/** A deposit Order's node entry through the follower's projection. */
export const followerDepositEntry = (
  order: SDK.DepositUTxO,
  policyId: string,
) => {
  const retained = order.history.retainedDataUtxo;
  const decoded = userEventEntry(
    projectOrderAsFollower(
      order.utxo,
      "deposit",
      policyId,
      retained === undefined ? [] : [retained],
    ),
    network,
  );
  if (decoded.kind !== "deposit") throw new Error("expected a deposit");
  return decoded.entry;
};

export const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");

export const field = (cbor: string, path: readonly number[]) =>
  ordered(plutusConstrFieldCbor(cbor, path));

const indexOf = (inputs: readonly UTxO[], target: UTxO) => {
  const index = [...inputs]
    .sort(compareOutRefs)
    .findIndex((input) => outRefLabel(input) === outRefLabel(target));
  if (index < 0) throw new Error("Missing actual raw admission input");
  return BigInt(index);
};

const distinct = (inputs: readonly UTxO[]) =>
  expect(new Set(inputs.map(outRefLabel)).size).toBe(inputs.length);

export const plain = (utxo: UTxO) =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

type Prepared = Effect.Effect.Success<
  ReturnType<typeof SDK.prepareDepositSubmissionProgram>
>;

/** Test-only admission assembly mirrors history-build's InsertOrder input,
 * reference, funding and observer rules. Raw bytes replace only datum encoding
 * BEFORE ordinary complete/local evaluation; no signed transaction is patched.
 * The fixture uses new sorted nonce keys, so no filler promotion is involved. */
export const admitRaw = async (
  prepared: Prepared,
  payloadCbor: string,
  externalData: UTxO | undefined,
  validFrom: number,
  validTo: number,
) => {
  const { context, request } = prepared;
  const payload = Data.from(request.payloadCbor, SDK.EventHistoryPayload);
  const { lucid, recipe, applied, hubReference, scriptReference } = context;
  expect(recipe.kind).toBe("Deposit");
  expect(
    hubReference.assets[recipe.hubPolicyId + fromText("MIDGARD_HUB_ORACLE")],
  ).toBe(1n);
  const hub = Data.from(hubReference.datum!, SDK.HubOracleDatum);
  expect(hub.deposit).toBe(applied.policyId);
  expect(Data.to(hub.deposit_addr, SDK.AddressData)).toBe(
    Data.to(
      Effect.runSync(SDK.addressDataFromBech32(applied.address)),
      SDK.AddressData,
    ),
  );
  if (scriptReference !== undefined)
    expect(validatorToScriptHash(scriptReference.scriptRef!)).toBe(
      applied.policyId,
    );
  if (!("DepositPayload" in payload))
    throw new Error("Expected prepared Deposit payload");
  const id = payload.DepositPayload.event.id;
  expect(id).toEqual({
    transactionId: request.nonce.txHash,
    outputIndex: BigInt(request.nonce.outputIndex),
  });
  const key = datumToHash(Data.to(id, SDK.OutputReference));
  const deployment = {
    policyId: applied.policyId,
    address: applied.address,
    retentionAddress: applied.retention.address,
    inlineLimitBytes: recipe.inlineLimitBytes,
  };
  const witness = await SDK.fetchEventHistoryWitness(lucid, deployment, id);
  if (witness.kind !== "Absent")
    throw new Error("Raw admission requires authentic unused nonce gap");
  const { anchor } = witness;
  expect(anchor.key).not.toBe(key);
  const lower = BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validFrom)));
  const upper =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))) - 1n;
  expect(lower).toBeGreaterThanOrEqual(anchor.node.protected_until);
  expect(upper - lower).toBeLessThanOrEqual(
    BigInt(SDK.MAX_VALIDITY_RANGE_LENGTH_MS),
  );
  const typedPayload = Data.to(payload, SDK.EventHistoryPayload);
  const inline = BigInt(payloadCbor.length / 2) <= recipe.inlineLimitBytes;
  expect(inline).toBe(externalData === undefined);
  let location: SDK.EventHistoryFacts["location"];
  if (externalData === undefined) location = { Inline: { payload } };
  else {
    const [actual] = await lucid.utxosByOutRef([externalData]);
    expect(actual).toEqual(externalData);
    expect(actual!.address).toBe(applied.retention.address);
    expect(actual!.scriptRef).toBeUndefined();
    expect(field(actual!.datum!, [1])).toBe(payloadCbor);
    location = {
      External: { storage_datum_hash: datumToHash(ordered(actual!.datum!)) },
    };
  }
  const inputs = [...context.fundingInputs, request.nonce, anchor.utxo];
  const references = [
    hubReference,
    ...(scriptReference === undefined ? [] : [scriptReference]),
    ...(externalData === undefined ? [] : [externalData]),
  ];
  distinct(inputs);
  distinct(references);
  expect(
    inputs.some((input) =>
      references.some((ref) => outRefLabel(ref) === outRefLabel(input)),
    ),
  ).toBe(false);
  const protectedUntil = upper + recipe.protectionDurationMs;
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: anchor.node.next,
    protected_until: protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: upper + BigInt(SDK.EVENT_WAIT_DURATION_MS),
          location,
          structural_lovelace: request.structuralLovelace,
          structural_refund_key: request.structuralRefundKey,
        },
      },
    },
  };
  SDK.assertEventHistoryAdmissionFunding(
    node,
    { ...request.assets, [applied.policyId + key]: 1n },
    applied.policyId,
    payload,
    request.structuralLovelace,
  );
  let nodeCbor = Data.to(node, SDK.EventHistoryNode);
  if (inline) {
    expect(field(nodeCbor, [3, 0, 2, 0])).toBe(ordered(typedPayload));
    nodeCbor = replacePlutusConstrFieldCbor(
      nodeCbor,
      [3, 0, 2, 0],
      payloadCbor,
    );
  }
  // Pointer/protection continuation preserves the previous raw facts verbatim.
  const nextSkeleton = Data.to(
    { ...anchor.node, next: key, protected_until: protectedUntil },
    SDK.EventHistoryNode,
  );
  const predecessorCbor = replacePlutusConstrFieldCbor(
    replacePlutusConstrFieldCbor(
      anchor.utxo.datum!,
      [1],
      plutusConstrFieldCbor(nextSkeleton, [1]),
    ),
    [2],
    Data.to(protectedUntil),
  );
  const operation: SDK.EventHistoryOperation = {
    InsertOrder: {
      predecessor_input_index: indexOf(inputs, anchor.utxo),
      predecessor_output_index: 0n,
      order_output_index: 1n,
      nonce_input_index: indexOf(inputs, request.nonce),
      external_reference_index:
        externalData === undefined ? null : indexOf(references, externalData),
    },
  };
  const observerCbor = Data.to(
    {
      Apply: {
        hub_reference_index: indexOf(references, hubReference),
        operation,
      },
    },
    SDK.EventHistoryObserve,
  );
  let tx = lucid
    .newTx()
    .collectFrom([...context.fundingInputs, request.nonce])
    .collectFrom([anchor.utxo], Data.to(indexOf(inputs, anchor.utxo)))
    .readFrom(references)
    .withdraw(applied.rewardAddress, 0n, observerCbor)
    .validFrom(validFrom)
    .validTo(validTo)
    .mintAssets({ [applied.policyId + key]: 1n }, Data.void())
    .pay.ToContract(
      applied.address,
      { kind: "inline", value: predecessorCbor },
      anchor.utxo.assets,
    )
    .pay.ToContract(
      applied.address,
      { kind: "inline", value: nodeCbor },
      { ...request.assets, [applied.policyId + key]: 1n },
    );
  if (scriptReference === undefined) tx = tx.attach.Script(applied.validator);
  const completed = await tx.complete({
    coinSelection: false,
    localUPLCEval: true,
    presetWalletInputs: [...context.fundingInputs, request.nonce],
  });
  const body = CML.Transaction.from_cbor_hex(completed.toCBOR()).body();
  const approved = new Set(
    [...context.fundingInputs, request.nonce].map(outRefLabel),
  );
  const collateral = body.collateral_inputs();
  for (let i = 0; i < (collateral?.len() ?? 0); i++) {
    const input = collateral!.get(i);
    expect(
      approved.has(`${input.transaction_id().to_hex()}#${input.index()}`),
    ).toBe(true);
  }
  return {
    completed,
    key,
    nodeCbor,
    predecessorCbor,
    anchor,
    inputs,
    references,
    observerCbor,
  };
};
