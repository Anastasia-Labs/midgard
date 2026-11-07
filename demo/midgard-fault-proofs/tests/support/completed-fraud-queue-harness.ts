/** Applied queue spend over fixture native issuers: one queue node, the hub
 * that names it, and the governed spending reference script. Queue issuance
 * and completed-proof issuance are native policies the wallet signs for. */
import { readFileSync } from "node:fs";
import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  fromText,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  mintingPolicyToId,
  scriptFromNative,
  toUnit,
  type TxSignBuilder,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { realBlueprintPath } from "./emulator/blueprints.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { measureCompleteSignedTransaction } from "./emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";

export const completedFraudBlueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(completedFraudBlueprintBytes.toString()),
);

export const setupCompletedFraudQueue = async ({
  previouslyMarked = false,
  records,
}: {
  readonly previouslyMarked?: boolean;
  readonly records: unknown[];
}) => {
  const wallet = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
  const queueIssuer = scriptFromNative({ type: "sig", keyHash: owner });
  const proofIssuer = scriptFromNative({
    type: "all",
    scripts: [{ type: "sig", keyHash: owner }],
  });
  const queuePolicy = mintingPolicyToId(queueIssuer);
  const proofPolicy = mintingPolicyToId(proofIssuer);
  const spend = {
    type: "PlutusV3" as const,
    script: SDK.applyBlueprintParams(blueprint, "state_queue.spend.spend", [
      queuePolicy,
      "aa".repeat(28),
      "bb".repeat(28),
      proofPolicy,
    ]),
  };
  const address = validatorToAddress("Custom", spend);
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  // Keep the production 60-second lower-bound backoff after emulator genesis.
  emulator.awaitSlot(100);
  const header = makeHeader(owner, emulator.now() - 10_000);
  const headerHash = Effect.runSync(SDK.hashBlockHeader(header));
  const proofAsset = "00000001" + headerHash;
  const unit = toUnit(queuePolicy, fromText("MBLC") + headerHash);
  const node: SDK.StateQueueNode = {
    header,
    block_time: header.endTime,
    da_attestation: SDK.NO_DA_ATTESTATION,
    proven_fraud: previouslyMarked ? proofAsset : null,
  };
  const view: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: { Key: { key: "ff".repeat(28) } },
    data: SDK.castStateQueueNodeToData(node) as SDK.LinkedListNodeView["data"],
  };
  const datum = SDK.encodeLinkedListNodeView(view);
  const submit = async (label: string, tx: TxSignBuilder) => {
    const signed = await tx.sign.withWallet().complete();
    const cbor = signed.toCBOR();
    const measurement = measureCompleteSignedTransaction(cbor);
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    expect(measurement.executionMemory).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    );
    expect(measurement.executionSteps).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
    );
    const txHash = await signed.submit().catch((cause: unknown) => {
      throw new Error(`${label}: ${inspect(cause, { depth: 12 })}`);
    });
    await lucid.awaitTx(txHash);
    records.push({
      label,
      queuePolicy,
      proofPolicy,
      address,
      headerHash,
      previouslyMarked,
      measurement,
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
      txHash,
      transactionCbor: cbor,
    });
    return txHash;
  };
  await submit(
    "fixture-queue-issuance",
    await lucid
      .newTx()
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(queueIssuer)
      .pay.ToContract(
        address,
        { kind: "inline", value: datum },
        { lovelace: 10_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true }),
  );
  const hubAddress = validatorToAddress("Custom", queueIssuer);
  const hubUnit = toUnit(queuePolicy, SDK.HUB_ORACLE_ASSET_NAME);
  const addr = Effect.runSync(SDK.addressDataFromBech32(address));
  const hubDatum: SDK.HubOracleDatum = {
    registered_operators: queuePolicy,
    active_operators: queuePolicy,
    retired_operators: queuePolicy,
    scheduler: queuePolicy,
    state_queue: queuePolicy,
    fraud_proof_catalogue: queuePolicy,
    fraud_proof: proofPolicy,
    deposit: queuePolicy,
    withdrawal: queuePolicy,
    tx_order: queuePolicy,
    settlement: queuePolicy,
    payout: queuePolicy,
    registered_operators_addr: addr,
    active_operators_addr: addr,
    retired_operators_addr: addr,
    scheduler_addr: addr,
    state_queue_addr: addr,
    fraud_proof_catalogue_addr: addr,
    fraud_proof_addr: addr,
    deposit_addr: addr,
    withdrawal_addr: addr,
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: queuePolicy,
  };
  await submit(
    "fixture-terminal-hub-and-reference",
    await lucid
      .newTx()
      .mintAssets({ [hubUnit]: 1n })
      .attach.MintingPolicy(queueIssuer)
      .pay.ToContract(
        hubAddress,
        { kind: "inline", value: Data.to(hubDatum, SDK.HubOracleDatum) },
        { lovelace: 10_000_000n, [hubUnit]: 1n },
      )
      .pay.ToAddressWithData(
        wallet.address,
        undefined,
        { lovelace: 50_000_000n },
        spend,
      )
      .complete({ localUPLCEval: true }),
  );
  const queueReferenceScript = (await lucid.utxosAt(wallet.address)).find(
    (u) => u.scriptRef?.script === spend.script,
  )!;
  expect(queueReferenceScript).toBeDefined();
  const input = await lucid.utxoByUnit(unit);
  return {
    wallet,
    owner,
    lucid,
    emulator,
    spend,
    address,
    queueIssuer,
    queuePolicy,
    proofIssuer,
    proofPolicy,
    queueReferenceScript,
    input,
    unit,
    node,
    view,
    submit,
    proofAsset,
    header,
    headerHash,
  };
};
