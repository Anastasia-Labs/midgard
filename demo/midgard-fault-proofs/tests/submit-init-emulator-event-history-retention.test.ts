/**
 * Applied retention-script scenarios. The list issuer is a fixture native
 * policy: these cases establish retention spending behavior conditional on
 * authenticated list state, not production list admission or retirement.
 */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import {
  addressDataFromBech32,
  applyEventHistoryRetentionValidator,
  encodeEventHistoryData,
  EventHistoryData,
  eventHistoryDataHash,
  HubOracleDatum,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Constr,
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
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const key = "80".repeat(32);
const records: unknown[] = [];
type Witness = "root" | "filler" | "order" | "other-gap" | "foreign-policy";

const setup = async (
  witness: Witness = "root",
  scriptOwner = false,
  payloadBytes = 0,
) => {
  const wallet = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
  const issuer = scriptFromNative({ type: "sig", keyHash: owner });
  const policy = mintingPolicyToId(issuer);
  const listAddress = validatorToAddress("Custom", issuer);
  const applied = applyEventHistoryRetentionValidator(
    blueprint,
    "Custom",
    policy,
    "Deposit",
  );
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const root = witness === "root" || witness === "foreign-policy";
  const nodeKey = witness === "other-gap" ? "ff".repeat(32) : key;
  const position = root ? new Constr(0, []) : new Constr(1, [nodeKey]);
  const content = root
    ? new Constr(0, [])
    : witness === "order"
      ? new Constr(2, [42n])
      : new Constr(1, [owner]);
  const link = new Constr(0, [position, new Constr(1, []), 0n, content]);
  const datum: EventHistoryData = {
    event_key: key,
    event_payload: payloadBytes === 0 ? 42n : "ab".repeat(payloadBytes),
    reclaim_auth: scriptOwner
      ? { ScriptCredential: [policy] }
      : { PublicKeyCredential: [owner] },
  };
  const unit = toUnit(policy, root ? "" : nodeKey);
  const fixtureAddress = Effect.runSync(addressDataFromBech32(listAddress));
  const hubDatum: HubOracleDatum = {
    registered_operators: policy,
    active_operators: policy,
    retired_operators: policy,
    scheduler: policy,
    state_queue: policy,
    fraud_proof_catalogue: policy,
    fraud_proof: policy,
    deposit: witness === "foreign-policy" ? "99".repeat(28) : policy,
    withdrawal: policy,
    tx_order: policy,
    settlement: policy,
    payout: policy,
    registered_operators_addr: fixtureAddress,
    active_operators_addr: fixtureAddress,
    retired_operators_addr: fixtureAddress,
    scheduler_addr: fixtureAddress,
    state_queue_addr: fixtureAddress,
    fraud_proof_catalogue_addr: fixtureAddress,
    fraud_proof_addr: fixtureAddress,
    deposit_addr: fixtureAddress,
    withdrawal_addr: fixtureAddress,
    tx_order_addr: fixtureAddress,
    settlement_addr: fixtureAddress,
    payout_addr: fixtureAddress,
    reserve_addr: fixtureAddress,
    reserve_observer: policy,
  };
  const hubUnit = toUnit(policy, fromText("MIDGARD_HUB_ORACLE"));

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
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    records.push({
      label,
      witness,
      scriptOwner,
      payloadBytes,
      listPolicy: policy,
      listAddress,
      retentionAddress: applied.address,
      datumHash: eventHistoryDataHash(datum),
      measurement,
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
      txHash,
      transactionCbor: cbor,
    });
    return txHash;
  };
  let publication = lucid
    .newTx()
    .mintAssets({ [unit]: 1n, [hubUnit]: 1n })
    .attach.MintingPolicy(issuer)
    .pay.ToContract(
      listAddress,
      { kind: "inline", value: Data.to(link) },
      { lovelace: 3_000_000n, [unit]: 1n },
    )
    .pay.ToContract(
      applied.address,
      { kind: "inline", value: encodeEventHistoryData(datum) },
      { lovelace: payloadBytes > 0 ? 60_000_000n : 3_000_000n },
    );
  publication = publication.pay.ToContract(
    listAddress,
    { kind: "inline", value: Data.to(hubDatum, HubOracleDatum) },
    { lovelace: 10_000_000n, [hubUnit]: 1n },
  );
  const rewardAddress = validatorToRewardAddress("Custom", issuer);
  if (scriptOwner) publication = publication.register.Stake(rewardAddress);
  await submit(
    "publication",
    await publication.complete({ localUPLCEval: true }),
  );
  const [stored] = await lucid.utxosAt(applied.address);
  const references = await lucid.utxosAt(listAddress);
  const reference = references.find((utxo) => utxo.assets[unit] === 1n)!;
  const hubReference = references.find((utxo) => utxo.assets[hubUnit] === 1n)!;
  expect(stored).toBeDefined();
  expect(reference.assets[unit]).toBe(1n);
  const reclaim = async (authorize: boolean, withdrawalAmount = 0n) => {
    let tx = lucid
      .newTx()
      .collectFrom([stored], Data.to(new Constr(0, [0n, 1n])))
      .readFrom([reference, hubReference])
      .attach.SpendingValidator(applied.validator);
    if (authorize) {
      tx = scriptOwner
        ? tx
            .withdraw(rewardAddress, withdrawalAmount)
            .attach.WithdrawalValidator(issuer)
        : tx.addSignerKey(owner);
    }
    return tx.complete({ localUPLCEval: true });
  };
  return { lucid, stored, reference, applied, submit, reclaim };
};

afterAll(() => {
  expect(readFileSync(realBlueprintPath).equals(blueprintBytes)).toBe(true);
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "retention-emulator.json"),
    JSON.stringify(
      {
        scope:
          "Applied retention validator; fixture list issuer; no full event lifecycle acceptance",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

describe("event history retained data", () => {
  it.each(["root", "filler"] as const)(
    "reclaims with %s absence and exact required signer",
    async (witness) => {
      const h = await setup(witness);
      await h.submit("reclaim", await h.reclaim(true));
      expect(await h.lucid.utxosAt(h.applied.address)).toHaveLength(0);
    },
  );

  it("retains data while an Order occupies its key despite the owner's authorization", async () => {
    const h = await setup("order");
    await expect(h.reclaim(true)).rejects.toThrow();
    expect(await h.lucid.utxosAt(h.applied.address)).toHaveLength(1);
  });

  it("refuses a filler outside the target gap", async () => {
    const h = await setup("other-gap");
    await expect(h.reclaim(true)).rejects.toThrow();
  });

  it("refuses absence authenticated by another list policy", async () => {
    const h = await setup("foreign-policy");
    await expect(h.reclaim(true)).rejects.toThrow();
  });

  it("requires the exact required signer even when the wallet signs the transaction", async () => {
    const h = await setup();
    await expect(h.reclaim(false)).rejects.toThrow();
  });

  it("accepts exact zero-withdrawal script authorization", async () => {
    const h = await setup("root", true);
    await h.submit("reclaim-script", await h.reclaim(true));
    expect(await h.lucid.utxosAt(h.applied.address)).toHaveLength(0);
  });

  it("refuses script-owner reclamation without a rewarding invocation", async () => {
    const h = await setup("root", true);
    await expect(h.reclaim(false)).rejects.toThrow();
  });

  it("publishes and separately reclaims a 10000-byte payload within production limits", async () => {
    const h = await setup("root", false, 10_000);
    await h.submit("reclaim-large", await h.reclaim(true));
    expect(await h.lucid.utxosAt(h.applied.address)).toHaveLength(0);
  });
});
