import { readFileSync } from "node:fs";
import { inspect } from "node:util";

import {
  addressDataFromBech32,
  applyEventHistoryValidators,
  eventHistoryKey,
  type EventHistoryKind,
  EventHistoryNode,
  hashHexWithBlake2b,
  HubOracleDatum,
  OutputReference,
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
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

export const blueprintBytes = readFileSync(realBlueprintPath);

const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);

export const records: unknown[] = [];

const same = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

const sorted = (xs: UTxO[]) =>
  [...xs].sort(
    (a, b) => a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
  );

export const index = (xs: UTxO[], x: UTxO) => {
  const result = sorted(xs).findIndex((v) => same(v, x));
  if (result < 0) throw new Error("Missing indexed UTxO");
  return BigInt(result);
};

// Lucid's emulator advances one second per slot. Preserve at least the original
// two-block confirmation interval and cross the actual node protection boundary.
export const awaitProtection = (emulator: Emulator, protectedUntil: bigint) => {
  emulator.awaitSlot(
    Math.max(
      40,
      Math.ceil((Number(protectedUntil) - emulator.now()) / 1000) + 1,
    ),
  );
  expect(BigInt(emulator.now())).toBeGreaterThan(protectedUntil);
};

export const candidateBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
};

// Prior exploratory recipes are retained only in explicitly named diagnostic cases.
export const exploratoryBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15000n,
  maxPayloadNodes: 1024n,
};

export const setup = async (
  kind: EventHistoryKind,
  payloadBounds = candidateBounds,
) => {
  const wallet = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
  const issuer = scriptFromNative({ type: "sig", keyHash: owner });
  const hubPolicy = mintingPolicyToId(issuer);
  const hubAddress = validatorToAddress("Custom", issuer);
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
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
    const txHash = await signed.submit().catch((error: unknown) => {
      throw new Error(`${label}: ${inspect(error, { depth: 8 })}`);
    });
    await lucid.awaitTx(txHash);
    records.push({
      kind,
      label,
      measurement,
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
      txHash,
      transactionCbor: cbor,
    });
    return txHash;
  };
  const splitHash = await submit(
    "reserve-nonces",
    await lucid
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true }),
  );
  const [initNonce, ...eventNonces] = await lucid.utxosByOutRef([
    { txHash: splitHash, outputIndex: 0 },
    { txHash: splitHash, outputIndex: 1 },
    { txHash: splitHash, outputIndex: 2 },
  ]);
  const keyedNonces = await Promise.all(
    eventNonces.map(async (nonce) => ({
      nonce,
      key: await Effect.runPromise(
        eventHistoryKey({
          transactionId: nonce.txHash,
          outputIndex: BigInt(nonce.outputIndex),
        }),
      ),
    })),
  );
  keyedNonces.sort((a, b) => a.key.localeCompare(b.key));
  const predecessorNonce = keyedNonces[0]!.nonce;
  const eventNonce = keyedNonces[1]!.nonce;
  const recipe = {
    hubPolicyId: hubPolicy,
    kind,
    initializationNonce: {
      transactionId: initNonce.txHash,
      outputIndex: BigInt(initNonce.outputIndex),
    },
    protectionDurationMs: 120_000n,
    ...payloadBounds,
  };
  const applied = applyEventHistoryValidators(blueprint, "Custom", recipe);
  const counterpart = applyEventHistoryValidators(blueprint, "Custom", {
    ...recipe,
    kind: kind === "Deposit" ? "Withdrawal" : "Deposit",
  });
  records.push({
    kind,
    label: "parameters",
    recipe,
    policyId: applied.policyId,
    address: applied.address,
    retentionAddress: applied.retention.address,
    scriptBytes: applied.validator.script.length / 2,
  });
  const addr = Effect.runSync(addressDataFromBech32(hubAddress));
  const listAddr = Effect.runSync(addressDataFromBech32(applied.address));
  const hubDatum: HubOracleDatum = {
    registered_operators: hubPolicy,
    active_operators: hubPolicy,
    retired_operators: hubPolicy,
    scheduler: hubPolicy,
    state_queue: hubPolicy,
    fraud_proof_catalogue: hubPolicy,
    fraud_proof: hubPolicy,
    deposit: kind === "Deposit" ? applied.policyId : counterpart.policyId,
    withdrawal: kind === "Withdrawal" ? applied.policyId : counterpart.policyId,
    tx_order: hubPolicy,
    settlement: hubPolicy,
    payout: hubPolicy,
    registered_operators_addr: addr,
    active_operators_addr: addr,
    retired_operators_addr: addr,
    scheduler_addr: addr,
    state_queue_addr: addr,
    fraud_proof_catalogue_addr: addr,
    fraud_proof_addr: addr,
    deposit_addr:
      kind === "Deposit"
        ? listAddr
        : Effect.runSync(addressDataFromBech32(counterpart.address)),
    withdrawal_addr:
      kind === "Withdrawal"
        ? listAddr
        : Effect.runSync(addressDataFromBech32(counterpart.address)),
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: hubPolicy,
  };
  const hubUnit = toUnit(hubPolicy, fromText("MIDGARD_HUB_ORACLE"));
  const funding = async () =>
    (await lucid.wallet().getUtxos()).filter(
      (u) =>
        !same(u, initNonce) &&
        !same(u, eventNonce) &&
        !same(u, predecessorNonce),
    );
  await submit(
    "publish-list-script-and-hub",
    await lucid
      .newTx()
      .collectFrom(await funding())
      .mintAssets({ [hubUnit]: 1n })
      .attach.MintingPolicy(issuer)
      .pay.ToContract(
        hubAddress,
        { kind: "inline", value: Data.to(hubDatum, HubOracleDatum) },
        { lovelace: 10_000_000n, [hubUnit]: 1n },
      )
      .pay.ToAddressWithData(
        hubAddress,
        undefined,
        { lovelace: 100_000_000n },
        applied.validator,
      )
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const references = await lucid.utxosAt(hubAddress);
  const hub = references.find((u) => u.assets[hubUnit] === 1n)!;
  const script = references.find((u) => u.scriptRef !== undefined)!;
  const bounds = () => {
    const lower = emulator.now();
    const validTo = lower + 10_000;
    return {
      lower,
      validTo,
      upper: BigInt(validTo - 1),
      protectedUntil: BigInt(validTo - 1) + recipe.protectionDurationMs,
    };
  };
  await submit(
    "register-observer",
    await lucid
      .newTx()
      .collectFrom(await funding())
      .readFrom([script])
      .register.Stake(applied.rewardAddress, Data.void())
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const b = bounds();
  const inputs = [...(await funding()), initNonce];
  const root: EventHistoryNode = {
    position: "Root",
    next: null,
    protected_until: b.protectedUntil,
    payload: "RootContent",
  };
  await submit(
    "initialize",
    await lucid
      .newTx()
      .collectFrom(inputs)
      .readFrom([script])
      .withdraw(
        applied.rewardAddress,
        0n,
        Data.to(new Constr(0, [index(inputs, initNonce), 0n])),
      )
      .mintAssets({ [applied.policyId]: 1n }, Data.void())
      .pay.ToContract(
        applied.address,
        { kind: "inline", value: Data.to(root, EventHistoryNode) },
        { lovelace: 3_000_000n, [applied.policyId]: 1n },
      )
      .validFrom(b.lower)
      .validTo(b.validTo)
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  awaitProtection(emulator, root.protected_until);
  const originalId: OutputReference = {
    transactionId: eventNonce.txHash,
    outputIndex: BigInt(eventNonce.outputIndex),
  };
  const key = await Effect.runPromise(
    hashHexWithBlake2b(Data.to(originalId, OutputReference), 32),
  );
  return {
    lucid,
    emulator,
    applied,
    owner,
    wallet,
    submit,
    funding,
    bounds,
    hub,
    script,
    eventNonce,
    predecessorNonce,
    recipe,
    key,
    originalId,
    issuer,
    hubPolicy,
    hubAddress,
  };
};

export type RetirementFault = "before-inclusion" | "wrong-confirmed-token";

export type AdmissionFault =
  | "hash-only"
  | "same-transaction-publication"
  | "wrong-retention-address"
  | "mismatched-datum-hash"
  | "backdated-inclusion"
  | "short-key"
  | "oversized-key";
