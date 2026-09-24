/** Provisional list lifecycle and fit scenarios; hub authority is fixture-issued. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { inspect } from "node:util";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  addressDataFromBech32,
  applyEventHistoryValidators,
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  commitCountedRootProgram,
  ConfirmedState,
  countHistoryDataNodes,
  DepositInfo,
  EMPTY_MERKLE_TREE_ROOT,
  encodeEventHistoryData,
  EventHistoryData,
  eventHistoryDataHash,
  eventHistoryKey,
  type EventHistoryKind,
  EventHistoryNode,
  EventHistoryObserve,
  EventHistoryPayload,
  eventHistoryRetirementOperation,
  EventHistoryRetirementWitness,
  eventHistoryWithdrawalFunding,
  type EventHistoryWitness,
  fetchEventHistoryWitness,
  hashHexWithBlake2b,
  HubOracleDatum,
  OutputReference,
  parseFaultProofBlueprint,
  PayoutDatum,
  prepareEventHistoryPayload,
  Proof,
  SettlementDatum,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  CML,
  Constr,
  credentialToAddress,
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
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
const same = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;
const sorted = (xs: UTxO[]) =>
  [...xs].sort(
    (a, b) => a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
  );
const index = (xs: UTxO[], x: UTxO) => {
  const result = sorted(xs).findIndex((v) => same(v, x));
  if (result < 0) throw new Error("Missing indexed UTxO");
  return BigInt(result);
};

const candidateBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
};
// Prior exploratory recipes are retained only in explicitly named diagnostic cases.
const exploratoryBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15000n,
  maxPayloadNodes: 1024n,
};
const setup = async (
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
    protectionDurationMs: 2_000n,
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
  emulator.awaitBlock(2);
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

const journey = async (
  kind: EventHistoryKind,
  external: boolean,
  retirement?: "settle" | "refund",
  suppliedDatum?:
    | Data
    | "inline-boundary"
    | "node-boundary"
    | "combined-boundary",
  rejectAdmission = false,
  branchLevels = 0,
  bounds = candidateBounds,
  maximumPredecessor = false,
  fundingFault?:
    | "node"
    | "deposit-reserve"
    | "withdrawal-target"
    | "withdrawal-payout"
    | "withdrawal-refund",
) => {
  const h = await setup(kind, bounds);
  const buildContext = async () => ({
    lucid: h.lucid,
    applied: h.applied,
    recipe: h.recipe,
    hubReference: h.hub,
    scriptReference: h.script,
    fundingInputs: await h.funding(),
  });
  const ownerAddress = Effect.runSync(addressDataFromBech32(h.wallet.address));
  const makePayload = (datum: Data | null): EventHistoryPayload =>
    kind === "Deposit"
      ? {
          DepositPayload: {
            event: {
              id: h.originalId,
              info: {
                l2_address: ownerAddress,
                l2_network_id: 0n,
                l2_datum: datum,
              },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id: h.originalId,
              info: {
                body: {
                  l2_outref: h.originalId,
                  l2_owner: h.owner,
                  l2_value: new Map([
                    [
                      "",
                      new Map([
                        [
                          "",
                          retirement === undefined ? 20_000_000n : 100_000_000n,
                        ],
                      ]),
                    ],
                  ]),
                  l1_address: ownerAddress,
                  l1_datum:
                    datum !== null
                      ? { InlineDatum: { data: datum } }
                      : "NoDatum",
                },
                signature: ["55".repeat(32), "66".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: ownerAddress,
            refund_datum: "NoDatum",
          },
        };
  let payload = makePayload(
    suppliedDatum === "inline-boundary"
      ? ""
      : suppliedDatum === "node-boundary" ||
          suppliedDatum === "combined-boundary"
        ? []
        : (suppliedDatum ?? (external ? "ab".repeat(4000) : null)),
  );
  const encodedPayload = (value: EventHistoryPayload) =>
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(value, EventHistoryPayload),
    );
  if (suppliedDatum === "inline-boundary") {
    let padding = 0;
    while (encodedPayload(payload).length / 2 < 512)
      payload = makePayload("ab".repeat(++padding));
    expect(encodedPayload(payload).length / 2).toBe(512);
  }
  if (
    suppliedDatum === "node-boundary" ||
    suppliedDatum === "combined-boundary"
  ) {
    const baseNodes = countHistoryDataNodes(
      Data.from(Data.to(payload, EventHistoryPayload)),
      h.recipe.maxPayloadNodes,
    );
    payload = makePayload(
      Array.from(
        { length: Number(h.recipe.maxPayloadNodes - baseNodes) },
        () => 0n,
      ),
    );
    expect(
      countHistoryDataNodes(
        Data.from(Data.to(payload, EventHistoryPayload)),
        h.recipe.maxPayloadNodes,
      ),
    ).toBe(h.recipe.maxPayloadNodes);
  }
  if (suppliedDatum === "combined-boundary") {
    const base = countHistoryDataNodes(
      Data.from(Data.to(makePayload([]), EventHistoryPayload)),
      h.recipe.maxPayloadNodes,
    );
    const zeros = Array.from(
      { length: Number(h.recipe.maxPayloadNodes - base - 1n) },
      () => 0n,
    );
    let lo = 0,
      hi = Number(h.recipe.maxPayloadBytes);
    while (lo < hi) {
      const mid = Math.ceil((lo + hi) / 2);
      if (
        encodedPayload(makePayload([...zeros, "ab".repeat(mid)])).length / 2 <=
        Number(h.recipe.maxPayloadBytes)
      )
        lo = mid;
      else hi = mid - 1;
    }
    const missing =
      Number(h.recipe.maxPayloadBytes) -
      encodedPayload(makePayload([...zeros, "ab".repeat(lo)])).length / 2;
    for (let i = 0; i < missing; i++) zeros[i] = 24n;
    payload = makePayload([...zeros, "ab".repeat(lo)]);
    expect(encodedPayload(payload).length / 2).toBe(
      Number(h.recipe.maxPayloadBytes),
    );
    expect(
      countHistoryDataNodes(
        Data.from(Data.to(payload, EventHistoryPayload)),
        h.recipe.maxPayloadNodes,
      ),
    ).toBe(h.recipe.maxPayloadNodes);
  }
  if ("WithdrawalPayload" in payload) {
    if (fundingFault === "withdrawal-target")
      payload.WithdrawalPayload.event.info.body.l2_value = new Map([
        ["", new Map([["", 2_000_000n]])],
      ]);
    if (fundingFault === "withdrawal-refund")
      payload.WithdrawalPayload.refund_datum = {
        InlineDatum: { data: "ab".repeat(4000) },
      };
  }
  if (!rejectAdmission) {
    const plan = prepareEventHistoryPayload(
      payload,
      { PublicKeyCredential: [h.owner] },
      h.recipe,
    );
    expect(plan.kind).toBe(external ? "External" : "Inline");
    expect(plan.key).toBe(h.key);
  }
  records.push({
    kind,
    label: "payload-shape",
    external,
    payloadBytes: encodedPayload(payload).length / 2,
    datumShape:
      suppliedDatum === undefined
        ? "default"
        : suppliedDatum === "inline-boundary" ||
            suppliedDatum === "node-boundary" ||
            suppliedDatum === "combined-boundary"
          ? suppliedDatum
          : typeof suppliedDatum === "string"
            ? "bytes"
            : Array.isArray(suppliedDatum)
              ? "list"
              : "constructed",
  });
  let externalRef: UTxO | undefined;
  const stored: EventHistoryData = {
    event_key: h.key,
    event_payload: Data.from(Data.to(payload, EventHistoryPayload)),
    reclaim_auth: { PublicKeyCredential: [h.owner] },
  };
  if (external) {
    // Rejected-data cases deliberately bypass SDK preflight to test the validator.
    const publication = rejectAdmission
      ? h.lucid
          .newTx()
          .collectFrom(await h.funding())
          .pay.ToContract(
            h.applied.retention.address,
            { kind: "inline", value: encodeEventHistoryData(stored) },
            { lovelace: 30_000_000n },
          )
          .complete({ coinSelection: false, localUPLCEval: true })
      : (
          await buildEventHistoryPublication(await buildContext(), payload, {
            PublicKeyCredential: [h.owner],
          })
        ).tx;
    await h.submit("prepublish-payload", await publication);
    [externalRef] = await h.lucid.utxosAt(h.applied.retention.address);
  }
  if (maximumPredecessor) {
    let padding = 0;
    let predecessorPayload = makePayload("");
    while (
      encodedPayload(predecessorPayload).length / 2 <
      Number(h.recipe.inlineLimitBytes)
    )
      predecessorPayload = makePayload("ab".repeat(++padding));
    const predecessorId = {
      transactionId: h.predecessorNonce.txHash,
      outputIndex: BigInt(h.predecessorNonce.outputIndex),
    };
    if ("DepositPayload" in predecessorPayload)
      predecessorPayload.DepositPayload.event.id = predecessorId;
    else predecessorPayload.WithdrawalPayload.event.id = predecessorId;
    expect(encodedPayload(predecessorPayload).length / 2).toBe(
      Number(h.recipe.inlineLimitBytes),
    );
    const p = h.bounds();
    const built = await buildEventHistoryAdmission(await buildContext(), {
      payload: predecessorPayload,
      reclaimAuth: { PublicKeyCredential: [h.owner] },
      nonce: h.predecessorNonce,
      assets: { lovelace: 10_000_000n },
      structuralLovelace: kind === "Deposit" ? 3_000_000n : 0n,
      structuralRefundKey: h.owner,
      validFrom: p.lower,
      validTo: p.validTo,
    });
    await h.submit("admit-maximum-inline-predecessor", built.tx);
    h.emulator.awaitBlock(2);
  }
  const beforeInsert = await fetchEventHistoryWitness(
    h.lucid,
    {
      policyId: h.applied.policyId,
      address: h.applied.address,
      retentionAddress: h.applied.retention.address,
      inlineLimitBytes: h.recipe.inlineLimitBytes,
    },
    h.originalId,
  );
  expect(beforeInsert.kind).toBe("Absent");
  const rootUtxo = beforeInsert.anchor.utxo;
  const root = Data.from(rootUtxo.datum!, EventHistoryNode);
  const b = h.bounds();
  const inputs = [rootUtxo, ...(await h.funding())];
  const refs = [h.hub, h.script];
  const filler: EventHistoryNode = {
    position: { Key: [h.key] },
    next: null,
    protected_until: b.protectedUntil,
    payload: { Filler: { refund_key: h.owner } },
  };
  await h.submit(
    "insert-filler",
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .collectFrom([rootUtxo], Data.to(index(inputs, rootUtxo)))
      .readFrom(refs)
      .withdraw(
        h.applied.rewardAddress,
        0n,
        Data.to(
          new Constr(1, [
            index(refs, h.hub),
            new Constr(0, [index(inputs, rootUtxo), 0n, 1n]),
          ]),
        ),
      )
      .mintAssets({ [toUnit(h.applied.policyId, h.key)]: 1n }, Data.void())
      .pay.ToContract(
        h.applied.address,
        {
          kind: "inline",
          value: Data.to(
            { ...root, next: h.key, protected_until: b.protectedUntil },
            EventHistoryNode,
          ),
        },
        rootUtxo.assets,
      )
      .pay.ToContract(
        h.applied.address,
        { kind: "inline", value: Data.to(filler, EventHistoryNode) },
        { lovelace: 5_000_000n, [toUnit(h.applied.policyId, h.key)]: 1n },
      )
      .validFrom(b.lower)
      .validTo(b.validTo)
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  h.emulator.awaitBlock(2);
  const fillerUtxo = (await h.lucid.utxosAt(h.applied.address)).find(
    (u) => u.assets[toUnit(h.applied.policyId, h.key)] === 1n,
  )!;
  const next = h.bounds();
  const node: EventHistoryNode = {
    ...filler,
    protected_until: next.protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: h.originalId,
          inclusion_time: next.upper + 60_000n,
          location: external
            ? { External: { storage_datum_hash: eventHistoryDataHash(stored) } }
            : { Inline: { payload } },
          structural_lovelace:
            fundingFault === "deposit-reserve"
              ? 9_999_999n
              : kind === "Deposit"
                ? 3_000_000n
                : 0n,
          structural_refund_key: h.owner,
        },
      },
    },
  };
  let orderLovelace = 10_000_000n;
  if ("WithdrawalPayload" in payload) {
    // Converge after ADA's CBOR width changes at the funding floor.
    for (;;) {
      const floor = eventHistoryWithdrawalFunding(payload, orderLovelace);
      const required =
        floor.payoutMinimum > floor.refundMinimum
          ? floor.payoutMinimum
          : floor.refundMinimum;
      if (orderLovelace >= required) break;
      orderLovelace = required;
    }
  }
  if (retirement === "settle" && "WithdrawalPayload" in payload) {
    const body = payload.WithdrawalPayload.event.info.body;
    const payoutDatum = Data.to(
      {
        l2_value: body.l2_value,
        l1_address: body.l1_address,
        l1_datum: body.l1_datum,
      },
      PayoutDatum,
    );
    const minimum = calculateMinLovelaceFromUTxO(
      EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
      {
        txHash: "00".repeat(32),
        outputIndex: 0,
        address: h.hubAddress,
        datum: payoutDatum,
        assets: { lovelace: orderLovelace, [toUnit(h.hubPolicy, h.key)]: 1n },
      },
    );
    records.push({
      kind,
      label: "payout-minimum-funding",
      minimumLovelace: minimum,
      payoutDatumBytes: payoutDatum.length / 2,
    });
    if (minimum > orderLovelace) orderLovelace = minimum;
  }
  if (fundingFault === "node") {
    orderLovelace = calculateMinLovelaceFromUTxO(
      EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
      {
        txHash: "00".repeat(32),
        outputIndex: 0,
        address: h.applied.address,
        datum: Data.to(node, EventHistoryNode),
        assets: {
          lovelace: 10_000_000n,
          [toUnit(h.applied.policyId, h.key)]: 1n,
        },
      },
    );
  }
  if (
    fundingFault === "withdrawal-payout" ||
    fundingFault === "withdrawal-refund"
  )
    orderLovelace = 10_000_000n;
  const promotionInputs = [fillerUtxo, h.eventNonce, ...(await h.funding())];
  const promotionRefs = [
    h.hub,
    h.script,
    ...(externalRef === undefined ? [] : [externalRef]),
  ];
  const extIndex =
    externalRef === undefined
      ? new Constr(1, [])
      : new Constr(0, [index(promotionRefs, externalRef)]);
  const promotion = async () =>
    h.lucid
      .newTx()
      .collectFrom([...(await h.funding()), h.eventNonce])
      .collectFrom([fillerUtxo], Data.to(index(promotionInputs, fillerUtxo)))
      .readFrom(promotionRefs)
      .withdraw(
        h.applied.rewardAddress,
        0n,
        Data.to(
          new Constr(1, [
            index(promotionRefs, h.hub),
            new Constr(2, [
              index(promotionInputs, fillerUtxo),
              0n,
              1n,
              index(promotionInputs, h.eventNonce),
              extIndex,
            ]),
          ]),
        ),
      )
      .pay.ToContract(
        h.applied.address,
        { kind: "inline", value: Data.to(node, EventHistoryNode) },
        { lovelace: orderLovelace, [toUnit(h.applied.policyId, h.key)]: 1n },
      )
      .pay.ToAddress(
        credentialToAddress("Custom", { type: "Key", hash: h.owner }),
        { lovelace: 5_000_000n },
      )
      .validFrom(next.lower)
      .validTo(next.validTo)
      .complete({ coinSelection: false, localUPLCEval: true });
  if (rejectAdmission) {
    const refusal = await promotion().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(inspect(refusal, { depth: 10 })).toMatch(/failed script execution/);
    records.push({
      kind,
      label: "admission-refusal",
      fundingFault: fundingFault ?? null,
      cause: inspect(refusal, { depth: 10 }),
    });
    expect(await h.lucid.utxosByOutRef([h.eventNonce])).toHaveLength(1);
    expect(await h.lucid.utxosByOutRef([fillerUtxo])).toHaveLength(1);
    if (!external) return;
    expect(externalRef).toBeDefined();
    if (externalRef === undefined)
      throw new Error("Missing rejection storage fixture");
    const reclaimRefs = [fillerUtxo, h.hub];
    await h.submit(
      "reclaim-rejected-data",
      await h.lucid
        .newTx()
        .collectFrom(
          [externalRef],
          Data.to(
            new Constr(0, [
              index(reclaimRefs, fillerUtxo),
              index(reclaimRefs, h.hub),
            ]),
          ),
        )
        .readFrom(reclaimRefs)
        .attach.SpendingValidator(h.applied.retention.validator)
        .addSignerKey(h.owner)
        .complete({ localUPLCEval: true }),
    );
    expect(await h.lucid.utxosByOutRef([externalRef])).toHaveLength(0);
    return;
  }
  const built = await buildEventHistoryAdmission(await buildContext(), {
    payload,
    reclaimAuth: { PublicKeyCredential: [h.owner] },
    nonce: h.eventNonce,
    assets: { lovelace: orderLovelace },
    structuralLovelace: kind === "Deposit" ? 3_000_000n : 0n,
    structuralRefundKey: h.owner,
    externalData: externalRef,
    validFrom: next.lower,
    validTo: next.validTo,
  });
  expect(built.node).toEqual(node);
  await h.submit("promote-filler-to-order", built.tx);
  const order = (await h.lucid.utxosAt(h.applied.address)).find(
    (u) => u.assets[toUnit(h.applied.policyId, h.key)] === 1n,
  )!;
  expect(Data.from(order.datum!, EventHistoryNode)).toEqual(node);
  expect(await h.lucid.utxosByOutRef([h.eventNonce])).toHaveLength(0);
  const deployment = {
    policyId: h.applied.policyId,
    address: h.applied.address,
    retentionAddress: h.applied.retention.address,
    inlineLimitBytes: 512n,
  };
  const before = await fetchEventHistoryWitness(
    h.lucid,
    deployment,
    h.originalId,
  );
  expect(before.kind).toBe("Present");
  if (before.kind !== "Present") throw new Error("Missing admitted event");
  expect(before.payload).toEqual(payload);
  expect(before.retainedDataUtxo !== undefined).toBe(external);
  h.emulator.awaitBlock(2);
  const continuedBounds = h.bounds();
  const successor = "ff".repeat(32);
  const continuationInputs = [order, ...(await h.funding())];
  const continuationRefs = [h.hub, h.script];
  const continuedNode: EventHistoryNode = {
    ...node,
    next: successor,
    protected_until: continuedBounds.protectedUntil,
  };
  const successorNode: EventHistoryNode = {
    position: { Key: [successor] },
    next: null,
    protected_until: continuedBounds.protectedUntil,
    payload: { Filler: { refund_key: h.owner } },
  };
  await h.submit(
    "continue-order-pointer",
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .collectFrom([order], Data.to(index(continuationInputs, order)))
      .readFrom(continuationRefs)
      .withdraw(
        h.applied.rewardAddress,
        0n,
        Data.to(
          new Constr(1, [
            index(continuationRefs, h.hub),
            new Constr(0, [index(continuationInputs, order), 0n, 1n]),
          ]),
        ),
      )
      .mintAssets({ [toUnit(h.applied.policyId, successor)]: 1n }, Data.void())
      .pay.ToContract(
        h.applied.address,
        { kind: "inline", value: Data.to(continuedNode, EventHistoryNode) },
        order.assets,
      )
      .pay.ToContract(
        h.applied.address,
        { kind: "inline", value: Data.to(successorNode, EventHistoryNode) },
        { lovelace: 5_000_000n, [toUnit(h.applied.policyId, successor)]: 1n },
      )
      .validFrom(continuedBounds.lower)
      .validTo(continuedBounds.validTo)
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const after = await fetchEventHistoryWitness(
    h.lucid,
    deployment,
    h.originalId,
  );
  expect(after.kind).toBe("Present");
  if (after.kind !== "Present") throw new Error("Continuation lost its event");
  expect(after.anchor.utxo.txHash).not.toBe(before.anchor.utxo.txHash);
  expect(after.anchor.utxo.assets).toEqual(before.anchor.utxo.assets);
  expect(after.anchor.node.payload).toEqual(before.anchor.node.payload);
  expect(after.payloadCbor).toBe(before.payloadCbor);
  expect(after.retainedDataUtxo).toEqual(before.retainedDataUtxo);
  if (retirement !== undefined)
    await retireJourney(h, after, payload, retirement, branchLevels);
};

const retireJourney = async (
  h: Awaited<ReturnType<typeof setup>>,
  present: Extract<EventHistoryWitness, { kind: "Present" }>,
  payload: EventHistoryPayload,
  mode: "settle" | "refund",
  branchLevels: number,
) => {
  const deposit = "DepositPayload" in payload;
  const keyBytes = Buffer.from(Data.to(h.originalId, OutputReference), "hex");
  const info = deposit
    ? payload.DepositPayload.event.info
    : {
        ...payload.WithdrawalPayload.event.info,
        validity:
          mode === "refund"
            ? ("IncorrectWithdrawalSignature" as const)
            : ("WithdrawalIsValid" as const),
      };
  const infoCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    deposit
      ? Data.to(payload.DepositPayload.event.info, DepositInfo)
      : Data.to(info as WithdrawalInfo, WithdrawalInfo),
  );
  const domain = deposit ? "DepositsRootDomain" : "WithdrawalsRootDomain";
  const valueBytes = Buffer.from(infoCbor, "hex");
  const singleton = await buildCountedRoot(domain, [
    { key: keyBytes, value: valueBytes },
  ]);
  const deep =
    branchLevels === 0
      ? undefined
      : syntheticDeepMembershipProof({
          key: keyBytes,
          value: valueBytes,
          branchLevels,
        });
  const phasRoot = deep?.transactionsPhasRoot ?? singleton.phasRoot;
  // 15 sibling leaves per branch plus the selected leaf fits the 10k event cap.
  // This tests proof carriage and execution, not the cost of grinding keys.
  const count =
    deep === undefined ? singleton.count : BigInt(1 + 15 * branchLevels);
  const counted = {
    phasRoot,
    count,
    root: await Effect.runPromise(
      commitCountedRootProgram({ domain, phasRoot, count }),
    ),
  };
  const proof =
    deep === undefined
      ? await keyValuePhasProof(
          { ...singleton, root: phasRoot },
          keyBytes,
          valueBytes,
        )
      : Data.from(deep.proofCbor, Proof);
  records.push({
    kind: deposit ? "Deposit" : "Withdrawal",
    label: "membership-shape",
    branchLevels,
    proofBytes: Data.to(proof, Proof).length / 2,
  });
  const membership = { phas_root: phasRoot, count, proof };
  const retirementScript = h.applied.retirement.validator;
  const retirementHash = validatorToScriptHash(retirementScript);
  const retirementReward = h.applied.retirement.rewardAddress;
  const confirmedUnit = toUnit(
    h.hubPolicy,
    fromText("MIDGARD_CONFIRMED_STATE"),
  );
  const settlementUnit = toUnit(
    h.hubPolicy,
    fromText("event-history-settlement"),
  );
  if (
    present.anchor.node.payload === "RootContent" ||
    !("Order" in present.anchor.node.payload)
  )
    throw new Error("Missing order facts");
  const facts = present.anchor.node.payload.Order.facts;
  const confirmed = Data.from(
    Data.to(
      {
        headerHash: "01".repeat(28),
        prevHeaderHash: "02".repeat(28),
        utxoRoot: EMPTY_MERKLE_TREE_ROOT,
        startTime: 0n,
        endTime: facts.inclusion_time,
        protocolVersion: 1n,
      },
      ConfirmedState,
    ),
  );
  const rootDatum = Data.to(
    new Constr(0, [new Constr(0, [confirmed]), new Constr(1, [])]),
  );
  const settlementDatum = Data.to(
    {
      deposits_root: counted.root,
      withdrawals_root: counted.root,
      forced_transactions_root: EMPTY_MERKLE_TREE_ROOT,
      transactions_root: EMPTY_MERKLE_TREE_ROOT,
      resolution_claim: null,
    },
    SettlementDatum,
  );
  await h.submit(
    "publish-settlement-authority",
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .mintAssets({ [confirmedUnit]: 1n, [settlementUnit]: 1n })
      .attach.MintingPolicy(h.issuer)
      .register.Stake(retirementReward)
      .pay.ToContract(
        h.hubAddress,
        { kind: "inline", value: rootDatum },
        { lovelace: 5_000_000n, [confirmedUnit]: 1n },
      )
      .pay.ToContract(
        h.hubAddress,
        { kind: "inline", value: settlementDatum },
        { lovelace: 5_000_000n, [settlementUnit]: 1n },
      )
      .pay.ToAddressWithData(
        h.hubAddress,
        undefined,
        { lovelace: 20_000_000n },
        retirementScript,
      )
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const authorityUtxos = await h.lucid.utxosAt(h.hubAddress);
  const confirmedRef = authorityUtxos.find(
    (u) => u.assets[confirmedUnit] === 1n,
  )!;
  const settlementRef = authorityUtxos.find(
    (u) => u.assets[settlementUnit] === 1n,
  )!;
  const retirementRef = authorityUtxos.find(
    (u) =>
      u.scriptRef != null &&
      validatorToScriptHash(u.scriptRef) === retirementHash,
  )!;
  expect(retirementRef).toBeDefined();
  h.emulator.awaitBlock(2);
  const allNodes = await h.lucid.utxosAt(h.applied.address);
  const predecessor = allNodes.find(
    (u) =>
      u.datum != null && Data.from(u.datum, EventHistoryNode).next === h.key,
  )!;
  const beforeRoot = Data.from(predecessor.datum!, EventHistoryNode);
  const b = h.bounds();
  const continued = {
    ...beforeRoot,
    next: present.anchor.node.next,
    protected_until: b.protectedUntil,
  };
  const inputs = [predecessor, present.anchor.utxo, ...(await h.funding())];
  const refs = [
    h.hub,
    h.script,
    confirmedRef,
    settlementRef,
    retirementRef,
    ...(present.retainedDataUtxo === undefined
      ? []
      : [present.retainedDataUtxo]),
  ];
  const witness: EventHistoryRetirementWitness = {
    predecessor_input_index: index(inputs, predecessor),
    order_input_index: index(inputs, present.anchor.utxo),
    predecessor_output_index: 0n,
    funds_output_index: 1n,
    structural_refund_output_index: deposit ? 2n : null,
    confirmed_reference_index: index(refs, confirmedRef),
    settlement_reference_index: index(refs, settlementRef),
    external_reference_index:
      present.retainedDataUtxo === undefined
        ? null
        : index(refs, present.retainedDataUtxo),
    membership,
    purpose: deposit
      ? "AbsorbDeposit"
      : mode === "settle"
        ? "InitializeWithdrawalPayout"
        : {
            RefundInvalidWithdrawal: {
              validity: "IncorrectWithdrawalSignature",
            },
          },
  };
  let tx = h.lucid
    .newTx()
    .collectFrom(await h.funding())
    .collectFrom([predecessor], Data.to(index(inputs, predecessor)))
    .collectFrom(
      [present.anchor.utxo],
      Data.to(index(inputs, present.anchor.utxo)),
    )
    .readFrom(refs)
    .withdraw(
      h.applied.rewardAddress,
      0n,
      Data.to(
        {
          Apply: {
            hub_reference_index: index(refs, h.hub),
            operation: eventHistoryRetirementOperation(witness),
          },
        },
        EventHistoryObserve,
      ),
    )
    .withdraw(
      retirementReward,
      0n,
      Data.to(
        new Constr(0, [
          index(refs, h.hub),
          Data.from(Data.to(witness, EventHistoryRetirementWitness)),
        ]),
      ),
    )
    .pay.ToContract(
      h.applied.address,
      { kind: "inline", value: Data.to(continued, EventHistoryNode) },
      predecessor.assets,
    )
    .validFrom(b.lower)
    .validTo(b.validTo);
  if (deposit) {
    tx = tx.pay
      .ToAddress(h.hubAddress, {
        lovelace:
          present.anchor.utxo.assets.lovelace - facts.structural_lovelace,
      })
      .pay.ToAddress(
        credentialToAddress("Custom", { type: "Key", hash: h.owner }),
        { lovelace: facts.structural_lovelace },
      );
  } else if (mode === "refund") {
    tx = tx.pay.ToAddress(h.wallet.address, {
      lovelace: present.anchor.utxo.assets.lovelace,
    });
  } else {
    const body = payload.WithdrawalPayload.event.info.body;
    const payoutUnit = toUnit(h.hubPolicy, h.key);
    // Keep the serialized mint map in policy order as well as its redeemers.
    // The provider validates native policies when locating Mint indices too.
    if (h.hubPolicy < h.applied.policyId) {
      tx = tx
        .mintAssets({ [payoutUnit]: 1n })
        .mintAssets({ [toUnit(h.applied.policyId, h.key)]: -1n }, Data.void());
    } else {
      tx = tx
        .mintAssets({ [toUnit(h.applied.policyId, h.key)]: -1n }, Data.void())
        .mintAssets({ [payoutUnit]: 1n });
    }
    tx = tx.attach.MintingPolicy(h.issuer).pay.ToContract(
      h.hubAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            l2_value: body.l2_value,
            l1_address: body.l1_address,
            l1_datum: body.l1_datum,
          },
          PayoutDatum,
        ),
      },
      { lovelace: present.anchor.utxo.assets.lovelace, [payoutUnit]: 1n },
    );
  }
  if (deposit || mode === "refund")
    tx = tx.mintAssets(
      { [toUnit(h.applied.policyId, h.key)]: -1n },
      Data.void(),
    );
  await h.submit(
    mode === "refund" ? "refund-and-unlink" : "settle-and-unlink",
    await tx
      .complete({ coinSelection: false, localUPLCEval: true })
      .catch((error: unknown) => {
        throw new Error(
          `Retirement completion: ${inspect(error, { depth: 10 })}`,
        );
      }),
  );
  const absence = await fetchEventHistoryWitness(
    h.lucid,
    {
      policyId: h.applied.policyId,
      address: h.applied.address,
      retentionAddress: h.applied.retention.address,
      inlineLimitBytes: 512n,
    },
    h.originalId,
  );
  expect(absence.kind).toBe("Absent");
  if (present.retainedDataUtxo !== undefined) {
    const reclaimRefs = [absence.anchor.utxo, h.hub];
    await h.submit(
      "reclaim-retired-data",
      await h.lucid
        .newTx()
        .collectFrom(
          [present.retainedDataUtxo],
          Data.to(
            new Constr(0, [
              index(reclaimRefs, absence.anchor.utxo),
              index(reclaimRefs, h.hub),
            ]),
          ),
        )
        .readFrom(reclaimRefs)
        .attach.SpendingValidator(h.applied.retention.validator)
        .addSignerKey(h.owner)
        .complete({ localUPLCEval: true }),
    );
    expect(
      await h.lucid.utxosByOutRef([present.retainedDataUtxo]),
    ).toHaveLength(0);
  }
};

afterAll(() => {
  expect(readFileSync(realBlueprintPath).equals(blueprintBytes)).toBe(true);
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "list-emulator.json"),
    JSON.stringify(
      {
        scope:
          "Applied initialization, admission, pointer continuation, retirement and reclamation; fixture hub/finality/settlement/payout mint authorities; provisional recipe recorded per scenario; exploratory bounds explicitly marked in test names; not production queue or payout-spend acceptance",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

// The external-payload diagnostics settle 1024-node and 14000-byte payloads
// through the emulator and take several seconds each.
describe("applied authenticated event lists", { timeout: 60_000 }, () => {
  it.each(["Deposit", "Withdrawal"] as const)(
    "admits inline %s through permissionless filler promotion",
    async (kind) => journey(kind, false),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "admits externally prepublished %s through permissionless filler promotion",
    async (kind) => journey(kind, true),
  );
  it.each([false, true])(
    "settles and unlinks deposit (external=%s)",
    async (external) => journey("Deposit", external, "settle"),
  );
  it.each([false, true])(
    "initializes payout and unlinks withdrawal (external=%s)",
    async (external) => journey("Withdrawal", external, "settle"),
  );
  it.each([false, true])(
    "refunds and unlinks invalid withdrawal (external=%s)",
    async (external) => journey("Withdrawal", external, "refund"),
  );

  it.each(["Deposit", "Withdrawal"] as const)(
    "settles exact inline boundary for %s",
    async (kind) => journey(kind, false, "settle", "inline-boundary"),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "diagnostic singleton settles 14000-byte external datum for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "ab".repeat(14000),
        false,
        0,
        exploratoryBounds,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "diagnostic singleton settles exact 1024-node external payload for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "node-boundary",
        false,
        0,
        exploratoryBounds,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "rejects and reclaims over-budget external payload for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        Array.from({ length: 14000 }, () => 0n),
        true,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with exact inline payload for %s",
    async (kind) =>
      journey(kind, false, "settle", "inline-boundary", false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with external bytes for %s",
    async (kind) => journey(kind, true, "settle", "ab".repeat(4000), false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with exact node boundary for %s",
    async (kind) => journey(kind, true, "settle", "node-boundary", false, 64),
  );

  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof at combined byte and node bounds for %s",
    async (kind) =>
      journey(kind, true, "settle", "combined-boundary", false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "rejects and reclaims oversized bytes under candidate bounds for %s",
    async (kind) => journey(kind, true, "settle", "ab".repeat(14000), true),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles maximum predecessor and 64-level proof at combined bounds for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "combined-boundary",
        false,
        64,
        candidateBounds,
        true,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "refuses a %s node funded only for its admission output",
    async (kind) =>
      journey(
        kind,
        false,
        undefined,
        undefined,
        true,
        0,
        candidateBounds,
        false,
        "node",
      ),
  );
  it("refuses a deposit whose structural ADA leaves an unspendable reserve Value", async () =>
    journey(
      "Deposit",
      false,
      undefined,
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "deposit-reserve",
    ));
  it("refuses locked withdrawal ADA above its target", async () =>
    journey(
      "Withdrawal",
      false,
      undefined,
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-target",
    ));
  it("refuses insufficient future payout ADA and reclaims unused data", async () =>
    journey(
      "Withdrawal",
      true,
      "settle",
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-payout",
    ));
  it("refuses insufficient future refund ADA and reclaims unused data", async () =>
    journey(
      "Withdrawal",
      true,
      "refund",
      "",
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-refund",
    ));
});
