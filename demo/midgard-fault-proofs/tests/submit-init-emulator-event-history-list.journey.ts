import { inspect } from "node:util";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  addressDataFromBech32,
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  countHistoryDataNodes,
  encodeEventHistoryData,
  EVENT_WAIT_DURATION_MS,
  EventHistoryData,
  eventHistoryDataHash,
  type EventHistoryKind,
  EventHistoryNode,
  EventHistoryPayload,
  eventHistoryWithdrawalFunding,
  fetchEventHistoryWitness,
  PayoutDatum,
  prepareEventHistoryPayload,
} from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Constr,
  credentialToAddress,
  Data,
  mintingPolicyToId,
  scriptFromNative,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { retireJourney } from "./submit-init-emulator-event-history-list.retire-journey.js";
import {
  type AdmissionFault,
  awaitProtection,
  candidateBounds,
  index,
  records,
  type RetirementFault,
  setup,
} from "./submit-init-emulator-event-history-list.setup.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

export const journey = async (
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
  admissionFault?: AdmissionFault,
  retirementFault?: RetirementFault,
  maximumOriginalValue = false,
) => {
  const h = await setup(kind, bounds);
  const wideAssets: Record<string, bigint> = {};
  if (maximumOriginalValue) {
    const quantity = 9_223_372_036_854_775_807n;
    const policies = Array.from({ length: 9 }, (_, i) =>
      scriptFromNative({
        type: "all",
        scripts: [
          { type: "sig", keyHash: h.owner },
          { type: "after", slot: i },
        ],
      }),
    ).sort((a, b) => mintingPolicyToId(a).localeCompare(mintingPolicyToId(b)));
    let mint = h.lucid.newTx().collectFrom(await h.funding());
    for (const policy of policies) {
      const unit = toUnit(mintingPolicyToId(policy), "ff".repeat(32));
      wideAssets[unit] = quantity;
      mint = mint.mintAssets({ [unit]: quantity }).attach.MintingPolicy(policy);
    }
    await h.submit(
      "mint-nine-wide-original-assets",
      await mint.validFrom(h.emulator.now()).complete({
        coinSelection: false,
        localUPLCEval: true,
      }),
    );
    const funding = await h.funding();
    for (const [unit, amount] of Object.entries(wideAssets))
      expect(
        funding.reduce((sum, utxo) => sum + (utxo.assets[unit] ?? 0n), 0n),
      ).toBe(amount);
    expect(
      new Set(Object.keys(wideAssets).map((unit) => unit.slice(0, 56))).size,
    ).toBe(9);
    records.push({
      kind,
      label: "maximum-original-value",
      assets: wideAssets,
      quantity,
      assetNameBytes: 32,
      distinctPolicies: 9,
      authorityScope:
        kind === "Deposit"
          ? "Actual minted original deposit Value, preserved through reserve absorption"
          : "Actual minted wallet assets; withdrawal target obligation only, ADA-only history funding and payout initialization, not reserve fulfillment",
    });
  }
  const orderTokens = kind === "Deposit" ? wideAssets : {};
  const buildContext = async () => ({
    lucid: h.lucid,
    applied: h.applied,
    recipe: h.recipe,
    hubReference: h.hub,
    scriptReference: h.script,
    fundingInputs: await h.funding(),
  });
  const ownerAddress = Effect.runSync(addressDataFromBech32(h.wallet.address));
  const makePayload = (
    datum: Data | null,
    includeMaximumValue = maximumOriginalValue,
  ): EventHistoryPayload =>
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
                    ...(includeMaximumValue
                      ? Object.entries(wideAssets).map(
                          ([unit, quantity]): [string, Map<string, bigint>] => [
                            unit.slice(0, 56),
                            new Map([[unit.slice(56), quantity]]),
                          ],
                        )
                      : []),
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
  if (
    external &&
    admissionFault !== "hash-only" &&
    admissionFault !== "same-transaction-publication"
  ) {
    // Rejected-data cases deliberately bypass SDK preflight to test the validator.
    const publication = rejectAdmission
      ? h.lucid
          .newTx()
          .collectFrom(await h.funding())
          .pay.ToContract(
            admissionFault === "wrong-retention-address"
              ? h.hubAddress
              : h.applied.retention.address,
            { kind: "inline", value: encodeEventHistoryData(stored) },
            { lovelace: 30_000_000n },
          )
          .complete({ coinSelection: false, localUPLCEval: true })
      : (
          await buildEventHistoryPublication(
            await buildContext(),
            payload,
            { PublicKeyCredential: [h.owner] },
            h.emulator.now() + 60_000,
          )
        ).tx;
    const publicationHash = await h.submit("prepublish", await publication);
    const publishedAddress =
      admissionFault === "wrong-retention-address"
        ? h.hubAddress
        : h.applied.retention.address;
    externalRef = (await h.lucid.utxosAt(publishedAddress)).find(
      (utxo) =>
        utxo.txHash === publicationHash &&
        utxo.datum === encodeEventHistoryData(stored),
    );
    expect(externalRef).toBeDefined();
    expect(externalRef!.datum).toBe(encodeEventHistoryData(stored));
  }
  if (maximumPredecessor) {
    let padding = 0;
    let predecessorPayload = makePayload("", false);
    while (
      encodedPayload(predecessorPayload).length / 2 <
      Number(h.recipe.inlineLimitBytes)
    )
      predecessorPayload = makePayload("ab".repeat(++padding), false);
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
    awaitProtection(h.emulator, p.protectedUntil);
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
  awaitProtection(h.emulator, filler.protected_until);
  const fillerUtxo = (await h.lucid.utxosAt(h.applied.address)).find(
    (u) => u.assets[toUnit(h.applied.policyId, h.key)] === 1n,
  )!;
  const next = h.bounds();
  const node: EventHistoryNode = {
    ...filler,
    position:
      admissionFault === "short-key"
        ? { Key: [h.key.slice(0, 62)] }
        : admissionFault === "oversized-key"
          ? { Key: [h.key + "00"] }
          : filler.position,
    protected_until: next.protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: h.originalId,
          inclusion_time:
            next.upper +
            BigInt(EVENT_WAIT_DURATION_MS) -
            (admissionFault === "backdated-inclusion" ? 1n : 0n),
          location: external
            ? {
                External: {
                  storage_datum_hash:
                    admissionFault === "mismatched-datum-hash"
                      ? "00".repeat(32)
                      : eventHistoryDataHash(stored),
                },
              }
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
  const malformedKey =
    admissionFault === "short-key"
      ? h.key.slice(0, 62)
      : admissionFault === "oversized-key"
        ? h.key + "00"
        : undefined;
  let promotionDatum: string;
  if (malformedKey === undefined) {
    promotionDatum = Data.to(node, EventHistoryNode);
  } else {
    // Keep the genuine 32-byte authentication NFT. Only the datum position is
    // malformed; raw Data bypasses SDK width validation to reach the validator.
    const raw = Data.from(
      Data.to({ ...node, position: filler.position }, EventHistoryNode),
    );
    if (!(raw instanceof Constr)) throw new Error("Expected node constructor");
    promotionDatum = Data.to(
      new Constr(raw.index, [
        new Constr(1, [malformedKey]),
        ...raw.fields.slice(1),
      ]),
    );
  }
  const promotion = async () => {
    let tx = h.lucid
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
        { kind: "inline", value: promotionDatum },
        {
          lovelace: orderLovelace,
          ...orderTokens,
          [toUnit(h.applied.policyId, h.key)]: 1n,
        },
      )
      .pay.ToAddress(
        credentialToAddress("Custom", { type: "Key", hash: h.owner }),
        { lovelace: 5_000_000n },
      )
      .validFrom(next.lower)
      .validTo(next.validTo);
    if (admissionFault === "same-transaction-publication")
      tx = tx.pay.ToContract(
        h.applied.retention.address,
        { kind: "inline", value: encodeEventHistoryData(stored) },
        { lovelace: 30_000_000n },
      );
    return tx.complete({ coinSelection: false, localUPLCEval: true });
  };
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
      admissionFault: admissionFault ?? null,
      cause: inspect(refusal, { depth: 10 }),
    });
    expect(await h.lucid.utxosByOutRef([h.eventNonce])).toHaveLength(1);
    expect(await h.lucid.utxosByOutRef([fillerUtxo])).toHaveLength(1);
    if (admissionFault !== undefined) {
      // Refusal must preserve the exact genuine filler and any prepublication.
      expect((await h.lucid.utxosByOutRef([fillerUtxo]))[0]).toEqual(
        fillerUtxo,
      );
      if (externalRef !== undefined)
        expect((await h.lucid.utxosByOutRef([externalRef]))[0]).toEqual(
          externalRef,
        );
      if (
        admissionFault === "hash-only" ||
        admissionFault === "same-transaction-publication" ||
        admissionFault === "wrong-retention-address"
      )
        expect(await h.lucid.utxosAt(h.applied.retention.address)).toEqual([]);
      return;
    }
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
    assets: { lovelace: orderLovelace, ...orderTokens },
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
  expect(order.assets).toEqual({
    lovelace: orderLovelace,
    ...orderTokens,
    [toUnit(h.applied.policyId, h.key)]: 1n,
  });
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
  awaitProtection(h.emulator, node.protected_until);
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
    await retireJourney(
      h,
      after,
      payload,
      retirement,
      branchLevels,
      retirementFault,
    );
  if (maximumOriginalValue && kind === "Withdrawal") {
    const remaining = await h.funding();
    for (const [unit, amount] of Object.entries(wideAssets))
      expect(
        remaining.reduce((sum, utxo) => sum + (utxo.assets[unit] ?? 0n), 0n),
      ).toBe(amount);
  }
};
