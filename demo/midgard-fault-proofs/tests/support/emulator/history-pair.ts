/** Applied history fixture: genuine list scripts, explicitly native hub/CT authority. */
import {
  addressDataFromBech32,
  applyEventHistoryValidators,
  eventHistoryKey,
  EventHistoryNode,
  EventHistoryObserve,
  type EventHistoryPayload,
  type EventHistoryWitness,
  type FaultProofBlueprint,
  HubOracleDatum,
  prepareEventHistoryPayload,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  Emulator,
  fromText,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  mintingPolicyToId,
  scriptFromNative,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { measureCompleteSignedTransaction } from "./measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";

export const same = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;
export const index = (inputs: readonly UTxO[], target: UTxO) => {
  const sorted = [...inputs].sort(
    (a, b) => a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
  );
  const result = sorted.findIndex((u) => same(u, target));
  if (result < 0) throw new Error("Missing transaction input");
  return BigInt(result);
};

export const setupHistoryPair = async ({
  blueprint,
  records,
  protectionDurationMs = 2_000n,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly records: unknown[];
  readonly protectionDurationMs?: bigint;
}) => {
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
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    records.push({
      label,
      measurement,
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
      txHash,
      transactionCbor: cbor,
    });
    return txHash;
  };
  const split = await submit(
    "reserve-initialization-and-event-nonces",
    await lucid
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .pay.ToAddress(wallet.address, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true }),
  );
  const reserved = await lucid.utxosByOutRef([
    { txHash: split, outputIndex: 0 },
    { txHash: split, outputIndex: 1 },
    { txHash: split, outputIndex: 2 },
    { txHash: split, outputIndex: 3 },
  ]);
  const nonces = reserved.slice(0, 2);
  const eventNonces = reserved.slice(2);
  const keys = await Promise.all(
    eventNonces.map((n) =>
      Effect.runPromise(
        eventHistoryKey({
          transactionId: n.txHash,
          outputIndex: BigInt(n.outputIndex),
        }),
      ),
    ),
  );
  const recipes = (["Deposit", "Withdrawal"] as const).map((kind, i) => ({
    hubPolicyId: hubPolicy,
    kind,
    initializationNonce: {
      transactionId: nonces[i]!.txHash,
      outputIndex: BigInt(nonces[i]!.outputIndex),
    },
    protectionDurationMs,
    inlineLimitBytes: 512n,
    maxPayloadBytes: 5000n,
    maxPayloadNodes: 512n,
  }));
  const applied = recipes.map((recipe) =>
    applyEventHistoryValidators(blueprint, "Custom", recipe),
  );
  records.push({
    label: "parameters",
    recipes,
    policies: applied.map((a) => a.policyId),
  });
  const addr = Effect.runSync(addressDataFromBech32(hubAddress));
  const hubDatum: HubOracleDatum = {
    registered_operators: hubPolicy,
    active_operators: hubPolicy,
    retired_operators: hubPolicy,
    scheduler: hubPolicy,
    state_queue: hubPolicy,
    fraud_proof_catalogue: hubPolicy,
    fraud_proof: hubPolicy,
    deposit: applied[0]!.policyId,
    withdrawal: applied[1]!.policyId,
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
    deposit_addr: Effect.runSync(addressDataFromBech32(applied[0]!.address)),
    withdrawal_addr: Effect.runSync(addressDataFromBech32(applied[1]!.address)),
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: hubPolicy,
  };
  const funding = async () =>
    (await lucid.wallet().getUtxos()).filter(
      (u) => !reserved.some((n) => same(u, n)),
    );
  const hubUnit = hubPolicy + fromText("MIDGARD_HUB_ORACLE");
  await submit(
    "publish-hub",
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
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  for (const a of applied)
    await submit(
      "publish-list-script",
      await lucid
        .newTx()
        .collectFrom(await funding())
        .pay.ToAddressWithData(
          hubAddress,
          undefined,
          { lovelace: 100_000_000n },
          a.validator,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
  const references = await lucid.utxosAt(hubAddress);
  const hub = references.find((u) => u.assets[hubUnit] === 1n)!;
  const scripts = applied.map(
    (a) =>
      references.find(
        (u) =>
          u.scriptRef != null &&
          validatorToScriptHash(u.scriptRef) === a.policyId,
      )!,
  );
  let register = lucid
    .newTx()
    .collectFrom(await funding())
    .readFrom(scripts);
  for (const a of applied)
    register = register.register.Stake(a.rewardAddress, Data.void());
  await submit(
    "register-deployment-observers",
    await register.complete({ coinSelection: false, localUPLCEval: true }),
  );
  const bounds = () => {
    const lower = emulator.now();
    const validTo = lower + 10_000;
    return {
      lower,
      validTo,
      protectedUntil: BigInt(validTo - 1) + protectionDurationMs,
    };
  };
  let b = bounds();
  const inputs = [...(await funding()), ...nonces];
  let init = lucid
    .newTx()
    .collectFrom(inputs)
    .readFrom(scripts)
    .validFrom(b.lower)
    .validTo(b.validTo);
  for (let i = 0; i < 2; i++) {
    const a = applied[i]!;
    const root: EventHistoryNode = {
      position: "Root",
      next: null,
      protected_until: b.protectedUntil,
      payload: "RootContent",
    };
    init = init
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          {
            Initialize: {
              nonce_input_index: index(inputs, nonces[i]!),
              root_output_index: BigInt(i),
            },
          },
          EventHistoryObserve,
        ),
      )
      .mintAssets({ [a.policyId]: 1n }, Data.void())
      .pay.ToContract(
        a.address,
        { kind: "inline", value: Data.to(root, EventHistoryNode) },
        { lovelace: 3_000_000n, [a.policyId]: 1n },
      );
  }
  await submit(
    "initialize-both-lists",
    await init.complete({ coinSelection: false, localUPLCEval: true }),
  );
  emulator.awaitSlot(
    Math.max(40, Number((protectionDurationMs + 10_999n) / 1000n)),
  );
  b = bounds();
  const roots = await Promise.all(
    applied.map(
      async (a) =>
        (await lucid.utxosAt(a.address)).find(
          (u) => u.assets[a.policyId] === 1n,
        )!,
    ),
  );
  const insertInputs = [...(await funding()), ...roots];
  const refs = [hub, ...scripts];
  let insert = lucid
    .newTx()
    .collectFrom(await funding())
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo);
  for (let i = 0; i < 2; i++) {
    const a = applied[i]!;
    const root = roots[i]!;
    const node = Data.from(root.datum!, EventHistoryNode);
    const key = keys[i]!;
    const filler: EventHistoryNode = {
      position: { Key: [key] },
      next: null,
      protected_until: b.protectedUntil,
      payload: { Filler: { refund_key: owner } },
    };
    insert = insert
      .collectFrom([root], Data.to(index(insertInputs, root)))
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          {
            Apply: {
              hub_reference_index: index(refs, hub),
              operation: {
                InsertFiller: {
                  predecessor_input_index: index(insertInputs, root),
                  predecessor_output_index: BigInt(2 * i),
                  filler_output_index: BigInt(2 * i + 1),
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      )
      .mintAssets({ [a.policyId + key]: 1n }, Data.void())
      .pay.ToContract(
        a.address,
        {
          kind: "inline",
          value: Data.to(
            { ...node, next: key, protected_until: b.protectedUntil },
            EventHistoryNode,
          ),
        },
        root.assets,
      )
      .pay.ToContract(
        a.address,
        { kind: "inline", value: Data.to(filler, EventHistoryNode) },
        { lovelace: 3_000_000n, [a.policyId + key]: 1n },
      );
  }
  await submit(
    "insert-fillers-in-both-lists",
    await insert.complete({ coinSelection: false, localUPLCEval: true }),
  );
  emulator.awaitSlot(
    Math.max(40, Number((protectionDurationMs + 10_999n) / 1000n)),
  );
  return {
    lucid,
    emulator,
    applied,
    recipes,
    hub,
    scripts,
    owner,
    wallet,
    funding,
    submit,
    bounds,
    keys,
    eventNonces,
    hubPolicy,
    hubAddress,
    issuer,
    protectionDurationMs,
  };
};

export const promoteHistoryPair = async (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
  sharedRefund = false,
  payloads = historyPairPayloads(h),
) => {
  const fillers = await Promise.all(
    h.applied.map(
      async (a, i) =>
        (await h.lucid.utxosAt(a.address)).find(
          (u) => u.assets[a.policyId + h.keys[i]!] === 1n,
        )!,
    ),
  );
  const plans = payloads.map((payload) =>
    prepareEventHistoryPayload(
      payload,
      { PublicKeyCredential: [h.owner] },
      { inlineLimitBytes: 512n, maxPayloadBytes: 5000n, maxPayloadNodes: 512n },
    ),
  );
  const retained: (UTxO | undefined)[] = [];
  for (let i = 0; i < plans.length; i++) {
    const plan = plans[i]!;
    if (plan.kind === "Inline") {
      retained.push(undefined);
      continue;
    }
    const hash = await h.submit(
      `prepublish-${i}-history-content`,
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .pay.ToContract(
          h.applied[i]!.retention.address,
          { kind: "inline", value: plan.datumCbor },
          { lovelace: 15_000_000n },
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    retained.push(
      (await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }]))[0]!,
    );
  }
  const funding = await h.funding();
  const inputs = [...funding, ...fillers, ...h.eventNonces];
  const refs = [
    h.hub,
    ...h.scripts,
    ...retained.filter((u): u is UTxO => u !== undefined),
  ];
  const b = h.bounds();
  let tx = h.lucid
    .newTx()
    .collectFrom([...funding, ...h.eventNonces])
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo);
  for (let i = 0; i < 2; i++) {
    const a = h.applied[i]!;
    const filler = fillers[i]!;
    const plan = plans[i]!;
    const id = i === 0 ? h.eventNonces[0]! : h.eventNonces[1]!;
    const node: EventHistoryNode = {
      position: { Key: [h.keys[i]!] },
      next: null,
      protected_until: b.protectedUntil,
      payload: {
        Order: {
          facts: {
            event_id: {
              transactionId: id.txHash,
              outputIndex: BigInt(id.outputIndex),
            },
            inclusion_time: BigInt(b.validTo - 1) + 60_000n,
            location: plan.location,
            structural_lovelace: i === 0 ? 3_000_000n : 0n,
            structural_refund_key: h.owner,
          },
        },
      },
    };
    tx = tx
      .collectFrom([filler], Data.to(index(inputs, filler)))
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          {
            Apply: {
              hub_reference_index: index(refs, h.hub),
              operation: {
                PromoteFiller: {
                  filler_input_index: index(inputs, filler),
                  order_output_index: BigInt(i),
                  refund_output_index: sharedRefund ? 2n : BigInt(2 + i),
                  nonce_input_index: index(inputs, h.eventNonces[i]!),
                  external_reference_index: retained[i]
                    ? index(refs, retained[i]!)
                    : null,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      )
      .pay.ToContract(
        a.address,
        { kind: "inline", value: Data.to(node, EventHistoryNode) },
        {
          lovelace: i === 0 ? 23_000_000n : 20_000_000n,
          [a.policyId + h.keys[i]!]: 1n,
        },
      );
  }
  tx = tx.pay.ToAddress(
    credentialToAddress("Custom", { type: "Key", hash: h.owner }),
    { lovelace: 3_000_000n },
  );
  if (!sharedRefund)
    tx = tx.pay.ToAddress(
      credentialToAddress("Custom", { type: "Key", hash: h.owner }),
      { lovelace: 3_000_000n },
    );
  const unsigned = await tx.complete({
    coinSelection: false,
    localUPLCEval: true,
  });
  // Wallet payment witnesses fund admission; no filler-owner required signer is added.
  expect(
    CML.Transaction.from_cbor_hex(unsigned.toCBOR())
      .body()
      .required_signers()
      ?.len() ?? 0,
  ).toBe(0);
  await h.submit("promote-both-without-filler-owner-approval", unsigned);
  h.emulator.awaitSlot(
    Math.max(40, Number((h.protectionDurationMs + 10_999n) / 1000n)),
  );
  return payloads;
};

export const insertHistoryFillerAfter = async (
  h: Pick<
    Awaited<ReturnType<typeof setupHistoryPair>>,
    "applied" | "lucid" | "funding" | "hub" | "scripts" | "bounds" | "owner"
  >,
  kind: "Deposit" | "Withdrawal",
  witness: EventHistoryWitness,
  key: string,
  excluded: UTxO[],
) => {
  const a = h.applied[kind === "Deposit" ? 0 : 1]!;
  const pred = witness.anchor.utxo;
  const node = witness.anchor.node;
  const funding = (await h.funding()).filter(
    (u) => !excluded.some((x) => same(x, u)),
  );
  const inputs = [...funding, pred];
  const refs = [h.hub, h.scripts[kind === "Deposit" ? 0 : 1]!];
  const b = h.bounds();
  return h.lucid
    .newTx()
    .collectFrom(funding)
    .collectFrom([pred], Data.to(index(inputs, pred)))
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo)
    .withdraw(
      a.rewardAddress,
      0n,
      Data.to(
        {
          Apply: {
            hub_reference_index: index(refs, h.hub),
            operation: {
              InsertFiller: {
                predecessor_input_index: index(inputs, pred),
                predecessor_output_index: 0n,
                filler_output_index: 1n,
              },
            },
          },
        },
        EventHistoryObserve,
      ),
    )
    .mintAssets({ [a.policyId + key]: 1n }, Data.void())
    .pay.ToContract(
      a.address,
      {
        kind: "inline",
        value: Data.to(
          { ...node, next: key, protected_until: b.protectedUntil },
          EventHistoryNode,
        ),
      },
      pred.assets,
    )
    .pay.ToContract(
      a.address,
      {
        kind: "inline",
        value: Data.to(
          {
            position: { Key: [key] },
            next: node.next,
            protected_until: b.protectedUntil,
            payload: { Filler: { refund_key: h.owner } },
          },
          EventHistoryNode,
        ),
      },
      { lovelace: 3_000_000n, [a.policyId + key]: 1n },
    )
    .complete({ coinSelection: false, localUPLCEval: true });
};

export const historyPairPayloads = (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
) => {
  const destination = Effect.runSync(addressDataFromBech32(h.hubAddress));
  const payloads: EventHistoryPayload[] = h.eventNonces.map((n, i) => {
    const id = { transactionId: n.txHash, outputIndex: BigInt(n.outputIndex) };
    return i === 0
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: destination,
                l2_network_id: 0n,
                l2_datum: null,
              },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: h.owner,
                  l2_value: new Map([["", new Map([["", 100_000_000n]])]]),
                  l1_address: destination,
                  l1_datum: "NoDatum",
                },
                signature: ["55".repeat(32), "66".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: destination,
            refund_datum: "NoDatum",
          },
        };
  });
  return payloads;
};
