import {
  addressDataFromBech32,
  applyEventHistoryValidators,
  eventHistoryKey,
  EventHistoryNode,
  EventHistoryObserve,
  type FaultProofBlueprint,
  HubOracleDatum,
} from "@al-ft/midgard-sdk";
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
  type TxSignBuilder,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { index, same } from "./history-pair.index.js";
import { measureCompleteSignedTransaction } from "./measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";

export const setupHistoryPair = async ({
  blueprint,
  records,
  protectionDurationMs = 120_000n,
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
