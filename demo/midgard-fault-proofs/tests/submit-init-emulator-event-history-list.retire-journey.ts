import { inspect } from "node:util";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  __reservePayoutTest,
  commitCountedRootProgram,
  ConfirmedState,
  DepositInfo,
  EMPTY_MERKLE_TREE_ROOT,
  EventHistoryNode,
  EventHistoryObserve,
  EventHistoryPayload,
  eventHistoryRetirementOperation,
  EventHistoryRetirementWitness,
  type EventHistoryWitness,
  fetchEventHistoryWitness,
  OutputReference,
  PayoutDatum,
  Proof,
  SettlementDatum,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  fromText,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import {
  awaitProtection,
  index,
  records,
  type RetirementFault,
  setup,
} from "./submit-init-emulator-event-history-list.setup.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

export const retireJourney = async (
  h: Awaited<ReturnType<typeof setup>>,
  present: Extract<EventHistoryWitness, { kind: "Present" }>,
  payload: EventHistoryPayload,
  mode: "settle" | "refund",
  branchLevels: number,
  retirementFault?: RetirementFault,
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
    fromText(
      retirementFault === "wrong-confirmed-token"
        ? "FAKE_CONFIRMED_STATE"
        : "MIDGARD_CONFIRMED_STATE",
    ),
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
        endTime:
          facts.inclusion_time -
          (retirementFault === "before-inclusion" ? 1n : 0n),
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
  const allNodes = await h.lucid.utxosAt(h.applied.address);
  const predecessor = allNodes.find(
    (u) =>
      u.datum != null && Data.from(u.datum, EventHistoryNode).next === h.key,
  )!;
  const beforeRoot = Data.from(predecessor.datum!, EventHistoryNode);
  // Match production retirement funding: create genuine disposable ADA inputs
  // and leave unrelated native assets outside both fee and collateral selection.
  await h.submit(
    "prepare-disposable-retirement-funding",
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .pay.ToAddress(h.wallet.address, { lovelace: 100_000_000n })
      .pay.ToAddress(h.wallet.address, { lovelace: 20_000_000n })
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const walletBeforeRetirement = await h.lucid.utxosAt(h.wallet.address);
  const unrelatedTokenInputs = walletBeforeRetirement.filter((utxo) =>
    Object.keys(utxo.assets).some((unit) => unit !== "lovelace"),
  );
  awaitProtection(
    h.emulator,
    beforeRoot.protected_until > present.anchor.node.protected_until
      ? beforeRoot.protected_until
      : present.anchor.node.protected_until,
  );
  const b = h.bounds();
  const continued = {
    ...beforeRoot,
    next: present.anchor.node.next,
    protected_until: b.protectedUntil,
  };
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
  const fundingExclusions = [
    predecessor,
    present.anchor.utxo,
    h.eventNonce,
    h.predecessorNonce,
    ...refs,
  ];
  const feeInput = await Effect.runPromise(
    __reservePayoutTest.selectFeeInputProgram(
      h.lucid,
      undefined,
      fundingExclusions,
    ),
  );
  const collateralInputs = __reservePayoutTest.disposableFeeInputCandidates(
    walletBeforeRetirement,
    [...fundingExclusions, feeInput],
  );
  expect(Object.keys(feeInput.assets)).toEqual(["lovelace"]);
  expect(collateralInputs.length).toBeGreaterThan(0);
  for (const utxo of collateralInputs)
    expect(Object.keys(utxo.assets)).toEqual(["lovelace"]);
  const inputs = [predecessor, present.anchor.utxo, feeInput];
  records.push({
    kind: deposit ? "Deposit" : "Withdrawal",
    label: "disposable-retirement-funding",
    feeInput,
    collateralInputs,
    unrelatedTokenInputs,
  });
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
    .collectFrom([feeInput])
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
  const { [toUnit(h.applied.policyId, h.key)]: historyNft, ...originalAssets } =
    present.anchor.utxo.assets;
  expect(historyNft).toBe(1n);
  originalAssets.lovelace -= facts.structural_lovelace;
  if (deposit) {
    tx = tx.pay
      .ToAddress(h.hubAddress, originalAssets)
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
  if (retirementFault !== undefined) {
    // Settlement membership, funds and list inputs remain genuine for this
    // applied-script fixture; change only CT timing or its authentication token.
    const refusal = await tx
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        presetWalletInputs: [...collateralInputs],
      })
      .then(
        () => undefined,
        (error: unknown) => error,
      );
    expect(inspect(refusal, { depth: 10 })).toMatch(/failed script execution/);
    expect((await h.lucid.utxosByOutRef([present.anchor.utxo]))[0]).toEqual(
      present.anchor.utxo,
    );
    expect((await h.lucid.utxosByOutRef([predecessor]))[0]).toEqual(
      predecessor,
    );
    expect((await h.lucid.utxosByOutRef([settlementRef]))[0]).toEqual(
      settlementRef,
    );
    expect((await h.lucid.utxosByOutRef([confirmedRef]))[0]).toEqual(
      confirmedRef,
    );
    if (present.retainedDataUtxo !== undefined)
      expect(
        (await h.lucid.utxosByOutRef([present.retainedDataUtxo]))[0],
      ).toEqual(present.retainedDataUtxo);
    expect((await h.lucid.utxosByOutRef([feeInput]))[0]).toEqual(feeInput);
    for (const untouched of unrelatedTokenInputs)
      expect((await h.lucid.utxosByOutRef([untouched]))[0]).toEqual(untouched);
    records.push({
      kind: deposit ? "Deposit" : "Withdrawal",
      label: "retirement-refusal",
      retirementFault,
      inclusionTime: facts.inclusion_time,
      confirmedDatum: rootDatum,
      confirmedUnit,
      cause: inspect(refusal, { depth: 10 }),
      authorityScope:
        "Actual applied list/retirement scripts; native fixture-issued CT and settlement authority, not a production merge proof",
    });
    return;
  }
  const retirementTxHash = await h.submit(
    mode === "refund" ? "refund-and-unlink" : "settle-and-unlink",
    await tx
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        presetWalletInputs: [...collateralInputs],
      })
      .catch((error: unknown) => {
        throw new Error(
          `Retirement completion: ${inspect(error, { depth: 10 })}`,
        );
      }),
  );
  for (const untouched of unrelatedTokenInputs)
    expect((await h.lucid.utxosByOutRef([untouched]))[0]).toEqual(untouched);
  const [fundsOutput] = await h.lucid.utxosByOutRef([
    { txHash: retirementTxHash, outputIndex: 1 },
  ]);
  expect(fundsOutput?.assets).toEqual(
    deposit || mode === "refund"
      ? originalAssets
      : { ...originalAssets, [toUnit(h.hubPolicy, h.key)]: 1n },
  );
  records.push({
    kind: deposit ? "Deposit" : "Withdrawal",
    label: "retirement-funds-preserved",
    mode,
    originalAssets,
    fundsOutput,
  });
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
  for (const untouched of unrelatedTokenInputs)
    expect((await h.lucid.utxosByOutRef([untouched]))[0]).toEqual(untouched);
};
