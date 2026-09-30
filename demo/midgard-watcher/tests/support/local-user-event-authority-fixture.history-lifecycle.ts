import { compareOutRefs, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type AddressData,
  ConfirmedState,
  EventHistoryNode,
  EventHistoryObserve,
  EventHistoryPayload,
  EventHistoryRetirementArgs,
  HubOracleDatum,
  LinkedListDatum,
  outputReferenceToPlutusDataCbor,
  PayoutDatum,
  PayoutMintRedeemer,
  prepareEventHistoryPayloadCbor,
  resolveEventInclusionTime,
  RootDomain,
  SettlementDatum,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { type WatcherUserEventOriginFacts } from "../../src/indexers/user-event-origin.js";
import {
  addressHex,
  h32,
  ledgerReferenceIndex,
  syntheticUserEventTransaction,
  transactionInput,
} from "./local-user-event-authority-fixture.synthetic-user-event-transaction.js";
import { buildWatcherOriginFixtureHistoryDeployments } from "./user-event-origin-fixture.js";

/** Ordinary semantic unit transaction bytes for a synthetic local block.
 * These do not claim ledger acceptance or public-chain transaction inclusion.
 * The checks mirror the existing deposit lifecycle fixture with this deployment's
 * real hub datum and parameter-derived scripts.
 */
export const historyLifecycle = (
  facts: WatcherUserEventOriginFacts,
  preserveDataEncoding = false,
  options: Readonly<{
    kind?: "deposit" | "withdrawal";
    nonceByte?: string;
    l2Address?: AddressData;
    structuralLovelace?: bigint;
    retirementScriptHash?: string;
    confirmedReferenceIndex?: bigint;
    omitStructuralRefundClaim?: boolean;
    fundsLovelaceOffset?: bigint;
    fundsAddressHex?: string;
    fundsDatumCborOverride?: string;

    confirmedEndOffset?: bigint;
    withdrawalPayout?: boolean;
    payoutRetirementRedeemerIndex?: bigint;
    rawDatumCbor?: string;
    external?: boolean;
    externalFault?: {
      stage: "admission" | "retirement";
      kind: "missing" | "substituted";
    };
    retirementOrderOutRef?: string;
    retirementOrderNext?: string;
  }> = {},
) => {
  const marker = "fa".repeat(40);
  const rawDatum = (cbor: string) =>
    options.rawDatumCbor === undefined
      ? cbor
      : cbor.replaceAll(Data.to(marker), options.rawDatumCbor);
  const kind = options.kind ?? "deposit";
  const payout = kind === "withdrawal" && options.withdrawalPayout === true;
  const scripts = facts.scripts[kind];
  const hub = Data.from(facts.activation.hubDatumCbor, HubOracleDatum);
  const eventId = {
    transactionId: h32(options.nonceByte ?? "b2"),
    outputIndex: 0n,
  };
  const eventIdCbor = outputReferenceToPlutusDataCbor({
    txHash: eventId.transactionId,
    outputIndex: 0,
  });
  const assetName = Buffer.from(
    blake2b(Buffer.from(eventIdCbor, "hex"), { dkLen: 32 }),
  ).toString("hex");
  const event = {
    id: eventId,
    info: {
      l2_address: options.l2Address ?? {
        paymentCredential: {
          PublicKeyCredential: ["88".repeat(28)] as [string],
        },
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: options.rawDatumCbor === undefined ? null : marker,
    },
  };
  const payload: EventHistoryPayload =
    kind === "deposit"
      ? { DepositPayload: { event } }
      : {
          WithdrawalPayload: {
            event: {
              id: eventId,
              info: {
                body: {
                  l2_outref: eventId,
                  l2_owner: "89".repeat(28),
                  l2_value: new Map(),
                  l1_address: event.info.l2_address,
                  l1_datum:
                    options.rawDatumCbor === undefined
                      ? "NoDatum"
                      : { InlineDatum: { data: marker } },
                },
                signature: ["aa", "bb"],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: event.info.l2_address,
            refund_datum:
              options.rawDatumCbor === undefined
                ? "NoDatum"
                : { InlineDatum: { data: marker } },
          },
        };
  const historyContracts = buildWatcherOriginFixtureHistoryDeployments(
    facts.canonicalOneShotOutRef,
    facts.scripts.hub.policyId,
  )[kind];
  const plan = options.external
    ? prepareEventHistoryPayloadCbor(
        rawDatum(Data.to(payload, EventHistoryPayload)),
        { PublicKeyCredential: ["88".repeat(28)] },
        historyContracts.recipe,
      )
    : null;
  if (plan !== null && plan.kind !== "External")
    throw new Error(
      "External fixture payload must exceed deployed inline bound",
    );
  const retentionBody = (datumCbor: string) => {
    const retainedOutputs = CML.TransactionOutputList.new();
    retainedOutputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(
          historyContracts.retention.spendingScriptAddress,
        ),
        CML.Value.from_coin(3_000_000n),
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
      ),
    );
    const retainedInputs = CML.TransactionInputList.new();
    retainedInputs.add(transactionInput(`${h32("b3")}#0`));
    return CML.TransactionBody.new(
      retainedInputs,
      retainedOutputs,
      200_000n,
    ).to_cbor_hex();
  };
  const retainedBody =
    plan?.kind === "External" ? retentionBody(plan.datumCbor) : null;
  const substitutedBody =
    plan?.kind === "External"
      ? retentionBody(
          plan.datumCbor.replace("a402a201020103", "a402a201020109"),
        )
      : null;
  const bodyRef = (cbor: string) =>
    `${CML.hash_transaction(CML.TransactionBody.from_cbor_hex(cbor)).to_hex()}#0`;
  const retainedRef = (stage: "admission" | "retirement") => {
    if (retainedBody === null) return null;
    if (options.externalFault?.stage === stage) {
      if (options.externalFault.kind === "missing") return null;
      if (substitutedBody === retainedBody)
        throw new Error("Fixture substitution did not change raw pairs");
      return bodyRef(substitutedBody!);
    }
    return bodyRef(retainedBody);
  };
  const admissionRetainedRef = retainedRef("admission");
  const retirementRetainedRef = retainedRef("retirement");
  const inclusion = BigInt(
    resolveEventInclusionTime(
      slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
      "Preprod",
    ),
  );
  const node: EventHistoryNode = {
    position: { Key: [assetName] },
    next: null,
    protected_until: inclusion,
    payload: {
      Order: {
        facts: {
          event_id: eventId,
          inclusion_time: inclusion,
          structural_lovelace: options.structuralLovelace ?? 0n,
          structural_refund_key: "88".repeat(28),
          location: plan?.location ?? { Inline: { payload } },
        },
      },
    },
  };
  const policy = CML.ScriptHash.from_hex(scripts.policyId);
  const listOutput = (name: string, datum: EventHistoryNode, coin: bigint) => {
    const assets = CML.MultiAsset.new();
    assets.set(policy, CML.AssetName.from_hex(name), 1n);
    return CML.TransactionOutput.new(
      CML.Address.from_hex(scripts.addressHex),
      CML.Value.new(coin, assets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          rawDatum(Data.to(datum, EventHistoryNode)),
        ),
      ),
    );
  };
  const root: EventHistoryNode = {
    position: "Root",
    next: assetName,
    protected_until: inclusion,
    payload: "RootContent",
  };
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    listOutput(
      assetName,
      node,
      3_000_000n + (options.structuralLovelace ?? 0n),
    ),
  );
  outputs.add(listOutput("", root, 2_000_000n));
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(`${eventId.transactionId}#0`));
  inputs.add(transactionInput(`${h32("ff")}#0`));
  const refs = CML.TransactionInputList.new();
  const createReferences = preserveDataEncoding
    ? [
        facts.activation.hubOutRef,
        `${facts.activation.transactionId}#${facts.activation.hubOutputIndex === 0 ? 1 : 0}`,
      ].sort((left, right) =>
        compareOutRefs(parseOutRefLabel(right), parseOutRefLabel(left)),
      )
    : [facts.activation.hubOutRef];
  if (admissionRetainedRef !== null)
    createReferences.push(admissionRetainedRef);
  for (const ref of createReferences) refs.add(transactionInput(ref));
  const mint = CML.Mint.new();
  mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(refs);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const withdrawal = CML.MapRewardAccountToCoin.new();
  withdrawal.insert(
    CML.RewardAddress.new(0, CML.Credential.new_script(policy)),
    0n,
  );
  body.set_withdrawals(withdrawal);
  const create = syntheticUserEventTransaction(
    body,
    [
      { tag: CML.RedeemerTag.Spend, index: 1n, cbor: Data.to(1n) },
      { tag: CML.RedeemerTag.Mint, index: 0n, cbor: Data.to(0n) },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: Data.to(
          {
            Apply: {
              hub_reference_index: ledgerReferenceIndex(
                refs,
                facts.activation.hubOutRef,
              ),
              operation: {
                InsertOrder: {
                  predecessor_input_index: 1n,
                  predecessor_output_index: 1n,
                  order_output_index: 0n,
                  nonce_input_index: 0n,
                  external_reference_index:
                    admissionRetainedRef === null
                      ? null
                      : ledgerReferenceIndex(refs, admissionRetainedRef),
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      },
    ],
    preserveDataEncoding,
  );
  const createId = CML.hash_transaction(
    CML.Transaction.from_cbor_hex(create).body(),
  ).to_hex();
  const phasRoot = h32("a4");
  const countedRoot = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1"),
        Buffer.from(
          Data.to(
            kind === "deposit" ? "DepositsRootDomain" : "WithdrawalsRootDomain",
            RootDomain,
          ),
          "hex",
        ),
        Buffer.from(phasRoot, "hex"),
        Buffer.from(Data.to(1n), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
  const settlementAssets = CML.MultiAsset.new();
  settlementAssets.set(
    CML.ScriptHash.from_hex(hub.settlement),
    CML.AssetName.from_hex(""),
    1n,
  );
  const settlementOutputs = CML.TransactionOutputList.new();
  settlementOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(addressHex(hub.settlement_addr)),
      CML.Value.new(5_000_000n, settlementAssets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              deposits_root: kind === "deposit" ? countedRoot : h32("a5"),
              withdrawals_root: kind === "withdrawal" ? countedRoot : h32("a5"),
              forced_transactions_root: h32("a6"),
              transactions_root: h32("a7"),
              resolution_claim: null,
            },
            SettlementDatum,
          ),
        ),
      ),
    ),
  );
  const confirmedAssets = CML.MultiAsset.new();
  confirmedAssets.set(
    CML.ScriptHash.from_hex(hub.state_queue),
    CML.AssetName.from_hex(
      Buffer.from("MIDGARD_CONFIRMED_STATE").toString("hex"),
    ),
    1n,
  );
  settlementOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(addressHex(hub.state_queue_addr)),
      CML.Value.new(3_000_000n, confirmedAssets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              data: {
                Root: {
                  data: Data.from(
                    Data.to(
                      {
                        headerHash: "a1".repeat(28),
                        prevHeaderHash: "a0".repeat(28),
                        utxoRoot: h32("a2"),
                        startTime: 0n,
                        endTime: inclusion + (options.confirmedEndOffset ?? 1n),
                        protocolVersion: 1n,
                      },
                      ConfirmedState,
                    ),
                  ),
                },
              },
              link: null,
            },
            LinkedListDatum,
          ),
        ),
      ),
    ),
  );
  const settlementInputs = CML.TransactionInputList.new();
  settlementInputs.add(transactionInput(`${h32("f0")}#0`));
  const settlementBody = CML.TransactionBody.new(
    settlementInputs,
    settlementOutputs,
    200_000n,
  ).to_cbor_hex();
  const settlementId = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(settlementBody),
  ).to_hex();
  const consumeInputs = CML.TransactionInputList.new();
  const orderRef = options.retirementOrderOutRef ?? `${createId}#0`;
  const predecessorRef = `${createId}#1`;
  const retiringInputs = [orderRef, predecessorRef].sort((left, right) =>
    compareOutRefs(parseOutRefLabel(left), parseOutRefLabel(right)),
  );
  for (const ref of retiringInputs) consumeInputs.add(transactionInput(ref));
  const orderInputIndex = BigInt(retiringInputs.indexOf(orderRef));
  const predecessorInputIndex = BigInt(retiringInputs.indexOf(predecessorRef));
  const consumeOutputs = CML.TransactionOutputList.new();
  const payoutAssets = CML.MultiAsset.new();
  if (payout)
    payoutAssets.set(
      CML.ScriptHash.from_hex(hub.payout),
      CML.AssetName.from_hex(assetName),
      1n,
    );
  consumeOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(
        options.fundsAddressHex ??
          (kind === "deposit"
            ? addressHex(hub.reserve_addr)
            : payout
              ? addressHex(hub.payout_addr)
              : `60${"88".repeat(28)}`),
      ),
      payout
        ? CML.Value.new(
            3_000_000n + (options.fundsLovelaceOffset ?? 0n),
            payoutAssets,
          )
        : CML.Value.from_coin(3_000_000n + (options.fundsLovelaceOffset ?? 0n)),
      options.fundsDatumCborOverride !== undefined
        ? CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(options.fundsDatumCborOverride),
          )
        : payout && "WithdrawalPayload" in payload
          ? CML.DatumOption.new_datum(
              CML.PlutusData.from_cbor_hex(
                rawDatum(
                  Data.to(
                    {
                      l2_value:
                        payload.WithdrawalPayload.event.info.body.l2_value,
                      l1_address:
                        payload.WithdrawalPayload.event.info.body.l1_address,
                      l1_datum:
                        payload.WithdrawalPayload.event.info.body.l1_datum,
                    },
                    PayoutDatum,
                  ),
                ),
              ),
            )
          : kind === "withdrawal" && options.rawDatumCbor !== undefined
            ? CML.DatumOption.new_datum(
                CML.PlutusData.from_cbor_hex(options.rawDatumCbor),
              )
            : undefined,
    ),
  );
  consumeOutputs.add(
    listOutput(
      "",
      {
        ...root,
        next: options.retirementOrderNext ?? null,
        protected_until: inclusion + 1n,
      },
      2_000_000n,
    ),
  );
  const structural = options.structuralLovelace ?? 0n;
  const structuralIndex = structural > 0n ? 2n : null;
  if (structuralIndex !== null)
    consumeOutputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(`60${"88".repeat(28)}`),
        CML.Value.from_coin(structural),
      ),
    );
  const burn = CML.Mint.new();
  burn.set(policy, CML.AssetName.from_hex(assetName), -1n);
  if (payout)
    burn.set(
      CML.ScriptHash.from_hex(hub.payout),
      CML.AssetName.from_hex(assetName),
      1n,
    );
  const consumeRefs = CML.TransactionInputList.new();
  for (const ref of [
    facts.activation.hubOutRef,
    `${settlementId}#0`,
    `${settlementId}#1`,
    ...(retirementRetainedRef === null ? [] : [retirementRetainedRef]),
  ])
    consumeRefs.add(transactionInput(ref));
  const consumeBody = CML.TransactionBody.new(
    consumeInputs,
    consumeOutputs,
    200_000n,
  );
  consumeBody.set_mint(burn);
  consumeBody.set_reference_inputs(consumeRefs);
  const retirement =
    options.retirementScriptHash ??
    historyContracts.retirement.withdrawalScriptHash;
  const withdrawals = CML.MapRewardAccountToCoin.new();
  for (const hash of [scripts.policyId, retirement])
    withdrawals.insert(
      CML.RewardAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(hash)),
      ),
      0n,
    );
  consumeBody.set_withdrawals(withdrawals);
  const witness: EventHistoryRetirementArgs = {
    hub_reference_index: ledgerReferenceIndex(
      consumeRefs,
      facts.activation.hubOutRef,
    ),
    witness: {
      order_input_index: orderInputIndex,
      predecessor_input_index: predecessorInputIndex,
      predecessor_output_index: 1n,
      funds_output_index: 0n,
      structural_refund_output_index: options.omitStructuralRefundClaim
        ? null
        : structuralIndex,
      confirmed_reference_index:
        options.confirmedReferenceIndex ??
        ledgerReferenceIndex(consumeRefs, `${settlementId}#1`),
      settlement_reference_index: ledgerReferenceIndex(
        consumeRefs,
        `${settlementId}#0`,
      ),
      external_reference_index:
        retirementRetainedRef === null
          ? null
          : ledgerReferenceIndex(consumeRefs, retirementRetainedRef),
      membership: { phas_root: phasRoot, count: 1n, proof: [] },
      purpose:
        kind === "deposit"
          ? "AbsorbDeposit"
          : payout
            ? "InitializeWithdrawalPayout"
            : {
                RefundInvalidWithdrawal: {
                  validity: "IncorrectWithdrawalSignature",
                },
              },
    },
  };
  const keys = CML.TransactionBody.from_cbor_hex(
    consumeBody.to_canonical_cbor_hex(),
  )
    .withdrawals()!
    .keys();
  const minted = CML.TransactionBody.from_cbor_hex(
    consumeBody.to_canonical_cbor_hex(),
  )
    .mint()!
    .keys();
  const retirementIndex = Array.from(
    { length: keys.len() },
    (_, index) => index,
  ).find(
    (index) => keys.get(index).payment().as_script()?.to_hex() === retirement,
  );
  if (retirementIndex === undefined)
    throw new Error("Missing retirement observer");
  // Ledger purpose indices use canonical policy/reward ordering; preserve only
  // arbitrary datum encoding, not an incidental construction order of maps.
  const orderedConsume = CML.TransactionBody.from_cbor_hex(
    consumeBody.to_canonical_cbor_hex(),
  );
  consumeBody.set_withdrawals(orderedConsume.withdrawals()!);
  consumeBody.set_mint(orderedConsume.mint()!);
  const consume = syntheticUserEventTransaction(
    consumeBody,
    [
      { tag: CML.RedeemerTag.Spend, index: 0n, cbor: Data.to(0n) },
      { tag: CML.RedeemerTag.Spend, index: 1n, cbor: Data.to(1n) },
      ...Array.from({ length: minted.len() }, (_, index) => ({
        tag: CML.RedeemerTag.Mint,
        index: BigInt(index),
        cbor:
          payout && minted.get(index).to_hex() === hub.payout
            ? Data.to(
                {
                  MintPayout: {
                    withdrawal_utxo_out_ref: {
                      transactionId: orderRef.split("#")[0]!,
                      outputIndex: BigInt(orderRef.split("#")[1]!),
                    },
                    withdrawal_input_index: orderInputIndex,
                    retirement_withdraw_redeemer_index:
                      options.payoutRetirementRedeemerIndex ??
                      BigInt(2 + minted.len() + retirementIndex),
                    hub_ref_input_index: witness.hub_reference_index,
                  },
                },
                PayoutMintRedeemer,
              )
            : Data.to(0n),
      })),
      ...Array.from({ length: keys.len() }, (_, index) => ({
        tag: CML.RedeemerTag.Reward,
        index: BigInt(index),
        cbor:
          keys.get(index).payment().as_script()?.to_hex() === retirement
            ? Data.to(witness, EventHistoryRetirementArgs)
            : Data.to(
                {
                  Apply: {
                    hub_reference_index: witness.hub_reference_index,
                    operation: {
                      RetireOrder: {
                        predecessor_output_index: 1n,
                        funds_output_index: 0n,
                        structural_refund_output_index:
                          options.omitStructuralRefundClaim
                            ? null
                            : structuralIndex,
                      },
                    },
                  },
                },
                EventHistoryObserve,
              ),
      })),
    ],
    preserveDataEncoding,
  );
  return Object.freeze({
    create,
    consume,
    createId,
    settlementBody,
    externalBodies:
      retainedBody === null
        ? []
        : [
            retainedBody,
            ...(substitutedBody === retainedBody ? [] : [substitutedBody!]),
          ],
    expectedPayloadCbor: plan?.payloadCbor ?? null,
    expectedEventId: eventIdCbor,
  });
};
