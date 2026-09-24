import { createHash } from "node:crypto";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core";
import { compareOutRefs, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
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
  TxOrderDatum,
  TxOrderMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import type { WatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import {
  admitWatcherUserEventOrigin,
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
} from "../../src/indexers/user-event-origin.js";
import {
  makeWatcherFinalityBootstrapState,
  readWatcherLocalBackfillFinality,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  type GenuineUserEventForcedPayload,
  genuineUserEventForcedPayloadForCanonicalTx,
} from "./user-event-forced-order-fixture.js";
import {
  buildWatcherOriginFixtureHistoryDeployments,
  createSyntheticUserEventOriginFixture,
} from "./user-event-origin-fixture.js";

/**
 * Ordinary user-event order bytes and private local publication for synthetic
 * local blocks. The transactions below are semantic unit bytes: they make no
 * ledger-acceptance or public-chain inclusion claim. Replay authority comes
 * only from the watcher's own local user-event publication, which is the
 * production originating-authority path for W25.
 */

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const h32 = (byte: string): string => byte.repeat(32);
const addressHex = (address: AddressData): string => {
  const credential = address.paymentCredential;
  if (address.stakeCredential !== null || !("ScriptCredential" in credential))
    throw new Error("fixture needs an enterprise script address");
  return `70${credential.ScriptCredential[0]}`;
};
export const transactionInput = (outRef: string) => {
  const [transactionId, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(transactionId!),
    BigInt(index!),
  );
};
export const ledgerReferenceIndex = (
  inputs: CML.TransactionInputList,
  outRef: string,
): bigint => {
  const ordered = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const target = parseOutRefLabel(outRef);
  const index = ordered.findIndex(
    (input) => compareOutRefs(input, target) === 0,
  );
  if (index < 0) throw new Error("Missing fixture reference input");
  return BigInt(index);
};
export const syntheticUserEventTransaction = (
  body: CML.TransactionBody,
  values: readonly Readonly<{
    tag: CML.RedeemerTag;
    index: bigint;
    cbor: string;
  }>[],
  preserveDataEncoding = false,
): string => {
  const witness = CML.TransactionWitnessSet.new();
  const redeemers = CML.LegacyRedeemerList.new();
  for (const value of values)
    redeemers.add(
      CML.LegacyRedeemer.new(
        value.tag,
        value.index,
        CML.PlutusData.from_cbor_hex(value.cbor),
        CML.ExUnits.new(0n, 0n),
      ),
    );
  if (values.length > 0) {
    witness.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0x6a)),
    );
  }
  const complete = CML.Transaction.new(body, witness, true, undefined);
  return preserveDataEncoding
    ? complete.to_cbor_hex()
    : complete.to_canonical_cbor_hex();
};

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

export const durableFixture = async (
  policy: WatcherFinalityPolicy,
  bootstrapStore?: WatcherDurableStore,
) => {
  let bytes: Uint8Array | null = null;
  let currentHead: WatcherRollbackDurableTrustedHead | null = null;
  let failAfterCas = false;
  let failNextRead = false;
  let beforePut: (() => Promise<void>) | null = null;
  let casCount = 0;
  const backend: WatcherDurableAtomicBackend = {
    read: async () => (bytes === null ? null : Uint8Array.from(bytes)),
    compareAndSwap: async (expected, next) => {
      if ((bytes === null ? null : sha256(bytes)) !== expected) return false;
      bytes = Uint8Array.from(next);
      return true;
    },
  };
  const client: WatcherTrustedHeadAuthorityClient = {
    readRecordAuthenticationKeyId: async () => h32("99"),
    readCurrent: async () => {
      if (failNextRead) {
        failNextRead = false;
        throw new Error("fixture read-back interruption");
      }
      return currentHead;
    },
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      if (!watcherSameCanonicalJson(expectedTrustedHead, currentHead))
        return false;
      currentHead = nextTrustedHead;
      casCount += 1;
      if (failAfterCas) {
        failAfterCas = false;
        failNextRead = true;
      }
      return true;
    },
  };
  const objects = new Map<string, Uint8Array>();
  const archive = {
    put: async (value: Uint8Array) => {
      await beforePut?.();
      const digest = watcherUserEventArchiveDigest(value);
      objects.set(digest, Uint8Array.from(value));
      return digest;
    },
    read: async (digest: string) => {
      const value = objects.get(digest);
      return value === undefined ? null : Uint8Array.from(value);
    },
  };
  const runtimeInput = {
    backend,
    policy,
    authenticationKey: Uint8Array.from({ length: 32 }, (_, index) => index + 1),
    client,
    userEventArchive: archive,
  };
  if (bootstrapStore !== undefined)
    await initializeWatcherRollbackDurableAuthority({
      backend,
      policy,
      authenticationKey: runtimeInput.authenticationKey,
      trustedHead: null,
      bootstrapStore,
      bootstrapFinalityState: makeWatcherFinalityBootstrapState(policy)!,
    });
  const runtime = await createWatcherDurableRuntime(runtimeInput);
  return {
    runtime,
    runtimeInput,
    archive,
    objects,
    casCount: () => casCount,
    interruptNextReadBack: () => {
      failAfterCas = true;
    },
    setBeforePut: (callback: (() => Promise<void>) | null) => {
      beforePut = callback;
    },
  };
};

type OpenedLocalOrigin = Readonly<{
  pair: Awaited<
    ReturnType<
      Awaited<
        ReturnType<typeof createSyntheticUserEventOriginFixture>
      >["openFinalizedBlock"]
    >
  >;
  input: Parameters<typeof admitWatcherUserEventOrigin>[0];
  origin: ReturnType<typeof admitWatcherUserEventOrigin>;
  facts: WatcherUserEventOriginFacts;
}>;

export const openOrigin = async (
  fixture: Awaited<ReturnType<typeof createSyntheticUserEventOriginFixture>>,
): Promise<OpenedLocalOrigin> => {
  const pair = await fixture.openFinalizedBlock(fixture.activationBlock);
  const input = {
    deploymentIdentity: fixture.deploymentIdentity,
    scriptBinding: fixture.scriptBinding,
    finality: pair.finality,
    observation: pair.observation,
  };
  const origin = admitWatcherUserEventOrigin(input);
  const facts = readWatcherUserEventOrigin({ ...input, origin });
  return { pair, input, origin, facts };
};

export const ordinaryLocalOrderCreation = (
  facts: WatcherUserEventOriginFacts,
  kind: "withdrawal" | "forced_order",
  nativeCbor: Uint8Array,
  options: Readonly<{
    nonceByte?: string;
    withdrawalInfo?: WithdrawalInfo;
    structuralLovelace?: bigint;
    withdrawalL2OutRef?: Readonly<{
      transactionId: string;
      outputIndex: bigint;
    }>;
    forcedPayload?: GenuineUserEventForcedPayload;
  }> = {},
) => {
  const eventId = {
    transactionId: h32(
      options.nonceByte ?? (kind === "withdrawal" ? "c2" : "c3"),
    ),
    outputIndex: 0n,
  };
  const eventIdCborHex = outputReferenceToPlutusDataCbor({
    txHash: eventId.transactionId,
    outputIndex: 0,
  });
  const assetName = Buffer.from(
    blake2b(Buffer.from(eventIdCborHex, "hex"), { dkLen: 32 }),
  ).toString("hex");
  const witness = userEventWitnessScriptHash(assetName);
  const scripts =
    kind === "withdrawal"
      ? facts.scripts.withdrawal
      : facts.scripts.forcedOrder;
  const address: {
    paymentCredential: { PublicKeyCredential: [string] };
    stakeCredential: null;
  } = {
    paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
    stakeCredential: null,
  };
  const common = {
    inclusion_time: BigInt(
      resolveEventInclusionTime(
        slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
        "Preprod",
      ),
    ),
    witness,
    refund_address: address,
    refund_datum: "NoDatum" as const,
  };
  const payload =
    options.forcedPayload ??
    genuineUserEventForcedPayloadForCanonicalTx(
      encodeMidgardForcedTxCanonical(
        decodeMidgardNativeTxFullFromCanonicalCbor(nativeCbor),
      ),
    );
  const datum =
    kind === "withdrawal"
      ? Data.to(
          {
            position: { Key: [assetName] },
            next: null,
            protected_until: common.inclusion_time,
            payload: {
              Order: {
                facts: {
                  event_id: eventId,
                  inclusion_time: common.inclusion_time,
                  structural_lovelace: options.structuralLovelace ?? 0n,
                  structural_refund_key: "88".repeat(28),
                  location: {
                    Inline: {
                      payload: {
                        WithdrawalPayload: {
                          event: {
                            id: eventId,
                            info: options.withdrawalInfo ?? {
                              body: {
                                l2_outref:
                                  options.withdrawalL2OutRef ?? eventId,
                                l2_owner: "89".repeat(28),
                                l2_value: new Map(),
                                l1_address: address,
                                l1_datum: "NoDatum",
                              },
                              signature: ["aa", "bb"],
                              validity: "WithdrawalIsValid",
                            },
                          },
                          refund_address: address,
                          refund_datum: "NoDatum",
                        },
                      },
                    },
                  },
                },
              },
            },
          },
          EventHistoryNode,
        )
      : Data.to(
          {
            ...common,
            event: {
              id: eventId,
              tx: {
                tx_id: payload.tx_id,
                transaction_commitment: payload.transaction_commitment,
                submitted_source: payload.submitted_source,
              },
            },
          },
          TxOrderDatum,
        );
  const policy = CML.ScriptHash.from_hex(scripts.policyId);
  const assets = CML.MultiAsset.new();
  assets.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scripts.addressHex),
      CML.Value.new(
        3_000_000n +
          (kind === "withdrawal" ? (options.structuralLovelace ?? 0n) : 0n),
        assets,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(`${eventId.transactionId}#0`));
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  if (kind === "withdrawal") {
    inputs.add(transactionInput(`${h32("ff")}#1`));
    const rootAssets = CML.MultiAsset.new();
    rootAssets.set(policy, CML.AssetName.from_hex(""), 1n);
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(scripts.addressHex),
        CML.Value.new(2_000_000n, rootAssets),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to(
              {
                position: "Root",
                next: assetName,
                protected_until: common.inclusion_time,
                payload: "RootContent",
              },
              EventHistoryNode,
            ),
          ),
        ),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    body.set_reference_inputs(refs);
    body.set_ttl(1_000n);
    const mint = CML.Mint.new();
    mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
    body.set_mint(mint);
    const withdrawals = CML.MapRewardAccountToCoin.new();
    withdrawals.insert(
      CML.RewardAddress.new(0, CML.Credential.new_script(policy)),
      0n,
    );
    body.set_withdrawals(withdrawals);
    const cbor = syntheticUserEventTransaction(body, [
      { tag: CML.RedeemerTag.Spend, index: 1n, cbor: Data.to(1n) },
      { tag: CML.RedeemerTag.Mint, index: 0n, cbor: Data.to(0n) },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: Data.to(
          {
            Apply: {
              hub_reference_index: 0n,
              operation: {
                InsertOrder: {
                  predecessor_input_index: 1n,
                  predecessor_output_index: 1n,
                  order_output_index: 0n,
                  nonce_input_index: 0n,
                  external_reference_index: null,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      },
    ]);
    return { cbor, eventId, eventIdCborHex, payload };
  }
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_reg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
      0n,
    ),
  );
  const mint = CML.Mint.new();
  mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(refs);
  body.set_certs(certificates);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const mintEvent = {
    AuthenticateEvent: {
      nonce_input_index: 0n,
      event_output_index: 0n,
      hub_ref_input_index: 0n,
      witness_registration_redeemer_index: 1n,
    },
  };
  const materialCarriage = payload.carriage.map((entry) => {
    if (
      typeof entry !== "object" ||
      entry === null ||
      !("Inline" in entry) ||
      typeof entry.Inline !== "object" ||
      entry.Inline === null ||
      !("preimage" in entry.Inline) ||
      typeof entry.Inline.preimage !== "string"
    )
      throw new Error("ordinary forced fixture requires inline carriage");
    return { Inline: { preimage: entry.Inline.preimage } };
  });
  const cbor = syntheticUserEventTransaction(body, [
    {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
      cbor: Data.to(
        { event: mintEvent, material_carriage: materialCarriage },
        TxOrderMintRedeemer,
      ),
    },
    {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
      cbor: Data.to(
        { MintOrBurn: { targetPolicy: scripts.policyId } },
        UserEventWitnessPublishRedeemer,
      ),
    },
  ]);
  return { cbor, eventId, eventIdCborHex, payload };
};

export type LocalReplayUserEventRequest = Readonly<{
  deposit?: Readonly<{ nonceByte: string; l2Address: AddressData }>;
  withdrawals?: readonly Readonly<{
    key: string;
    nonceByte: string;
    l2OutRef: Readonly<{ transactionId: string; outputIndex: bigint }>;
    info?: WithdrawalInfo;
  }>[];
  forcedOrders?: readonly Readonly<{
    key: string;
    nonceByte: string;
    payload: GenuineUserEventForcedPayload;
  }>[];
}>;

export type LocalReplayUserEventAuthorities = Readonly<{
  deposit: WatcherLocalUserEventAuthority | null;
  withdrawals: Readonly<Record<string, WatcherLocalUserEventAuthority>>;
  forcedOrders: Readonly<Record<string, WatcherLocalUserEventAuthority>>;
  /** Rule bundle bound to the local publication's deployment identity. */
  ruleBundle: WatcherRuleBundle;
  ruleBundleCommitment: string;
  close: () => Promise<void>;
}>;

/**
 * Publishes the requested ordinary orders in one synthetic finalized block and
 * returns private local replay capabilities for each. The publisher stays open
 * until `close`, so the capabilities remain current for replay.
 */
export const createLocalReplayUserEventAuthorities = async (
  request: LocalReplayUserEventRequest,
): Promise<LocalReplayUserEventAuthorities> => {
  const fixture = await createSyntheticUserEventOriginFixture();
  let publisher:
    | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
    | undefined;
  try {
    const { pair, input, origin, facts } = await openOrigin(fixture);
    const durable = await durableFixture(
      readWatcherLocalBackfillFinality(pair.finality).policy,
    );
    publisher = await createWatcherLocalUserEventPublisher({
      ...input,
      origin,
      runtime: durable.runtime,
      archive: durable.archive,
    });
    await publisher.publish(pair);
    await publisher.publish(
      await fixture.openFinalizedBlock(fixture.emptySuccessorBlock),
    );
    const deposit =
      request.deposit === undefined
        ? null
        : historyLifecycle(facts, false, {
            ...request.deposit,
            structuralLovelace: 1_000_000n,
          });
    const placeholderNative = makeNativeTx().txCbor;
    const withdrawals = (request.withdrawals ?? []).map((withdrawal) => ({
      key: withdrawal.key,
      order: ordinaryLocalOrderCreation(
        facts,
        "withdrawal",
        placeholderNative,
        {
          nonceByte: withdrawal.nonceByte,
          withdrawalL2OutRef: withdrawal.l2OutRef,
          withdrawalInfo: withdrawal.info,
          structuralLovelace: 1_000_000n,
        },
      ),
    }));
    const forcedOrders = (request.forcedOrders ?? []).map((forced) => ({
      key: forced.key,
      order: ordinaryLocalOrderCreation(
        facts,
        "forced_order",
        placeholderNative,
        { nonceByte: forced.nonceByte, forcedPayload: forced.payload },
      ),
    }));
    const block = await fixture.makeBlock({
      parent: fixture.emptySuccessorBlock,
      transactions: [
        ...(deposit === null ? [] : [deposit.create]),
        ...withdrawals.map(({ order }) => order.cbor),
        ...forcedOrders.map(({ order }) => order.cbor),
      ],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publisher.publish(await fixture.openFinalizedBlock(block));
    const fresh = await fixture.openFinalizedBlock(block);
    const active = publisher;
    const depositAuthority =
      deposit === null
        ? null
        : await active.eventAuthority({
            ...fresh,
            kind: "deposit",
            eventId: deposit.expectedEventId,
          });
    const withdrawalAuthorities: Record<
      string,
      WatcherLocalUserEventAuthority
    > = {};
    for (const { key, order } of withdrawals)
      withdrawalAuthorities[key] = await active.eventAuthority({
        ...fresh,
        kind: "withdrawal",
        eventId: order.eventIdCborHex,
      });
    const forcedAuthorities: Record<string, WatcherLocalUserEventAuthority> =
      {};
    for (const { key, order } of forcedOrders)
      forcedAuthorities[key] = await active.eventAuthority({
        ...fresh,
        kind: "forced_order",
        eventId: order.eventIdCborHex,
      });
    const ruleBundle = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        manifestId: fixture.deploymentIdentity.manifestId,
        blueprintHash: fixture.deploymentIdentity.blueprintHash,
        network: fixture.deploymentIdentity.network,
        programCommitments: fixture.deploymentIdentity.programCommitments,
      },
      targetParameterSnapshot: { finalityDepth: 12 },
    });
    return Object.freeze({
      deposit: depositAuthority,
      withdrawals: Object.freeze(withdrawalAuthorities),
      forcedOrders: Object.freeze(forcedAuthorities),
      ruleBundle,
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
      close: async () => {
        active.close();
        await fixture.close();
      },
    });
  } catch (error) {
    publisher?.close();
    await fixture.close();
    throw error;
  }
};

export const historyPointerContinuation = (
  facts: WatcherUserEventOriginFacts,
  lifecycle: ReturnType<typeof historyLifecycle>,
  kind: "deposit" | "withdrawal",
  current?: Readonly<{ outRef: string; outputCborHex: string }>,
): string => {
  const original =
    current === undefined
      ? CML.Transaction.from_cbor_hex(lifecycle.create).body().outputs().get(0)
      : CML.TransactionOutput.from_cbor_hex(current.outputCborHex);
  const node = Data.from(
    original.datum()!.as_datum()!.to_cbor_hex(),
    EventHistoryNode,
  );
  const fillerKey =
    current === undefined
      ? "ff".repeat(32)
      : (
          (BigInt(
            "0x" + (node.position === "Root" ? "0" : node.position.Key[0]),
          ) +
            BigInt("0x" + (node.next ?? "ff".repeat(32)))) /
          2n
        )
          .toString(16)
          .padStart(64, "0");
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      original.address(),
      original.amount(),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          replacePlutusConstrFieldCbor(
            Data.to(
              {
                ...node,
                next: fillerKey,
                protected_until: node.protected_until + 1n,
              },
              EventHistoryNode,
            ),
            [3],
            plutusConstrFieldCbor(original.datum()!.as_datum()!.to_cbor_hex(), [
              3,
            ]),
          ),
        ),
      ),
    ),
  );
  const fillerAssets = CML.MultiAsset.new();
  fillerAssets.set(
    CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
    CML.AssetName.from_hex(fillerKey),
    1n,
  );
  outputs.add(
    CML.TransactionOutput.new(
      original.address(),
      CML.Value.new(2_000_000n, fillerAssets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              position: { Key: [fillerKey] },
              next: node.next,
              protected_until: node.protected_until + 1n,
              payload: { Filler: { refund_key: "88".repeat(28) } },
            },
            EventHistoryNode,
          ),
        ),
      ),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(current?.outRef ?? `${lifecycle.createId}#0`));
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
    CML.AssetName.from_hex(fillerKey),
    1n,
  );
  body.set_mint(mint);
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  body.set_reference_inputs(refs);
  const withdrawals = CML.MapRewardAccountToCoin.new();
  withdrawals.insert(
    CML.RewardAddress.new(
      0,
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(facts.scripts[kind].policyId),
      ),
    ),
    0n,
  );
  body.set_withdrawals(withdrawals);
  return syntheticUserEventTransaction(
    body,
    [
      { tag: CML.RedeemerTag.Mint, index: 0n, cbor: Data.to(0n) },
      { tag: CML.RedeemerTag.Spend, index: 0n, cbor: Data.to(0n) },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: Data.to(
          {
            Apply: {
              hub_reference_index: 0n,
              operation: {
                InsertFiller: {
                  predecessor_input_index: 0n,
                  predecessor_output_index: 0n,
                  filler_output_index: 1n,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      },
    ],
    true,
  );
};
