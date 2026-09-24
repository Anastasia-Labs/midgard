import {
  asLucidDataValue,
  asLucidSchema,
} from "@al-ft/midgard-core/lucid-data";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCbor,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  type BuildTxWithRedeemer,
  Constr,
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type OutputDatum,
  type RedeemerContext,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import { WithdrawalBody } from "./ledger-state.js";
import { getLinkedListNodeViewFromUTxO } from "./linked-list.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import {
  addAssets,
  assertAssetsNonNegative,
  assertNoAssetExceeds,
  assetsEqual,
  assetsToValue,
  hasNonZeroAssetQuantity,
  minPositiveAssets,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "./reserve-payout/assets.js";
import {
  type BuiltReservePayoutTx,
  completeWithFinalLayoutProgram,
} from "./reserve-payout/completion.js";
import { formatLayout } from "./reserve-payout/diagnostics.js";
import { fail, ReservePayoutTxError } from "./reserve-payout/errors.js";
import { fetchHubOracleReferenceProgram } from "./reserve-payout/hub-reference.js";
import {
  disposableFeeInputCandidates,
  selectFeeInputProgram,
} from "./reserve-payout/inputs.js";
import {
  type AbsorbDepositLayout,
  type AddReserveFundsLayout,
  type ConcludePayoutLayout,
  type InitializePayoutLayout,
  type RefundWithdrawalLayout,
} from "./reserve-payout/layout.js";
import * as SDK from "./reserve-payout/primitives.js";
import {
  attachIfMissing,
  mergeReferenceScripts,
  referenceInputs,
  type ReservePayoutReferenceScripts,
  resolveReferenceScriptsProgram,
} from "./reserve-payout/references.js";
import { getConfirmedStateFromStateQueueDatum } from "./state-queue.js";
import { STATE_QUEUE_ROOT_ASSET_NAME } from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryObserve,
  EventHistoryRetirementArgs,
  eventHistoryRetirementOperation,
  type EventHistoryRetirementWitness,
} from "./user-events/history.js";
import {
  eventHistoryDeploymentFromContracts,
  requireEventHistoryContracts,
} from "./user-events/history-deployment.js";
import { historyEventFromPresence } from "./user-events/history-events.js";
import { eventHistoryMinimumOutputLovelace } from "./user-events/history-funding.js";
import {
  authenticateHistoryNodes,
  readEventHistoryOrders,
} from "./user-events/history-query.js";

export {
  addAssets,
  assetsEqual,
  assetsToValue,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "./reserve-payout/assets.js";
export type { BuiltReservePayoutTx } from "./reserve-payout/completion.js";
export { ReservePayoutTxError } from "./reserve-payout/errors.js";
export type { ReservePayoutReferenceScripts } from "./reserve-payout/references.js";
export { mergeReferenceScripts } from "./reserve-payout/references.js";

type CommonBuilderConfig = {
  readonly hubOracleRefInput?: UTxO;
  readonly feeInput?: UTxO;
  readonly referenceScripts?: ReservePayoutReferenceScripts;
  readonly referenceScriptsAddress?: string;
};

export type AbsorbConfirmedDepositConfig = CommonBuilderConfig & {
  readonly deposit: SDK.DepositUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
};

export type InitializePayoutConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
};

export type AddReserveFundsConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
};

export type ConcludePayoutConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
};

export type RefundInvalidWithdrawalConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
  readonly validityOverride: Exclude<
    SDK.WithdrawalValidity,
    "WithdrawalIsValid"
  >;
};

const encodeHexBytesData = (hex: string): unknown =>
  Data.from(Data.to(hex, asLucidSchema(Data.Bytes())));

const requireNetwork = (
  lucid: LucidEvolution,
): Effect.Effect<Network, ReservePayoutTxError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* fail(
        "Cardano network not found while preparing reserve/payout transaction",
        "Lucid network configuration is undefined",
      );
    }
    return network;
  });

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return { type: "Key", hash: credential.PublicKeyCredential[0] };
  }
  return { type: "Script", hash: credential.ScriptCredential[0] };
};

const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error(
    "Pointer stake credentials are not supported by node builders",
  );
};

const withdrawalPayoutDatumCbor = (payloadCbor: string): string => {
  const bodyCbor = plutusConstrFieldCbor(payloadCbor, [0, 1, 0]);
  const body = Data.from(bodyCbor, WithdrawalBody);
  let payoutCbor = Data.to(
    {
      l2_value: body.l2_value,
      l1_address: body.l1_address,
      l1_datum: body.l1_datum,
    },
    SDK.PayoutDatum,
  );
  for (let field = 0; field < 3; field++)
    payoutCbor = replacePlutusConstrFieldCbor(
      payoutCbor,
      [field],
      plutusConstrFieldCbor(bodyCbor, [field + 2]),
    );
  return payoutCbor;
};

const cardanoDatumCborToOutputDatum = (
  datumCbor: string,
): OutputDatum | undefined => {
  const datum = Data.from(datumCbor, SDK.CardanoDatum);
  if (datum === "NoDatum") {
    return undefined;
  }
  if ("DatumHash" in datum) {
    return {
      kind: "hash",
      value: datum.DatumHash.hash,
    };
  }
  return {
    kind: "inline",
    value: plutusConstrFieldCbor(datumCbor, [0]),
  };
};

const payToAddressWithCardanoDatum = (
  tx: TxBuilder,
  address: string,
  datumCbor: string,
  assets: Assets,
): TxBuilder => {
  const outputDatum = cardanoDatumCborToOutputDatum(datumCbor);
  return outputDatum === undefined
    ? tx.pay.ToAddress(address, assets)
    : tx.pay.ToAddressWithData(address, outputDatum, assets);
};

const encodeMembershipProofWithdrawalRedeemer = (
  keyCbor: string,
  valueCbor: string,
  proof: SDK.RootMembershipProof<unknown, unknown>,
): string => {
  const rootData = Data.from(Data.to(proof.phas_root, SDK.MerkleRoot));
  const keyData = encodeHexBytesData(keyCbor);
  const valueData = encodeHexBytesData(valueCbor);
  const proofData = Data.from(Data.to(proof.proof, asLucidSchema(SDK.Proof)));
  return Data.to(
    asLucidDataValue([rootData, keyData, valueData, proofData]),
    asLucidSchema(Data.Array(Data.Any())),
  );
};

const outputHasNoDatum = (output: TxOutput): boolean =>
  output.datum == null && output.datumHash == null;

const outputDatumMatches = (output: TxOutput, datumCbor: string): boolean => {
  const datum = Data.from(datumCbor, SDK.CardanoDatum);
  if (datum === "NoDatum") {
    return outputHasNoDatum(output);
  }
  if ("DatumHash" in datum) {
    return output.datumHash === datum.DatumHash.hash && output.datum == null;
  }
  return outputDatumCborMatches(output, plutusConstrFieldCbor(datumCbor, [0]));
};

const reserveOutputIndex = (
  outputs: readonly TxOutput[],
  reserveAddress: string,
  reserveAssets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === reserveAddress &&
      outputHasNoDatum(output) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, reserveAssets),
    label,
  );

const outputWithDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datumCbor: string,
  assets: Assets,
  label: string,
): bigint => {
  return requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumCborMatches(output, datumCbor) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );
};

const outputWithCardanoDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datumCbor: string,
  assets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumMatches(output, datumCbor) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );

const requireResolvedLayout = <L>(layout: L | undefined, label: string): L => {
  if (layout === undefined) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout.`);
  }
  return layout;
};

type RetirementConfig = CommonBuilderConfig & {
  readonly settlementRefInput: UTxO;
  readonly confirmedRefInput?: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly nowMs?: number;
};

type RetirementLayout = {
  readonly witness: EventHistoryRetirementWitness;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly payoutMintRedeemerIndex: bigint | null;
};

/** Rebuild from current authenticated nodes. Pointer churn is allowed, immutable
 * event facts and original Value must still match the caller's settlement leaf. */
const buildHistoryRetirementProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RetirementConfig,
  requested: SDK.DepositUTxO | SDK.WithdrawalUTxO,
  purpose: EventHistoryRetirementWitness["purpose"],
) =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hub = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const prepared = yield* Effect.tryPromise({
      try: async () => {
        const pair = requireEventHistoryContracts(contracts);
        const history =
          requested.kind === "Deposit" ? pair.deposit : pair.withdrawal;
        const configured =
          requested.kind === "Deposit"
            ? contracts.deposit
            : contracts.withdrawal;
        if (
          history.recipe.kind !== requested.kind ||
          history.recipe.hubPolicyId !== contracts.hubOracle.policyId ||
          history.list.policyId !== configured.policyId ||
          history.list.spendingScriptAddress !==
            configured.spendingScriptAddress
        )
          throw new Error("History recipe differs from configured deployment");
        const deployment = eventHistoryDeploymentFromContracts(history);
        const nodeUtxos = await lucid.utxosAt(deployment.address);
        const retained = await lucid.utxosAt(deployment.retentionAddress);
        const orders = readEventHistoryOrders(nodeUtxos, retained, deployment);
        const presence = orders.find(
          ({ anchor }) => anchor.key === requested.assetName,
        );
        if (presence === undefined)
          throw new Error("Event is no longer an authenticated live Order");
        const event = historyEventFromPresence(presence, deployment);
        if (
          event.kind !== requested.kind ||
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(event.utxo.datum!, [3, 0]),
          ) !==
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(requested.utxo.datum!, [3, 0]),
            ) ||
          event.history.payloadCbor !== requested.history.payloadCbor ||
          !assetsEqual(event.originalAssets, requested.originalAssets) ||
          !event.idCbor.equals(requested.idCbor) ||
          !event.infoCbor.equals(requested.infoCbor)
        ) {
          throw new Error(
            "Authenticated event immutable facts or original Value changed",
          );
        }
        const nodes = authenticateHistoryNodes(nodeUtxos, deployment);
        const predecessors = nodes.filter(
          ({ node }) => node.next === event.assetName,
        );
        if (predecessors.length !== 1)
          throw new Error("Event has no unique current predecessor");
        const predecessor = predecessors[0]!;
        const confirmedUnit =
          contracts.stateQueue.policyId + STATE_QUEUE_ROOT_ASSET_NAME;
        const confirmedCandidates =
          config.confirmedRefInput === undefined
            ? await lucid.utxosAtWithUnit(
                contracts.stateQueue.spendingScriptAddress,
                confirmedUnit,
              )
            : await lucid.utxosByOutRef([config.confirmedRefInput]);
        if (
          confirmedCandidates.length !== 1 ||
          confirmedCandidates[0]!.assets[confirmedUnit] !== 1n ||
          confirmedCandidates[0]!.address !==
            contracts.stateQueue.spendingScriptAddress ||
          confirmedCandidates[0]!.datum == null
        )
          throw new Error("Missing authenticated confirmed-state reference");
        const confirmedNode = await Effect.runPromise(
          getLinkedListNodeViewFromUTxO(confirmedCandidates[0]!),
        );
        const confirmedState = await Effect.runPromise(
          getConfirmedStateFromStateQueueDatum(confirmedNode),
        );
        if (
          event.facts.inclusion_time <= 0n ||
          event.facts.inclusion_time > confirmedState.data.endTime
        )
          throw new Error("Event eligibility interval is not confirmed");
        const settlementCandidates = await lucid.utxosByOutRef([
          config.settlementRefInput,
        ]);
        const settlement = settlementCandidates[0];
        if (
          settlementCandidates.length !== 1 ||
          settlement?.datum == null ||
          settlement.address !== contracts.settlement.spendingScriptAddress ||
          Object.entries(settlement.assets).filter(
            ([unit, quantity]) =>
              unit.startsWith(contracts.settlement.policyId) && quantity === 1n,
          ).length !== 1
        )
          throw new Error("Missing current authenticated settlement reference");
        Data.from(settlement.datum, SDK.SettlementDatum);
        const now = config.nowMs ?? Date.now();
        if (!Number.isSafeInteger(now))
          throw new Error("Retirement clock must be a safe integer timestamp");
        const protocolLower =
          predecessor.node.protected_until >
          presence.anchor.node.protected_until
            ? predecessor.node.protected_until
            : presence.anchor.node.protected_until;
        if (protocolLower > BigInt(now))
          throw new Error("History predecessor or Order is still protected");
        const desiredLower = Number(
          protocolLower > BigInt(now - 60_000)
            ? protocolLower
            : BigInt(now - 60_000),
        );
        // Round upward so slot conversion never backdates below protection.
        let lowerSlot = lucid.unixTimeToSlot(desiredLower);
        if (lucid.slotToUnixTime(lowerSlot) < desiredLower) lowerSlot++;
        const validFrom = lucid.slotToUnixTime(lowerSlot);
        const validTo = validFrom + Number(MAX_VALIDITY_RANGE_LENGTH_MS);
        const upper =
          BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))) - 1n;
        if (
          upper + history.recipe.protectionDurationMs >
          EVENT_HISTORY_MAX_PROTECTION_TIME
        )
          throw new Error("History protection exceeds funded encoding width");
        const continuedCbor = replacePlutusConstrFieldCbor(
          replacePlutusConstrFieldCbor(
            predecessor.utxo.datum!,
            [1],
            Data.to(
              presence.anchor.node.next === null
                ? new Constr(1, [])
                : new Constr(0, [presence.anchor.node.next]),
            ),
          ),
          [2],
          Data.to(upper + history.recipe.protectionDurationMs),
        );
        return {
          history,
          event,
          predecessor,
          confirmed: confirmedCandidates[0]!,
          settlement,
          validFrom,
          validTo,
          continuedCbor,
        };
      },
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to resolve authenticated retirement state",
          cause,
        }),
    });
    const {
      history,
      event,
      predecessor,
      confirmed,
      settlement,
      validFrom,
      validTo,
      continuedCbor,
    } = prepared;
    const initialize = purpose === "InitializeWithdrawalPayout";
    let fundsAddress: string;
    let fundsDatum = Data.to("NoDatum", SDK.CardanoDatum);
    let fundsAssets = event.originalAssets;
    if (purpose === "AbsorbDeposit")
      fundsAddress = contracts.reserve.spendingScriptAddress;
    else {
      if (event.kind !== "Withdrawal")
        return yield* fail(
          "Withdrawal retirement requires a withdrawal Order",
          event.kind,
        );
      if (initialize) {
        if (event.event.info.validity !== "WithdrawalIsValid")
          return yield* fail(
            "Payout requires valid withdrawal",
            event.event.info.validity,
          );
        const body = event.event.info.body;
        assertNoAssetExceeds(
          event.originalAssets,
          valueToAssets(body.l2_value),
          "Initial payout accumulator",
        );
        fundsAddress = contracts.payout.spendingScriptAddress;
        fundsAssets = addAssets(event.originalAssets, {
          [contracts.payout.policyId + event.assetName]: 1n,
        });
        const payoutCbor = withdrawalPayoutDatumCbor(event.history.payloadCbor);
        fundsDatum = replacePlutusConstrFieldCbor(
          Data.to({ InlineDatum: { data: 0n } }, SDK.CardanoDatum),
          [0],
          payoutCbor,
        );
      } else {
        fundsAddress = addressDataToBech32(network, event.refundAddress);
        fundsDatum = plutusConstrFieldCbor(event.history.payloadCbor, [2]);
      }
    }
    const refundAddress = credentialToAddress(network, {
      type: "Key",
      hash: event.facts.structural_refund_key,
    });
    const structuralMinimum = eventHistoryMinimumOutputLovelace(
      { lovelace: event.facts.structural_lovelace },
      "NoDatum",
    );
    const structuralRefundAssets = {
      lovelace:
        event.facts.structural_lovelace > structuralMinimum
          ? event.facts.structural_lovelace
          : structuralMinimum,
    };
    const resolved = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name:
            event.kind === "Deposit"
              ? "deposit spending"
              : "withdrawal spending",
          script: history.list.spendingScript,
        },
        {
          name:
            event.kind === "Deposit"
              ? "deposit history retirement"
              : "withdrawal history retirement",
          script: history.retirement.withdrawalScript,
        },
        ...(initialize
          ? [{ name: "payout minting", script: contracts.payout.mintingScript }]
          : []),
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(config.referenceScripts, resolved);
    const listReference =
      refs.historyList ??
      (event.kind === "Deposit"
        ? refs.depositSpending
        : refs.withdrawalSpending);
    const references = referenceInputs(hub, [
      confirmed,
      settlement,
      event.history.retainedDataUtxo,
      listReference,
      refs.historyRetirement,
      ...(initialize ? [refs.payoutMinting] : []),
    ]);
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      event.utxo,
      predecessor.utxo,
      ...references,
    ]);
    const retirementAddress = scriptRewardAddress(
      network,
      history.retirement.withdrawalScript,
    );
    const listAddress = scriptRewardAddress(
      network,
      history.list.withdrawalScript,
    );
    let layout: RetirementLayout | undefined;
    const resolve = (ctx: RedeemerContext): RetirementLayout => {
      const structuralRefundIndex =
        event.facts.structural_lovelace === 0n
          ? null
          : requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === refundAddress &&
                outputHasNoDatum(output) &&
                output.scriptRef === undefined &&
                assetsEqual(output.assets, structuralRefundAssets),
              "structural refund",
            );
      const witness: EventHistoryRetirementWitness = {
        predecessor_input_index: requireInputIndex(
          ctx,
          predecessor.utxo,
          "retirement predecessor",
        ),
        order_input_index: requireInputIndex(
          ctx,
          event.utxo,
          "retirement Order",
        ),
        predecessor_output_index: outputWithDatumIndex(
          ctx.outputs,
          predecessor.utxo.address,
          continuedCbor,
          predecessor.utxo.assets,
          "continued predecessor",
        ),
        funds_output_index: outputWithCardanoDatumIndex(
          ctx.outputs,
          fundsAddress,
          fundsDatum,
          fundsAssets,
          "retirement funds",
        ),
        structural_refund_output_index: structuralRefundIndex,
        confirmed_reference_index: requireReferenceInputIndex(
          ctx,
          confirmed,
          "confirmed state",
        ),
        settlement_reference_index: requireReferenceInputIndex(
          ctx,
          settlement,
          "settlement",
        ),
        external_reference_index:
          event.history.retainedDataUtxo === undefined
            ? null
            : requireReferenceInputIndex(
                ctx,
                event.history.retainedDataUtxo,
                "retained data",
              ),
        membership: {
          phas_root: config.membershipProof.phas_root,
          count: config.membershipProof.count,
          proof: config.membershipProof.proof,
        },
        purpose,
      };
      layout = {
        witness,
        hubRefInputIndex: requireReferenceInputIndex(ctx, hub, "hub oracle"),
        settlementRefInputIndex: witness.settlement_reference_index,
        retirementWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          retirementAddress,
          "retirement observer",
        ),
        listWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          listAddress,
          "list observer",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          history.list.policyId,
          "Order burn",
        ),
        payoutMintRedeemerIndex: initialize
          ? requireMintRedeemerIndex(
              ctx,
              contracts.payout.policyId,
              "payout mint",
            )
          : null,
      };
      return layout;
    };
    const spend =
      (input: UTxO): BuildTxWithRedeemer =>
      (ctx) => {
        requireOwnSpendPurpose(ctx, input, "history node");
        return Data.to(requireInputIndex(ctx, input, "history node"));
      };
    return yield* completeWithFinalLayoutProgram({
      label: "event history retirement",
      lucid,
      walletInputExclusions: [
        event.utxo,
        predecessor.utxo,
        feeInput,
        ...references,
      ],
      resolveLayout: () =>
        requireResolvedLayout(layout, "event history retirement"),
      makeTx: () => {
        let tx = lucid.newTx().readFrom([...references]);
        tx = attachIfMissing(tx, history.list.spendingScript, listReference);
        tx = attachIfMissing(
          tx,
          history.retirement.withdrawalScript,
          refs.historyRetirement,
        );
        tx = tx
          .collectFrom([predecessor.utxo], spend(predecessor.utxo))
          .collectFrom([event.utxo], spend(event.utxo))
          .collectFrom([feeInput])
          .mintAssets(
            { [history.list.policyId + event.assetName]: -1n },
            Data.void(),
          )
          .pay.ToAddressWithData(
            predecessor.utxo.address,
            { kind: "inline", value: continuedCbor },
            predecessor.utxo.assets,
          )
          .withdraw(listAddress, 0n, ((ctx) => {
            const resolved = resolve(ctx);
            return Data.to(
              {
                Apply: {
                  hub_reference_index: resolved.hubRefInputIndex,
                  operation: eventHistoryRetirementOperation(resolved.witness),
                },
              },
              EventHistoryObserve,
            );
          }) satisfies BuildTxWithRedeemer)
          .withdraw(retirementAddress, 0n, ((ctx) => {
            const resolved = resolve(ctx);
            return Data.to(
              {
                hub_reference_index: resolved.hubRefInputIndex,
                witness: resolved.witness,
              },
              EventHistoryRetirementArgs,
            );
          }) satisfies BuildTxWithRedeemer)
          .validFrom(validFrom)
          .validTo(validTo);
        tx = payToAddressWithCardanoDatum(
          tx,
          fundsAddress,
          fundsDatum,
          fundsAssets,
        );
        if (event.facts.structural_lovelace > 0n)
          tx = tx.pay.ToAddress(refundAddress, structuralRefundAssets);
        if (initialize) {
          tx = attachIfMissing(
            tx,
            contracts.payout.mintingScript,
            refs.payoutMinting,
          );
          tx = tx.mintAssets(
            { [contracts.payout.policyId + event.assetName]: 1n },
            ((ctx) => {
              requireOwnMintPurpose(
                ctx,
                contracts.payout.policyId,
                "payout mint",
              );
              const resolved = resolve(ctx);
              return Data.to(
                {
                  MintPayout: {
                    withdrawal_utxo_out_ref: {
                      transactionId: event.utxo.txHash,
                      outputIndex: BigInt(event.utxo.outputIndex),
                    },
                    withdrawal_input_index: resolved.witness.order_input_index,
                    retirement_withdraw_redeemer_index:
                      resolved.retirementWithdrawalRedeemerIndex,
                    hub_ref_input_index: resolved.hubRefInputIndex,
                  },
                },
                SDK.PayoutMintRedeemer,
              );
            }) satisfies BuildTxWithRedeemer,
          );
        }
        return tx;
      },
    });
  });

export const buildAbsorbConfirmedDepositToReserveTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AbsorbConfirmedDepositConfig,
) =>
  buildHistoryRetirementProgram(
    lucid,
    contracts,
    config,
    config.deposit,
    "AbsorbDeposit",
  ).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        depositInputIndex: layout.witness.order_input_index,
        reserveOutputIndex: layout.witness.funds_output_index,
      } satisfies AbsorbDepositLayout,
    })),
  );

export const buildInitializePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: InitializePayoutConfig,
) =>
  buildHistoryRetirementProgram(
    lucid,
    contracts,
    config,
    config.withdrawal,
    "InitializeWithdrawalPayout",
  ).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        withdrawalInputIndex: layout.witness.order_input_index,
        payoutOutputIndex: layout.witness.funds_output_index,
        withdrawalBurnRedeemerIndex: layout.burnRedeemerIndex,
        payoutMintRedeemerIndex: layout.payoutMintRedeemerIndex!,
      } satisfies InitializePayoutLayout,
    })),
  );

const decodePayoutDatum = (payoutInput: UTxO): SDK.PayoutDatum => {
  if (payoutInput.datum == null) {
    throw new Error(
      `Payout input ${outRefLabel(payoutInput)} has no inline datum`,
    );
  }
  return Data.from(payoutInput.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

const payoutAssetNameFromInput = (
  payoutInput: UTxO,
  payoutPolicyId: string,
): string => {
  const matches = Object.entries(payoutInput.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(payoutPolicyId) && unit.length >= 56 && quantity === 1n,
  );
  if (matches.length !== 1) {
    throw new Error(
      `Expected payout input ${outRefLabel(
        payoutInput,
      )} to contain exactly one payout NFT for policy ${payoutPolicyId}, found ${matches.length.toString()}`,
    );
  }
  return matches[0]![0].slice(56);
};

export const buildAddReserveFundsToPayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AddReserveFundsConfig,
): Effect.Effect<
  BuiltReservePayoutTx<AddReserveFundsLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutDatumCbor = config.payoutInput.datum!;
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    assertNoAssetExceeds(
      currentPayoutAssets,
      targetAssets,
      "Current payout input",
    );
    const neededAssets = subtractAssets(targetAssets, currentPayoutAssets);
    assertAssetsNonNegative(neededAssets, "Payout needed value");
    const takenAssets = minPositiveAssets(
      config.reserveInput.assets,
      neededAssets,
    );
    if (Object.keys(takenAssets).length === 0) {
      return yield* fail(
        "Reserve input does not contribute to any still-needed payout asset",
        {
          reserveInput: outRefLabel(config.reserveInput),
          neededAssets,
        },
      );
    }
    const payoutOutputAssets = addAssets(
      config.payoutInput.assets,
      takenAssets,
    );
    const reserveChangeAssets = subtractAssets(
      config.reserveInput.assets,
      takenAssets,
    );
    assertAssetsNonNegative(reserveChangeAssets, "Reserve change value");
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "reserve spending", script: contracts.reserve.spendingScript },
        { name: "payout spending", script: contracts.payout.spendingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      config.reserveInput,
      hubOracleRefInput,
      ...(refs.reserveSpending === undefined ? [] : [refs.reserveSpending]),
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
    ]);
    const txInputs = [config.payoutInput, config.reserveInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.reserveSpending,
      refs.payoutSpending,
    ]);
    const reserveChangeOutputIndex = (outputs: readonly TxOutput[]) =>
      hasNonZeroAssetQuantity(reserveChangeAssets)
        ? reserveOutputIndex(
            outputs,
            contracts.reserve.spendingScriptAddress,
            reserveChangeAssets,
            "reserve change",
          )
        : null;
    let addReserveFundsLayout: AddReserveFundsLayout | undefined;
    const payoutSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.payoutInput, "reserve funding payout");
      const layout: AddReserveFundsLayout = {
        payoutInputIndex: requireInputIndex(
          ctx,
          config.payoutInput,
          "reserve funding payout",
        ),
        payoutOutputIndex: outputWithDatumIndex(
          ctx.outputs,
          contracts.payout.spendingScriptAddress,
          payoutDatumCbor,
          payoutOutputAssets,
          "updated payout",
        ),
        reserveInputIndex: requireInputIndex(
          ctx,
          config.reserveInput,
          "reserve funding reserve",
        ),
        reserveChangeOutputIndex: reserveChangeOutputIndex(ctx.outputs),
        reserveSpendRedeemerIndex: requireSpendRedeemerIndex(
          ctx,
          config.reserveInput,
          "reserve funding reserve",
        ),
        payoutSpendRedeemerIndex: requireOwnRedeemerIndex(
          ctx,
          "reserve funding payout",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "reserve funding hub oracle",
        ),
      };
      addReserveFundsLayout = layout;
      return Data.to(
        {
          AddFunds: {
            payout_input_index: layout.payoutInputIndex,
            payout_output_index: layout.payoutOutputIndex,
            reserve_input_index: layout.reserveInputIndex,
            reserve_change_output_index: layout.reserveChangeOutputIndex,
            reserve_spend_redeemer_index: layout.reserveSpendRedeemerIndex,
            payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
            hub_ref_input_index: layout.hubRefInputIndex,
          },
        } satisfies SDK.PayoutSpendRedeemer,
        SDK.PayoutSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const reserveSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.reserveInput,
        "reserve funding reserve",
      );
      return Data.to(
        {
          reserve_input_index: requireInputIndex(
            ctx,
            config.reserveInput,
            "reserve funding reserve",
          ),
          payout_input_index: requireInputIndex(
            ctx,
            config.payoutInput,
            "reserve funding payout",
          ),
          payout_spend_redeemer_index: requireSpendRedeemerIndex(
            ctx,
            config.payoutInput,
            "reserve funding payout",
          ),
          hub_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleRefInput,
            "reserve funding hub oracle",
          ),
        } satisfies SDK.ReserveSpendRedeemer,
        SDK.ReserveSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.reserve.spendingScript,
        refs.reserveSpending,
      );
      tx = tx
        .collectFrom([config.payoutInput], payoutSpendRedeemer)
        .collectFrom([config.reserveInput], reserveSpendRedeemer)
        .collectFrom([feeInput])
        .pay.ToAddressWithData(
          contracts.payout.spendingScriptAddress,
          { kind: "inline", value: payoutDatumCbor },
          payoutOutputAssets,
        );
      if (hasNonZeroAssetQuantity(reserveChangeAssets)) {
        tx = tx.pay.ToAddress(
          contracts.reserve.spendingScriptAddress,
          reserveChangeAssets,
        );
      }
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "reserve funding",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () =>
        requireResolvedLayout(addReserveFundsLayout, "reserve funding"),
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Reserve funding layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildConcludePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: ConcludePayoutConfig,
): Effect.Effect<
  BuiltReservePayoutTx<ConcludePayoutLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const l1Assets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    if (!assetsEqual(currentPayoutAssets, l1Assets)) {
      return yield* fail(
        "Payout input value does not exactly equal the payout datum target",
        {
          payoutInput: outRefLabel(config.payoutInput),
          currentPayoutAssets,
          targetAssets: l1Assets,
        },
      );
    }
    const l1Address = addressDataToBech32(network, payoutDatum.l1_address);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "payout spending", script: contracts.payout.spendingScript },
        { name: "payout minting", script: contracts.payout.mintingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      hubOracleRefInput,
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
      ...(refs.payoutMinting === undefined ? [] : [refs.payoutMinting]),
    ]);
    const txInputs = [config.payoutInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.payoutSpending,
      refs.payoutMinting,
    ]);
    let concludePayoutLayout: ConcludePayoutLayout | undefined;
    const payoutSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.payoutInput, "payout conclusion");
      const layout: ConcludePayoutLayout = {
        payoutInputIndex: requireInputIndex(
          ctx,
          config.payoutInput,
          "payout conclusion",
        ),
        l1OutputIndex: outputWithCardanoDatumIndex(
          ctx.outputs,
          l1Address,
          plutusConstrFieldCbor(config.payoutInput.datum!, [2]),
          l1Assets,
          "payout destination",
        ),
        payoutSpendRedeemerIndex: requireOwnRedeemerIndex(
          ctx,
          "payout conclusion",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.payout.policyId,
          "payout burn",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "payout conclusion hub oracle",
        ),
      };
      concludePayoutLayout = layout;
      return Data.to(
        {
          ConcludeWithdrawal: {
            payout_input_index: layout.payoutInputIndex,
            l1_output_index: layout.l1OutputIndex,
            burn_redeemer_index: layout.burnRedeemerIndex,
            hub_ref_input_index: layout.hubRefInputIndex,
          },
        } satisfies SDK.PayoutSpendRedeemer,
        SDK.PayoutSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const payoutBurnRedeemer = ((ctx) => {
      requireOwnMintPurpose(ctx, contracts.payout.policyId, "payout burn");
      return Data.to(
        {
          BurnPayout: {
            payout_input_index: requireInputIndex(
              ctx,
              config.payoutInput,
              "payout burn",
            ),
            payout_asset_name: payoutAssetName,
            payout_spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              config.payoutInput,
              "payout burn",
            ),
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleRefInput,
              "payout burn hub oracle",
            ),
          },
        } satisfies SDK.PayoutMintRedeemer,
        SDK.PayoutMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.mintingScript,
        refs.payoutMinting,
      );
      tx = tx
        .collectFrom([config.payoutInput], payoutSpendRedeemer)
        .collectFrom([feeInput])
        .mintAssets({ [payoutUnit]: -1n }, payoutBurnRedeemer);
      tx = payToAddressWithCardanoDatum(
        tx,
        l1Address,
        plutusConstrFieldCbor(config.payoutInput.datum!, [2]),
        l1Assets,
      );
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "payout conclusion",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () =>
        requireResolvedLayout(concludePayoutLayout, "payout conclusion"),
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Payout conclusion layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildRefundInvalidWithdrawalTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RefundInvalidWithdrawalConfig,
) =>
  buildHistoryRetirementProgram(lucid, contracts, config, config.withdrawal, {
    RefundInvalidWithdrawal: { validity: config.validityOverride },
  }).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        withdrawalInputIndex: layout.witness.order_input_index,
        refundOutputIndex: layout.witness.funds_output_index,
      } satisfies RefundWithdrawalLayout,
    })),
  );

export const __reservePayoutTest = {
  cardanoDatumCborToOutputDatum,
  payToAddressWithCardanoDatum,
  outputDatumMatches,
  outputWithCardanoDatumIndex,
  withdrawalPayoutDatumCbor,
  addAssets,
  assetsToValue,
  assetsEqual,
  encodeMembershipProofWithdrawalRedeemer,
  disposableFeeInputCandidates,
  aikenSerialisedPlutusDataCbor,
  minPositiveAssets,
  removeAssetUnit,
  selectFeeInputProgram,
  subtractAssets,
  valueToAssets,
};
