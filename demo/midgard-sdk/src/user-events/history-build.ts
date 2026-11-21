import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  type BuildTxWithRedeemer,
  CML,
  Constr,
  credentialToAddress,
  Data,
  datumToHash,
  fromText,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  type CredentialD,
} from "../common.js";
import { HubOracleDatum } from "../hub-oracle.js";
import {
  EVENT_WAIT_DURATION_MS,
  MAX_VALIDITY_RANGE_LENGTH_MS,
} from "../protocol-parameters.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  type applyEventHistoryValidators,
  EventHistoryNode,
  EventHistoryObserve,
  type EventHistoryOperation,
  type EventHistoryPayload,
  type EventHistoryRecipe,
} from "./history.js";
import { assertEventHistoryAdmissionFunding } from "./history-funding.js";
import {
  encodeEventHistoryOrderNode,
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "./history-payload.js";
import { fetchEventHistoryWitness } from "./history-query.js";

export type EventHistoryBuildContext = {
  readonly lucid: LucidEvolution;
  readonly applied: Pick<
    ReturnType<typeof applyEventHistoryValidators>,
    "validator" | "policyId" | "address" | "rewardAddress" | "retention"
  >;
  readonly recipe: EventHistoryRecipe;
  readonly hubReference: UTxO;
  readonly scriptReference?: UTxO;
  /** Explicit funding keeps a publication from accidentally consuming its event nonce. */
  readonly fundingInputs: readonly UTxO[];
};

const requireDistinct = (inputs: readonly UTxO[]) => {
  if (new Set(inputs.map(outRefLabel)).size !== inputs.length) {
    throw new Error("History transaction inputs must be distinct");
  }
};

const authenticateContext = (context: EventHistoryBuildContext) => {
  const { applied, recipe, hubReference, scriptReference } = context;
  const unit = recipe.hubPolicyId + fromText("MIDGARD_HUB_ORACLE");
  if (hubReference.assets[unit] !== 1n || hubReference.datum == null) {
    throw new Error("Missing authenticated event history hub");
  }
  const hub = Data.from(hubReference.datum, HubOracleDatum);
  const policy = recipe.kind === "Deposit" ? hub.deposit : hub.withdrawal;
  const address =
    recipe.kind === "Deposit" ? hub.deposit_addr : hub.withdrawal_addr;
  const appliedAddress = Effect.runSync(addressDataFromBech32(applied.address));
  if (
    policy !== applied.policyId ||
    Data.to(address, AddressData) !== Data.to(appliedAddress, AddressData)
  ) {
    throw new Error("History deployment does not match the authenticated hub");
  }
  if (
    scriptReference !== undefined &&
    (scriptReference.scriptRef == null ||
      validatorToScriptHash(scriptReference.scriptRef) !== applied.policyId)
  ) {
    throw new Error("History reference script does not match its policy");
  }
};

/** Separate publication only; callers confirm this exact output before
 * admission. No script checks its validity, so `validTo` (Unix ms) is the
 * caller's: every publication must be bounded, since a publication that
 * never lands holds its funding inputs until its validity ends. */
export const buildEventHistoryPublication = async (
  context: EventHistoryBuildContext,
  payload: EventHistoryPayload | string,
  reclaimAuth: CredentialD,
  validTo: number,
) => {
  authenticateContext(context);
  const plan =
    typeof payload === "string"
      ? prepareEventHistoryPayloadCbor(payload, reclaimAuth, context.recipe)
      : prepareEventHistoryPayload(payload, reclaimAuth, context.recipe);
  if (plan.kind !== "External")
    throw new Error("Inline history data needs no publication");
  const id =
    "DepositPayload" in plan.payload
      ? plan.payload.DepositPayload.event.id
      : plan.payload.WithdrawalPayload.event.id;
  if (
    context.fundingInputs.some(
      (input) =>
        input.txHash === id.transactionId &&
        BigInt(input.outputIndex) === id.outputIndex,
    )
  ) {
    throw new Error("History publication cannot consume the event nonce");
  }
  requireDistinct(context.fundingInputs);
  if (!Number.isSafeInteger(validTo))
    throw new Error("History publication requires a safe integer validity");
  const tx = await context.lucid
    .newTx()
    .collectFrom([...context.fundingInputs])
    .pay.ToContract(
      context.applied.retention.address,
      { kind: "inline", value: plan.datumCbor },
      {},
    )
    .validTo(validTo)
    .complete({ coinSelection: false, localUPLCEval: true });
  return { tx, plan, publicationOutputIndex: 0 };
};

export type EventHistoryPayloadInput =
  | { readonly payload: EventHistoryPayload; readonly payloadCbor?: never }
  | { readonly payload?: never; readonly payloadCbor: string };

export type EventHistoryAdmission = EventHistoryPayloadInput & {
  readonly reclaimAuth: CredentialD;
  readonly nonce: UTxO;
  /** Locked funds before adding the authentication NFT; structural ADA is explicit. */
  readonly assets: Assets;
  readonly structuralLovelace: bigint;
  readonly structuralRefundKey: string;
  readonly externalData?: UTxO;
  readonly validFrom: number;
  readonly validTo: number;
};

export class EventHistoryPredecessorProtectedError extends Error {
  constructor(readonly protectedUntil: bigint) {
    super("History predecessor is still protected");
    this.name = "EventHistoryPredecessorProtectedError";
  }
}

export class EventHistoryPredecessorConflictError extends Error {
  constructor(
    readonly predecessor: UTxO,
    readonly cause: unknown,
  ) {
    super(
      "History predecessor changed before transaction construction completed",
    );
    this.name = "EventHistoryPredecessorConflictError";
  }
}

/** Build against the current authenticated gap/filler. A retry reruns this
 * function and refreshes indices and time; the nonce and prepublished data stay fixed. */
export const buildEventHistoryAdmission = async (
  context: EventHistoryBuildContext,
  request: EventHistoryAdmission,
) => {
  authenticateContext(context);
  const { lucid, applied, recipe } = context;
  if (request.payload !== undefined && request.payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const plan =
    request.payloadCbor === undefined
      ? prepareEventHistoryPayload(request.payload, request.reclaimAuth, recipe)
      : prepareEventHistoryPayloadCbor(
          request.payloadCbor,
          request.reclaimAuth,
          recipe,
        );
  const payload = plan.payload;
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  if (
    "DepositPayload" in payload !== (recipe.kind === "Deposit") ||
    request.nonce.txHash !== id.transactionId ||
    BigInt(request.nonce.outputIndex) !== id.outputIndex
  ) {
    throw new Error("History admission payload kind or nonce does not match");
  }
  if (
    Object.keys(request.assets).some((unit) =>
      unit.startsWith(applied.policyId),
    )
  ) {
    throw new Error("History admission funding cannot contain history tokens");
  }
  if (
    !Number.isSafeInteger(request.validFrom) ||
    !Number.isSafeInteger(request.validTo)
  )
    throw new Error("History admission requires safe integer validity times");
  const lower = BigInt(
    lucid.slotToUnixTime(lucid.unixTimeToSlot(request.validFrom)),
  );
  const upper =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(request.validTo))) - 1n;
  if (
    lower < 0n ||
    upper < lower ||
    upper - lower > MAX_VALIDITY_RANGE_LENGTH_MS
  ) {
    throw new Error("Invalid history admission validity interval");
  }
  const deployment = {
    policyId: applied.policyId,
    address: applied.address,
    retentionAddress: applied.retention.address,
    inlineLimitBytes: recipe.inlineLimitBytes,
  };
  const witness = await fetchEventHistoryWitness(lucid, deployment, id);
  if (witness.kind !== "Absent")
    throw new Error("History event is already admitted");
  const { anchor } = witness;
  if (lower < anchor.node.protected_until)
    throw new EventHistoryPredecessorProtectedError(
      anchor.node.protected_until,
    );
  let retained: UTxO | undefined;
  if (plan.kind === "External") {
    if (request.externalData === undefined)
      throw new Error("History admission requires confirmed prepublished data");
    [retained] = await lucid.utxosByOutRef([request.externalData]);
    if (
      retained?.datum == null ||
      retained.address !== applied.retention.address ||
      retained.scriptRef != null ||
      datumToHash(
        aikenSerialisedPlutusDataCborPreservingMapOrder(retained.datum),
      ) !== plan.location.External.storage_datum_hash
    ) {
      throw new Error(
        "History publication is unavailable or does not authenticate the complete datum",
      );
    }
  } else if (request.externalData !== undefined)
    throw new Error("Inline history admission cannot substitute external data");
  const inputs = [...context.fundingInputs, request.nonce, anchor.utxo];
  const references = [
    context.hubReference,
    ...(context.scriptReference === undefined ? [] : [context.scriptReference]),
    ...(retained === undefined ? [] : [retained]),
  ];
  requireDistinct(inputs);
  requireDistinct(references);
  if (
    inputs.some((input) =>
      references.some((ref) => outRefLabel(ref) === outRefLabel(input)),
    )
  ) {
    throw new Error("History admission cannot consume a reference input");
  }
  const protectedUntil = upper + recipe.protectionDurationMs;
  const node: EventHistoryNode = {
    position: { Key: [plan.key] },
    next: anchor.node.next,
    protected_until: protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: upper + BigInt(EVENT_WAIT_DURATION_MS),
          location: plan.location,
          structural_lovelace: request.structuralLovelace,
          structural_refund_key: request.structuralRefundKey,
        },
      },
    },
  };
  const nodeCbor = encodeEventHistoryOrderNode(node, plan);
  assertEventHistoryAdmissionFunding(
    nodeCbor,
    { ...request.assets, [applied.policyId + plan.key]: 1n },
    applied.policyId,
    plan.payloadCbor,
    request.structuralLovelace,
  );
  const promotion = anchor.key === plan.key;
  const orderUnit = applied.policyId + plan.key;
  let refundAddress: string | undefined;
  if (promotion) {
    if (
      anchor.node.payload === "RootContent" ||
      !("Filler" in anchor.node.payload)
    )
      throw new Error("Only a filler can be promoted");
    const network = lucid.config().network;
    if (network === undefined)
      throw new Error("Missing history deployment network");
    refundAddress = credentialToAddress(network, {
      type: "Key",
      hash: anchor.node.payload.Filler.refund_key,
    });
  }
  // Every index resolves against the builder's final transaction, so the
  // redeemer cannot drift from the ledger's input and output order.
  // The refund key may also own the wallet's change address, so the refund is
  // the first output paying it: the builder appends change after every pay.
  const refundOutputIndex = (ctx: RedeemerContext): bigint => {
    const index = ctx.outputs.findIndex(
      (output) => output.address === refundAddress,
    );
    if (index < 0)
      throw new Error("History filler refund is missing from final outputs");
    return BigInt(index);
  };
  // The builder's last redeemer pass sees the final transaction, so the order
  // index it resolves is the one the caller is told about.
  let finalOrderOutputIndex: bigint | undefined;
  const operationFor = (ctx: RedeemerContext): EventHistoryOperation => {
    const orderOutputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === applied.address &&
        (output.assets[orderUnit] ?? 0n) === 1n,
      "history order",
    );
    finalOrderOutputIndex = orderOutputIndex;
    const nonceInputIndex = requireInputIndex(
      ctx,
      request.nonce,
      "history nonce",
    );
    const externalIndex =
      retained === undefined
        ? null
        : requireReferenceInputIndex(ctx, retained, "history publication");
    return refundAddress !== undefined
      ? {
          PromoteFiller: {
            filler_input_index: requireInputIndex(
              ctx,
              anchor.utxo,
              "history filler",
            ),
            order_output_index: orderOutputIndex,
            refund_output_index: refundOutputIndex(ctx),
            nonce_input_index: nonceInputIndex,
            external_reference_index: externalIndex,
          },
        }
      : {
          InsertOrder: {
            predecessor_input_index: requireInputIndex(
              ctx,
              anchor.utxo,
              "history predecessor",
            ),
            predecessor_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === applied.address &&
                (output.assets[orderUnit] ?? 0n) === 0n,
              "history predecessor",
            ),
            order_output_index: orderOutputIndex,
            nonce_input_index: nonceInputIndex,
            external_reference_index: externalIndex,
          },
        };
  };
  let tx = lucid
    .newTx()
    .collectFrom([...context.fundingInputs, request.nonce])
    .collectFrom([anchor.utxo], ((ctx) =>
      Data.to(
        requireInputIndex(ctx, anchor.utxo, "history anchor"),
      )) satisfies BuildTxWithRedeemer)
    .readFrom(references)
    .withdraw(applied.rewardAddress, 0n, ((ctx) =>
      Data.to(
        {
          Apply: {
            hub_reference_index: requireReferenceInputIndex(
              ctx,
              context.hubReference,
              "history hub",
            ),
            operation: operationFor(ctx),
          },
        },
        EventHistoryObserve,
      )) satisfies BuildTxWithRedeemer)
    .validFrom(request.validFrom)
    .validTo(request.validTo);
  if (context.scriptReference === undefined)
    tx = tx.attach.Script(applied.validator);
  if (!promotion) {
    tx = tx.mintAssets({ [orderUnit]: 1n }, Data.void()).pay.ToContract(
      applied.address,
      {
        kind: "inline",
        value: replacePlutusConstrFieldCbor(
          replacePlutusConstrFieldCbor(
            anchor.utxo.datum!,
            [1],
            Data.to(new Constr(0, [plan.key])),
          ),
          [2],
          Data.to(protectedUntil),
        ),
      },
      anchor.utxo.assets,
    );
  }
  tx = tx.pay.ToContract(
    applied.address,
    { kind: "inline", value: nodeCbor },
    { ...request.assets, [orderUnit]: 1n },
  );
  if (refundAddress !== undefined)
    tx = tx.pay.ToAddress(refundAddress, {
      lovelace: anchor.utxo.assets.lovelace,
    });
  let completed;
  try {
    completed = await tx.complete({
      coinSelection: false,
      localUPLCEval: true,
      // Lucid chooses collateral independently of coinSelection. Restrict it
      // to this admission's consumed wallet inputs, preserving other nonces.
      presetWalletInputs: [...context.fundingInputs, request.nonce],
    });
  } catch (cause) {
    // Nothing was broadcast. A disappeared predecessor permits a fresh build;
    // an unchanged predecessor preserves the original construction failure.
    if ((await lucid.utxosByOutRef([anchor.utxo])).length === 0)
      throw new EventHistoryPredecessorConflictError(anchor.utxo, cause);
    throw cause;
  }
  const body = CML.Transaction.from_cbor_hex(completed.toCBOR()).body();
  const collateral = body.collateral_inputs();
  const approved = new Set(
    [...context.fundingInputs, request.nonce].map(outRefLabel),
  );
  if (collateral !== undefined) {
    for (let index = 0; index < collateral.len(); index++) {
      const input = collateral.get(index);
      if (
        !approved.has(
          `${input.transaction_id().to_hex()}#${input.index().toString()}`,
        )
      )
        throw new Error(
          "History admission collateral must come from its consumed wallet inputs",
        );
    }
  }
  if (finalOrderOutputIndex === undefined)
    throw new Error("History admission order output was never resolved");
  return {
    tx: completed,
    plan,
    node,
    anchor,
    orderOutputIndex: Number(finalOrderOutputIndex),
  };
};
