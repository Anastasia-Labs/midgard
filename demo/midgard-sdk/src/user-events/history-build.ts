import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  CML,
  Constr,
  credentialToAddress,
  Data,
  datumToHash,
  fromText,
  type LucidEvolution,
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
  type applyEventHistoryValidators,
  EventHistoryNode,
  EventHistoryObserve,
  type EventHistoryOperation,
  type EventHistoryPayload,
  type EventHistoryRecipe,
} from "./history.js";
import { assertEventHistoryAdmissionFunding } from "./history-funding.js";
import {
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

const inputIndex = (inputs: readonly UTxO[], target: UTxO): bigint => {
  const index = [...inputs]
    .sort(compareOutRefs)
    .findIndex((input) => outRefLabel(input) === outRefLabel(target));
  if (index < 0) throw new Error("Missing history transaction input");
  return BigInt(index);
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

/** Separate publication only; callers confirm this exact output before admission. */
export const buildEventHistoryPublication = async (
  context: EventHistoryBuildContext,
  payload: EventHistoryPayload | string,
  reclaimAuth: CredentialD,
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
  const tx = await context.lucid
    .newTx()
    .collectFrom([...context.fundingInputs])
    .pay.ToContract(
      context.applied.retention.address,
      { kind: "inline", value: plan.datumCbor },
      {},
    )
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
  const nodeCbor =
    plan.kind === "Inline"
      ? replacePlutusConstrFieldCbor(
          Data.to(node, EventHistoryNode),
          [3, 0, 2, 0],
          plan.payloadCbor,
        )
      : Data.to(node, EventHistoryNode);
  assertEventHistoryAdmissionFunding(
    nodeCbor,
    { ...request.assets, [applied.policyId + plan.key]: 1n },
    applied.policyId,
    plan.payloadCbor,
    request.structuralLovelace,
  );
  const promotion = anchor.key === plan.key;
  const orderIndex = promotion ? 0n : 1n;
  const externalIndex =
    retained === undefined ? null : inputIndex(references, retained);
  const operation: EventHistoryOperation = promotion
    ? {
        PromoteFiller: {
          filler_input_index: inputIndex(inputs, anchor.utxo),
          order_output_index: orderIndex,
          refund_output_index: 1n,
          nonce_input_index: inputIndex(inputs, request.nonce),
          external_reference_index: externalIndex,
        },
      }
    : {
        InsertOrder: {
          predecessor_input_index: inputIndex(inputs, anchor.utxo),
          predecessor_output_index: 0n,
          order_output_index: orderIndex,
          nonce_input_index: inputIndex(inputs, request.nonce),
          external_reference_index: externalIndex,
        },
      };
  let tx = lucid
    .newTx()
    .collectFrom([...context.fundingInputs, request.nonce])
    .collectFrom([anchor.utxo], Data.to(inputIndex(inputs, anchor.utxo)))
    .readFrom(references)
    .withdraw(
      applied.rewardAddress,
      0n,
      Data.to(
        {
          Apply: {
            hub_reference_index: inputIndex(references, context.hubReference),
            operation,
          },
        },
        EventHistoryObserve,
      ),
    )
    .validFrom(request.validFrom)
    .validTo(request.validTo);
  if (context.scriptReference === undefined)
    tx = tx.attach.Script(applied.validator);
  if (!promotion) {
    tx = tx
      .mintAssets({ [applied.policyId + plan.key]: 1n }, Data.void())
      .pay.ToContract(
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
    { ...request.assets, [applied.policyId + plan.key]: 1n },
  );
  if (promotion) {
    if (
      anchor.node.payload === "RootContent" ||
      !("Filler" in anchor.node.payload)
    )
      throw new Error("Only a filler can be promoted");
    const network = lucid.config().network;
    if (network === undefined)
      throw new Error("Missing history deployment network");
    tx = tx.pay.ToAddress(
      credentialToAddress(network, {
        type: "Key",
        hash: anchor.node.payload.Filler.refund_key,
      }),
      { lovelace: anchor.utxo.assets.lovelace },
    );
  }
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
  return {
    tx: completed,
    plan,
    node,
    anchor,
    orderOutputIndex: Number(orderIndex),
  };
};
