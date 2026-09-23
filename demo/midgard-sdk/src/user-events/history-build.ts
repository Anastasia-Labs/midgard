import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
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
import { prepareEventHistoryPayload } from "./history-payload.js";
import { fetchEventHistoryWitness } from "./history-query.js";

export type EventHistoryBuildContext = {
  readonly lucid: LucidEvolution;
  readonly applied: ReturnType<typeof applyEventHistoryValidators>;
  readonly recipe: EventHistoryRecipe;
  readonly hubReference: UTxO;
  readonly scriptReference: UTxO;
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
    scriptReference.scriptRef == null ||
    validatorToScriptHash(scriptReference.scriptRef) !== applied.policyId
  ) {
    throw new Error("History reference script does not match its policy");
  }
};

/** Separate publication only; callers confirm this exact output before admission. */
export const buildEventHistoryPublication = async (
  context: EventHistoryBuildContext,
  payload: EventHistoryPayload,
  reclaimAuth: CredentialD,
) => {
  authenticateContext(context);
  const plan = prepareEventHistoryPayload(payload, reclaimAuth, context.recipe);
  if (plan.kind !== "External")
    throw new Error("Inline history data needs no publication");
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
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

export type EventHistoryAdmission = {
  readonly payload: EventHistoryPayload;
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

/** Build against the current authenticated gap/filler. A retry reruns this
 * function and refreshes indices and time; the nonce and prepublished data stay fixed. */
export const buildEventHistoryAdmission = async (
  context: EventHistoryBuildContext,
  request: EventHistoryAdmission,
) => {
  authenticateContext(context);
  const { lucid, applied, recipe } = context;
  const plan = prepareEventHistoryPayload(
    request.payload,
    request.reclaimAuth,
    recipe,
  );
  const id =
    "DepositPayload" in request.payload
      ? request.payload.DepositPayload.event.id
      : request.payload.WithdrawalPayload.event.id;
  if (
    "DepositPayload" in request.payload !== (recipe.kind === "Deposit") ||
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
    throw new Error("History predecessor is still protected");
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
    context.scriptReference,
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
  assertEventHistoryAdmissionFunding(
    node,
    { ...request.assets, [applied.policyId + plan.key]: 1n },
    applied.policyId,
    request.payload,
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
  if (!promotion) {
    tx = tx
      .mintAssets({ [applied.policyId + plan.key]: 1n }, Data.void())
      .pay.ToContract(
        applied.address,
        {
          kind: "inline",
          value: Data.to(
            { ...anchor.node, next: plan.key, protected_until: protectedUntil },
            EventHistoryNode,
          ),
        },
        anchor.utxo.assets,
      );
  }
  tx = tx.pay.ToContract(
    applied.address,
    { kind: "inline", value: Data.to(node, EventHistoryNode) },
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
  return {
    tx: await tx.complete({ coinSelection: false, localUPLCEval: true }),
    plan,
    node,
    anchor,
    orderOutputIndex: Number(orderIndex),
  };
};
