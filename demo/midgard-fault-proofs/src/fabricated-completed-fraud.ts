/** Authenticate the current queue node and bind terminal proof minting to its
 * completed-fraud marker. A pointer/DA continuation is fetched afresh. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  fromText,
  getAddressDetails,
  type LucidEvolution,
  type RedeemerContext,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { fabricatedProofValidity } from "./fabricated-proof-validity.js";
import { requireSingletonUtxo } from "./runtime.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import { governedUserEventAddress } from "./workflow/user-event-address.js";

export const prepareFabricatedCompletedFraud = async ({
  lucid,
  hubOraclePolicyId,
  stateQueuePolicyId,
  headerHash,
  headerEnd,
  proofAsset,
  queueReferenceScript,
  now,
}: {
  readonly lucid: LucidEvolution;
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly headerHash: string;
  readonly headerEnd: bigint;
  readonly proofAsset: string;
  readonly queueReferenceScript?: UTxO;
  readonly now: number;
}) => {
  const network = lucid.config().network;
  if (network === undefined)
    throw new Error("Terminal proof requires a network");
  const validity = fabricatedProofValidity(headerEnd, now);
  if (!/^[0-9a-f]{64}$/u.test(proofAsset) || proofAsset.slice(8) !== headerHash)
    throw new Error("Terminal proof asset does not name the carried header");
  const hubAddress = credentialToAddress(
    network,
    scriptHashToCredential(hubOraclePolicyId),
  );
  const hub = await requireSingletonUtxo({
    lucid,
    address: hubAddress,
    unit: toUnit(hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
    label: "terminal proof hub oracle",
  });
  if (hub.address !== hubAddress || hub.datum == null)
    throw new Error("Terminal proof hub has no inline datum");
  const hubDatum = Data.from(hub.datum, SDK.HubOracleDatum);
  if (hubDatum.state_queue !== stateQueuePolicyId)
    throw new Error("Terminal proof carried queue policy differs from the hub");
  const address = governedUserEventAddress(network, hubDatum.state_queue_addr);
  const credential = getAddressDetails(address).paymentCredential;
  if (credential?.type !== "Script")
    throw new Error("Queue address is not a script");
  const unit = toUnit(stateQueuePolicyId, fromText("MBLC") + headerHash);
  const input = await requireSingletonUtxo({
    lucid,
    address,
    unit,
    label: "terminal proof queue node",
  });
  if (
    input.address !== address ||
    input.datum == null ||
    input.scriptRef != null ||
    Object.entries(input.assets).some(
      ([asset, quantity]) =>
        asset !== "lovelace" && (asset !== unit || quantity !== 1n),
    )
  )
    throw new Error(
      "Terminal proof queue node has invalid datum, assets or script reference",
    );
  const view = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(input),
  );
  const node = Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(view));
  if (
    !(typeof view.key === "object" && view.key.Key.key === headerHash) ||
    Effect.runSync(SDK.hashBlockHeader(node.header)) !== headerHash ||
    node.header.endTime !== headerEnd
  )
    throw new Error(
      "Terminal proof queue header does not match the carried state",
    );
  if (
    node.proven_fraud !== null &&
    (!/^[0-9a-f]{64}$/u.test(node.proven_fraud) ||
      node.proven_fraud.slice(8) !== headerHash)
  )
    throw new Error("Existing completed-fraud marker names another header");
  const previouslyRecorded = node.proven_fraud !== null;
  if (
    !previouslyRecorded &&
    (queueReferenceScript?.scriptRef == null ||
      validatorToScriptHash(queueReferenceScript.scriptRef) !== credential.hash)
  )
    throw new Error(
      "Terminal proof requires the governed queue spending reference script",
    );
  const datum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      proven_fraud: proofAsset,
    }) as SDK.LinkedListNodeView["data"],
  });
  const matches = outputWithDatumAndUnitPredicate({ address, datum, unit });
  const witness = (ctx: RedeemerContext): SDK.CompletedFraudWitness =>
    previouslyRecorded
      ? {
          PreviouslyRecorded: {
            reference_input_index: SDK.requireReferenceInputIndex(
              ctx,
              input,
              "completed fraud queue",
            ),
          },
        }
      : {
          RecordedOutput: {
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              matches,
              "completed fraud queue",
            ),
          },
        };
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, input, "completed fraud queue");
    return Data.to(
      {
        RecordCompletedFraud: {
          state_queue_input_index: SDK.requireInputIndex(
            ctx,
            input,
            "completed fraud queue",
          ),
          state_queue_output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            matches,
            "completed fraud queue",
          ),
          fraud_proof_asset_name: proofAsset,
        },
      },
      SDK.StateQueueSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  return {
    input,
    previouslyRecorded,
    queueReferenceScript: previouslyRecorded ? undefined : queueReferenceScript,
    witness,
    apply: (tx: TxBuilder): TxBuilder => {
      const bounded = tx
        .validFrom(validity.validFrom)
        .validTo(validity.validTo);
      return previouslyRecorded
        ? bounded.readFrom([input])
        : bounded
            .collectFrom([input], redeemer)
            .readFrom([queueReferenceScript!])
            .pay.ToContract(
              address,
              { kind: "inline", value: datum },
              input.assets,
            );
    },
  };
};
