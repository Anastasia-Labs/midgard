/** Record an already-finalized fraud proof on its state-queue node.
 *
 * Finalize mints the permanent proof token without touching the queue node.
 * This standalone transaction writes `proven_fraud` afterwards, citing that
 * token by reference, so the convicted block cannot merge. It is
 * permissionless: the authenticated token is the authority, and any wallet
 * pays the fee. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { resolveCompletedFraudQueueNode } from "../fabricated-completed-fraud.js";
import { fabricatedProofValidity } from "../fabricated-proof-validity.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";

export type RecordCompletedFraudByReferenceOutcome =
  | { readonly kind: "built"; readonly tx: TxSignBuilder }
  /** The node already carries a completed-fraud marker; nothing to do. */
  | { readonly kind: "already-recorded" }
  /** No validity window remains before the node's merge deadline. Callers
   * must surface this (log and count); the builder submits nothing. */
  | {
      readonly kind: "past-maturity";
      readonly headerHash: string;
      readonly nodeBlockTime: bigint;
      readonly deadline: bigint;
    };

export type RecordCompletedFraudByReferenceParams = {
  readonly lucid: LucidEvolution;
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly fraudProofPolicyId: string;
  /** 28-byte hex header hash of the convicted node. */
  readonly headerHash: string;
  /** The header's `end_time`, used to authenticate the node. */
  readonly headerEnd: bigint;
  /** 32-byte hex asset name: 4-byte category id followed by `headerHash`. */
  readonly proofAsset: string;
  /** A UTxO holding exactly one `(fraudProofPolicyId, proofAsset)` token and
   * nothing else but ADA, with the fraud-proof inline datum: normally the
   * permanent output finalize left at the fraud-proof address. */
  readonly fraudProofTokenUtxo: UTxO;
  /** Governed state-queue spending reference script. */
  readonly queueReferenceScript: UTxO;
  readonly now: number;
};

const HEX_ASSET_NAME = /^[0-9a-f]{64}$/u;

const requireProofTokenUtxo = (
  utxo: UTxO,
  fraudProofPolicyId: string,
  proofAsset: string,
): void => {
  const unit = toUnit(fraudProofPolicyId, proofAsset);
  const nonAda = Object.entries(utxo.assets).filter(
    ([asset, quantity]) => asset !== "lovelace" && quantity !== 0n,
  );
  if (nonAda.length !== 1 || nonAda[0]![0] !== unit || nonAda[0]![1] !== 1n)
    throw new Error(
      "Completed-fraud record reference must hold exactly the one proof token",
    );
  if (utxo.datum == null)
    throw new Error("Completed-fraud record reference has no inline datum");
  Data.from(utxo.datum, SDK.FraudProofTokenDatum);
};

/** Build (not submit) the standalone record of an existing proof token. */
export const buildRecordCompletedFraudByReferenceTx = async ({
  lucid,
  hubOraclePolicyId,
  stateQueuePolicyId,
  fraudProofPolicyId,
  headerHash,
  headerEnd,
  proofAsset,
  fraudProofTokenUtxo,
  queueReferenceScript,
  now,
}: RecordCompletedFraudByReferenceParams): Promise<RecordCompletedFraudByReferenceOutcome> => {
  if (!HEX_ASSET_NAME.test(proofAsset) || proofAsset.slice(8) !== headerHash)
    throw new Error("Completed-fraud proof asset does not name the header");
  requireProofTokenUtxo(fraudProofTokenUtxo, fraudProofPolicyId, proofAsset);
  const { input, view, node, address, credential, unit } =
    await resolveCompletedFraudQueueNode({
      lucid,
      hubOraclePolicyId,
      stateQueuePolicyId,
      headerHash,
      headerEnd,
    });
  if (node.proven_fraud !== null) return { kind: "already-recorded" };
  const deadline = node.block_time + SDK.MATURITY_DURATION_MS;
  // `fabricatedProofValidity` needs at least one second before the deadline;
  // its exclusive `validTo` may equal the deadline, which the ledger turns
  // into an inclusive upper bound strictly below it.
  if (BigInt(now) + 1000n >= deadline)
    return {
      kind: "past-maturity",
      headerHash,
      nodeBlockTime: node.block_time,
      deadline,
    };
  const validity = fabricatedProofValidity({
    nodeBlockTime: node.block_time,
    now,
  });
  if (
    queueReferenceScript.scriptRef == null ||
    validatorToScriptHash(queueReferenceScript.scriptRef) !== credential.hash
  )
    throw new Error(
      "Completed-fraud record requires the governed queue spending reference script",
    );
  const datum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      proven_fraud: proofAsset,
    }) as SDK.LinkedListNodeView["data"],
  });
  const matches = outputWithDatumAndUnitPredicate({ address, datum, unit });
  const label = "completed fraud record by reference";
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, input, label);
    return Data.to(
      {
        RecordCompletedFraud: {
          state_queue_input_index: SDK.requireInputIndex(ctx, input, label),
          state_queue_output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            matches,
            label,
          ),
          fraud_proof_asset_name: proofAsset,
          m_fraud_proof_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            fraudProofTokenUtxo,
            label,
          ),
        },
      },
      SDK.StateQueueSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const walletUtxos = [fraudProofTokenUtxo, queueReferenceScript].reduce<
    readonly UTxO[]
  >(
    (candidates, excluded) => excludeUtxo(candidates, excluded),
    await lucid.wallet().getUtxos(),
  );
  const tx = await lucid
    .newTx()
    .collectFrom([input], redeemer)
    .readFrom([fraudProofTokenUtxo, queueReferenceScript])
    .pay.ToContract(address, { kind: "inline", value: datum }, input.assets)
    .validFrom(validity.validFrom)
    .validTo(validity.validTo)
    .complete({
      localUPLCEval: true,
      presetWalletInputs: [...walletUtxos],
    });
  return { kind: "built", tx };
};
