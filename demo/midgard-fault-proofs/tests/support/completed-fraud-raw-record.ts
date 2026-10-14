/** A hand-built standalone record spend of a completed-fraud queue node, and
 * the state-queue spend's trace for each refused record. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { excludeUtxo } from "../../src/spend-input-witness.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import type { setupCompletedFraudQueue } from "./completed-fraud-queue-harness.js";
import { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";

type Queue = Awaited<ReturnType<typeof setupCompletedFraudQueue>>;

/** Where the state-queue spend refuses each standalone record. */
export const RECORD_REFUSED_AT = {
  pastMaturity: /upper < before\.block_time \+ block_maturity_duration_v1/u,
  citedName: /asset_name == proof_asset/u,
  recordedHeader: /asset_name_to_header_hash\(proof_asset\) == header_hash/u,
  citedPolicy: /expect input_nft_policy_id == nft_policy_id/u,
  citedSingleToken: /expect \[Pair\(sym, tn_qty_pairs\)\]/u,
  unmarkedNode: /before\.proven_fraud == None/u,
  mintedHere: /quantity_of\(tx\.mint, fraud_proof_policy, proof_asset\) == 1/u,
} as const;

export const currentNode = async (h: Queue) => {
  const input = await h.lucid.utxoByUnit(h.unit);
  const view = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(input),
  );
  const node = Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(view));
  return { input, view, node };
};

/** A hand-built record spend of the current node. `cite: null` is the
 * same-transaction path (`None`); otherwise the redeemer names `cite`'s
 * position among the final reference inputs. */
export const buildRawRecord = async (
  h: Queue,
  {
    cite,
    read = [],
    proofName = h.proofAsset,
    mintHere = false,
    validFrom,
    validTo,
  }: {
    readonly cite: UTxO | null;
    readonly read?: readonly UTxO[];
    readonly proofName?: string;
    readonly mintHere?: boolean;
    readonly validFrom?: number;
    readonly validTo?: number;
  },
) => {
  const { input, view, node } = await currentNode(h);
  const datum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      proven_fraud: proofName,
    }) as SDK.LinkedListNodeView["data"],
  });
  const label = "raw completed fraud record";
  const matches = outputWithDatumAndUnitPredicate({
    address: h.address,
    datum,
    unit: h.unit,
  });
  const redeemer = ((ctx) =>
    Data.to(
      {
        RecordCompletedFraud: {
          state_queue_input_index: SDK.requireInputIndex(ctx, input, label),
          state_queue_output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            matches,
            label,
          ),
          fraud_proof_asset_name: proofName,
          m_fraud_proof_ref_input_index:
            cite === null
              ? null
              : SDK.requireReferenceInputIndex(ctx, cite, label),
        },
      },
      SDK.StateQueueSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const references = [
    ...(cite === null ? [] : [cite]),
    ...read,
    h.queueReferenceScript,
  ];
  const walletUtxos = references.reduce<readonly UTxO[]>(
    (candidates, excluded) => excludeUtxo(candidates, excluded),
    await h.lucid.wallet().getUtxos(),
  );
  let tx = h.lucid
    .newTx()
    .collectFrom([input], redeemer)
    .readFrom(references)
    .pay.ToContract(h.address, { kind: "inline", value: datum }, input.assets)
    .validFrom(validFrom ?? Number(node.block_time) + 1000)
    .validTo(validTo ?? h.emulator.now() + 120_000);
  if (mintHere)
    tx = tx
      .mintAssets({ [toUnit(h.proofPolicy, proofName)]: 1n })
      .attach.MintingPolicy(h.proofIssuer);
  return tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...walletUtxos],
  });
};

/** The state-queue spend refuses `buildRawRecord(h, options)` at `check`. */
export const refuseRecord = (
  h: Queue,
  options: Parameters<typeof buildRawRecord>[1],
  check: RegExp,
) =>
  expectOnchainRefusal(() => buildRawRecord(h, options), {
    refusedBy: "state_queue",
    check,
  });
