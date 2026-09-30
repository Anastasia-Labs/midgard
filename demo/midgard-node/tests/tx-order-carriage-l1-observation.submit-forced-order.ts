import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach } from "vitest";

import { observeVisibleTxOrderCarriage } from "../src/fibers/fetch-and-insert-tx-order-utxos.js";
import { NodeConfig } from "../src/services/index.js";
import {
  type Harness,
  makeHarness,
  publishCarriage,
  submitAndObserve,
} from "./tx-order-carriage-l1-observation.native-transaction-cbor.js";

/**
 * Builds, submits and observes a forced order carrying `submittedTxCbor`'s material
 * under whichever tiers `inlineReserveBytes` leaves the planner.
 */
export const submitForcedOrder = async ({
  harness,
  submittedTxCbor,
  inlineReserveBytes,
}: {
  readonly harness: Harness;
  readonly submittedTxCbor: Buffer;
  readonly inlineReserveBytes?: number;
}): Promise<{
  readonly plan: SDK.TxOrderCarriagePlan;
  readonly orderUtxo: SDK.TxOrderUTxOV1;
}> => {
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor,
    owner: harness.creatorKeyHash,
  });
  const plan = SDK.planTxOrderMaterialCarriage({
    material,
    owner: harness.creatorKeyHash,
    ...(inlineReserveBytes === undefined ? {} : { inlineReserveBytes }),
  });
  const carriageUtxos = await publishCarriage(harness, plan);
  // Largest first: the order spends the biggest UTxO, so coin selection never
  // needs to reach for the small one the transaction is also referencing.
  const walletUtxos = [...(await harness.lucid.wallet().getUtxos())].sort(
    (left, right) => Number(right.assets.lovelace - left.assets.lovelace),
  );
  const nonceInput = walletUtxos[0];
  const unrelatedReference = walletUtxos[walletUtxos.length - 1];
  if (
    nonceInput === undefined ||
    unrelatedReference === undefined ||
    walletUtxos.length < 2
  ) {
    throw new Error("the emulator creator must hold two spendable UTxOs");
  }
  // The order's reference-input set is deliberately *not* only carriage: an
  // unrelated input rides along the way a hub-oracle reference input does in a
  // real order, so the vector's positional indices have to skip it and the
  // reader's own canonical ordering has to agree with the ledger's about where
  // it lands.
  const referenceInputs = [...carriageUtxos, unrelatedReference];
  const unit = `${harness.contracts.txOrder.policyId}${Buffer.from("order").toString("hex")}`;
  const datum: SDK.TxOrderDatum = {
    event: {
      id: SDK.outputReferenceFromUTxO(nonceInput),
      tx: {
        tx_id: material.transactionId,
        transaction_commitment: material.transactionCommitment,
        submitted_source: material.submitted_source,
      },
    },
    inclusion_time: BigInt(harness.emulator.now()),
    witness: harness.contracts.txOrder.policyId,
    refund_address: {
      paymentCredential: {
        PublicKeyCredential: [harness.creatorKeyHash.toString("hex")],
      },
      stakeCredential: null,
    },
    refund_datum: "NoDatum",
  };
  const mintRedeemer = Data.to(
    {
      event: {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 0n,
        },
      },
      material_carriage: [
        ...SDK.txOrderMaterialCarriageVector({
          plan,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          referenceInputs,
        }),
      ],
    } satisfies SDK.TxOrderMintRedeemer,
    SDK.TxOrderMintRedeemer,
  );
  const tx = await harness.lucid
    .newTx()
    .collectFrom([nonceInput])
    .readFrom([...referenceInputs])
    .mintAssets({ [unit]: 1n }, mintRedeemer)
    .attach.MintingPolicy(harness.contracts.txOrder.mintingScript)
    .pay.ToAddressWithData(
      harness.contracts.txOrder.spendingScriptAddress,
      { kind: "inline", value: Data.to(datum, SDK.TxOrderDatum) },
      { lovelace: 3_000_000n, [unit]: 1n },
    )
    .complete({ localUPLCEval: true });
  await submitAndObserve(harness, tx);

  // Read the order back off the ledger through the same authenticator the
  // ingestion walk uses, so the payload under test is the one L1 holds.
  const orderUtxos = await harness.lucid.utxosAt(
    harness.contracts.txOrder.spendingScriptAddress,
  );
  const [orderUtxo] = await Effect.runPromise(
    SDK.utxosToTxOrderUTxOs(orderUtxos, harness.contracts.txOrder.policyId),
  );
  if (orderUtxo === undefined) {
    throw new Error("the submitted forced order is not visible on the ledger");
  }
  return { plan, orderUtxo };
};

const carriageEffect = (harness: Harness, orderUtxo: SDK.TxOrderUTxOV1) =>
  observeVisibleTxOrderCarriage(orderUtxo, harness.contracts.txOrder.policyId, {
    timeoutMs: 10_000,
  }).pipe(
    Effect.provideService(NodeConfig, {
      L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
      L1_KUPO_KEY: harness.l1.kupoUrl,
    } as never),
  );

export const readCarriage = (harness: Harness, orderUtxo: SDK.TxOrderUTxOV1) =>
  Effect.runPromise(carriageEffect(harness, orderUtxo));

/**
 * The `LucidError` a read failed with, kept whole.
 *
 * `Effect.runPromise` rejects with a fiber failure that copies the error's
 * message and drops its `cause`, so a negative that wants to pin *which* refusal
 * fired — rather than only that the walk refused — flips the failure into the
 * success channel instead of catching the rejection. A read that unexpectedly
 * succeeds still fails the test: the flip puts its value in the error channel.
 */
export const readCarriageFailure = (
  harness: Harness,
  orderUtxo: SDK.TxOrderUTxOV1,
): Promise<SDK.LucidError> =>
  Effect.runPromise(Effect.flip(carriageEffect(harness, orderUtxo)));

/** Kupo's answer for one output reference, asked for directly. */
export const kupoMatchesOf = async (
  harness: Harness,
  outRef: { readonly txHash: string; readonly outputIndex: number },
  query = "",
): Promise<readonly Record<string, unknown>[]> => {
  const response = await fetch(
    `${harness.l1.kupoUrl}/matches/${outRef.outputIndex.toString()}@${outRef.txHash}${query}`,
  );
  return (await response.json()) as readonly Record<string, unknown>[];
};

let openHarness: Harness | null = null;

export const withHarness = async (): Promise<Harness> => {
  const harness = await makeHarness();
  openHarness = harness;
  return harness;
};

afterEach(async () => {
  await openHarness?.l1.close();
  openHarness = null;
});
