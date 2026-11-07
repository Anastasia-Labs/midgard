import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  Data,
  mintingPolicyToId,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import { type AvailabilityFixture } from "./helpers/availability-challenge-emulator.js";

export const GOLD = "676f6c64";
export const OPENING_LOVELACE =
  parameters.challenger_bond_lovelace +
  parameters.challenge_record_lovelace +
  parameters.max_open_fee_lovelace;
export const label = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;
export const outRef = (value: string) => {
  const [txHash, outputIndex] = value.split("#");
  return { txHash: txHash!, outputIndex: Number(outputIndex) };
};

export const flipped = <A>(
  program: Effect.Effect<A, SDK.DaAvailabilityTransactionError>,
) => Effect.runPromise(Effect.flip(program));

/**
 * Moves every challenger coin into token-bearing coins of `coins` lovelace
 * (ten units of a challenger-minted token each); the rest leaves the wallet.
 * Afterwards the wallet holds no ADA-only coin.
 */
export const makeTokenOnly = async (
  f: AvailabilityFixture,
  coins: readonly bigint[],
) => {
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const policy = scriptFromNative({ type: "sig", keyHash: f.challengerKey });
  const unit = mintingPolicyToId(policy) + GOLD;
  let tx = f.lucid
    .newTx()
    .collectFrom(await f.lucid.wallet().getUtxos())
    .mintAssets({ [unit]: 10n * BigInt(coins.length) })
    .attach.MintingPolicy(policy);
  for (const lovelace of coins)
    tx = tx.pay.ToAddress(f.challenger.address, { lovelace, [unit]: 10n });
  const built = await tx.complete({
    coinSelection: false,
    changeAddress: f.responder.address,
    localUPLCEval: true,
  });
  await (await built.sign.withWallet().complete()).submit();
  f.emulator.awaitBlock(1);
  const wallet = await f.lucid.wallet().getUtxos();
  expect(wallet).toHaveLength(coins.length);
  for (const u of wallet) expect(u.assets[unit]).toBe(10n);
  return unit;
};

/** The fee, inputs and outputs a transaction's bytes carry. */
const layoutOf = (cbor: string) => {
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  return {
    fee: body.fee(),
    inputs: Array.from({ length: body.inputs().len() }, (_, i) => {
      const input = body.inputs().get(i);
      return `${input.transaction_id().to_hex()}#${input.index()}`;
    }),
    outputs: Array.from({ length: body.outputs().len() }, (_, i) =>
      coreToTxOutput(body.outputs().get(i)),
    ),
  };
};

/**
 * Checks a preparation's bytes: the exact fee, only `spendable` coins spent,
 * `reserved` paid as ADA-only coins with no datum first, then change at the
 * wallet that returns every spent token.
 */
export const expectPreparation = (
  cbor: string,
  input: {
    readonly address: string;
    readonly spendable: readonly UTxO[];
    readonly reserved: readonly bigint[];
    readonly unit: string;
  },
) => {
  const layout = layoutOf(cbor);
  expect(layout.fee).toBe(parameters.max_open_fee_lovelace);
  const spent = input.spendable.filter((u) => layout.inputs.includes(label(u)));
  expect(spent).toHaveLength(layout.inputs.length);
  expect(spent.every((u) => (u.assets[input.unit] ?? 0n) > 0n)).toBe(true);
  expect(layout.outputs.slice(0, input.reserved.length)).toEqual(
    input.reserved.map((lovelace) => ({
      address: input.address,
      assets: { lovelace },
    })),
  );
  const change = layout.outputs.slice(input.reserved.length);
  expect(change.length).toBeGreaterThan(0);
  for (const o of change) expect(o.address).toBe(input.address);
  expect(change.reduce((n, o) => n + (o.assets[input.unit] ?? 0n), 0n)).toBe(
    spent.reduce((n, u) => n + u.assets[input.unit]!, 0n),
  );
  return layout;
};

/** The Open's commitment source: the retained DA attestation datum. */
export const attestationHistory =
  (f: AvailabilityFixture, commitment: SDK.DaAvailabilityCommitment) =>
  async () => [
    Data.to(
      {
        header_hash: f.target.headerHash,
        availability_commitment: commitment,
        da_threshold: 1n,
        committee_signers_hash: "88".repeat(32),
        rescue_beneficiary: {
          paymentCredential: { PublicKeyCredential: [f.responderKey] },
          stakeCredential: null,
        },
        attested_signers: "00".repeat(32),
        attestation_count: 1n,
      },
      SDK.DaAttestationDatum,
    ),
  ];

export const workflowCollateral = (f: AvailabilityFixture) => {
  const protocol = f.lucid.config().protocolParameters!;
  return SDK.daAvailabilityWorkflowCollateralLovelace({
    parameters,
    challengerAddress: f.challenger.address,
    collateralPercentage: protocol.collateralPercentage,
    coinsPerUtxoByte: protocol.coinsPerUtxoByte,
  });
};

/** The smallest coin of `tokens` at `address` that meets its own min-ADA. */
export const smallestTokenChange = (
  coinsPerUtxoByte: bigint,
  address: string,
  tokens: Assets,
) => {
  let lovelace = 0n;
  for (;;) {
    const next = calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
      address,
      assets: { ...tokens, lovelace },
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    if (next <= lovelace) return lovelace;
    lovelace = next;
  }
};
