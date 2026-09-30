import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  buildUnsignedFieldPreimageCertificationProgram,
  buildUnsignedFieldPreimagePublicationProgram,
  fieldPreimagePublicationOutputs,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  certificatePolicy,
  type Harness,
  loadContracts,
  MAX_L1_TX_BYTES,
  NETWORK,
  SIGNING_KEY,
  THREAD_ASSET_NAME,
  WALLET_ADDRESS,
} from "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";

export const setupEmulator = async (): Promise<Harness> => {
  const contracts = await loadContracts();
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    THREAD_ASSET_NAME,
  );
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: SIGNING_KEY.to_bech32(),
        address: WALLET_ADDRESS,
        assets: { lovelace: 500_000_000_000n },
      },
      {
        // The computation-thread token, held by the prover until the chain
        // starts. Minting it would need the real thread policy and a hub
        // oracle; what this file is about is the carriage, so the token is
        // seeded and the thread's own authenticity is out of scope here (it is
        // covered by the fault-proofs lifecycle suites).
        seedPhrase: "",
        privateKey: SIGNING_KEY.to_bech32(),
        address: WALLET_ADDRESS,
        assets: { lovelace: 100_000_000n, [threadUnit]: 1n },
      },
    ],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize: MAX_L1_TX_BYTES },
  );
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromPrivateKey(SIGNING_KEY.to_bech32());
  return { lucid, contracts, threadUnit, walletAddress: WALLET_ADDRESS };
};

/**
 * CML normalises Plutus datums when it frames a transaction while lucid's
 * `Data.to` emits Aiken-style indefinite arrays; the validators compare parsed
 * values, so datum equality here is value-level too.
 */
export const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

export const signedByteCount = (transactionCbor: string): number =>
  transactionCbor.length / 2;

export const feeInputFor = async (harness: Harness): Promise<UTxO> => {
  const candidates = (await harness.lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[harness.threadUnit] === undefined,
  );
  const selected = candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
  return selected;
};

export const submitAndAwait = async (
  harness: Harness,
  unsigned: Awaited<
    ReturnType<ReturnType<LucidEvolution["newTx"]>["complete"]>
  >,
): Promise<{ readonly txHash: string; readonly signedCbor: string }> => {
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  return { txHash, signedCbor };
};

/** Publishes a validator as a plain reference script, parked out of reach of coin selection. */
export const publishReferenceScript = async (
  harness: Harness,
  script: Script,
): Promise<UTxO> => {
  const parkAddress = credentialToAddress(
    NETWORK,
    scriptHashToCredential("2f".repeat(28)),
  );
  const unsigned = await harness.lucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 60_000_000n },
      script,
    )
    .complete();
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
  expect(signedByteCount(signedCbor)).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(
      "reference-script publication omitted its script-ref output",
    );
  }
  const published = await harness.lucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  const utxo = published[0];
  if (published.length !== 1 || utxo === undefined || utxo.scriptRef == null) {
    throw new Error("published reference script was not found");
  }
  return utxo;
};

// ## The carriage, published to and resolved back out of the ledger

export type PublishedCarriage = {
  readonly plan: MidgardFieldCarriagePlan;
  readonly chunkUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
  readonly certificatePolicyId: string;
  readonly publicationBytes: readonly number[];
  readonly certificationBytes: number | undefined;
};

export const publishCarriage = async ({
  harness,
  plan,
  compactCbor,
  witnessSetCompactCbor,
}: {
  readonly harness: Harness;
  readonly plan: MidgardFieldCarriagePlan;
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
}): Promise<PublishedCarriage> => {
  const policy = certificatePolicy();
  const chunkUtxos: UTxO[] = [];
  const publicationBytes: number[] = [];
  for (const publication of fieldPreimagePublicationOutputs(plan)) {
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationProgram(harness.lucid, {
        publication,
        publisherAddress: harness.walletAddress,
      }),
    );
    const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
    publicationBytes.push(signedByteCount(signedCbor));
    const published = (await harness.lucid.utxosAt(harness.walletAddress)).find(
      (utxo) =>
        utxo.txHash === txHash &&
        utxo.datum != null &&
        sameDatumValue(utxo.datum, publication.datumCbor),
    );
    if (published === undefined) {
      throw new Error(
        `published carriage chunk ${publication.chunkIndex.toString()} was not found`,
      );
    }
    chunkUtxos.push(published);
  }
  if (plan.tier !== "Certified") {
    return {
      plan,
      chunkUtxos,
      certificateUtxo: undefined,
      certificatePolicyId: policy.policyId,
      publicationBytes,
      certificationBytes: undefined,
    };
  }
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimageCertificationProgram(harness.lucid, {
      sourceKind: 0n,
      plan,
      certificatePolicyId: policy.policyId,
      certificateAddress: policy.address,
      certificateWitness: {
        kind: "inline_emulator_only",
        certificateScript: policy.script,
      },
      // Handed in reverse ledger order on purpose: the redeemer's indices must
      // be indices into the canonically-sorted reference-input list, not into
      // the order the builder was given.
      chunkUtxos: [...chunkUtxos].reverse(),
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
  const certificateUtxo = (await harness.lucid.utxosAt(policy.address)).find(
    (utxo) => utxo.txHash === txHash,
  );
  if (certificateUtxo === undefined) {
    throw new Error("minted §8.6 certificate was not found");
  }
  return {
    plan,
    chunkUtxos,
    certificateUtxo,
    certificatePolicyId: policy.policyId,
    publicationBytes,
    certificationBytes: signedByteCount(signedCbor),
  };
};

// ## The staged chain

export type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

export type StageResult = {
  readonly nextThreadUtxo: UTxO;
  readonly signedBytes: number;
  readonly outputDatum: string;
};
