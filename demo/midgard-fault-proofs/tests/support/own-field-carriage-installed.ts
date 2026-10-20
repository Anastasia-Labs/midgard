/**
 * The field-carriage roles of the installed application, read off the
 * transactions the ledger watch records: the field publications (inline
 * byte-string datums at the prover address) and the field certificate (the
 * output holding the certificate token). A row places other parties'
 * outputs with the same datums at those addresses and checks that the
 * application's own transactions read only the carriage it published.
 */
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk";
import {
  Data,
  type Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  scriptFromNative,
} from "@lucid-evolution/lucid";

import type {
  LedgerTransaction,
  LedgerWatch,
} from "./own-outputs-installed.js";

/** One of the application's carriage outputs. */
export type CarriageOutput = Readonly<{
  outRef: string;
  address: string;
  datum: string;
}>;

/** Where the installed application's field carriage goes. */
export type FieldCarriageSites = Readonly<{
  /** The prover address field publications are paid to. */
  publisherAddress: string;
  /** Script-decode staging datums, also at the prover address. */
  stagingDatums: readonly string[];
  /** The field-preimage certificate policy. */
  certificatePolicyId: string;
}>;

const isByteStringDatum = (datum: string): boolean => {
  try {
    return typeof Data.from(datum) === "string";
  } catch {
    return false;
  }
};

/** The field publications `transaction` pays, in output order. */
export const publicationsOf = (
  transaction: LedgerTransaction,
  sites: FieldCarriageSites,
): readonly CarriageOutput[] =>
  transaction.outputs.flatMap((output, index) =>
    output.address === sites.publisherAddress &&
    typeof output.datum === "string" &&
    !sites.stagingDatums.includes(output.datum) &&
    Object.keys(output.assets).every((unit) => unit === "lovelace") &&
    isByteStringDatum(output.datum)
      ? [
          {
            outRef: `${transaction.txHash}#${index.toString()}`,
            address: output.address,
            datum: output.datum,
          },
        ]
      : [],
  );

/** The field certificate `transaction` pays, if it pays one. */
export const certificateOf = (
  transaction: LedgerTransaction,
  sites: FieldCarriageSites,
): CarriageOutput | undefined => {
  const unit = `${sites.certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  const index = transaction.outputs.findIndex(
    (output) => output.assets[unit] === 1n,
  );
  const output = transaction.outputs[index];
  if (output === undefined || typeof output.datum !== "string")
    return undefined;
  return {
    outRef: `${transaction.txHash}#${index.toString()}`,
    address: output.address,
    datum: output.datum,
  };
};

/** The application's field carriage over a run, in the order it landed. */
export const fieldCarriageOf = (
  watch: LedgerWatch,
  sites: FieldCarriageSites,
) => {
  const ours = watch.ours();
  const publications = ours.flatMap((transaction) => {
    const outputs = publicationsOf(transaction, sites);
    return outputs.length === 0 ? [] : [{ transaction, outputs }];
  });
  const certificates = ours.flatMap((transaction) => {
    const output = certificateOf(transaction, sites);
    return output === undefined ? [] : [{ transaction, output }];
  });
  /** The application's transactions that read `outRef`, in order. */
  const readersOf = (outRef: string) =>
    ours.filter(({ referenceInputs }) => referenceInputs.includes(outRef));
  return { publications, certificates, readersOf };
};

/**
 * Ada for an output another party places beside a publication: past the
 * minimum a publication-sized inline datum needs.
 */
const BESIDE_LOVELACE = 120_000_000n;

/** Coins the other party's wallet holds, and the ada of each. */
const OTHER_PARTY_COINS = 6;
const OTHER_PARTY_COIN_LOVELACE = 400_000_000n;

/**
 * Another party with a wallet at an enterprise address, funded by `funder`
 * with a few plain coins: from it a copy of the widest publication fits in a
 * transaction just as the application's own publication does from its
 * enterprise address.
 */
export const otherParty = async (
  watch: LedgerWatch,
  { emulator, funder }: { emulator: Emulator; funder: LucidEvolution },
): Promise<LucidEvolution> => {
  const account = generateEmulatorAccount({});
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase, {
    addressType: "Enterprise",
  });
  const address = await lucid.wallet().address();
  await watch.asOtherParty(async () => {
    funder.clearUTxOOverride();
    let tx = funder.newTx();
    for (let coin = 0; coin < OTHER_PARTY_COINS; coin += 1)
      tx = tx.pay.ToAddress(address, { lovelace: OTHER_PARTY_COIN_LOVELACE });
    const unsigned = await tx.complete();
    await (await unsigned.sign.withWallet().complete()).submit();
  });
  return lucid;
};

/**
 * An exact copy of `datum` at `address`, ada only, and with `token` another
 * output holding it beside a token the other party mints: each in a
 * transaction of its own, as a publication-sized datum fills most of one.
 */
export const placeSameDatum = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  { address, datum, token }: { address: string; datum: string; token: boolean },
) => {
  const keyHash = getAddressDetails(await lucid.wallet().address())
    .paymentCredential!.hash;
  const policy = scriptFromNative({ type: "sig", keyHash });
  const unit = `${mintingPolicyToId(policy)}beef`;
  await watch.asOtherParty(async () => {
    // Cleared once: the second transaction spends the first one's change.
    lucid.clearUTxOOverride();
    for (const withToken of token ? [false, true] : [false]) {
      let tx = lucid.newTx();
      if (withToken)
        tx = tx.mintAssets({ [unit]: 1n }).attach.MintingPolicy(policy);
      const unsigned = await tx.pay
        .ToAddressWithData(
          address,
          { kind: "inline", value: datum },
          { lovelace: BESIDE_LOVELACE, ...(withToken ? { [unit]: 1n } : {}) },
        )
        .complete();
      await (await unsigned.sign.withWallet().complete()).submit();
    }
  });
};

/**
 * An exact copy of `datum` at `address` whose transaction id sorts before
 * `below`: submitted ahead of the transaction it copies, into the same block.
 */
export const placeSameDatumBefore = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  { address, datum, below }: { address: string; datum: string; below: string },
) => {
  lucid.clearUTxOOverride();
  for (let attempt = 0n; attempt < 4_096n; attempt += 1n) {
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        { kind: "inline", value: datum },
        { lovelace: BESIDE_LOVELACE + attempt },
      )
      .complete();
    const signed = await unsigned.sign.withWallet().complete();
    if (signed.toHash() < below)
      return await watch.asOtherParty(async () => await signed.submit());
  }
  throw new Error("no copy sorting first was found");
};
