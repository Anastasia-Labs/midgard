import { type Assets, assetsToValue, CML } from "@lucid-evolution/lucid";

import type { WatcherProverFundingReservationInput } from "./prover-funding-reservation.js";
import { transactionInputOutRefs } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

/** Consecutive complete reads that must agree before an orphan is cancelled:
 * with the read backoff they span several blocks, so one stale tip, rollback
 * or mempool lag never starts a cancel. */
export const WATCHER_ORPHAN_CANCEL_AFTER_READS = 3;

/** How long after signing a cancel stays valid. */
export const WATCHER_ORPHAN_CANCEL_VALIDITY_MS = 3_600_000;

/** The largest fee a cancel pays: above any one-witness spend's fee. */
export const WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE = 1_000_000n;

/** The prover wallet as one cancel sees it, opened only to sign it. */
export type WatcherOrphanCancelSession = Readonly<{
  slotOf: (unixTimeMs: number) => number;
  timeOf: (slot: number) => number;
  sign: (transaction: CML.Transaction) => Promise<CML.TransactionWitnessSet>;
}>;

/** Signs and submits cancels of orphaned funding back to the prover wallet. */
export type WatcherOrphanCancelWallet = Readonly<{
  address: string;
  open: () => Promise<WatcherOrphanCancelSession>;
  submit: (signedTransactionCborHex: string) => Promise<void>;
}>;

export type WatcherOrphanCancelFees = Readonly<{
  minFeeA: bigint;
  minFeeB: bigint;
  coinsPerUtxoByte: bigint;
}>;

export type WatcherOrphanCancelClause =
  | "inputs"
  | "output"
  | "mint"
  | "certificates"
  | "governance"
  | "withdrawals"
  | "scripts"
  | "metadata"
  | "reference_inputs"
  | "collateral"
  | "fee"
  | "validity";

/** A cancel the wallet refuses to sign: it is not a plain return of the
 * orphan's own funding to the wallet. */
export class WatcherOrphanCancelRefusedError extends Error {
  constructor(
    readonly clause: WatcherOrphanCancelClause,
    detail: string,
  ) {
    super(`orphan cancel refused (${clause}): ${detail}`);
    this.name = "WatcherOrphanCancelRefusedError";
  }
}

const refuse = (
  clause: WatcherOrphanCancelClause,
  violated: boolean,
  detail: string,
) => {
  if (violated) throw new WatcherOrphanCancelRefusedError(clause, detail);
};

/**
 * The only transaction a cancel signs: it spends nothing but out-refs the
 * orphan leases, pays one output to the wallet's own address, carries no
 * mint, certificate, governance action, withdrawal, script, datum,
 * redeemer, metadata, reference input or collateral, pays at most the
 * maximum fee, and expires by `latestValidTo`. Checked on the decoded
 * transaction that is signed.
 */
export const assertWatcherOrphanCancel = (
  transaction: CML.Transaction,
  bounds: Readonly<{
    leased: ReadonlySet<string>;
    address: string;
    latestValidTo: number;
    timeOf: (slot: number) => number;
  }>,
): void => {
  const body = transaction.body();
  const witnesses = transaction.witness_set();
  const spent = transactionInputOutRefs(body.inputs());
  refuse(
    "inputs",
    spent.length === 0 || spent.some((outRef) => !bounds.leased.has(outRef)),
    "it spends an out-ref the orphan does not lease",
  );
  const outputs = body.outputs();
  const output = outputs.len() === 1 ? outputs.get(0) : undefined;
  refuse(
    "output",
    output === undefined ||
      Buffer.from(output.address().to_raw_bytes()).toString("hex") !==
        Buffer.from(
          CML.Address.from_bech32(bounds.address).to_raw_bytes(),
        ).toString("hex"),
    "it does not pay exactly one output to the wallet's own address",
  );
  refuse("mint", body.mint() !== undefined, "it mints");
  refuse("certificates", body.certs() !== undefined, "it carries certificates");
  refuse(
    "governance",
    body.voting_procedures() !== undefined ||
      body.proposal_procedures() !== undefined ||
      body.current_treasury_value() !== undefined ||
      body.donation() !== undefined,
    "it carries a governance action",
  );
  refuse(
    "withdrawals",
    body.withdrawals() !== undefined,
    "it withdraws rewards",
  );
  refuse(
    "scripts",
    !transaction.is_valid() ||
      body.script_data_hash() !== undefined ||
      body.required_signers() !== undefined ||
      output?.datum() !== undefined ||
      output?.script_ref() !== undefined ||
      witnesses.native_scripts() !== undefined ||
      witnesses.plutus_v1_scripts() !== undefined ||
      witnesses.plutus_v2_scripts() !== undefined ||
      witnesses.plutus_v3_scripts() !== undefined ||
      witnesses.plutus_datums() !== undefined ||
      witnesses.redeemers() !== undefined,
    "it carries scripts, datums or redeemers",
  );
  refuse(
    "metadata",
    body.auxiliary_data_hash() !== undefined ||
      transaction.auxiliary_data() !== undefined,
    "it carries auxiliary data",
  );
  refuse(
    "reference_inputs",
    body.reference_inputs() !== undefined,
    "it reads reference inputs",
  );
  refuse(
    "collateral",
    body.collateral_inputs() !== undefined ||
      body.collateral_return() !== undefined ||
      body.total_collateral() !== undefined,
    "it carries collateral",
  );
  refuse(
    "fee",
    body.fee() > WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE,
    `its fee exceeds ${WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE.toString()} lovelace`,
  );
  const ttl = body.ttl();
  refuse(
    "validity",
    ttl === undefined || bounds.timeOf(Number(ttl)) > bounds.latestValidTo,
    "it is not valid only until its validity bound",
  );
};

/** The sum of `inputs`, as lucid assets. */
const total = (inputs: readonly WatcherProverFundingReservationInput[]) => {
  const assets: Assets = { lovelace: 0n };
  for (const input of inputs) {
    assets.lovelace += BigInt(input.lovelace);
    for (const { unit, quantity } of input.assets)
      assets[unit] = (assets[unit] ?? 0n) + BigInt(quantity);
  }
  return assets;
};

/**
 * Signs a cancel of `inputs`, funding an orphan leases: one transaction
 * spending them all back to the wallet, valid for an hour. The guard reads
 * the decoded final body immediately before the wallet signs it. Returns
 * the signed bytes, which are recorded before they are ever submitted.
 */
export const signWatcherOrphanCancel = async (
  input: Readonly<{
    inputs: readonly WatcherProverFundingReservationInput[];
    wallet: WatcherOrphanCancelWallet;
    fees: WatcherOrphanCancelFees;
    now: number;
  }>,
): Promise<string> => {
  const session = await input.wallet.open();
  const latestValidTo = input.now + WATCHER_ORPHAN_CANCEL_VALIDITY_MS;
  const ttl = session.slotOf(latestValidTo);
  const value = total(input.inputs);
  const spent = [...input.inputs].sort((left, right) =>
    left.outRef < right.outRef ? -1 : left.outRef > right.outRef ? 1 : 0,
  );
  const build = (fee: bigint) => {
    const inputs = CML.TransactionInputList.new();
    for (const { outRef } of spent) {
      const [hash, index] = outRef.split("#");
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(hash!),
          BigInt(index!),
        ),
      );
    }
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(input.wallet.address),
        assetsToValue({ ...value, lovelace: value.lovelace - fee }),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(BigInt(ttl));
    return body;
  };
  // Sized with the widest fee and one throwaway key witness, as signed.
  const sizing = build(WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE);
  const witness = CML.VkeywitnessList.new();
  witness.add(
    CML.make_vkey_witness(
      CML.hash_transaction(sizing),
      CML.PrivateKey.generate_ed25519(),
    ),
  );
  const sized = CML.TransactionWitnessSet.new();
  sized.set_vkeywitnesses(witness);
  const size = CML.Transaction.new(
    sizing,
    sized,
    true,
    undefined,
  ).to_cbor_bytes().length;
  const fee = input.fees.minFeeA * BigInt(size + 16) + input.fees.minFeeB;
  const body = fee < value.lovelace ? build(fee) : undefined;
  if (
    body === undefined ||
    value.lovelace - fee <
      CML.min_ada_required(body.outputs().get(0), input.fees.coinsPerUtxoByte)
  )
    throw new Error("orphan cancel cannot cover its fee and minimum output");
  const decoded = CML.Transaction.from_cbor_hex(
    CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
      undefined,
    ).to_cbor_hex(),
  );
  assertWatcherOrphanCancel(decoded, {
    leased: new Set(input.inputs.map(({ outRef }) => outRef)),
    address: input.wallet.address,
    latestValidTo,
    timeOf: session.timeOf,
  });
  const keys = (await session.sign(decoded)).vkeywitnesses();
  if (keys === undefined) throw new Error("orphan cancel was not signed");
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_vkeywitnesses(keys);
  return CML.Transaction.new(
    decoded.body(),
    witnesses,
    true,
    undefined,
  ).to_cbor_hex();
};
