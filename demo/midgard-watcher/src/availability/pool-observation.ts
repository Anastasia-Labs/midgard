import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

/**
 * The pooled DA bond as the watcher last read it at a finalized point. It is
 * served as JSON, so amounts are decimal lovelace strings. `state` and the two
 * alerts are independent: a `Withdrawing` pool can also be short, and a
 * `Bonded` pool drained by a slash is short until someone tops it up. Neither
 * alert changes the availability phase or the watcher's readiness (spec #685
 * E5); they tell the operator that attestations are paused or the committee is
 * leaving.
 */
export type WatcherDaBondPoolObservation = Readonly<{
  state: "missing" | "bonded" | "withdrawing";
  /** The pool UTxO's whole lovelace, floor included; absent when missing. */
  lovelace?: string;
  /** Lovelace above the pool floor; absent when missing. */
  backing?: string;
  /** `da_bond_lovelace`: the backing one attestation needs. */
  requiredBacking: string;
  /** `backing < da_bond_lovelace`; a missing pool backs nothing. */
  belowBond: boolean;
  /** Present iff `state` is `withdrawing`. */
  unlockAt?: string;
  /** Present iff `state` is `withdrawing`: `CompleteWithdraw` is admissible now. */
  unlockable?: boolean;
  alerts: Readonly<{ underBacked: boolean; withdrawing: boolean }>;
}>;

/**
 * The one pool UTxO among the outputs at the pool address, authenticated as
 * the SDK snapshot does: the pool NFT exactly once, beside lovelace only, at
 * the pool address whose payment credential is the pool policy, with no
 * reference script and an inline pool datum that decodes, by value, to a
 * canonical pool datum (a datum hash leaves `datum` unset). The bytes are
 * not compared: the validator checks the datum as a Data value, and the
 * local source re-encodes outputs canonically. Outputs without the NFT
 * are ignored (anyone can pay the address). No NFT holder means the pool is
 * missing; any other shape fails closed.
 */
export const authenticWatcherDaBondPool = (input: {
  readonly utxos: readonly UTxO[];
  readonly policyId: string;
  readonly address: string;
}): UTxO | undefined => {
  const unit = SDK.daBondPoolUnit(input.policyId);
  const holders = input.utxos.filter(
    (utxo) => (utxo.assets[unit] ?? 0n) !== 0n,
  );
  if (holders.length === 0) return undefined;
  if (holders.length !== 1)
    throw new Error("DA bond pool NFT is held by more than one output");
  const pool = holders[0]!;
  const credential = getAddressDetails(pool.address).paymentCredential;
  if (
    pool.address !== input.address ||
    credential?.type !== "Script" ||
    credential.hash !== input.policyId ||
    pool.scriptRef != null ||
    typeof pool.datum !== "string" ||
    pool.assets[unit] !== 1n ||
    Object.keys(pool.assets).some((key) => key !== "lovelace" && key !== unit)
  )
    throw new Error(
      `Unauthentic DA bond pool output ${pool.txHash}#${pool.outputIndex}`,
    );
  SDK.decodeDaBondPoolDatum(pool.datum);
  return pool;
};

/** The pool readout and its two alerts, from an authenticated pool read. */
export const deriveWatcherDaBondPoolObservation = (input: {
  readonly pool: UTxO | undefined;
  readonly policyId: string;
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly nowMs: bigint;
}): WatcherDaBondPoolObservation => {
  const requiredBacking = input.parameters.da_bond_lovelace.toString();
  if (input.pool === undefined)
    return Object.freeze({
      state: "missing",
      requiredBacking,
      belowBond: true,
      alerts: Object.freeze({ underBacked: true, withdrawing: false }),
    });
  const { pool } = input;
  if (
    pool.assets[SDK.daBondPoolUnit(input.policyId)] !== 1n ||
    typeof pool.datum !== "string"
  )
    throw new Error("DA bond pool readout requires an authenticated pool");
  const status = SDK.daBondPoolStatus({
    lovelace: pool.assets.lovelace ?? 0n,
    datum: SDK.decodeDaBondPoolDatum(pool.datum),
    parameters: input.parameters,
  });
  return Object.freeze({
    state: status.state,
    lovelace: status.lovelace.toString(),
    backing: status.backing.toString(),
    requiredBacking: status.requiredBacking.toString(),
    belowBond: status.belowBond,
    ...(status.unlockAt === undefined
      ? {}
      : {
          unlockAt: status.unlockAt.toString(),
          // CompleteWithdraw needs its validity lower bound at or after unlock_at.
          unlockable: input.nowMs >= status.unlockAt,
        }),
    alerts: Object.freeze({
      underBacked: status.belowBond,
      withdrawing: status.state === "withdrawing",
    }),
  });
};
