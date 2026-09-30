import * as SDK from "@al-ft/midgard-sdk";
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import {
  type AvailabilityRefusalExpectation,
  type AvailabilityRefusalPurpose,
} from "./availability-challenge-emulator.measure-availability-transaction.js";

/**
 * Maps a redeemer (purpose, ledger index) of `txCbor` to the script it runs:
 * a spend to its sorted input's payment script, a mint to its sorted policy,
 * a withdrawal to its sorted reward account's script.
 */
export const availabilityRedeemerScript = (
  txCbor: string,
  utxos: readonly UTxO[],
  purpose: AvailabilityRefusalPurpose | "other",
  redeemerIndex: number,
): string | undefined => {
  const body = CML.Transaction.from_cbor_hex(txCbor).body();
  if (purpose === "spend") {
    const inputs = body.inputs();
    const refs = Array.from({ length: inputs.len() }, (_, i) => ({
      txHash: inputs.get(i).transaction_id().to_hex(),
      outputIndex: Number(inputs.get(i).index()),
    })).sort((a, b) =>
      a.txHash < b.txHash
        ? -1
        : a.txHash > b.txHash
          ? 1
          : a.outputIndex - b.outputIndex,
    );
    const spent = refs[redeemerIndex];
    const utxo = utxos.find(
      (u) =>
        spent !== undefined &&
        u.txHash === spent.txHash &&
        u.outputIndex === spent.outputIndex,
    );
    const credential = utxo
      ? getAddressDetails(utxo.address).paymentCredential
      : undefined;
    return credential?.type === "Script" ? credential.hash : undefined;
  }
  if (purpose === "mint") {
    const policies = body.mint()?.keys();
    return Array.from({ length: policies?.len() ?? 0 }, (_, i) =>
      policies!.get(i).to_hex(),
    ).sort()[redeemerIndex];
  }
  if (purpose === "withdraw") {
    const withdrawals = body.withdrawals();
    const accounts = withdrawals?.keys();
    const credentials = Array.from({ length: accounts?.len() ?? 0 }, (_, i) => {
      const account = accounts!.get(i);
      return {
        bytes: account.to_address().to_hex(),
        payment: account.payment(),
      };
    }).sort((a, b) => (a.bytes < b.bytes ? -1 : a.bytes > b.bytes ? 1 : 0));
    const account = credentials[redeemerIndex];
    return account?.payment.as_script()?.to_hex();
  }
  return undefined;
};

export const describeExpectation = (
  expected: AvailabilityRefusalExpectation,
) =>
  "trace" in expected
    ? `trace ${String(expected.trace)}`
    : `${expected.purpose} ${expected.script}${expected.index === undefined ? "" : `[${expected.index}]`}`;

/** The reference-script roles the availability, DA and pool flows read. */
export const availabilityReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly SDK.ReferenceScriptTarget[] => [
  {
    name: "availability-challenge spending",
    script: contracts.availabilityChallenge.spendingScript,
  },
  {
    name: "availability-challenge minting",
    script: contracts.availabilityChallenge.mintingScript,
  },
  ...(["open", "settle", "close", "timeout"] as const).map((arm) => ({
    name: `availability-challenge ${arm} withdrawal`,
    script: contracts.availabilityChallenge.yields[arm].withdrawalScript,
  })),
  {
    name: "da-attestation spending",
    script: contracts.daAttestation.spendingScript,
  },
  {
    name: "da-attestation minting",
    script: contracts.daAttestation.mintingScript,
  },
  {
    name: "da-bond-pool spending",
    script: contracts.daBondPool.spendingScript,
  },
  {
    name: "da-bond-pool minting",
    script: contracts.daBondPool.mintingScript,
  },
  { name: "state-queue spending", script: contracts.stateQueue.spendingScript },
  { name: "state-queue minting", script: contracts.stateQueue.mintingScript },
  {
    name: "state-queue unavailable-timeout withdrawal",
    script: contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
  },
  {
    name: "state-queue merge withdrawal",
    script: contracts.stateQueue.yields.merge.withdrawalScript,
  },
  {
    name: "correction-lock spending",
    script: contracts.correctionLock.spendingScript,
  },
];

/** Script hashes by contract name, for `assertAvailabilityRefusal`. */
export const availabilityScriptNames = (contracts: SDK.MidgardValidators) =>
  Object.freeze({
    "availability-challenge spending":
      contracts.availabilityChallenge.spendingScriptHash,
    "availability-challenge minting": contracts.availabilityChallenge.policyId,
    "availability-challenge open withdrawal":
      contracts.availabilityChallenge.yields.open.withdrawalScriptHash,
    "availability-challenge settle withdrawal":
      contracts.availabilityChallenge.yields.settle.withdrawalScriptHash,
    "availability-challenge close withdrawal":
      contracts.availabilityChallenge.yields.close.withdrawalScriptHash,
    "availability-challenge timeout withdrawal":
      contracts.availabilityChallenge.yields.timeout.withdrawalScriptHash,
    "da-attestation spending": contracts.daAttestation.spendingScriptHash,
    "da-attestation minting": contracts.daAttestation.policyId,
    "da-bond-pool": contracts.daBondPool.policyId,
    "state-queue spending": contracts.stateQueue.spendingScriptHash,
    "state-queue minting": contracts.stateQueue.policyId,
    "state-queue unavailable-timeout withdrawal":
      contracts.stateQueue.yields.unavailableTimeout.withdrawalScriptHash,
    "state-queue merge withdrawal":
      contracts.stateQueue.yields.merge.withdrawalScriptHash,
    "correction-lock spending": contracts.correctionLock.spendingScriptHash,
  } as const);

export type AvailabilityFixtureOptions = {
  /**
   * The DA bond pool's genesis state: an inline datum (default `Bonded`) and
   * the pool NFT at `Script(pool policy)` with `lovelace` (default
   * `floor + 2 * da_bond`). `false` seeds no pool and leaves the hub one-shot
   * out-reference unspent, so `initPoolReal` can run the real `InitPool`.
   */
  readonly seedPool?:
    | false
    | {
        readonly lovelace?: bigint;
        readonly datum?: SDK.DaBondPoolDatum;
      };
};

/** Reference-script roles only a commit fixture publishes. */
export const availabilityCommitReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly SDK.ReferenceScriptTarget[] => [
  {
    name: "state-queue commit withdrawal",
    script: contracts.stateQueue.yields.commit.withdrawalScript,
  },
  {
    name: "active-operators spending",
    script: contracts.activeOperators.spendingScript,
  },
];
