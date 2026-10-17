import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { type AddressData } from "../src/common.js";
import {
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  CorrectionLockError,
  fetchCorrectionLockUTxOProgram,
} from "../src/correction-lock.js";
import {
  fetchHubOracleUTxOProgram,
  HubOracleDatum,
  HubOracleError,
} from "../src/hub-oracle.js";
import {
  fetchSchedulerUTxOProgram,
  INITIAL_SCHEDULER_DATUM,
  SchedulerDatum,
  SchedulerError,
} from "../src/scheduler.js";

const policyId = "ab".repeat(28);
const address = "singleton-fixture-address";
const addressData: AddressData = {
  paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
  stakeCredential: null,
};
const hubDatum = Data.to(
  Object.fromEntries([
    ...[
      "registered_operators",
      "active_operators",
      "retired_operators",
      "scheduler",
      "state_queue",
      "fraud_proof_catalogue",
      "fraud_proof",
      "deposit",
      "withdrawal",
      "tx_order",
      "settlement",
      "payout",
      "reserve_observer",
    ].map((field) => [field, "bb".repeat(28)]),
    ...[
      "registered_operators",
      "active_operators",
      "retired_operators",
      "scheduler",
      "state_queue",
      "fraud_proof_catalogue",
      "fraud_proof",
      "deposit",
      "withdrawal",
      "tx_order",
      "settlement",
      "reserve",
      "payout",
    ].map((field) => [field + "_addr", addressData]),
  ]) as HubOracleDatum,
  HubOracleDatum,
);

const utxo = (
  index: number,
  datum: string | undefined,
  assetName: string,
): UTxO => ({
  txHash: index.toString(16).padStart(64, "0"),
  outputIndex: 0,
  address,
  assets: { lovelace: 5_000_000n, [policyId + assetName]: 1n },
  datum,
});
const donation: UTxO = {
  txHash: "ee".repeat(32),
  outputIndex: 0,
  address,
  assets: { lovelace: 1_000_000n },
};

const lucidServing = (utxos: readonly UTxO[]) =>
  ({ utxosAt: async () => [...utxos] }) as unknown as LucidEvolution;

type Singleton = {
  readonly label: string;
  readonly datum: string;
  readonly assetName: string;
  readonly errorClass: abstract new (...args: never[]) => unknown;
  readonly fetch: (
    lucid: LucidEvolution,
  ) => Effect.Effect<{ readonly utxo: UTxO }, unknown>;
};

const singletons: readonly Singleton[] = [
  {
    label: "hub oracle",
    datum: hubDatum,
    assetName: "",
    errorClass: HubOracleError,
    fetch: (lucid: LucidEvolution) =>
      fetchHubOracleUTxOProgram(lucid, {
        hubOracleAddress: address,
        hubOraclePolicyId: policyId,
      }),
  },
  {
    label: "scheduler",
    datum: Data.to(INITIAL_SCHEDULER_DATUM, SchedulerDatum),
    assetName: "",
    errorClass: SchedulerError,
    fetch: (lucid: LucidEvolution) =>
      fetchSchedulerUTxOProgram(lucid, {
        schedulerAddress: address,
        schedulerPolicyId: policyId,
      }),
  },
  {
    label: "correction lock",
    datum: Data.to("Idle" as CorrectionLockDatum, CorrectionLockDatum),
    assetName: CORRECTION_LOCK_ASSET_NAME,
    errorClass: CorrectionLockError,
    fetch: (lucid: LucidEvolution) =>
      fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: address,
        hubOraclePolicyId: policyId,
      }),
  },
];

type CountRefusal = {
  readonly message: string;
  readonly cause: unknown;
  readonly retryable?: boolean;
  readonly unexpectedCount?: {
    readonly reason: string;
    readonly authenticCount: number;
    readonly rawCount: number;
    readonly policyHolderCount: number;
  };
};

const refusal = async (
  fetch: (lucid: LucidEvolution) => Effect.Effect<unknown, unknown>,
  utxos: readonly UTxO[],
) => {
  const result = await Effect.runPromise(
    Effect.either(fetch(lucidServing(utxos))),
  );
  expect(result._tag).toBe("Left");
  return (result as { left: CountRefusal }).left;
};

describe("single authentic UTxO reads tell a missing UTxO from a duplicated one", () => {
  it.each(singletons)(
    "accepts exactly one authentic $label UTxO among donations",
    async ({ datum, assetName, fetch }) => {
      const only = utxo(1, datum, assetName);
      const result = await Effect.runPromise(
        Effect.either(fetch(lucidServing([donation, only]))),
      );
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") expect(result.right.utxo).toBe(only);
    },
  );

  it.each(singletons)(
    "reports a retryable none-found when no $label UTxO is indexed",
    async ({ label, errorClass, fetch }) => {
      const error = await refusal(fetch, [donation]);
      expect(error).toBeInstanceOf(errorClass);
      expect(error.retryable).toBe(true);
      expect(error.unexpectedCount).toEqual({
        reason: "none-found",
        authenticCount: 0,
        rawCount: 1,
        policyHolderCount: 0,
      });
      expect(String(error.cause)).toContain(`no authentic ${label} UTxO`);
    },
  );

  it.each(singletons)(
    "counts a $label policy holder that does not authenticate",
    async ({ datum, assetName, fetch }) => {
      const undecodable = utxo(2, undefined, assetName);
      const doubled = {
        ...utxo(3, datum, assetName),
        assets: { lovelace: 5_000_000n, [policyId + assetName]: 2n },
      };
      const error = await refusal(fetch, [donation, undecodable, doubled]);
      expect(error.unexpectedCount).toEqual({
        reason: "none-found",
        authenticCount: 0,
        rawCount: 3,
        policyHolderCount: 2,
      });
      expect(String(error.cause)).toContain("2 hold a token under the policy");
    },
  );

  it.each(singletons)(
    "refuses several authentic $label UTxOs as non-retryable",
    async ({ label, datum, assetName, errorClass, fetch }) => {
      const error = await refusal(fetch, [
        utxo(4, datum, assetName),
        donation,
        utxo(5, datum, assetName),
      ]);
      expect(error).toBeInstanceOf(errorClass);
      expect(error.retryable).toBe(false);
      expect(error.unexpectedCount).toEqual({
        reason: "several-found",
        authenticCount: 2,
        rawCount: 3,
        policyHolderCount: 2,
      });
      expect(String(error.cause)).toContain(`2 authentic ${label} UTxOs`);
    },
  );
});
