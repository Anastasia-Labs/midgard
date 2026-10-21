/**
 * What the defending operator's fiber plays against: its own Lucid, with
 * the operator wallet and its pocket selected (`selectNodeWallet`), the
 * dispute's contracts, the published references its moves read, and the
 * L1 outputs every Defend reads (our active-operator node, the case
 * configuration and the hub oracle), read from the follower's facts by
 * their tokens, never from a provider or from a predicted output.
 */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { liveUtxosIn, postgresDialect } from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import { type DefenseMaterialCache, DefenseSurveyFailed } from "./survey.js";

export type ValidationTraceDisputeContracts =
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];

/** The published reference scripts the defender's transactions read. */
export type ValidationTraceDefenseReferences = Readonly<{
  case: UTxO;
  game: UTxO;
  timeout: UTxO;
  boundary: UTxO;
  threadExpiry: UTxO;
  activeOperators: UTxO;
}>;

export type ValidationTraceDefensePort = Readonly<{
  /** The defender's own Lucid: the operator wallet, with its pocket. */
  lucid: LucidEvolution;
  network: Network;
  dispute: ValidationTraceDisputeContracts;
  activeOperatorsPolicyId: string;
  hubOraclePolicyId: string;
  /** Our operator key hash. */
  ourVkh: string;
  /** The operator wallet's Base address. */
  baseAddress: string;
  /** The operator key's Enterprise address: the defense pocket. */
  pocketAddress: string;
  l1SlotConfig: L1SlotConfig;
  /** Resolves the published references; they are read once a move needs them. */
  references: Effect.Effect<ValidationTraceDefenseReferences, Error>;
  /** The state-queue mutation lease a Defend holds. */
  lease: Readonly<{ ttlMs: number; renewIntervalMs: number }>;
  /** The fiber's tick: the bound past a move's validity a hold waits. */
  tickMs: number;
  /** How often a held Defend looks for its landing. */
  pollMs: number;
  /** Deterministic defense material by case, kept across ticks. */
  cache: DefenseMaterialCache;
}>;

/** The outputs a Defend reads beside the case; undefined when absent. */
export type ValidationTraceDefenseAnchors = Readonly<{
  activeNode: UTxO | undefined;
  caseConfig: UTxO | undefined;
  hubOracle: UTxO | undefined;
}>;

/** The live output holding `unit`, when exactly one does. */
const liveByUnitIn = async (
  tx: Parameters<typeof liveUtxosIn>[0],
  unit: string,
): Promise<UTxO | undefined> => {
  const read = await liveUtxosIn(tx, postgresDialect, {
    by: "unit",
    policyId: Buffer.from(unit.slice(0, 56), "hex"),
    assetName: Buffer.from(unit.slice(56), "hex"),
  });
  if (read.kind !== "ok")
    throw new Error(`the follower cannot read live outputs: ${read.kind}`);
  const [only, ...rest] = read.utxos;
  return only === undefined || rest.length > 0
    ? undefined
    : toLucidUtxo(only.outRef, only.output);
};

/** The units a defense reads, beside a case's own. */
export const defenseAnchorUnits = (port: ValidationTraceDefensePort) => ({
  activeNode:
    port.activeOperatorsPolicyId +
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
    port.ourVkh,
  caseConfig:
    port.dispute.caseConfig.policyId + SDK.VALIDATION_CASE_CONFIG_ASSET_NAME,
  hubOracle: port.hubOraclePolicyId + SDK.HUB_ORACLE_ASSET_NAME,
});

/**
 * The live outputs holding `units`, in one follower snapshot, by key;
 * each is undefined when no single live output holds it.
 */
export const readLiveByUnits = <K extends string>(
  units: Readonly<Record<K, string>>,
): Effect.Effect<
  Readonly<Record<K, UTxO | undefined>>,
  DefenseSurveyFailed,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise(async () => {
          const found = {} as Record<K, UTxO | undefined>;
          for (const key of Object.keys(units) as K[])
            found[key] = await liveByUnitIn(tx, units[key]);
          return found;
        }),
      ),
    );
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DefenseSurveyFailed({
          message: "the defense's live outputs could not be read",
          cause,
        }),
    ),
  );

/** Our active node, the case configuration and the hub oracle, now. */
export const readDefenseAnchors = (
  port: ValidationTraceDefensePort,
): Effect.Effect<
  ValidationTraceDefenseAnchors,
  DefenseSurveyFailed,
  SqlClient.SqlClient
> => readLiveByUnits(defenseAnchorUnits(port));
