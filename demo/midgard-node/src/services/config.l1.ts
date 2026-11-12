import { DEFAULT_NODE_BEHIND_MS } from "@al-ft/midgard-l1-follower";
import { Config } from "effect";

import { hubOracleOriginConfig } from "./config.hub-oracle-origin.js";
import { l1ContentSourcesConfig } from "./config.l1-content-sources.js";
import { integerConfig, positiveSafeIntegerConfig } from "./config.parse.js";
import {
  NATIVE_LEDGER_SETTING_NAMES,
  parseNativeLedgerSettings,
} from "./native-ledger.js";

/**
 * How the node reaches and follows L1: the local node it reads and submits
 * through, its startup queries, how far behind the chain it may fall, the
 * hub oracle origin and where it fetches L1 transactions by id.
 * `L1_VIEW_FATAL_MS` is read here and resolved in `makeConfig`, against the
 * retention sweep interval.
 */
export const l1Config = Config.all([
  Config.all({
    L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: positiveSafeIntegerConfig(
      "L1_PROVIDER_PREFLIGHT_TIMEOUT_MS",
      15_000,
    ),
    /** Local ledger for reward-account reads; Ogmios cannot answer them. */
    L1_NATIVE_LEDGER: Config.all(
      Object.fromEntries(
        NATIVE_LEDGER_SETTING_NAMES.map((name) => [
          name,
          Config.string(name).pipe(Config.withDefault("")),
        ]),
      ) as Record<
        (typeof NATIVE_LEDGER_SETTING_NAMES)[number],
        Config.Config<string>
      >,
    ).pipe(Config.mapAttempt(parseNativeLedgerSettings)),
    /**
     * The node-behind bound (plan §9 Time): while the node's L1 tip is more
     * than this many ms behind wall-clock time, the follower reports
     * `l1_node_behind` and the node holds its own sends (the intent
     * journal's S6 decision) until it catches up. Never an exit. Default
     * `DEFAULT_NODE_BEHIND_MS` (300 000, 5 min); a non-positive or
     * fractional value refuses startup.
     */
    L1_NODE_BEHIND_MAX_MS: Config.number("L1_NODE_BEHIND_MAX_MS").pipe(
      Config.withDefault(DEFAULT_NODE_BEHIND_MS),
      Config.validate({
        message: "L1_NODE_BEHIND_MAX_MS must be a positive safe integer",
        validation: (value) => Number.isSafeInteger(value) && value > 0,
      }),
    ),
    L1_VIEW_FATAL_MS: Config.option(Config.string("L1_VIEW_FATAL_MS")),
    STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS: integerConfig(
      "STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS",
      120,
    ),
    STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: integerConfig(
      "STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS",
      5_000,
    ),
  }),
  hubOracleOriginConfig,
  l1ContentSourcesConfig,
]).pipe(
  Config.map(([settings, hubOracleOrigin, l1ContentSources]) => ({
    ...settings,
    ...hubOracleOrigin,
    ...l1ContentSources,
  })),
);
