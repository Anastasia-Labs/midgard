import { parseL1Origin } from "@al-ft/midgard-core/l1-origin";
import { Config, Option } from "effect";

/**
 * The hub oracle one-shot outref that `prepare-hub-oracle-one-shot-nonce`
 * prints, and an operator override of the deployment's L1 origin (§5.3 of
 * the L1 architecture plan). `L1_ORIGIN` is `<slot>.<block hash>`, as the
 * nonce command and `midgard-l1-follower find-origin` print it; unset means
 * the follower starts at the manifest's recorded `l1Origin`.
 */
export const hubOracleOriginConfig = Config.all({
  HUB_ORACLE_ONE_SHOT_TX_HASH: Config.string(
    "HUB_ORACLE_ONE_SHOT_TX_HASH",
  ).pipe(Config.withDefault("")),
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: Config.integer(
    "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX",
  ).pipe(Config.withDefault(-1)),
  L1_ORIGIN: Config.option(Config.string("L1_ORIGIN")).pipe(
    Config.mapAttempt((value) =>
      Option.isNone(value) || value.value === ""
        ? null
        : parseL1Origin(value.value, "L1_ORIGIN"),
    ),
  ),
});
