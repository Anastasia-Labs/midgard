import { DEFAULT_NODE_BEHIND_MS } from "@al-ft/midgard-l1-follower";
import { Config } from "effect";

/**
 * The node-behind bound (plan §9 Time): while the node's L1 tip is more
 * than this many ms behind wall-clock time, the follower reports
 * `l1_node_behind` and the node holds its own sends (the intent journal's
 * S6 decision) until it catches up. Never an exit. Default
 * `DEFAULT_NODE_BEHIND_MS` (300 000, 5 min); a non-positive or fractional
 * value refuses startup.
 */
export const nodeBehindConfig = Config.all({
  L1_NODE_BEHIND_MAX_MS: Config.number("L1_NODE_BEHIND_MAX_MS").pipe(
    Config.withDefault(DEFAULT_NODE_BEHIND_MS),
    Config.validate({
      message: "L1_NODE_BEHIND_MAX_MS must be a positive safe integer",
      validation: (value) => Number.isSafeInteger(value) && value > 0,
    }),
  ),
});
