import "./config.parse-providers.js";

import { parseStrictJson } from "@al-ft/midgard-l1-follower";

import { parseWatcherConfig } from "./config.parse-watcher-config.js";
import {
  fail,
  WATCHER_CONFIG_BOUNDS,
  type WatcherConfig,
} from "./config.watcher-config.js";

export const parseWatcherConfigJson = (source: string): WatcherConfig => {
  if (
    typeof source !== "string" ||
    source.length < WATCHER_CONFIG_BOUNDS.configJsonBytes.min ||
    Buffer.byteLength(source, "utf8") >
      WATCHER_CONFIG_BOUNDS.configJsonBytes.max
  ) {
    fail("out_of_bounds", "$");
  }
  return parseWatcherConfig(parseStrictJson(source, fail));
};

/** Duplicate-key rejecting JSON admission shared by production identity files. */
export const parseWatcherStrictJsonValue = (source: string): unknown =>
  parseStrictJson(source, fail);
