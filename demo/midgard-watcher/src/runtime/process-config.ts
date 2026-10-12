import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "./config.js";
import "./process-config.fault-proof-infrastructure.js";
import "./process-config.parse-watcher-process-config.js";
export {
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
  watcherSecretSourceIdentity,
} from "./process-config.fault-proof-infrastructure.js";
export {
  decodeWatcherAuthenticationKey32,
  loadWatcherProcessConfigFile,
  loadWatcherSecretText,
  parseWatcherProcessConfig,
} from "./process-config.parse-watcher-process-config.js";
