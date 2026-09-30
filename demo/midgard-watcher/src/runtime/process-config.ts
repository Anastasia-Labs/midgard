import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "../l1/finality-engine.js";
import "./config.js";
import "./process-config.historical-native-script-history.js";
import "./process-config.parse-watcher-process-config.js";
export {
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
  watcherSecretSourceIdentity,
} from "./process-config.historical-native-script-history.js";
export {
  decodeWatcherAuthenticationKey32,
  decodeWatcherHttpBearerSecret,
  loadWatcherProcessConfigFile,
  loadWatcherSecretText,
  loadWatcherTrustedHeadAuthorityProcessConfigFile,
  parseWatcherProcessConfig,
  parseWatcherTrustedHeadAuthorityProcessConfig,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "./process-config.parse-watcher-process-config.js";
