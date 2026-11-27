export const STRESS_WALLET_RECORD_SCHEMA_VERSION = "midgard-stress-wallet-v1";
export const STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION =
  "midgard-stress-wallet-create-result-v1";
export const STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION =
  "midgard-stress-wallet-prepare-result-v1";
export const STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION =
  "midgard-stress-wallet-fanout-result-v1";
export const STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION =
  "midgard-stress-wallet-fanout-report-v1";
export const STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION =
  "midgard-stress-wallet-consolidation-journal-v1";
export const STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION =
  "midgard-stress-wallet-consolidation-result-v1";
export const STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION =
  "midgard-stress-wallet-consolidation-report-v1";
export const STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION =
  "midgard-stress-wallet-consolidation-readiness-v1";
export const STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION =
  "midgard-stress-wallet-terminal-drain-journal-v1";
export const STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION =
  "midgard-stress-wallet-terminal-drain-result-v1";
export const STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION =
  "midgard-stress-wallet-terminal-drain-report-v1";
export const DEFAULT_STRESS_WALLET_DIR = ".stress-wallets";
export const DEFAULT_STRESS_WALLET_ENV_PREFIX = "STRESS_WALLET_SEED_PHRASE";
export const DEFAULT_PROJECTION_WAIT_MS = 120_000;
export const DEFAULT_VERIFY_TIMEOUT_MS = 300_000;
export const DEFAULT_VERIFY_POLL_INTERVAL_MS = 5_000;
export const DEFAULT_FANOUT_BRANCH_FACTOR = 16;
export const DEFAULT_FANOUT_MAX_IN_FLIGHT = 32;
export const DEFAULT_FANOUT_FEE_HEADROOM_LOVELACE = 500_000n;
export const DEFAULT_CONSOLIDATE_RESERVE_LOVELACE = 100_000n;
export const DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT = 32;
export const FANOUT_UTXO_QUERY_MAX_ATTEMPTS = 6;
export const FANOUT_UTXO_QUERY_INITIAL_RETRY_MS = 250;
export const FANOUT_UTXO_QUERY_MAX_RETRY_MS = 5_000;

export const ENV_NAME_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;
