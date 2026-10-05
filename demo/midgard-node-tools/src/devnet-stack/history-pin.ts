import { type ExecResult, lastJsonValue, requireSuccess } from "./exec.js";
import { Journal } from "./journal.js";
import type { Layout } from "./layout.js";

const JOURNAL_KEY = "historyGenesisPin";

/** What the node's `history-genesis-pin` verb prints. */
export type HistoryGenesisPin = {
  readonly algorithm: string;
  readonly sha256: string;
};

type PinRecord = HistoryGenesisPin & { readonly recordedAt: string };

const parsePin = (result: ExecResult): HistoryGenesisPin => {
  const printed = lastJsonValue(
    requireSuccess(result, "history-genesis-pin").stdout,
  ) as {
    variable?: unknown;
    algorithm?: unknown;
    sha256?: unknown;
  };
  if (
    printed.variable !== "L1_HISTORY_GENESIS_LOSSLESS_SHA256" ||
    typeof printed.algorithm !== "string" ||
    typeof printed.sha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(printed.sha256)
  )
    throw new Error(
      `history-genesis-pin printed no pin; transcript ${result.log}`,
    );
  return { algorithm: printed.algorithm, sha256: printed.sha256 };
};

/**
 * Binds the run to its L1 chain. The first call records the Shelley genesis
 * pin the node derives from the live chain; every later call derives it again
 * and refuses a different one, so a chain that was regenerated or replaced
 * under this run is never adopted as the run's own. `derive` runs the node's
 * `history-genesis-pin` verb against the run's Ogmios.
 */
export const ensureHistoryGenesisPin = async (
  layout: Layout,
  derive: () => Promise<ExecResult>,
): Promise<string> => {
  const observed = parsePin(await derive());
  const journal = new Journal(layout.journal);
  const recorded = journal.get<PinRecord>(JOURNAL_KEY);
  if (recorded === undefined) {
    journal.set(JOURNAL_KEY, {
      ...observed,
      recordedAt: new Date().toISOString(),
    });
    return observed.sha256;
  }
  if (
    recorded.algorithm !== observed.algorithm ||
    recorded.sha256 !== observed.sha256
  )
    throw new Error(
      `the L1 now serves Shelley genesis ${observed.algorithm}:${observed.sha256}, but ${layout.runDir} ` +
        `recorded ${recorded.algorithm}:${recorded.sha256}; this is not the run's chain, refusing to continue`,
    );
  return recorded.sha256;
};

/** The run's recorded pin, for every node process after `up` recorded it. */
export const recordedHistoryGenesisPin = (layout: Layout): string => {
  const recorded = new Journal(layout.journal).get<PinRecord>(JOURNAL_KEY);
  if (recorded === undefined)
    throw new Error(
      `${layout.runDir} records no L1 history genesis pin; run up first`,
    );
  return recorded.sha256;
};
