import { appendFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Emulator, type LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

/**
 * Set when the suite runs against the traced-refusal blueprint that
 * scripts/run-traced-refusals.mjs builds: every validator a pinned negative
 * names as `refusedBy` carries its verbose traces, so the negative can assert
 * which check refused it.
 */
export const TRACED_REFUSALS =
  process.env.MIDGARD_EMULATOR_TRACED_REFUSALS === "1";

/**
 * The one validator module the traced run traces. Every other validator runs
 * plain and fails without a trace, so a traced refusal in this run came from
 * this module.
 */
export const TRACED_REFUSAL_MODULE = process.env.MIDGARD_TRACED_REFUSAL_MODULE;

/**
 * A verbose-traced step is up to about twice its plain size, so many cannot
 * be published under the 16,384-byte L1 envelope. The traced run exists only
 * to attribute each refusal to its check; the plain run is the one that proves
 * every script and transaction fits, so the traced run lifts the envelope for
 * reference-script publication alone.
 */
const TRACED_PUBLICATION_MAX_TX_SIZE = 65_536;

/**
 * Run a reference-script publication under the traced run's lifted envelope.
 * Outside the traced run this is `operation()` unchanged.
 */
export const withTracedPublicationEnvelope = async <T>(
  lucid: LucidEvolution,
  operation: () => Promise<T>,
): Promise<T> => {
  if (!TRACED_REFUSALS) return operation();
  // Lucid reads maxTxSize into its transaction-builder config when it binds a
  // provider, so the lift re-binds the emulator and then restores both.
  const config = lucid.config();
  const emulator = config.provider;
  if (!(emulator instanceof Emulator)) {
    throw new Error("traced publication requires an emulator provider");
  }
  const saved = {
    emulatorParameters: emulator.protocolParameters,
    protocolParameters: config.protocolParameters,
    txbuilderconfig: config.txbuilderconfig,
    costModels: config.costModels,
  };
  emulator.protocolParameters = {
    ...saved.emulatorParameters,
    maxTxSize: TRACED_PUBLICATION_MAX_TX_SIZE,
  };
  await lucid.switchProvider(emulator);
  try {
    return await operation();
  } finally {
    emulator.protocolParameters = saved.emulatorParameters;
    config.protocolParameters = saved.protocolParameters;
    config.txbuilderconfig = saved.txbuilderconfig;
    config.costModels = saved.costModels;
  }
};

const packageRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../..",
);

/**
 * Record that a pin was checked against its trace, so the runner can fail a
 * declared pin that never ran.
 */
export const recordCheckedPin = (refusedBy: string): void => {
  const log = process.env.MIDGARD_TRACED_REFUSALS_LOG;
  const testPath = expect.getState().testPath;
  if (log === undefined || testPath === undefined) return;
  appendFileSync(
    log,
    `${JSON.stringify({ file: relative(packageRoot, testPath), refusedBy })}\n`,
  );
};
