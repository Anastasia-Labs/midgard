import { readFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  healthyJourneyReplayTiming,
  type JourneyCadence,
  type JourneyTiming,
  journeyTimingForCategory,
  MAX_TIMER_MS,
  object,
  type ReadJourneyTimingOptions,
} from "./journey-timing.journey-timing-for-plan.js";

/** Safe at Vitest module load: reads public configuration only, without providers or keys. */
export const readJourneyCadence = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
): Promise<JourneyCadence> => {
  if (!isAbsolute(runDirectory))
    throw new Error("Journey run directory must be absolute");
  const [genesisBytes, manifestBytes] = await Promise.all([
    readFile(join(runDirectory, "genesis/shelley-genesis.json"), "utf8"),
    readFile(join(runDirectory, "deploymentInfo/manifest.json"), "utf8"),
  ]);
  const genesis = object(JSON.parse(genesisBytes), "Shelley genesis");
  const manifest = verifyFinalizedDeploymentManifest(JSON.parse(manifestBytes));
  const confirmationDepth = manifest.l1Finality.confirmationDepth;
  if (
    typeof genesis.slotLength !== "number" ||
    typeof genesis.activeSlotsCoeff !== "number"
  ) {
    throw new Error(
      "Journey configuration omitted numeric cadence or finality depth",
    );
  }
  if (
    options.authenticatedConfirmationDepth !== undefined &&
    options.authenticatedConfirmationDepth !== confirmationDepth
  ) {
    throw new Error(
      "Journey timing differs from authenticated release finality depth",
    );
  }
  return {
    slotLengthSeconds: genesis.slotLength,
    activeSlotsCoeff: genesis.activeSlotsCoeff,
    confirmationDepth,
    actionDepth: options.actionDepth,
    fixtureStagingAllowanceMs: options.fixtureStagingAllowanceMs,
  };
};

/** The audited plan for transitionTrace; the finite generic plan for every other family. */
export const readJourneyTiming = async (
  runDirectory: string,
  category: string,
  options: ReadJourneyTimingOptions = {},
) =>
  journeyTimingForCategory(
    category,
    await readJourneyCadence(runDirectory, options),
  );

export const readTransitionTraceJourneyTiming = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
) => readJourneyTiming(runDirectory, "transitionTrace", options);

export interface JourneyTimingTip {
  blockNo: number;
  slot: number;
  blockHash: string;
}

/** Same atomic chain-sync tip query used by the production Ogmios source. */
const readJourneyTimingTip = async (
  runDirectory: string,
): Promise<JourneyTimingTip> => {
  const lines = (await readFile(join(runDirectory, "run.env"), "utf8")).split(
    "\n",
  );
  const port = lines
    .find((line) => line.startsWith("MIDGARD_PHASE4_OGMIOS_PORT="))
    ?.split("=")[1];
  if (
    port === undefined ||
    !/^\d+$/.test(port) ||
    Number(port) < 1 ||
    Number(port) > 65535
  )
    throw new Error("Journey timing requires its actual local Ogmios port");
  const socket = new WebSocket(`ws://127.0.0.1:${port}`);
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await new Promise<JourneyTimingTip>((resolve, reject) => {
      timer = setTimeout(
        () => reject(new Error("Journey timing tip query timed out")),
        10_000,
      );
      socket.addEventListener(
        "error",
        () => reject(new Error("Journey timing tip query failed")),
        { once: true },
      );
      socket.addEventListener(
        "close",
        () =>
          reject(
            new Error("Journey timing tip query closed before its response"),
          ),
        { once: true },
      );
      socket.addEventListener(
        "open",
        () =>
          socket.send(
            JSON.stringify({
              jsonrpc: "2.0",
              id: "journey-timing-tip",
              method: "findIntersection",
              params: { points: ["origin"] },
            }),
          ),
        { once: true },
      );
      socket.addEventListener(
        "message",
        (event) => {
          try {
            if (typeof event.data !== "string" || event.data.length > 16_384)
              throw new Error(
                "Journey timing tip response is not bounded JSON",
              );
            const response = object(
              JSON.parse(event.data),
              "Ogmios timing response",
            );
            if (
              response.id !== "journey-timing-tip" ||
              response.error !== undefined
            )
              throw new Error("Ogmios refused the journey timing tip query");
            const result = object(
              response.result,
              "Ogmios timing intersection",
            );
            if (result.intersection !== "origin")
              throw new Error("Ogmios timing query did not intersect origin");
            const tip = object(result.tip, "Ogmios timing tip");
            if (
              typeof tip.height !== "number" ||
              !Number.isSafeInteger(tip.height) ||
              tip.height < 0 ||
              typeof tip.slot !== "number" ||
              !Number.isSafeInteger(tip.slot) ||
              tip.slot < 0 ||
              typeof tip.id !== "string" ||
              !/^[0-9a-f]{64}$/.test(tip.id)
            )
              throw new Error(
                "Ogmios timing tip omitted actual block number, slot, or hash",
              );
            resolve({ blockNo: tip.height, slot: tip.slot, blockHash: tip.id });
          } catch (cause) {
            reject(cause instanceof Error ? cause : new Error(String(cause)));
          }
        },
        { once: true },
      );
    });
  } finally {
    if (timer !== undefined) clearTimeout(timer);
    socket.close();
  }
};

export const journeyExecutionTiming = (
  timing: JourneyTiming,
  capturedTip: JourneyTimingTip,
  capturedAtMonotonicMs: number,
) => {
  const beforeReplayAllowanceMs =
    timing.journeyTimeoutMs - timing.allowances.healthySuccessorObservationMs;
  const healthyReplay = healthyJourneyReplayTiming({
    ...timing.cadence,
    tipBlockNo: capturedTip.blockNo,
    beforeReplayAllowanceMs,
    // Native recorder startup and final operations/provider checks remain bounded.
    observationAllowanceMs: 90_000,
  });
  const journeyTimeoutMs = beforeReplayAllowanceMs + healthyReplay.timeoutMs;
  if (
    !Number.isSafeInteger(journeyTimeoutMs) ||
    journeyTimeoutMs > MAX_TIMER_MS ||
    !Number.isFinite(capturedAtMonotonicMs) ||
    capturedAtMonotonicMs < 0
  )
    throw new Error(
      "Journey execution timing exceeds the supported timer range",
    );
  return Object.freeze({
    ...timing,
    capturedTip: Object.freeze({ ...capturedTip }),
    capturedAtMonotonicMs,
    healthyReplay,
    journeyTimeoutMs,
    deadlineMonotonicMs: capturedAtMonotonicMs + journeyTimeoutMs,
  });
};

export const transitionTraceJourneyExecutionTiming = journeyExecutionTiming;

/** Suite-load planning opens only a bounded read-only RPC and reads public configuration. */
export const readJourneyExecutionTiming = async (
  runDirectory: string,
  category: string,
  options: ReadJourneyTimingOptions = {},
) => {
  const timing = await readJourneyTiming(runDirectory, category, options);
  const capturedTip = await readJourneyTimingTip(runDirectory);
  return journeyExecutionTiming(timing, capturedTip, performance.now());
};

export const readTransitionTraceJourneyExecutionTiming = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
) => readJourneyExecutionTiming(runDirectory, "transitionTrace", options);
