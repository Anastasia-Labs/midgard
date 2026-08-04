import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import { type Network, type Script } from "@lucid-evolution/lucid";

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
const blueprintCandidates = [
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;

export const loadPhasMembershipWithdrawalScript = (): Script => {
  const configured = process.env.MIDGARD_REAL_BLUEPRINT_PATH?.trim();
  const blueprintPath = configured
    ? configured
    : blueprintCandidates.find((candidate) => existsSync(candidate));
  if (blueprintPath === undefined) {
    throw new Error(
      `Failed to locate Aiken blueprint for PHAS membership validator. Looked in: ${blueprintCandidates.join(", ")}`,
    );
  }
  const blueprint = SDK.parsePhasMembershipBlueprint(
    JSON.parse(readFileSync(blueprintPath, "utf8")),
  );
  try {
    return SDK.phasMembershipWithdrawalScriptFromBlueprint(blueprint);
  } catch (cause) {
    const detail = cause instanceof Error ? `: ${cause.message}` : "";
    throw new Error(
      `Missing phas.membership.withdraw in ${blueprintPath}${detail}`,
    );
  }
};

export const phasMembershipRewardAddress = (
  network: Network,
  script: Script = loadPhasMembershipWithdrawalScript(),
): string => SDK.phasMembershipRewardAddress(network, script);
