import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Script } from "@lucid-evolution/lucid";

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
const blueprintCandidates = [
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;

export const loadPhasMembershipWithdrawalScript = (): Script => {
  const configured = process.env.MIDGARD_REAL_BLUEPRINT_PATH?.trim();
  const blueprintPath =
    configured && configured.length > 0
      ? configured
      : blueprintCandidates.find((candidate) => existsSync(candidate));
  if (blueprintPath === undefined) {
    throw new Error(
      `Failed to locate Aiken blueprint for PHAS membership validator. Looked in: ${blueprintCandidates.join(", ")}`,
    );
  }
  const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
    readonly validators?: readonly {
      readonly title?: unknown;
      readonly compiledCode?: unknown;
    }[];
  };
  const validator = blueprint.validators?.find(
    (candidate) => candidate.title === "phas.membership.withdraw",
  );
  if (
    typeof validator?.compiledCode !== "string" ||
    validator.compiledCode.length === 0
  ) {
    throw new Error(`Missing phas.membership.withdraw in ${blueprintPath}`);
  }
  return {
    type: "PlutusV3",
    script: validator.compiledCode,
  };
};
