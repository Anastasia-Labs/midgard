import { readFile } from "node:fs/promises";
import { dirname, isAbsolute, normalize, resolve } from "node:path";

/**
 * The Shelley genesis an attached stack's node configuration declares, as
 * its text: the stack's one source of its slot clock and cadence.
 */
export const readStackShelleyGenesisText = async (
  nodeConfigPath: string,
): Promise<string> => {
  const nodeConfig = JSON.parse(await readFile(nodeConfigPath, "utf8")) as {
    ShelleyGenesisFile?: unknown;
  } | null;
  const declared = nodeConfig?.ShelleyGenesisFile;
  if (typeof declared !== "string" || declared.length === 0)
    throw new Error("Node config does not declare ShelleyGenesisFile");
  return await readFile(
    normalize(
      isAbsolute(declared)
        ? declared
        : resolve(dirname(nodeConfigPath), declared),
    ),
    "utf8",
  );
};
