import type { OriginConfig } from "@al-ft/midgard-l1-follower";

/** The `hubOracleOneShot` outref of the manifest (`txHash#index`). */
export const parseHubOracleOneShot = (
  outRef: string,
): OriginConfig["hubOracleOneShot"] => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(outRef);
  if (match === null)
    throw new Error(`hubOracleOneShot outref is malformed: ${outRef}`);
  return { txHash: Buffer.from(match[1]!, "hex"), index: Number(match[2]) };
};
