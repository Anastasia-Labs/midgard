import {
  type Hash32,
  type MidgardCekProgramMaterialEntry,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";

import type { MidgardCekConstantValueWitness } from "./cek-builtin.js";
import { parseScriptDecodeWrapper } from "./script-decode.step.js";

export type MidgardCekProgramMaterialKind =
  MidgardCekProgramMaterialEntry["kind"];

export type MidgardCekProgramMaterialNode = MidgardCekProgramMaterialEntry;

export type MidgardCanonicalCekProgram = {
  readonly uplcVersion: readonly [bigint, bigint, bigint];
  readonly termRoot: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialNode>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitness
  >;
};

export type MidgardCanonicalScriptArtifactLanguage = "PlutusV3" | "MidgardV1";

export type MidgardCanonicalScriptArtifactInput = {
  readonly language: MidgardCanonicalScriptArtifactLanguage;
  readonly sourceRawFlatProgramBytes: Uint8Array;
};

/** Stored Cardano-compatible script identity. Material is derived only for disputes. */
export type MidgardCanonicalScriptArtifact = {
  readonly canonicalMidgardCredentialScript: MidgardVersionedScript;
  readonly canonicalMidgardCredentialScriptHash: string;
};

export const rootHex = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

export const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

/** Extracts exact Flat bytes for local derivation; admission uses the decode record. */
export const storedFlatProgramBytes = (scriptBytes: Uint8Array): Buffer => {
  const wrapper = parseScriptDecodeWrapper(scriptBytes);
  if (
    wrapper.k !== "wrapped" ||
    wrapper.flatStart + wrapper.flatLength !== scriptBytes.length
  ) {
    throw new Error(
      "CEK material requires one complete stored script byte string",
    );
  }
  return Buffer.from(
    scriptBytes.subarray(
      wrapper.flatStart,
      wrapper.flatStart + wrapper.flatLength,
    ),
  );
};
