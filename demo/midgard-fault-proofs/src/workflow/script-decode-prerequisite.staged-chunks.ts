/**
 * Where a script's staged chunks stand for the script-decode port: each
 * chunk is the output our own confirmed stage transaction for it created,
 * read by its out-ref. Nothing else at the staging address is ever taken
 * for a chunk.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import { scriptDecodeStagingDatums } from "../script-decode/submit.js";
import { inlineDatumCarriageSlot, resolveOwnOutputs } from "./own-outputs.js";
import {
  confirmedScriptDecodeOutputs,
  scriptDecodeDatumDigest,
} from "./script-decode-prerequisite.actions.js";

/**
 * Ready with the chunks in order; else the first chunk to publish (naming
 * the out-ref it replaces), or the out-ref a provider read lags behind
 * (confirmed `recent`ly, not yet visible).
 */
export const scriptDecodeStagedChunks = ({
  lucid,
  category,
  address,
  recent,
  entries,
  scriptHashHex,
  scriptBytes,
}: {
  readonly lucid: LucidEvolution;
  readonly category: FraudProofCatalogueCategoryName;
  /** The staging address: the prover's own. */
  readonly address: string;
  /** Whether a confirmation journaled at `confirmedAt` is within the provider grace. */
  readonly recent: (confirmedAt: string) => boolean;
  readonly entries: Parameters<
    typeof confirmedScriptDecodeOutputs
  >[0]["entries"];
  readonly scriptHashHex: string;
  readonly scriptBytes: Uint8Array;
}) => {
  const stages = confirmedScriptDecodeOutputs({
    entries,
    category,
    scriptHashHex,
    stages: ["script_decode_stage"],
  });
  return resolveOwnOutputs({
    slots: scriptDecodeStagingDatums(scriptBytes).map((datum, chunkIndex) => {
      const digest = scriptDecodeDatumDigest(datum);
      return {
        slot: inlineDatumCarriageSlot(address, datum),
        claims: stages
          .filter(
            ({ action, recovery }) =>
              action.chunkIndex === chunkIndex &&
              recovery.datumCborSha256 === digest &&
              recovery.address === address,
          )
          .map(({ recovery, confirmedAt }) => ({
            outRef: recovery.outRef,
            confirmedAt,
          })),
      };
    }),
    read: (outRefs) => lucid.utxosByOutRef([...outRefs]),
    recent: ({ confirmedAt }) =>
      confirmedAt !== undefined && recent(confirmedAt),
  });
};
