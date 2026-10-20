/**
 * Staging a script's bytes outside a workflow (the CLI and direct record
 * paths): each chunk is found again by the out-ref its own stage
 * transaction gave it, kept in an in-memory journal.
 */
import { createHash } from "node:crypto";

import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  claimOwnOutputs,
  createOwnOutputJournal,
  inlineDatumCarriageSlot,
  type OwnOutputJournal,
  resolveOwnOutputs,
} from "../workflow/own-outputs.js";
import {
  buildScriptDecodeStageTx,
  scriptDecodeStagingDatums,
  submitScriptDecodeTx,
} from "./submit.js";

const scriptDecodeStagingDigest = (datum: string): string =>
  createHash("sha256").update(datum).digest("hex");

/**
 * Stages `scriptBytes` in order across UTxOs at `address` (the wallet's own by
 * default, so their Ada stays reclaimable), one transaction each. With
 * `ownOutputs`, a chunk an earlier call staged and journaled there, still
 * live, is reused; every chunk staged here is journaled there.
 */
export const stageScriptDecodeBytes = async ({
  lucid,
  scriptBytes,
  address,
  ownOutputs,
}: {
  readonly lucid: LucidEvolution;
  readonly scriptBytes: Uint8Array;
  readonly address?: string;
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<UTxO[]> => {
  const target = address ?? (await lucid.wallet().address());
  const journal = ownOutputs ?? createOwnOutputJournal();
  const datums = scriptDecodeStagingDatums(scriptBytes);
  const keyOf = (ordinal: number) =>
    `script-decode-staging:${target}:${ordinal.toString()}:${scriptDecodeStagingDigest(datums[ordinal]!)}`;
  for (;;) {
    const resolved = await resolveOwnOutputs({
      slots: datums.map((datum, ordinal) => ({
        slot: inlineDatumCarriageSlot(target, datum),
        claims: journal.claims(keyOf(ordinal)),
      })),
      read: (outRefs) => lucid.utxosByOutRef([...outRefs]),
    });
    if (resolved.kind === "ready") return [...resolved.utxos];
    if (resolved.kind === "pending") {
      throw new Error("script decode: a staged chunk is not yet visible");
    }
    const built = await buildScriptDecodeStageTx({
      lucid,
      datum: datums[resolved.ordinal]!,
      address: target,
    });
    // The out-ref is journaled from the signed body before it is sent.
    const [claim] = claimOwnOutputs({
      signed: built.signed,
      slots: [{ address: built.address, matches: built.matches }],
    });
    journal.claim(keyOf(resolved.ordinal), claim!.outRef);
    await submitScriptDecodeTx(lucid, built);
  }
};
