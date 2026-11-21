/**
 * Staging a script's bytes outside a workflow (the CLI and direct record
 * paths): each chunk is found again by the out-ref its own stage
 * transaction gave it, kept in an in-memory journal.
 */
import { createHash } from "node:crypto";

import {
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";

import { proofMaterialsAddress } from "../funding/proof-materials-address.js";
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
 * Stages `scriptBytes` in order across UTxOs at `address` (by default the
 * proof-materials address of the selected wallet's key, which that key
 * spends back), one transaction each. With
 * `ownOutputs`, a chunk an earlier call staged and claimed there, still
 * live, is reused; every chunk staged here is claimed there.
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
  const target =
    address ??
    proofMaterialsAddress(
      lucid.config().network ?? "Custom",
      paymentCredentialOf(await lucid.wallet().address()).hash,
    );
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
    // The out-ref is claimed from the signed body before it is sent.
    const [claim] = claimOwnOutputs({
      signed: built.signed,
      slots: [{ address: built.address, matches: built.matches }],
    });
    journal.claim(keyOf(resolved.ordinal), claim!.outRef);
    await submitScriptDecodeTx(lucid, built);
  }
};
