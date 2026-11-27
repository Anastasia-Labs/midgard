/**
 * A field-preimage publication is funded from the funding address's largest
 * plain coin alone, so a rebuild over an unchanged wallet is the same
 * transaction and at most one copy lands. Once the largest coin has changed a
 * rebuild spends another coin and a second copy can land: the workflow uses
 * one, neither is swept while it lives, and its ending sweeps both.
 */
import {
  buildUnsignedFieldPreimagePublicationProgram,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk/state-queue";
import type { TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  actionOf,
  ADA,
  CONFLICT,
  contextOf,
  HEADER_A,
  HEADER_B,
  holderOf,
  label,
  prover,
  type Scene,
  submitSweep,
  sweeping,
  sweptOutRefs,
  viewOf,
} from "./support/proof-materials-sweep-scene.js";

const BYTES = Buffer.alloc(2_048, 0x5c);
const DATUM = fieldPreimagePublicationDatumCbor(BYTES);

const publicationOf = async (scene: Scene) =>
  await Effect.runPromise(
    buildUnsignedFieldPreimagePublicationProgram(scene.lucid, {
      publication: {
        chunkIndex: 0,
        datumCbor: DATUM,
        byteLength: BYTES.length,
        digestHex: "00".repeat(32),
      },
      publisherAddress: scene.materialsAddress,
    }),
  );

const spent = (unsigned: TxSignBuilder) => {
  const inputs = unsigned.toTransaction().body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

describe("a field-preimage publication rebuilt over the funding wallet", () => {
  it("is identical over an unchanged wallet; once the largest coin changes a second copy lands, one is used and both are swept at the end", async () => {
    const scene = await prover();
    await scene.fund(300n * ADA);
    const first = await publicationOf(scene);
    expect(spent(first)).toHaveLength(1);
    expect((await publicationOf(scene)).toHash()).toBe(first.toHash());

    // A larger coin arrives: the rebuild spends it instead.
    const larger = await scene.fund(8_000n * ADA);
    const second = await publicationOf(scene);
    expect(spent(second)).toEqual([`${larger}#0`]);
    expect(second.toHash()).not.toBe(first.toHash());
    for (const unsigned of [first, second])
      await (await unsigned.sign.withWallet().complete()).submit();
    scene.emulator.awaitBlock();
    const copies = (await scene.at(scene.materialsAddress)).filter(
      (utxo) => utxo.datum === DATUM,
    );
    expect(copies.map((utxo) => utxo.txHash).sort()).toEqual(
      [first.toHash(), second.toHash()].sort(),
    );

    // The workflow uses one copy; while it lives neither is swept.
    const view = viewOf(scene);
    const [used] = await view.find([DATUM], holderOf("workflow-a", HEADER_A));
    expect(copies.map(label)).toContain(label(used!));
    const contextB = contextOf({}, "workflow-b", HEADER_B);
    expect(await sweeping(scene, view, CONFLICT).observe(contextB)).toEqual(
      CONFLICT,
    );
    expect(await scene.at(scene.materialsAddress)).toHaveLength(2);

    // Its ending sweeps the unused copy with the used one.
    const adapter = sweeping(scene, view, CONFLICT);
    const action = actionOf(await adapter.observe(contextOf()));
    expect([...sweptOutRefs(action)].sort()).toEqual(copies.map(label).sort());
    await submitSweep(scene, adapter, action);
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
  }, 300_000);
});
