import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { WATCHER_DA_ATTESTATIONS_TABLE } from "../../src/l1-follower/tables.js";
import {
  D,
  harness,
  hex,
  okValue,
} from "../support/l1-follower-raw-reads-fixture.js";
import {
  attestTx,
  commitTx,
  initTx,
  queueState,
} from "../support/l1-follower-state-queue-traffic.js";

// One validator mints the DAAT and holds its output (the deployed
// da_attestation script hash is both daAttestationMint and
// daAttestationSpend), so the DAAT output sits at that credential.
const DA_ATTESTATION_ADDRESS = Buffer.concat([
  Buffer.of(0x70),
  Buffer.from(D.daAttestationMint, "hex"),
]);

describe("watcher projection: DA attestation AddSignatures", () => {
  it("stores the tx that only re-outputs a DAAT, so Apply's commitment recovery can read it", async () => {
    const h = await harness();
    try {
      await h.forward([initTx(D)]);
      const commit = commitTx(queueState(h.chain, D)!, D);
      await h.forward([commit]);
      const header = [
        ...(commit.mint?.get(D.stateQueueMint)?.keys() ?? []),
      ][0]!.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
      const minted = attestTx(queueState(h.chain, D)!, D);
      const attest: SimTx = {
        ...minted,
        outputs: minted.outputs.map((output) => ({
          ...output,
          address: DA_ATTESTATION_ADDRESS,
        })),
      };
      const [attestHash] = (await h.forward([attest])).hashes as [string];
      // AddSignatures: spends the DAAT output and re-outputs it; no mint.
      const signatures: SimTx = {
        inputs: [{ txHash: Buffer.from(attestHash, "hex"), index: 0 }],
        outputs: [attest.outputs[0]!],
        nonce: h.chain.nonce(),
      };
      const landed = await h.forward([signatures]);
      const [signaturesHash] = landed.hashes as [string];

      const reads = h.reads();
      const inclusion = okValue(
        await reads.transactionInclusion(signaturesHash),
      );
      expect(inclusion?.pointId).toBe(landed.point.pointId);
      const stored = okValue(
        await reads.rawTransaction(signaturesHash, landed.point),
      );
      expect(stored.transaction.txHash).toBe(signaturesHash);
      // Its DAAT output is recorded for the queued header, so it is pinned.
      const rows = await h.store.transaction("read", (tx) =>
        tx.query(
          `SELECT tx_hash FROM ${WATCHER_DA_ATTESTATIONS_TABLE} WHERE header_hash = ?`,
          [Buffer.from(header, "hex")],
        ),
      );
      expect(
        rows.map((row) => hex(Buffer.from(row.tx_hash as Uint8Array))).sort(),
      ).toEqual([attestHash, signaturesHash].sort());
    } finally {
      await h.store.close();
    }
  });
});
