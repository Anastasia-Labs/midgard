/**
 * The watcher's L1 source reads an attempt whose phase-2-invalid landing a
 * rollback removed: the transaction is absent again and every input it
 * recorded, its collateral among them, is live, so the source reports it as
 * one that could be sent again.
 */
import { fraudProofSignedTransactionRecovery } from "@al-ft/midgard-fault-proofs";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import { expect, it } from "vitest";

import { harness, T } from "../support/l1-follower-raw-reads-fixture.js";
import {
  nodeDouble,
  RELEASE,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

it("reads an attempt whose phase-2-invalid landing a rollback removed as absent with every input live, ready to be sent again", async () => {
  const h = await harness();
  const funding: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [
      { address: T, lovelace: 2_000_000n },
      { address: T, lovelace: 3_000_000n },
    ],
    nonce: h.chain.nonce(),
  };
  const { hashes } = await h.forward([funding]);
  for (let block = 0; block < 4; block += 1) await h.forward([]);
  const at = (index: number) => ({
    txHash: Buffer.from(hashes[0]!, "hex"),
    index,
  });
  const attempt: SimTx = {
    inputs: [at(0)],
    collaterals: [at(1)],
    collateralReturn: { address: T, lovelace: 2_500_000n },
    outputs: [{ address: T, lovelace: 1_500_000n }],
    nonce: h.chain.nonce(),
  };
  const double = nodeDouble();
  const recovery = fraudProofSignedTransactionRecovery(
    sourceOver(h, h.reads(), double.node),
    RELEASE,
  );
  const observe = () => recovery.observeSignedTransaction(signedOf(attempt));

  await h.forward([{ ...attempt, isValid: false }]);
  expect(await observe()).toMatchObject({ status: "landed_invalid" });

  await h.backward(1);
  const observed = await observe();
  expect(observed).toMatchObject({
    status: "rebroadcast",
    canonicalPoint: h.tipPoint(),
  });
  expect(observed.inputs.map(({ outRef }) => outRef).sort()).toEqual(
    [`${hashes[0]!}#0`, `${hashes[0]!}#1`].sort(),
  );
});
