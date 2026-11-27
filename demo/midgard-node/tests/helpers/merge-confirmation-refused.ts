import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Either } from "effect";
import { expect } from "vitest";

import {
  DepositsDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import type { MergeActionResult } from "../../src/fibers/merge.js";
import type { MergeLifecycle } from "../merge-landed-finalization-emulator.test.js";
import { dropPendingEmulatorTransaction } from "./emulator-rollback.js";

/**
 * A merge of the oldest block whose transaction L1 reports failed: the
 * attempt fails as a merge that did not confirm (`failed to confirm the
 * merge tx`, naming the merge), and nothing is folded, at once or at the
 * next driver run. The merge then leaves the mempool, so L1 has not taken
 * it.
 */
export const assertRefusedMergeFoldsNothing = async (m: MergeLifecycle) => {
  const { fixture } = m;
  const block = await m.commitDepositBlock(12_000_000n);
  let heldTxHash: string | undefined;
  const refusingLucid = m.withConfirmationWait(
    async () => false,
    async (txHash, read) =>
      txHash === heldTxHash
        ? { status: "failed", txHash, reason: "L1 refused it" }
        : read(txHash),
  );
  const submitThrough = fixture.emulator.submitTx;
  fixture.emulator.submitTx = async (signedCbor) => {
    heldTxHash = await submitThrough.call(fixture.emulator, signedCbor);
    return heldTxHash;
  };
  let refused: Either.Either<MergeActionResult, unknown>;
  try {
    refused = await m.mergeUntilSubmitted(refusingLucid);
  } finally {
    fixture.emulator.submitTx = submitThrough;
  }
  expect(heldTxHash).toBeDefined();
  expect(
    Either.isLeft(refused) &&
      formatUnknownError(refused.left, { includeCause: true }),
  ).toContain("failed to confirm the merge tx");
  expect(Either.isLeft(refused) && refused.left).toMatchObject({
    _tag: "TxConfirmError",
    txHash: heldTxHash,
  });
  const applied = async () => {
    expect(await m.ownStatus(block)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
    );
    await m.expectDeposits(block, DepositsDB.Status.Projected);
  };
  await applied();
  dropPendingEmulatorTransaction(fixture.emulator, heldTxHash!);
  m.h.observer.forgetDropped(heldTxHash!);
  m.h.lucidService.api.clearUTxOOverride();
  fixture.operatorLucid.clearUTxOOverride();
  expect(await m.queuedBlocks()).toBe(1);
  await m.h.synchronize();
  await applied();
};
