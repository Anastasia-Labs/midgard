import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";

import { computeFraudProofRawL1PointId } from "../../src/workflow/raw-l1-snapshot.js";
import type {
  SignedTransactionRecoveryObservation,
  SignedWorkflowTransaction,
} from "../../src/workflow/signed-transaction-reconciliation.js";

/** Exact signed bytes of one wallet spend, as a workflow records its intent. */
export const signedWorkflowTransactionFixture = async ({
  ttl = 6000,
}: { readonly ttl?: number | null } = {}) => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const planned = lucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: 5_000_000n });
  if (ttl !== null) planned.validTo(lucid.slotToUnixTime(ttl));
  const signed = await (await planned.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const input: SignedWorkflowTransaction = {
    transactionHash: signed.toHash(),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
  };
  return { input, signed };
};

const point = (blockNo: number) => {
  const raw = {
    slot: String(blockNo * 2 + 100),
    blockHash: blockNo.toString(16).padStart(64, "0"),
    blockNo: String(blockNo),
  };
  return { ...raw, pointId: computeFraudProofRawL1PointId(raw) };
};

/**
 * A canonical recovery observation of exact signed bytes. `depth` is the
 * number of blocks the canonical tip lies beyond the inclusion or
 * release-final point; past the recovery horizon by default.
 */
export const signedRecoveryObservation = (
  input: SignedWorkflowTransaction,
  status: SignedTransactionRecoveryObservation["status"],
  {
    depth = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1,
  }: { readonly depth?: number } = {},
): SignedTransactionRecoveryObservation => {
  const boundary = point(50);
  const canonicalPoint = point(50 + depth);
  return {
    ...input,
    status,
    ...(status === "included" ? { inclusionPoint: boundary } : {}),
    canonicalPoint,
    releaseFinalPoint: boundary,
    inputs: [],
    reason: status,
  };
};
