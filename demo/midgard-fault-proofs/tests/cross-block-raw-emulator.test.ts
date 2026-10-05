import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotRequest,
} from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

// This proves the emulator fixture's clock/bytes, not native chain authentication.
describe("cross-block raw emulator clock", () => {
  it("reads real inclusion and empty-block depth without changing signed bytes or outrefs", async () => {
    const unit = `${"ab".repeat(28)}01`;
    const account = generateEmulatorAccount({
      lovelace: 40_000_000n,
      [unit]: 1n,
    });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const recorder = recordCrossBlockRawEmulator();
    const releaseFinality = {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: "11".repeat(32),
      blueprintHash: "22".repeat(32),
      policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
        DEPLOYMENT_MANIFEST_L1_FINALITY,
      ),
      policy: DEPLOYMENT_MANIFEST_L1_FINALITY,
    };
    const request: FraudProofRawL1SnapshotRequest = {
      deploymentIdentityDigest: "11".repeat(32),
      blueprintHash: "22".repeat(32),
      finalityPolicyDigest: releaseFinality.policyDigest,
      headerHash: "44".repeat(28),
      scopes: [{ role: "state_queue", address: account.address }],
      historyUnits: [unit],
    };
    const capture = async () =>
      admitFraudProofRawL1Snapshot({
        value: await recorder.authority.capture(request),
        request,
        releaseFinality,
        observationDepth: "inclusion",
      });
    try {
      const built = await lucid
        .newTx()
        .pay.ToAddress(account.address, {
          lovelace: 2_000_000n,
          [unit]: 1n,
        })
        .complete();
      const signed = await built.sign.withWallet().complete();
      const cbor = signed.toCBOR();
      const txHash = await signed.submit();
      expect
        .soft(await emulator.getTransactionStatus(txHash))
        .toMatchObject({ status: "pending" });
      const pendingCapture = await recorder.authority
        .capture(request)
        .catch((cause: unknown) => cause);
      expect.soft(pendingCapture).toBeInstanceOf(Error);
      expect.soft(pendingCapture).toMatchObject({
        message: "no confirmed emulator transactions captured",
      });
      emulator.awaitBlock();
      const included = await emulator.getTransactionStatus(txHash);
      if (included.status !== "confirmed")
        throw new Error("actual transaction did not land");
      const first = await capture();
      expect.soft(await capture()).toEqual(first);
      expect.soft(first.transactions).toHaveLength(1);
      expect.soft(first.transactions[0]!.inclusionPoint).toMatchObject({
        blockNo: String(included.confirmation.blockHeight),
        slot: String(included.confirmation.slot),
      });
      expect.soft(first.transactions[0]!.confirmationDepth).toBe(1);
      expect.soft(first.cursor.tip).toMatchObject({
        blockNo: String(emulator.blockHeight),
        slot: String(emulator.slot),
      });
      const originalOutputs = first.scopes;
      const originalTransaction = first.transactions[0]!;
      emulator.awaitBlock(
        DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
      );
      const shallow = await capture();
      expect.soft(shallow.transactions[0]!.confirmationDepth).toBe(2161);
      expect.soft(shallow.cursor.confirmationDepth).toBe(2161);
      expect.soft(shallow.cursor.tip).toMatchObject({
        blockNo: String(emulator.blockHeight),
        slot: String(emulator.slot),
      });
      expect.soft(shallow.scopes).toEqual(originalOutputs);
      expect
        .soft(shallow.transactions[0])
        .toEqual({ ...originalTransaction, confirmationDepth: 2161 });
      emulator.awaitBlock();
      const deep = await capture();
      expect
        .soft(deep.transactions[0])
        .toEqual({ ...originalTransaction, confirmationDepth: 2162 });
      expect.soft(deep.cursor.confirmationDepth).toBe(2162);
      expect.soft(await capture()).toEqual(deep);
      expect.soft(deep.scopes).toEqual(originalOutputs);
      expect.soft(recorder.signedCbors.get(txHash)).toBe(cbor);
      expect
        .soft(CML.Transaction.from_cbor_hex(cbor).body().to_cbor_hex())
        .toBe(originalTransaction.bodyCbor);
      expect.soft(await emulator.getTransactionStatus(txHash)).toMatchObject({
        status: "confirmed",
        confirmation: { ...included.confirmation, confirmations: 2162 },
      });
    } finally {
      recorder.restore();
    }
  });
});
