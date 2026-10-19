import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  createFraudProofAuthenticatedPublicationObserver,
  createRawDatumPreimageRequirement,
  FIELD_CARRIAGE_RECOVERY,
  fraudProofSignedTransactionRecovery,
  type FraudProofWorkflowAction,
  withFraudProofL1Recovery,
} from "@al-ft/midgard-fault-proofs";
import { type SimTx, simTxHash } from "@al-ft/midgard-l1-follower/testing";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  bech32,
  fixture,
  hex,
  pruneFixture,
  T,
} from "../support/l1-follower-raw-reads-fixture.js";
import {
  nodeDouble,
  RELEASE,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

/**
 * A field publication of ours that the node dropped, reconciled through the
 * field-carriage port over the watcher's fault-proof L1 source once the
 * follower has pruned: the exact read finds no output, signed recovery
 * rebroadcasts the journaled bytes, and once they land the publication is
 * confirmed.
 */

const baseAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `step_02:${"44".repeat(32)}#0:${"55".repeat(32)}#0`,
  input: Object.freeze({
    schemaVersion: "midgard-production-linear-family-action-v1",
    category: "nonExistentInput",
    stage: "step_02",
    ordinal: 2,
    threadOutRef: `${"44".repeat(32)}#0`,
    stateQueueBlockOutRef: `${"55".repeat(32)}#0`,
  }),
});

describe("field publication on a pruned follower", () => {
  it("rebroadcasts a dropped publication and confirms it once it lands", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const double = nodeDouble();
    const source = sourceOver(fx, fx.reads(), double.node);
    const { publications } = withFraudProofL1Recovery(
      {
        publications: createFraudProofAuthenticatedPublicationObserver({
          authority: source.snapshotAuthority({
            releaseFinality: RELEASE,
            observationDepth: "inclusion",
          }),
          releaseFinality: RELEASE,
        }),
      },
      fraudProofSignedTransactionRecovery(source, RELEASE),
    );
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("8101", "hex"),
    });
    const address = bech32(T);
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid: { utxosAt: async () => [] } as unknown as LucidEvolution,
      network: "Preview",
      signer: {
        source: "test",
        address,
        paymentKeyHash: "12".repeat(28),
        selectWallet: () => undefined,
      },
      publications,
      requirementForAction: () => requirement,
      // Whether the follower holds the transaction at all.
      transactionConfirmed: async ({ txHash }) =>
        (await fx.store.txByHash(Buffer.from(txHash, "hex"))) !== null,
    });
    const required = await port.inspect({
      headerHash: fx.header,
      baseAction,
      artifact: {},
      entries: [],
    });
    if (required.kind !== "required")
      throw new Error("expected a publication requirement");
    const datumCbor = requirement.publicationDatums[0]!;
    // Our publication: spends V#0 (live at T) and pays the datum to T. It
    // was submitted and journaled, then dropped: it never reaches the chain.
    const publication: SimTx = {
      inputs: [{ txHash: Buffer.from(fx.hashes.v, "hex"), index: 0 }],
      outputs: [
        {
          address: T,
          lovelace: 2_000_000n,
          datum: Buffer.from(datumCbor, "hex"),
        },
      ],
      nonce: fx.chain.nonce(),
    };
    const txHash = hex(simTxHash(publication));
    const signed = signedOf(publication);
    const input = {
      headerHash: fx.header,
      action: required.action,
      artifact: {},
      txHash,
      durableRecovery: {
        fieldCarriage: {
          schemaVersion: FIELD_CARRIAGE_RECOVERY,
          kind: "publication",
          requirementSha256: required.action.input.requirementSha256 as string,
          outRef: `${txHash}#0`,
          datumCbor,
          unit: null,
        },
      },
      signedTransactionCborHex: signed.signedTransactionCborHex,
      authorizeResubmission: async () => undefined,
    } as const;

    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "pending",
      txHash,
    });
    expect(double.calls).toEqual([
      `hasTx:${txHash}`,
      `submit:${signed.signedTransactionCborHex}`,
    ]);

    await fx.forward([publication]);
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "confirmed",
      txHash,
    });
  });
});
