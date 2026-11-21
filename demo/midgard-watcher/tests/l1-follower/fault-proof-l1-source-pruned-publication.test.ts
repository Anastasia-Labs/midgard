import { createAuthenticatedFieldCarriagePrerequisitePort } from "@al-ft/midgard-fault-proofs/workflow/field-carriage-prerequisite";
import {
  fraudProofSignedTransactionRecovery,
  withFraudProofL1Recovery,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import type { FraudProofWorkflowAction } from "@al-ft/midgard-fault-proofs/workflow/orchestrator";
import { createRawDatumPreimageRequirement } from "@al-ft/midgard-fault-proofs/workflow/raw-datum-preimage-prerequisite";
import { createFraudProofAuthenticatedPublicationObserver } from "@al-ft/midgard-fault-proofs/workflow/raw-l1-publication-observation";
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
 * A field publication of ours that the node dropped, continued in process
 * over the watcher's fault-proof L1 source once the follower has pruned: the
 * field-carriage port still requires it, signed recovery reads the exact
 * bytes this process sent as worth rebroadcasting and resends them
 * unchanged, and once they land it reads them included.
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
  it("rebroadcasts a dropped publication this process sent and reads it included once it lands", async () => {
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
        address: "addr_test1_funding",
        proofMaterialsAddress: address,
        paymentKeyHash: "12".repeat(28),
        selectWallet: () => undefined,
      },
      publications,
      requirementForAction: () => requirement,
    });
    const required = await port.inspect({
      headerHash: fx.header,
      holder: {
        workflowId: "pruned-publication",
        objective: { category: "nonExistentInput", headerHash: fx.header },
      },
      baseAction,
      artifact: {},
    });
    if (required.kind !== "required")
      throw new Error("expected a publication requirement");
    const datumCbor = requirement.publicationDatums[0]!;
    // Our publication: spends V#0 (live at T) and pays the datum to T. This
    // process sent it, then the node dropped it: it never reaches the chain.
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
    const sent = {
      transactionHash: txHash,
      signedTransactionCborHex: signed.signedTransactionCborHex,
    } as const;
    await expect(
      publications.observeSignedTransaction!(sent),
    ).resolves.toMatchObject({ status: "rebroadcast" });
    await expect(
      publications.rebroadcastSignedTransaction!({
        ...sent,
        authorizeResubmission: async () => undefined,
      }),
    ).resolves.toBe(txHash);
    expect(double.calls).toContain(`submit:${signed.signedTransactionCborHex}`);

    await fx.forward([publication]);
    await expect(
      publications.observeSignedTransaction!(sent),
    ).resolves.toMatchObject({ status: "included" });
  });
});
