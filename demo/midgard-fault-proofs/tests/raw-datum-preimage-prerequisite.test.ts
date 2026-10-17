import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { ResolvedProverSigner } from "../src/runtime.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createChunkedRawDatumPreimageRequirement,
  createRawDatumPreimageRequirement,
  rawDatumPreimagePublicationPlan,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "../src/workflow/raw-l1-publication-observation.js";
import {
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
} from "./support/submit-init-emulator-shared.js";

const headerHash = "ab".repeat(28);
const action: FraudProofWorkflowAction = {
  actionId: "consume-raw-native-policy",
  input: { category: "mintAuthorization", stage: "step_03" },
};

describe("raw datum preimage publication prerequisite", () => {
  it.each([15000, 4096])(
    "publishes maximum chunks of %i bytes, re-admits recovery after restart and refuses mutations",
    async (chunkBytes) => {
      const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
      const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
      const lucid = await Lucid(emulator, network);
      const signer: ResolvedProverSigner = {
        source: "raw-preimage-test",
        address: account.address,
        paymentKeyHash: getAddressDetails(account.address).paymentCredential!
          .hash,
        selectWallet: (target) =>
          target.selectWallet.fromSeed(account.seedPhrase),
      };
      signer.selectWallet(lucid);
      const bytes = Buffer.from(
        Array.from({ length: 65_536 }, (_, index) => index % 251),
      );
      const requirement = (
        chunkBytes === 4096
          ? createChunkedRawDatumPreimageRequirement
          : createRawDatumPreimageRequirement
      )({ preimage: bytes });
      const count = Math.ceil(65536 / chunkBytes);
      bytes.fill(0);
      expect(requirement.publicationDatums).toHaveLength(count);
      expect(Object.isFrozen(requirement.publicationDatums)).toBe(true);
      expect(
        rawDatumPreimagePublicationPlan(requirement).plan.publications.map(
          (item) => item.bytes.length,
        ),
      ).toEqual(
        Array.from({ length: count }, (_, i) =>
          Math.min(chunkBytes, 65536 - i * chunkBytes),
        ),
      );
      let confirm = true;
      const publications: FraudProofAuthenticatedPublicationObserver = {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        // Emulator authority: exact output content and out-ref, never a Boolean confirmation stub.
        observeExact: async (input) => {
          const found = (await lucid.utxosAt(input.address)).find(
            (utxo) =>
              `${utxo.txHash}#${utxo.outputIndex}` === input.expectedOutRef &&
              utxo.datum === input.expectedDatumCbor &&
              utxo.scriptRef == null &&
              Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
          );
          return confirm && found !== undefined
            ? { kind: "confirmed", outRef: input.expectedOutRef }
            : { kind: "not_found" };
        },
      };
      const port = () =>
        createAuthenticatedRawDatumPreimagePrerequisitePort({
          category: "mintAuthorization",
          lucid,
          network,
          signer,
          publications,
          requirementForAction: ({ action: candidate }) =>
            candidate.actionId === action.actionId ? requirement : null,
          transactionConfirmed: async () => false,
        });
      for (let index = 0; index < count; index++) {
        const inspection = await port().inspect({
          headerHash,
          baseAction: action,
          artifact: {},
          entries: [],
        });
        if (inspection.kind !== "required")
          throw new Error(`missing chunk ${index}`);
        expect(inspection.action.input.schemaVersion).toBe(
          "midgard-raw-datum-preimage-prerequisite-v1",
        );
        expect(inspection.action.input.publicationIndex).toBe(index);
        const captured = await port().capture({
          headerHash,
          action: inspection.action,
          artifact: {},
        });
        const submission = await captureEmulatorSubmission(emulator, () =>
          captured.transaction.signed.submit(),
        );
        await lucid.awaitTx(submission.result);
        expect(submission.measurements).toHaveLength(1);
        expect(
          submission.measurements[0]!.completeSignedBytes,
        ).toBeLessThanOrEqual(15_872);
        const recovery = JSON.parse(JSON.stringify(captured.durableRecovery));
        const restarted = port();
        await expect(
          restarted.reconcile({
            headerHash,
            action: inspection.action,
            artifact: {},
            txHash: captured.transaction.txHash,
            durableRecovery: recovery,
          }),
        ).resolves.toMatchObject({
          kind: "confirmed",
          txHash: captured.transaction.txHash,
        });
        await expect(
          restarted.reconcile({
            headerHash,
            action: inspection.action,
            artifact: {},
            txHash: "00".repeat(32),
            durableRecovery: recovery,
          }),
        ).resolves.toMatchObject({ kind: "conflict" });
        await expect(
          restarted.capture({
            headerHash,
            action: {
              ...inspection.action,
              input: {
                ...inspection.action.input,
                publicationDigest: "00".repeat(32),
              },
            },
            artifact: {},
          }),
        ).rejects.toThrow();
      }
      const resolved = await port().resolveAuthenticated({
        headerHash,
        action,
        artifact: {},
      });
      expect(resolved.publications.map((utxo) => utxo.datum)).toEqual(
        requirement.publicationDatums,
      );
      expect(resolved.certificate).toBeUndefined();
      await expect(
        port().inspect({
          headerHash,
          baseAction: action,
          artifact: {},
          entries: [],
        }),
      ).resolves.toMatchObject({ kind: "satisfied" });
      confirm = false;
      await expect(
        port().resolveAuthenticated({ headerHash, action, artifact: {} }),
      ).rejects.toThrow("unauthenticated");
      await expect(
        port().inspect({
          headerHash,
          baseAction: action,
          artifact: {},
          entries: [],
        }),
      ).resolves.toMatchObject({ kind: "pending" });
    },
  );
  it("rejects empty, oversized and mutated publication identities", () => {
    expect(() =>
      createRawDatumPreimageRequirement({ preimage: Buffer.alloc(0) }),
    ).toThrow();
    expect(() =>
      createRawDatumPreimageRequirement({ preimage: Buffer.alloc(65_537) }),
    ).toThrow();
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("abc"),
    });
    expect(() =>
      rawDatumPreimagePublicationPlan({
        ...requirement,
        publicationDigests: ["00".repeat(32)],
      }),
    ).toThrow();
    expect(() =>
      rawDatumPreimagePublicationPlan({
        ...requirement,
        publicationDatums: ["40"],
      }),
    ).toThrow();
  });
});
