import { deriveValidationProofItemPublication } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { ResolvedProverSigner } from "../src/runtime.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  createBoundDataPublicationRequirement,
} from "../src/workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
} from "./support/submit-init-emulator-shared.js";

const headerHash = "ab".repeat(28);
const action: FraudProofWorkflowAction = {
  actionId: "semantic_resolution:bound-observe",
  input: {
    category: "validationTraceDispute",
    stage: "semantic_resolution",
    threadOutRef: `${"cd".repeat(32)}#0`,
  },
};

describe("bound typed publication prerequisite", () => {
  it("authenticates one exact address and datum, recovers cold, and reacquires a spent output", async () => {
    const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
    const recipient = generateEmulatorAccount({ lovelace: 1_000_000_000n });
    const emulator = new Emulator(
      [account, recipient],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const lucid = await Lucid(emulator, network);
    const signer: ResolvedProverSigner = {
      source: "bound-publication-test",
      address: account.address,
      paymentKeyHash: getAddressDetails(account.address).paymentCredential!
        .hash,
      selectWallet: (target) =>
        target.selectWallet.fromSeed(account.seedPhrase),
    };
    signer.selectWallet(lucid);
    const publication = deriveValidationProofItemPublication({
      transactionId: "11".repeat(32),
      transactionCommitment: "22".repeat(32),
      fieldPreimage: "33".repeat(14_336),
    });
    const requirement = createBoundDataPublicationRequirement({
      publicationAddress: recipient.address,
      datumCbor: publication.datumCbor,
      sourceIdentity: {
        headerHash,
        action,
        sourceKind: "0",
        fieldIndex: 2,
        transactionId: "11".repeat(32),
        transactionCommitment: "22".repeat(32),
      },
    });
    const confirmed = new Set<string>();
    const port = () =>
      createAuthenticatedFieldCarriagePrerequisitePort({
        category: "validationTraceDispute",
        lucid,
        network,
        signer,
        requirementForAction: ({ action: candidate }) =>
          candidate.actionId === action.actionId ? requirement : null,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (input) => {
            const found = (await lucid.utxosAt(input.address)).find(
              (utxo) =>
                `${utxo.txHash}#${utxo.outputIndex}` === input.expectedOutRef &&
                utxo.datum === input.expectedDatumCbor &&
                utxo.datumHash == null &&
                utxo.scriptRef == null &&
                Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
            );
            return found === undefined
              ? { kind: "not_found" }
              : { kind: "confirmed", outRef: input.expectedOutRef };
          },
        },
        transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
      });
    // The same complete datum at the publisher's address is not the bound evidence.
    const wrong = await lucid
      .newTx()
      .pay.ToAddressWithData(
        account.address,
        { kind: "inline", value: publication.datumCbor },
        { lovelace: 70_000_000n },
      )
      .complete({ localUPLCEval: true });
    await (await wrong.sign.withWallet().complete()).submit();
    emulator.awaitBlock();
    const inspect = async () =>
      port().inspect({
        headerHash,
        baseAction: action,
        artifact: {},
        entries: [],
      });
    expect((await inspect()).kind).toBe("required");
    let oldOutRef: string | undefined;
    for (let attempt = 0; attempt < 2; attempt++) {
      const inspection = await inspect();
      if (inspection.kind !== "required")
        throw new Error("missing bound publication action");
      expect(inspection.action.input.publicationAddress).toBe(
        recipient.address,
      );
      const captured = await port().capture({
        headerHash,
        action: inspection.action,
        artifact: {},
      });
      const submitted = await captureEmulatorSubmission(emulator, () =>
        captured.transaction.signed.submit(),
      );
      confirmed.add(submitted.result);
      await lucid.awaitTx(submitted.result);
      expect(
        submitted.measurements[0]!.completeSignedBytes,
      ).toBeLessThanOrEqual(16_384);
      const recovered = structuredClone(captured.durableRecovery);
      const reconcile = (overrides = {}) =>
        port().reconcile({
          headerHash,
          action: inspection.action,
          artifact: {},
          txHash: captured.transaction.txHash,
          durableRecovery: recovered,
          ...overrides,
        });
      expect((await reconcile()).kind).toBe("confirmed");
      expect((await reconcile({ headerHash: "ff".repeat(28) })).kind).toBe(
        "conflict",
      );
      for (const input of [
        { ...inspection.action.input, publicationAddress: account.address },
        {
          ...inspection.action.input,
          sourceIdentity: { ...requirement.sourceIdentity, sourceKind: "1" },
        },
      ])
        expect(
          (await reconcile({ action: { ...inspection.action, input } })).kind,
        ).toBe("conflict");
      await expect(
        port().capture({
          headerHash: "ff".repeat(28),
          action: inspection.action,
          artifact: {},
        }),
      ).rejects.toThrow("header");
      const resolved = await port().resolveAuthenticated({
        headerHash,
        action,
        artifact: {},
      });
      expect(resolved.publications).toHaveLength(1);
      const utxo = resolved.publications[0]!;
      expect(utxo.address).toBe(recipient.address);
      expect(utxo.datum).toBe(publication.datumCbor);
      const outRef = `${utxo.txHash}#${utxo.outputIndex}`;
      expect(outRef).not.toBe(oldOutRef);
      expect((await inspect()).kind).toBe("satisfied");
      if (attempt === 0) {
        oldOutRef = outRef;
        lucid.selectWallet.fromSeed(recipient.seedPhrase);
        const unsigned = await lucid
          .newTx()
          .collectFrom([utxo])
          .pay.ToAddress(recipient.address, { lovelace: 2_000_000n })
          .complete({ localUPLCEval: true });
        await (await unsigned.sign.withWallet().complete()).submit();
        emulator.awaitBlock();
        expect((await reconcile()).kind).toBe("conflict");
        expect((await inspect()).kind).toBe("required");
      }
    }
  });

  it("reacquires an exact typed script-address publication when its old receipt is unavailable in a cold read view", async () => {
    const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, network);
    const signer: ResolvedProverSigner = {
      source: "bound-publication-unavailable-test",
      address: account.address,
      paymentKeyHash: getAddressDetails(account.address).paymentCredential!
        .hash,
      selectWallet: (target) =>
        target.selectWallet.fromSeed(account.seedPhrase),
    };
    signer.selectWallet(lucid);
    const address = credentialToAddress(network, {
      type: "Script",
      hash: "44".repeat(28),
    });
    const publication = deriveValidationProofItemPublication({
      transactionId: "11".repeat(32),
      transactionCommitment: "22".repeat(32),
      fieldPreimage: "33".repeat(14_336),
    });
    const requirement = createBoundDataPublicationRequirement({
      publicationAddress: address,
      datumCbor: publication.datumCbor,
      sourceIdentity: {
        headerHash,
        action,
        sourceKind: "0",
        fieldIndex: 2,
        transactionId: "11".repeat(32),
        transactionCommitment: "22".repeat(32),
      },
    });
    const unavailable = new Set<string>();
    const chainUtxosAt = lucid.utxosAt.bind(lucid);
    lucid.utxosAt = async (target) =>
      (await chainUtxosAt(target)).filter(
        (utxo) => !unavailable.has(`${utxo.txHash}#${utxo.outputIndex}`),
      );
    const port = () =>
      createAuthenticatedFieldCarriagePrerequisitePort({
        category: "validationTraceDispute",
        lucid,
        network,
        signer,
        requirementForAction: () => requirement,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (input) => {
            const found = (await lucid.utxosAt(input.address)).find(
              (utxo) =>
                `${utxo.txHash}#${utxo.outputIndex}` === input.expectedOutRef &&
                utxo.datum === input.expectedDatumCbor &&
                utxo.datumHash == null &&
                utxo.scriptRef == null &&
                Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
            );
            return found === undefined
              ? { kind: "not_found" }
              : { kind: "confirmed", outRef: input.expectedOutRef };
          },
        },
        transactionConfirmed: async () => true,
      });
    let previous: string | undefined;
    for (let attempt = 0; attempt < 2; attempt++) {
      const inspection = await port().inspect({
        headerHash,
        baseAction: action,
        artifact: {},
        entries: [],
      });
      if (inspection.kind !== "required")
        throw new Error("missing typed publication requirement");
      const captured = await port().capture({
        headerHash,
        action: inspection.action,
        artifact: {},
      });
      await captured.transaction.signed.submit();
      emulator.awaitBlock();
      // Reopening the port and JSON receipt discards all previous process selection.
      const durableRecovery = JSON.parse(
        JSON.stringify(captured.durableRecovery),
      );
      expect(
        (
          await port().reconcile({
            headerHash,
            action: inspection.action,
            artifact: {},
            txHash: captured.transaction.txHash,
            durableRecovery,
          })
        ).kind,
      ).toBe("confirmed");
      const authenticated = await port().resolveAuthenticated({
        headerHash,
        action,
        artifact: {},
      });
      const outRef = `${authenticated.publications[0]!.txHash}#${authenticated.publications[0]!.outputIndex}`;
      expect(outRef).not.toBe(previous);
      expect(authenticated.publications[0]!.address).toBe(address);
      expect(authenticated.publications[0]!.datum).toBe(publication.datumCbor);
      if (attempt === 0) {
        previous = outRef;
        // An unavailable receipt in the current read view is not a legal spend
        // of append-only proof-item evidence. Never consume this script output.
        unavailable.add(outRef);
        expect(
          (
            await port().reconcile({
              headerHash,
              action: inspection.action,
              artifact: {},
              txHash: captured.transaction.txHash,
              durableRecovery,
            })
          ).kind,
        ).toBe("conflict");
      }
    }
    expect(await chainUtxosAt(address)).toHaveLength(2);
  });

  it("refuses split-sized or noncanonical typed Data", () => {
    const address = generateEmulatorAccount({ lovelace: 1n }).address;
    const input = {
      publicationAddress: address,
      sourceIdentity: { headerHash, action },
    };
    expect(() =>
      createBoundDataPublicationRequirement({
        ...input,
        datumCbor: Data.to("aa".repeat(15_148)),
      }),
    ).toThrow("bounded");
    expect(() =>
      createBoundDataPublicationRequirement({ ...input, datumCbor: "1800" }),
    ).toThrow("canonical");
  });
});
