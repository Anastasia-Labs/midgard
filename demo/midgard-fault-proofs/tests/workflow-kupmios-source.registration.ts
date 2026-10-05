import "./workflow-kupmios-source.production-signed-intent-recovery-through-concrete-kupo-ogmios-transports.js";

import { expect, it, vi } from "vitest";

import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/cursor-family-adapter.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import {
  LocalKupmiosCheckpointChangedError,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
} from "../src/workflow/index.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/linear-family-adapter.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import type { SignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
import {
  DEPLOYMENT,
  hash,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { signedRecoveryFixture } from "./workflow-kupmios-source.signed-recovery-fixture.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

it.each([
  [{ referenceSpent: "stable", scriptOrdinary: true }, "not_found"],
  [{ referenceSpent: "volatile", scriptOrdinary: true }, "pending"],
  [
    { referenceSpent: "stable", scriptOrdinary: true, spent: true },
    "not_found",
  ],
  [
    { referenceSpent: "stable", scriptOrdinary: true, scriptCollateral: true },
    "not_found",
  ],
  [
    { referenceSpent: "stable", scriptOrdinary: true, missing: true },
    "unknown",
  ],
  [{ referenceSpent: "stable", keyCollateral: true }, "not_found"],
  [{ referenceSpent: "volatile", keyCollateral: true }, "pending"],
  [{ referenceSpent: "stable" }, "not_found"],
  [{ referenceSpent: "stable", ttl: null }, "not_found"],
  [{ referenceSpent: "volatile", spent: true }, "pending"],
  [{ referenceSpent: "volatile" }, "pending"],
  [{ referenceSpent: "stable", spent: true }, "not_found"],
  [{ referenceSpent: "stable", missing: true }, "unknown"],
  [{ ttl: 399 }, "not_found"],
  [{ missing: true }, "unknown"],
  [{ ttl: null }, "pending"],
  [{ ttl: null, mempoolPresent: true }, "pending"],
  [{ ttl: null, missing: true }, "unknown"],
  [{ ttl: null, included: true }, "pending"],
  [{ ttl: null, spent: true }, "not_found"],
  [{ spent: true }, "not_found"],
  [{}, "pending"],
] as const)(
  "cursor and linear production adapters reconcile real signed source evidence: %j",
  async (options, outcome) => {
    for (const family of ["cursor", "linear"] as const) {
      const fixture = await signedRecoveryFixture(options);
      const protocolRemoval =
        "scriptOrdinary" in options && options.scriptOrdinary;
      const proofOutRef = `${hash(78)}#0`;
      const target = fixture.reference ?? fixture.funding;
      let currentHeaderOutRef = `${target.txHash}#${target.outputIndex}`;
      const baseL1 = {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (): Promise<never> => {
            throw new Error("unused publication observer");
          },
        },
        observeHeader: async (): Promise<never> => {
          throw new Error("unused header observer");
        },
        transactionConfirmed: async () => false,
        observe: async () => ({
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "local-kupmios",
            grade: "security",
          } as const,
          stage: protocolRemoval
            ? ({
                kind: "proof_token",
                fraudProofOutRef: proofOutRef,
                stateQueueBlockOutRef: currentHeaderOutRef,
                nextRemovalOutRef: currentHeaderOutRef,
              } as const)
            : ({
                kind: "not_started",
                stateQueueBlockOutRef: currentHeaderOutRef,
              } as const),
        }),
        observeSignedTransaction: (input: SignedWorkflowTransaction) =>
          readAdmittedLocalKupmiosSignedTransactionRecovery({
            ...input,
            source: fixture.source,
          }),
        rebroadcastSignedTransaction: (
          input: SignedWorkflowTransaction & {
            authorizeResubmission: (
              input: SignedWorkflowTransaction,
            ) => Promise<void>;
          },
        ) =>
          rebroadcastAdmittedLocalKupmiosSignedTransaction({
            ...input,
            source: fixture.source,
          }),
      };
      const prepare = async (): Promise<never> => {
        throw new Error("recovery must not prepare new evidence");
      };
      const capture = async (): Promise<never> => {
        throw new Error("recovery must not rebuild signed transaction");
      };
      const stateQueueMutationLeaseCoordinator = {
        acquire: async (): Promise<never> => {
          throw new Error("unused lease");
        },
      };
      const adapter =
        family === "cursor"
          ? createCursorFamilyWorkflowAdapter({
              spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
              l1: {
                ...baseL1,
                category: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC.category,
              },
              transactions: {
                portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
                category: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC.category,
                prepare,
                capture,
              },
              stateQueueMutationLeaseCoordinator,
            })
          : createLinearFamilyWorkflowAdapter({
              category: "daHashPreimage",
              l1: { ...baseL1, category: "daHashPreimage" },
              transactions: {
                portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
                category: "daHashPreimage",
                prepare,
                capture,
              },
              stateQueueMutationLeaseCoordinator,
            });
      const context = {
        identity: {
          schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
          deploymentFingerprint: DEPLOYMENT,
          category: adapter.category,
          target: { kind: "state_queue_header", headerHash: "aa".repeat(28) },
        } as const,
        workflowId: hash(88),
        artifact: {},
        entries: [],
      };
      const observed = await adapter.observe(context);
      if (observed.kind !== "action_required")
        throw new Error("Expected predecessor action");
      // A DA attachment or linked-list update recreates the same HeaderV1
      // while its signed init/removal still uses the old exact output.
      if (fixture.reference !== undefined)
        currentHeaderOutRef = `${hash(77)}#0`;
      const authorizeResubmission = vi.fn(async () => {});
      expect(
        (
          await adapter.reconcile({
            ...context,
            action: observed.action,
            txHash: fixture.input.transactionHash,
            signedTransactionCborHex: fixture.input.signedTransactionCborHex,
            authorizeResubmission,
          })
        ).kind,
      ).toBe(outcome);
      if (protocolRemoval && outcome === "not_found") {
        const replacement = await adapter.observe(context);
        if (replacement.kind !== "action_required")
          throw new Error("missing replacement removal");
        expect(replacement.action.actionId).not.toBe(observed.action.actionId);
        expect(replacement.action.input).toMatchObject({
          stage: "remove",
          fraudProofOutRef: proofOutRef,
          stateQueueBlockOutRef: currentHeaderOutRef,
          nextRemovalOutRef: currentHeaderOutRef,
        });
      }
      if (
        Object.keys(options).length === 0 ||
        ("ttl" in options &&
          options.ttl === null &&
          Object.keys(options).length === 1)
      ) {
        expect(authorizeResubmission).toHaveBeenCalledOnce();
        expect(fixture.submissions).toEqual([
          fixture.input.signedTransactionCborHex,
        ]);
      } else expect(fixture.submissions).toEqual([]);
    }
  },
);

it("restarts at most three complete boundary captures after typed head changes", async () => {
  for (const changes of [1, 2, 3]) {
    const fixture = sourceFixture();
    const readBoundary = fixture.source.readBoundary.bind(fixture.source);
    let remaining = changes;
    const capture = vi
      .spyOn(fixture.source, "readBoundary")
      .mockImplementation(async () => {
        if (remaining > 0) {
          remaining -= 1;
          throw new LocalKupmiosCheckpointChangedError(
            "Kupo advanced or rolled back during raw snapshot capture: test",
          );
        }
        return await readBoundary();
      });
    const result = readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    if (changes < 3)
      await expect(result).resolves.toMatchObject({ confirmationDepth: 30 });
    else
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosCheckpointChangedError,
      );
    expect(capture).toHaveBeenCalledTimes(Math.min(changes + 1, 3));
  }
});

it("propagates a boundary failure that is not a typed head change unchanged", async () => {
  const fixture = sourceFixture();
  const failure = new Error("ordinary boundary transport failure");
  const capture = vi
    .spyOn(fixture.source, "readBoundary")
    .mockRejectedValue(failure);
  await expect(
    readAdmittedLocalKupmiosBoundary({ source: fixture.source }),
  ).rejects.toBe(failure);
  expect(capture).toHaveBeenCalledOnce();
});

it("restarts at most three complete signed-recovery captures after typed head changes", async () => {
  for (const changes of [2, 3]) {
    const fixture = await signedRecoveryFixture({
      ttl: 399,
      captureHeadChanges: changes,
    });
    const capture = vi.spyOn(fixture.source, "readBoundary");
    const result = readAdmittedLocalKupmiosSignedTransactionRecovery(
      fixture.input,
    );
    if (changes === 2)
      await expect(result).resolves.toMatchObject({ status: "expired" });
    else
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosCheckpointChangedError,
      );
    expect(capture).toHaveBeenCalledTimes(changes === 2 ? 4 : 3);
    expect(fixture.submissions).toEqual([]);
  }
});
