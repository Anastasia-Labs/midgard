import { setTimeout as pause } from "node:timers/promises";
import { inspect } from "node:util";

import { verifyReferenceScriptPublicationAuthority } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  inspectSignedTxValidityInterval,
  parseOutsideValidityIntervalDetails,
  resolveEarlyValidityRetry,
} from "../../src/transactions/utils.js";

// The user accepted the hard limit without reserve for these five roles only.
export const hardLimitAcceptedContracts = new Set<string>([
  "fraudProofValueNotPreservedStep02",
  "fraudProofResolvedOutputNonCanonicalStep04",
  "fraudProofExecutionSourceScriptDecodingStep02",
  "fraudProofReceivePurposeLanguageStep02",
  "fraudProofExecutionNativeScriptInvalidStep02",
]);

export type PublishedWorkflowDeploymentAccounts = Readonly<{
  operator: ReturnType<typeof generateEmulatorAccount>;
  publisher: ReturnType<typeof generateEmulatorAccount>;
  cosigner: ReturnType<typeof generateEmulatorAccount>;
}>;

/** Prepare test signers before producing release-bound funding profiles. */
export const createPublishedWorkflowDeploymentAccounts =
  (): PublishedWorkflowDeploymentAccounts => ({
    operator: generateEmulatorAccount({ lovelace: 200_000_000_000n }),
    publisher: generateEmulatorAccount({ lovelace: 4_000_000_000_000n }),
    cosigner: generateEmulatorAccount({ lovelace: 0n }),
  });

/**
 * Ordinary deployment only: every reference is published, the real atomic
 * initialization consumes its nonce, and every availability yield is registered.
 * Callers get a complete deployment for subsequent authenticated workflow tests.
 */
export type PublishedWorkflowChain = Readonly<{
  now: () => number;
  /** Elapsed polling delay; advances simulated slots in the emulator. */
  delaySlots: (slots: number) => void | Promise<void>;
  /** Wait until the canonical ledger reaches this absolute protocol time. */
  awaitLedgerTime: (targetUnixTimeMs: number) => void | Promise<void>;
  /** Canonical tip height; absent where finality has no block depth. */
  blockHeight?: () => Promise<number>;
}>;

export type PublishedWorkflowDeploymentResume = Readonly<{
  nonce: UTxO;
  authPolicy: SDK.ReferenceScriptAuthPolicy;
  publications: readonly {
    role: string;
    signedCbor: string;
    outRef: { txHash: string; outputIndex: number };
  }[];
  initializationCbor?: string;
}>;

/** Wait for an indexed canonical tip strictly beyond the authority's expiry. */
export const waitForPublicationAuthorityExpiry = async ({
  expiresAtSlot,
  synchronize,
  awaitSlot,
}: Readonly<{
  expiresAtSlot: number;
  synchronize: () => Promise<number>;
  awaitSlot: (slots: number) => void | Promise<void>;
}>): Promise<number> => {
  while (true) {
    // This barrier observes the canonical node tip and waits for its exact
    // indexed checkpoint. Local clocks and elapsed waits cannot close authority.
    const canonicalSlot = await synchronize();
    if (!Number.isSafeInteger(canonicalSlot) || canonicalSlot < 0)
      throw new Error(
        "Invalid canonical slot while closing publication authority",
      );
    if (canonicalSlot > expiresAtSlot) return canonicalSlot;
    await awaitSlot(Math.min(30, expiresAtSlot - canonicalSlot + 1));
  }
};

/** Publisher-authorized publication is ready for audit at inclusion. Historical
 * time-only policies still require expiry before their issuance is fixed. */
export const awaitReferenceScriptPublicationReadiness = async ({
  authPolicy,
  publisherAddress,
  synchronize,
  awaitSlot,
}: Readonly<{
  authPolicy: SDK.ReferenceScriptAuthPolicy;
  publisherAddress: string;
  synchronize: () => Promise<number>;
  awaitSlot: (slots: number) => void | Promise<void>;
}>) => {
  const metadata = SDK.referenceScriptAuthPolicyDeploymentInfo(authPolicy);
  const authority = verifyReferenceScriptPublicationAuthority({
    cborHex: metadata.nativeScript.cborHex,
    expiresAtSlot: metadata.nativeScript.expiresAtSlot,
    publisherAddress,
    postTimelockAuditRequired: metadata.postTimelockAudit.required,
  });
  const canonicalSlot =
    authority.kind === "time-only"
      ? await waitForPublicationAuthorityExpiry({
          expiresAtSlot: authPolicy.expiresAtSlot,
          synchronize,
          awaitSlot,
        })
      : await synchronize();
  if (!Number.isSafeInteger(canonicalSlot) || canonicalSlot < 0)
    throw new Error("Invalid canonical slot while auditing publication");
  return { canonicalSlot, authorityKind: authority.kind };
};

/** Persist initialization identity before submit; replay the exact signed bytes. */
export const submitPublishedInitialization = async ({
  lucid,
  nonce,
  signedCbor,
  onPrepared,
  synchronize,
  now = Date.now,
  waitForRetry = async (milliseconds) => {
    await pause(milliseconds);
  },
}: Readonly<{
  lucid: LucidEvolution;
  nonce: UTxO;
  signedCbor: string;
  onPrepared: (signedCbor: string) => void | Promise<void>;
  synchronize: () => Promise<number>;
  /** Chain wall clock; an emulator supplies its own time, never the host's. */
  now?: () => number;
  waitForRetry?: (milliseconds: number) => Promise<void>;
}>): Promise<string> => {
  const body = CML.Transaction.from_cbor_hex(signedCbor).body();
  if (
    !Array.from({ length: body.inputs().len() }, (_, index) =>
      body.inputs().get(index),
    ).some(
      (input) =>
        input.transaction_id().to_hex() === nonce.txHash &&
        Number(input.index()) === nonce.outputIndex,
    )
  )
    throw new Error(
      "Recorded initialization does not spend its deployment nonce",
    );
  const txHash = CML.hash_transaction(body).to_hex();
  await onPrepared(signedCbor);
  const validity = inspectSignedTxValidityInterval(signedCbor);
  const ttl = validity.invalidHereafterSlot;
  let retryCount = 0;
  let waitedMs = 0;
  let submissionFailure: unknown;
  while (true) {
    const canonicalSlot = await synchronize();
    const status = await lucid.transactionStatus(txHash);
    if (status.status === "confirmed") return txHash;
    if (
      ttl !== undefined &&
      (canonicalSlot >= ttl || now() >= lucid.slotToUnixTime(ttl))
    )
      throw new Error(
        "The recorded initialization validity interval expired before confirmation; reconcile before constructing a replacement",
      );
    if (
      status.status === "not_found" &&
      (await lucid.utxosByOutRef([nonce])).length !== 1
    )
      throw new Error(
        "Initialization nonce is spent without the recorded transaction on the canonical chain; reconcile before retrying",
      );
    let submittedHash: string | undefined;
    try {
      submittedHash = await lucid.config().provider!.submitTx(signedCbor);
    } catch (cause) {
      submissionFailure = cause;
    }
    if (submittedHash !== undefined) {
      if (submittedHash !== txHash)
        throw new Error(
          "Initialization submission hash differs from durable signed bytes",
        );
      submissionFailure = undefined;
      break;
    }
    const providerValidity =
      parseOutsideValidityIntervalDetails(submissionFailure);
    if (providerValidity === null) break;
    if (ttl === undefined || validity.invalidBeforeSlot === undefined)
      throw new Error(
        "Cannot recover a provider validity rejection without recorded initialization validity bounds",
      );
    // A structured rejection is known not to have entered the mempool. Use the
    // provider's current slot, while retaining the exact signed body's bounds.
    const retry = resolveEarlyValidityRetry(
      {
        currentSlot: providerValidity.currentSlot,
        invalidBeforeSlot: validity.invalidBeforeSlot,
        invalidHereafterSlot: ttl,
      },
      retryCount,
    );
    if (retry.status !== "wait" || waitedMs + retry.waitMs > 60_000)
      throw new Error(
        `Cannot safely retry recorded initialization validity: ${retry.status}`,
      );
    retryCount += 1;
    waitedMs += retry.waitMs;
    await waitForRetry(retry.waitMs);
  }
  // Lost acknowledgements and duplicate mempool submissions remain ambiguous
  // until the recorded hash is observed on the canonical chain.
  await lucid.awaitTx(txHash, 500).catch((cause) => {
    throw new Error(
      `Recorded initialization remains unresolved: ${inspect(submissionFailure ?? cause, { depth: 8 })}`,
    );
  });
  return txHash;
};
