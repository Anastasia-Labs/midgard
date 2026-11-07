import {
  CML,
  fromText,
  getAddressDetails,
  type LucidEvolution,
  mintingPolicyToId,
  type Script,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { MintingValidator } from "./common.js";
import { REFERENCE_SCRIPT_AUTH_TIMELOCK_MS } from "./reference-scripts.reference-script-auth-timelock-ms.js";
import { REFERENCE_SCRIPT_AUTH_TOKEN_NAMES } from "./reference-scripts.reference-script-auth-token-names.js";
import { StateQueueError } from "./state-queue.js";

export type ReferenceScriptAuthTokenTarget =
  keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES;

export type ReferenceScriptAuthPolicy = MintingValidator & {
  readonly expiresAtSlot: number;
  readonly expiresAtUnixTime: number;
  readonly timelockDurationMs: number;
};

export type ReferenceScriptAuthPolicyRef = Pick<
  ReferenceScriptAuthPolicy,
  "policyId"
>;

export type ReferenceScriptAuthMintingPolicy = MintingValidator & {
  readonly expiresAtUnixTime?: number;
};

export type ReferenceScriptAuthExpiredDiagnostic = {
  readonly policyId: string;
  readonly scopeName: string;
  readonly targetNames: readonly string[];
  readonly currentSlot: number;
  /** Absent for a policy without an expiry, which publication refuses. */
  readonly lastValidSlot?: number;
};

export type ReferenceScriptAuthPolicyDeploymentInfo = {
  readonly policyId: string;
  readonly nativeScript: {
    readonly type: "Native";
    readonly cborHex: string;
    readonly expiresAtSlot: number;
    readonly expiresAtUnixTime: number;
    readonly timelockDurationMs: number;
  };
  readonly tokenNames: Readonly<Record<ReferenceScriptAuthTokenTarget, string>>;
  readonly postTimelockAudit: {
    readonly required: boolean;
    readonly rule: string;
  };
};

export type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

/**
 * Release-bound Cardano transaction envelope used for reference-script
 * publication. A script body at or above this size cannot possibly fit once
 * the output, funding input, auth mint and signature are added.
 */
export const REFERENCE_SCRIPT_PUBLICATION_L1_MAX_TX_BYTES = 16_384;

/**
 * Fail-fast lower-bound admission for production reference-script
 * publication. This deliberately does not claim that a smaller raw body fits:
 * the completed, signed transaction remains the authoritative fit check.
 */
export const assertReferenceScriptRawBodiesFitL1Envelope = (
  targets: readonly ReferenceScriptTarget[],
  maxTxBytes = REFERENCE_SCRIPT_PUBLICATION_L1_MAX_TX_BYTES,
): void => {
  for (const target of targets) {
    const rawScriptBytes = target.script.script.length / 2;
    if (rawScriptBytes >= maxTxBytes) {
      throw new StateQueueError({
        message:
          `${target.name} raw script is ${rawScriptBytes.toString()} bytes, ` +
          `exceeding the ${maxTxBytes.toString()}-byte L1 transaction envelope ` +
          `by at least ${(rawScriptBytes - maxTxBytes).toString()} bytes`,
        cause: "reference_script_raw_body_exceeds_l1_envelope_v1",
      });
    }
  }
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type ReferenceScriptWalletReplenishmentTxParams = {
  readonly lucid: LucidEvolution;
  /** The funding wallet's view, which coin selection picks from (never empty). */
  readonly fundingWalletInputs: readonly UTxO[];
  readonly referenceScriptAddress: string;
  readonly topUpAmount: bigint;
};

export type ReferenceScriptPublicationTxParams = {
  readonly lucid: LucidEvolution;
  readonly selectedFundingInputs: readonly UTxO[];
  readonly walletAddress: string;
  readonly referenceScriptsAddress: string;
  readonly missingTargets: readonly ReferenceScriptTarget[];
  readonly authPolicy: ReferenceScriptAuthMintingPolicy;
};

export type ReferenceScriptPublicationLayout = {
  readonly localReferenceOutputs: ReadonlyMap<string, Omit<UTxO, "txHash">>;
  readonly walletOutputs: readonly Omit<UTxO, "txHash">[];
};

export type BuiltReferenceScriptPublicationTx = {
  readonly tx: TxSignBuilder;
  readonly layout: ReferenceScriptPublicationLayout;
};

export type TxCompleteOptions = NonNullable<
  Parameters<TxBuilder["complete"]>[0]
>;

export const REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS = 5 * 60_000;

export const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;

export const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n;

export const referenceScriptAuthTokenNameText = (
  targetName: string,
): string => {
  const tokenName =
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[
      targetName as ReferenceScriptAuthTokenTarget
    ];
  if (tokenName === undefined) {
    throw new Error(`Missing reference-script auth token name: ${targetName}`);
  }
  return tokenName;
};

export const referenceScriptAuthTokenName = (targetName: string): string =>
  fromText(referenceScriptAuthTokenNameText(targetName));

export const referenceScriptAuthUnit = (
  policyId: string,
  targetName: string,
): string => toUnit(policyId, referenceScriptAuthTokenName(targetName));

export const createReferenceScriptAuthPolicy = async (
  lucid: LucidEvolution,
  nowMs: number = Date.now(),
  timelockDurationMs: number = REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
): Promise<ReferenceScriptAuthPolicy> => {
  const { paymentCredential } = getAddressDetails(
    await lucid.wallet().address(),
  );
  if (paymentCredential?.type !== "Key") {
    throw new Error(
      "Reference-script publisher must have a payment key credential",
    );
  }
  const expiresAtUnixTime = nowMs + timelockDurationMs;
  const expiresAtSlot = lucid.unixTimeToSlot(expiresAtUnixTime);
  const conditions = CML.NativeScriptList.new();
  conditions.add(
    CML.NativeScript.new_script_pubkey(
      CML.Ed25519KeyHash.from_hex(paymentCredential.hash),
    ),
  );
  conditions.add(
    CML.NativeScript.new_script_invalid_hereafter(BigInt(expiresAtSlot)),
  );
  const nativeScript = CML.NativeScript.new_script_all(conditions);
  const mintingScript: Script = {
    type: "Native",
    script: nativeScript.to_cbor_hex(),
  };
  return {
    mintingScriptCBOR: mintingScript.script,
    policyId: mintingPolicyToId(mintingScript),
    mintingScript,
    expiresAtSlot,
    expiresAtUnixTime,
    timelockDurationMs,
  };
};

export const referenceScriptAuthPolicyFromDeploymentInfo = (
  deploymentInfo: Pick<
    ReferenceScriptAuthPolicyDeploymentInfo,
    "policyId" | "nativeScript"
  >,
): ReferenceScriptAuthPolicy => ({
  mintingScriptCBOR: deploymentInfo.nativeScript.cborHex,
  mintingScript: {
    type: "Native",
    script: deploymentInfo.nativeScript.cborHex,
  },
  policyId: deploymentInfo.policyId,
  expiresAtSlot: deploymentInfo.nativeScript.expiresAtSlot,
  expiresAtUnixTime: deploymentInfo.nativeScript.expiresAtUnixTime,
  timelockDurationMs: deploymentInfo.nativeScript.timelockDurationMs,
});

/**
 * The slot from which no transaction minting under `policy` can be valid:
 * publication clamps every transaction's upper bound to the slot before the
 * policy's expiry slot, and a transaction is valid only before its upper
 * bound. Undefined for a policy without an expiry.
 */
export const referenceScriptAuthLastValidSlot = (
  lucid: LucidEvolution,
  policy: ReferenceScriptAuthMintingPolicy,
): number | undefined =>
  policy.expiresAtUnixTime === undefined
    ? undefined
    : lucid.unixTimeToSlot(policy.expiresAtUnixTime) - 1;

/** Whether no transaction can mint under `policy` any more. */
export const referenceScriptAuthExpired = (
  lucid: LucidEvolution,
  policy: ReferenceScriptAuthMintingPolicy,
): boolean => {
  const lastValidSlot = referenceScriptAuthLastValidSlot(lucid, policy);
  return lastValidSlot === undefined || lucid.currentSlot() >= lastValidSlot;
};

export class ReferenceScriptAuthExpiredError extends Error {
  readonly diagnostic: ReferenceScriptAuthExpiredDiagnostic;

  constructor(diagnostic: ReferenceScriptAuthExpiredDiagnostic) {
    super(
      diagnostic.lastValidSlot === undefined
        ? `Reference-script auth policy ${diagnostic.policyId} has no expiry, so ${diagnostic.scopeName} cannot publish under it (targets: ${diagnostic.targetNames.join(", ")})`
        : `Reference-script auth policy ${diagnostic.policyId} expired at slot ${diagnostic.lastValidSlot.toString()} (now slot ${diagnostic.currentSlot.toString()}), so ${diagnostic.scopeName} can no longer publish under it (targets: ${diagnostic.targetNames.join(", ")})`,
    );
    this.name = "ReferenceScriptAuthExpiredError";
    this.diagnostic = diagnostic;
  }
}

/** Refuses once no transaction can mint under `policy` any more. */
export const assertReferenceScriptAuthUnexpired = ({
  lucid,
  policy,
  scopeName,
  targetNames,
}: {
  readonly lucid: LucidEvolution;
  readonly policy: ReferenceScriptAuthMintingPolicy;
  readonly scopeName: string;
  readonly targetNames: readonly string[];
}): void => {
  if (referenceScriptAuthExpired(lucid, policy)) {
    const lastValidSlot = referenceScriptAuthLastValidSlot(lucid, policy);
    throw new ReferenceScriptAuthExpiredError({
      policyId: policy.policyId,
      scopeName,
      targetNames,
      currentSlot: lucid.currentSlot(),
      ...(lastValidSlot === undefined ? {} : { lastValidSlot }),
    });
  }
};

export const referenceScriptAuthPolicyDeploymentInfo = (
  policy: ReferenceScriptAuthPolicy,
): ReferenceScriptAuthPolicyDeploymentInfo => {
  if (policy.mintingScript.type !== "Native") {
    throw new Error("Reference-script auth policy must be a native script");
  }
  const nativeScript = CML.NativeScript.from_cbor_hex(
    policy.mintingScript.script,
  );
  const conditions = nativeScript.as_script_all()?.native_scripts();
  const publisherAuthorized =
    conditions?.len() === 2 &&
    conditions.get(0).as_script_pubkey() !== undefined &&
    conditions.get(1).as_script_invalid_hereafter()?.after() ===
      BigInt(policy.expiresAtSlot);
  return {
    policyId: policy.policyId,
    nativeScript: {
      type: "Native",
      cborHex: policy.mintingScript.script,
      expiresAtSlot: policy.expiresAtSlot,
      expiresAtUnixTime: policy.expiresAtUnixTime,
      timelockDurationMs: policy.timelockDurationMs,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: {
      required: !publisherAuthorized,
      rule: publisherAuthorized
        ? "After publication confirms, verify exactly one role token per listed token name and its expected reference script. The publisher payment key is trusted not to authorize additional minting until policy expiry."
        : "After the timelock expires, verify there is exactly one role token under this policy for every listed token name before treating the deployment as production-ready.",
    },
  };
};
